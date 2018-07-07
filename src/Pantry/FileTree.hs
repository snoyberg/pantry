module Pantry.FileTree
  ( treeFromTarball
  ) where

import Pantry.Import
import Conduit
import Data.Conduit.Tar
import Data.Bits ((.&.))
import RIO.FilePath (takeDirectory, (</>))
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import qualified RIO.Map as Map
import Data.ByteString.Builder (toLazyByteString)

-- | Render a file tree into its binary representation and a key (hash
-- of that binary representation).
renderFileTree :: FileTree -> (FileTreeKey, ByteString)
renderFileTree (FileTree m) =
  (mkFileTreeKey bs, bs)
  where
    bs = BL.toStrict $ toLazyByteString $ getUtf8Builder $ foldMap go (Map.toList m)

    go (unSafeFilePath -> path, fte) =
      display path <> "\n" <>
      (case fte of
         FTEExecutable key' -> "X\n" <> display key' <> "\n"
         FTENormal key' -> "N\n" <> display key' <> "\n"
         FTELink (link') -> "L\n" <> display link' <> "\n")

storeFileTree
  :: HasPantryBackend env
  => FileTree
  -> RIO env FileTreeKey
storeFileTree tree = liftPB $ \pb -> do
  let (key, bs) = renderFileTree tree
   in key <$ pbStoreFileTree pb key tree bs

treeFromTarball
  :: HasPantryBackend env
  => (Text -> Maybe Text) -- ^ modify filepath
  -> ConduitM ByteString o (RIO env) (FileTreeKey, FileTree)
treeFromTarball modFilePath = do
  ref <- newIORef mempty
  untar (worker ref)
  tree' <- readIORef ref
  tree <- fmap FileTree $ forM tree' $ \e ->
    case e of
      Left sfp ->
        case Map.lookup sfp tree' of
          -- FIXME ditch the lookup logic here
          Nothing -> throwPantry $ "Symbolic link destination not found: " <> display sfp <> "\n" <> displayShow (Map.keys tree')
          Just (Left _) -> throwPantry $ "We don't support multiple levels of symbolic links: " <> displayShow sfp
          Just (Right fte) -> pure fte
      Right fte -> pure fte
  key <- lift $ storeFileTree tree
  pure (key, tree)
  where
    worker ref fi = do
      patht <- either throwIO pure $ decodeUtf8' $ filePath fi
      forM_ (modFilePath patht) $ \patht' -> do
        path <- either (throwString . T.unpack) pure $ mkSafeFilePath $ T.unpack $ T.dropPrefix "/" patht'
        case fileType fi of
          FTNormal -> do
            bs <- BL.toStrict <$> sinkLazy
            let exe = fileMode fi .&. 0o100 /= 0
            key <- lift $ storeBlob bs
            let fte
                  | exe = FTEExecutable key
                  | otherwise = FTENormal key
            m <- readIORef ref
            case Map.lookup path m of
              Nothing -> writeIORef ref $! Map.insert path (Right fte) m
              -- Probably _someone_ will intentionally upload broken
              -- tarballs to screw with this code in the future. Likely
              -- not worth running this check. Unless we want to just
              -- ban some packages from Hackage...
              --
              -- OK, better option: some packages will require
              -- downloading the tarball directly.
              Just _ -> throwPantry $ "Path appears twice in tarball, that's just crazy: " <> displayShow patht
          FTDirectory -> pure ()
          FTSymbolicLink dest -> do
            destT <- either throwIO pure $ decodeUtf8' dest
            destSFP <- either (throwString . T.unpack) pure $ mkSafeFilePath $ takeDirectory (T.unpack patht) </> T.unpack destT
            m <- readIORef ref
            case Map.lookup path m of
              Just _ -> throwPantry $ "Path appears twice in tarball, that's just crazy: " <> displayShow patht
              Nothing -> writeIORef ref $! Map.insert path (Left destSFP) m
          _ -> throwPantry $ "Unsupported tar entry: " <> displayShow fi
