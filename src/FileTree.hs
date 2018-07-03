{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FileTree where

import Import
import Conduit
import Data.Conduit.Tar
import Data.Bits ((.&.))
import RIO.FilePath (takeDirectory, (</>))
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Crypto.Hash

renderFileTree :: FileTree -> (FileTreeKey, ByteString)
renderFileTree (FileTree m) =
  (FileTreeKey $ Crypto.Hash.hash bs, bs)
  where
    bs = encodeUtf8 $ utf8BuilderToText $ foldMap go (Map.toList m)

    go (SafeFilePath path, fte) =
      display path <> "\n" <>
      (case fte of
         FTEExecutable key' -> "X\n" <> fromString (blobKeyString key') <> "\n"
         FTENormal key' -> "N\n" <> fromString (blobKeyString key') <> "\n"
         FTELink (SafeFilePath link') -> "L\n" <> display link' <> "\n")

storeFileTree :: HasPantryBackend env => FileTree -> RIO env FileTreeKey
storeFileTree tree = do
  let (key, bs) = renderFileTree tree
  pb <- view pantryBackendL
  liftIO $ pbStoreFileTree pb key tree bs
  pure key

treeFromTarball
  :: HasPantryBackend env
  => ConduitM ByteString o (RIO env) (FileTreeKey, FileTree)
treeFromTarball = do
  ref <- newIORef mempty
  untar (worker ref)
  tree' <- readIORef ref
  tree <- fmap FileTree $ forM tree' $ \e ->
    case e of
      Left sfp ->
        case Map.lookup sfp tree' of
          Nothing -> error $ "Symbolic link destination not found: " ++ show sfp ++ "\n" ++ show (Map.keys tree')
          Just (Left _) -> error $ "We don't support multiple levels of symbolic links: " ++ show sfp
          Just (Right fte) -> pure fte
      Right fte -> pure fte
  key <- lift $ storeFileTree tree
  pure (key, tree)
  where
    worker ref fi = do
      patht <- either throwIO pure $ decodeUtf8' $ filePath fi
      path <- either (throwString . T.unpack) pure $ mkSafeFilePath $ T.unpack patht
      case fileType fi of
        FTNormal -> do
          bs <- BL.toStrict <$> sinkLazy
          let exe = fileMode fi .&. 0o100 /= 0
          key <- lift $ storeBlob bs
          let fte
                | exe = FTEExecutable key
                | otherwise = FTENormal key
          modifyIORef' ref $ \m ->
            case Map.lookup path m of
              Just _ -> error $ "Path appears twice in tarball, that's just crazy: " ++ show patht
              Nothing -> Map.insert path (Right fte) m
        FTDirectory -> pure ()
        FTSymbolicLink dest -> do
          destT <- either throwIO pure $ decodeUtf8' dest
          destSFP <- either (throwString . T.unpack) pure $ mkSafeFilePath $ takeDirectory (T.unpack patht) </> T.unpack destT
          modifyIORef' ref $ \m ->
            case Map.lookup path m of
              Just _ -> error $ "Path appears twice in tarball, that's just crazy: " ++ show patht
              Nothing -> Map.insert path (Left destSFP) m
        _ -> error $ "Unsupported tar entry: " ++ show fi
