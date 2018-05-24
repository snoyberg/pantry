{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FileTree where

import Import
import Conduit
import Data.Conduit.Tar
import Data.Bits ((.&.))
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
      (if fteExecutable fte then "X\n" else "\n") <>
      fromString (blobKeyString $ fteBlobKey fte) <> "\n"

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
  tree <- FileTree <$> readIORef ref
  key <- lift $ storeFileTree tree
  pure (key, tree)
  where
    worker ref fi = do
      case fileType fi of
        FTNormal -> do
          bs <- BL.toStrict <$> sinkLazy
          let exe = fileMode fi .&. 0o100 /= 0
          patht <- either throwIO pure $ decodeUtf8' $ filePath fi
          path <- either (throwString . T.unpack) pure $ mkSafeFilePath $ T.unpack patht
          key <- lift $ storeBlob bs
          let fte = FileTreeEntry
                { fteExecutable = exe
                , fteBlobKey = key
                }
          modifyIORef' ref $ \m ->
            case Map.lookup path m of
              Just _ -> error $ "Path appears twice in tarball, that's just crazy: " ++ show patht
              Nothing -> Map.insert path fte m
        FTDirectory -> pure ()
        _ -> error $ "Unsupported tar entry: " ++ show fi