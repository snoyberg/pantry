module Pantry.Unpack
  ( unpackTree
  ) where

import Pantry.Import
import qualified RIO.Map as Map
import RIO.Directory
import RIO.FilePath
import qualified RIO.Text as T

unpackTree
  :: HasPantryBackend env
  => FileTreeKey -- ^ tree to unpack
  -> FilePath -- ^ dest directory
  -> RIO env ()
unpackTree key dest = do
  mtree <- loadFileTree key
  FileTree tree <-
    case mtree of
      Just tree -> pure tree
      Nothing -> throwPantry $ "Tried to unpack unknown tree " <> display key
  forM_ (Map.toList tree) $ \(fp, treeEntry) -> do
    let destfp = dest </> T.unpack (unSafeFilePath fp)
    case treeEntry of
      FTEExecutable blob -> do
        unpackBlob blob destfp
        -- FIXME make it executable
      FTENormal blob -> unpackBlob blob destfp
      FTELink sfp -> do
        createDirectoryIfMissing True $ takeDirectory destfp
        throwPantry "FIXME make symlinks"

unpackBlob
  :: HasPantryBackend env
  => BlobKey -- ^ blob to unpack
  -> FilePath -- ^ dest file
  -> RIO env ()
unpackBlob key dest = do
  mbs <- loadBlob key
  bs <-
    case mbs of
      Nothing -> throwPantry $ "Blob not found: " <> display key
      Just bs -> pure bs
  createDirectoryIfMissing True $ takeDirectory dest
  writeFileBinary dest bs
