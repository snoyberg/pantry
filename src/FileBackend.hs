{-# LANGUAGE NoImplicitPrelude #-}
module FileBackend where

import Import
import System.IO.Error (isDoesNotExistError)
import RIO.FilePath (takeDirectory, (</>))
import RIO.Directory (createDirectoryIfMissing, doesFileExist)
import RIO.List (splitAt)

filePantryBackend
  :: FilePath -- ^ root dir
  -> PantryBackend
filePantryBackend root = PantryBackend
  { pbStoreBlob = \blobKey bs -> do
      let fp = mkBlobFP blobKey
      createDirectoryIfMissing True $ takeDirectory fp
      -- FIXME do a cautious write
      unlessM (doesFileExist fp) (writeFileBinary fp bs)
  , pbLoadBlob = \blobKey -> do
      let fp = mkBlobFP blobKey
      (Just <$> readFileBinary fp) `catch` \e ->
        if isDoesNotExistError e
          then pure Nothing
          else throwIO e
  }
  where
    mkBlobFP key =
      let (x, y) = splitAt 2 $ blobKeyString key
       in root </> "blob" </> x </> y
