module Pantry.Archive
  ( fetchArchive
  ) where

import Pantry.Import
import Pantry.FileTree
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Simple
import qualified RIO.Text as T
import Data.Conduit.Zlib (ungzip)

-- FIXME check if it's a tarball, compressed tarball, zip, etc
fetchArchive
  :: HasPantryBackend env
  => String -- ^ URL to download from
  -> FilePath -- ^ subdirectory to grab
  -> RIO env (FileTreeKey, FileTree)
fetchArchive url subdir = do
  req <- parseUrlThrow url
  httpSink req $ \_res -> ungzip .| treeFromTarball (T.stripPrefix $ T.pack subdir)
