{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process
import RIO.Orphans
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Hash

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsRoot :: !FilePath
  , optionsTarball :: !FilePath
  , optionsSqlite :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appPantryBackend :: !PantryBackend
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

newtype BlobKey = BlobKey (Digest SHA256)

blobKeyString :: BlobKey -> String
blobKeyString (BlobKey digest) = show digest

data PantryBackend = PantryBackend
  { pbStoreBlob :: !(BlobKey -> ByteString -> IO ())
  , pbLoadBlob :: !(BlobKey -> IO (Maybe ByteString))
  }
instance Semigroup PantryBackend where
  pb1 <> pb2 = PantryBackend
    { pbStoreBlob = \key bs ->
        pbStoreBlob pb1 key bs *>
        pbStoreBlob pb2 key bs
    , pbLoadBlob = \key -> do
        mbs <- pbLoadBlob pb1 key
        case mbs of
          Nothing -> pbLoadBlob pb2 key
          Just _ -> pure mbs
    }

class HasPantryBackend env where
  pantryBackendL :: Lens' env PantryBackend
instance HasPantryBackend PantryBackend where
  pantryBackendL = id
instance HasPantryBackend App where
  pantryBackendL = lens appPantryBackend (\x y -> x { appPantryBackend = y })

storeBlob :: HasPantryBackend env => ByteString -> RIO env BlobKey
storeBlob bs = do
  let key = BlobKey $ Hash.hash bs
  pb <- view pantryBackendL
  liftIO $ pbStoreBlob pb key bs
  pure key

loadBlob :: HasPantryBackend env => BlobKey -> RIO env (Maybe ByteString)
loadBlob key = do
  pb <- view pantryBackendL
  liftIO $ pbLoadBlob pb key
