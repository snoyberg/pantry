module Pantry.Types
  ( PantryBackend (..)
  , HasPantryBackend (..)
  , storeBlob
  , loadBlob
  , loadFileTree
  , loadPackageSource
  , BlobKey
  , FileTreeEntry (..)
  , blobKeyText
  , blobKeyFromText
  , FileTreeKey
  , mkFileTreeKey
  , fileTreeKeyText
  , fileTreeKeyFromText
  , SafeFilePath
  , mkSafeFilePath
  , unSafeFilePath
  , FileTree (..)
  , liftPB
  , PackageSource (..)
  , Archive (..)
  , PackageInfo (..)
  ) where

import RIO hiding (pi)
import RIO.Process
import RIO.Orphans ()
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Hash
import qualified RIO.Text as T
import Data.ByteArray.Encoding

newtype BlobKey = BlobKey (Digest SHA256)

blobKeyFromText :: Text -> Maybe BlobKey
blobKeyFromText t =
  case convertFromBase Base16 $ encodeUtf8 t of
    Left _ -> Nothing
    Right bs -> BlobKey <$> Hash.digestFromByteString (bs :: ByteString)

blobKeyText :: BlobKey -> Text
blobKeyText (BlobKey digest) = T.pack (show digest)

instance Display BlobKey where
  display = display . blobKeyText

newtype FileTreeKey = FileTreeKey (Digest SHA256)

mkFileTreeKey :: ByteString -> FileTreeKey
mkFileTreeKey = FileTreeKey . Hash.hash

fileTreeKeyText :: FileTreeKey -> Text
fileTreeKeyText (FileTreeKey digest) = T.pack (show digest)

fileTreeKeyFromText :: Text -> Maybe FileTreeKey
fileTreeKeyFromText t =
  case convertFromBase Base16 $ encodeUtf8 t of
    Left _ -> Nothing
    Right bs -> FileTreeKey <$> Hash.digestFromByteString (bs :: ByteString)

instance Display FileTreeKey where
  display = display . fileTreeKeyText

newtype SafeFilePath = SafeFilePath Text
  deriving (Show, Eq, Ord)
instance Display SafeFilePath where
  display (SafeFilePath t) = display t

mkSafeFilePath :: Text -> Either Text SafeFilePath
mkSafeFilePath t =
  if T.any (\c -> c == '\0' || c == '\n') t
    then Left $ T.pack $ "Invalid SafeFilePath: " ++ show t
    else Right $ SafeFilePath t

unSafeFilePath :: SafeFilePath -> Text
unSafeFilePath (SafeFilePath t) = t

newtype FileTree = FileTree (Map SafeFilePath FileTreeEntry)

data FileTreeEntry
  = FTEExecutable !BlobKey
  | FTENormal !BlobKey
  | FTELink !SafeFilePath

data PackageSource
  = PSArchive !Archive

data Archive = Archive
  { archiveUrl :: !Text
  , archiveSubdir :: !Text
  }

data PackageInfo = PackageInfo
  { piTree :: !FileTreeKey
  , piCabalFile :: !BlobKey
  }

data PantryBackend = PantryBackend
  { pbStoreBlob :: !(BlobKey -> ByteString -> RIO PB ())
  , pbLoadBlob :: !(BlobKey -> RIO PB (Maybe ByteString))
  , pbStoreFileTree :: !(FileTreeKey -> FileTree -> ByteString -> RIO PB ())
  , pbLoadFileTree :: !(FileTreeKey -> RIO PB (Maybe FileTree))
  , pbStorePackageSource :: !(PackageSource -> PackageInfo -> RIO PB ())
  , pbLoadPackageSource :: !(PackageSource -> RIO PB (Maybe PackageInfo))
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
    , pbStoreFileTree = \key tree bs ->
        pbStoreFileTree pb1 key tree bs *>
        pbStoreFileTree pb2 key tree bs
    , pbLoadFileTree = \key -> do
        mtree <- pbLoadFileTree pb1 key
        case mtree of
          Nothing -> pbLoadFileTree pb2 key
          Just _ -> pure mtree
    , pbStorePackageSource = \ps pi ->
        pbStorePackageSource pb1 ps pi *>
        pbStorePackageSource pb2 ps pi
    , pbLoadPackageSource = \ps -> do
        mpi <- pbLoadPackageSource pb1 ps
        case mpi of
          Nothing -> pbLoadPackageSource pb2 ps
          Just _ -> pure mpi
    }

-- FIXME It may be a messy shortcut to place these superclasses here...
class (HasLogFunc env, HasProcessContext env) => HasPantryBackend env where
  pantryBackendL :: Lens' env PantryBackend

data PB = PB
  { pblf :: !LogFunc
  , pbpc :: !ProcessContext
  }
instance HasLogFunc PB where
  logFuncL = lens pblf (\x y -> x { pblf = y })
instance HasProcessContext PB where
  processContextL = lens pbpc (\x y -> x { pbpc = y })

liftPB
  :: HasPantryBackend env
  => (PantryBackend -> RIO PB a)
  -> RIO env a
liftPB f = do
  lf <- view logFuncL
  pc <- view processContextL
  pb <- view pantryBackendL
  runRIO (PB lf pc) $ f pb

storeBlob
  :: HasPantryBackend env
  => ByteString
  -> RIO env BlobKey
storeBlob bs = liftPB $ \pb ->
  let key = BlobKey $ Hash.hash bs
   in key <$ pbStoreBlob pb key bs

loadBlob
  :: HasPantryBackend env
  => BlobKey
  -> RIO env (Maybe ByteString)
loadBlob key = liftPB $ \pb -> pbLoadBlob pb key

loadFileTree
  :: HasPantryBackend env
  => FileTreeKey
  -> RIO env (Maybe FileTree)
loadFileTree key = liftPB $ \pb -> pbLoadFileTree pb key

loadPackageSource
  :: HasPantryBackend env
  => PackageSource
  -> RIO env (Maybe PackageInfo)
loadPackageSource key = liftPB $ \pb -> pbLoadPackageSource pb key
