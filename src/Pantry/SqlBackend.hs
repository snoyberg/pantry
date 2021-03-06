{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Pantry.SqlBackend
  ( sqlitePantryBackend
  ) where

import Pantry.Import
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified RIO.Text as T
import qualified RIO.Map as Map

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Blob
    hash Text
    contents ByteString
    UniqueBlobHash hash
Tree
    hash Text
    UniqueTreeHash hash
TreeEntry
    tree TreeId
    path Text
    blob BlobId Maybe
    linkDest Text Maybe
    executable Bool
    UniqueTreeEntry tree path
ArchiveCache
    url Text
    subdir Text
    tree TreeId
    cabal BlobId
    UniqueArchiveCache url subdir
|]

sqlitePantryBackend -- FIXME generalize to allow Postgres too
  :: HasLogFunc env
  => FilePath -- ^ SQLite file
  -> RIO env PantryBackend
sqlitePantryBackend fp = do
  pool <- createSqlitePool (T.pack fp) 1
  migrates <- runSqlPool (runMigrationSilent migrateAll) pool
  forM_ migrates $ \mig -> logDebug $ "Migration output: " <> display mig
  pure PantryBackend
    { pbStoreBlob = \key bs -> flip runSqlPool pool $ do
        res <- insertBy $ Blob (blobKeyText key) bs
        case res of
          Left (Entity _ (Blob _ bs'))
            | bs /= bs' -> throwPantry "Mismatched blobs!"
            | otherwise -> pure ()
          Right _ -> pure ()
    , pbLoadBlob = \key ->
        fmap (fmap (blobContents . entityVal)) $
        flip runSqlPool pool $
        getBy $ UniqueBlobHash $ blobKeyText key
    , pbStoreFileTree = \key (FileTree m) _rendered -> flip runSqlPool pool $ do
        res <- insertBy $ Tree $ fileTreeKeyText key
        let tid = either entityKey id res
        forM_ (Map.toList m) $ \(unSafeFilePath -> fpt, fte) -> do
          (mblob, mlink, exe) <-
            case fte of
              FTELink (unSafeFilePath -> link') -> pure (Nothing, Just link', False)
              FTEExecutable key' -> pure (Just key', Nothing, True)
              FTENormal key' -> pure (Just key', Nothing, False)
          mbid <- forM mblob $ \blob -> do
            mbid <- getBy $ UniqueBlobHash $ blobKeyText blob
            case mbid of
              Just (Entity bid _) -> pure bid
              Nothing -> throwPantry $ "Blob key not found in database: " <> display blob
          void $ insertBy TreeEntry
            { treeEntryTree = tid
            , treeEntryPath = fpt
            , treeEntryBlob = mbid
            , treeEntryLinkDest = mlink
            , treeEntryExecutable = exe
            }
    , pbLoadFileTree = \key -> flip runSqlPool pool $ do
        mtreeent <- getBy $ UniqueTreeHash $ fileTreeKeyText key
        for mtreeent $ \(Entity tid _) -> do
          ents <- selectList [TreeEntryTree ==. tid] []
          entries <- forM ents $ \(Entity tekey te) -> do
            sfp <-
              case mkSafeFilePath $ treeEntryPath te of
                Left e -> throwPantry $ "Invalid SafeFilePath in file tree " <> display key <> ": " <> display e
                Right sfp -> pure sfp
            entry <-
              case treeEntryBlob te of
                Nothing ->
                  case treeEntryLinkDest te of
                    Nothing -> throwPantry $ "Can't have both null blob and link dest " <> displayShow tekey
                    Just linkdest ->
                      case mkSafeFilePath linkdest of
                        Left e -> throwPantry $ "Invalid link dest: " <> display e
                        Right x -> pure $ FTELink x
                Just blobkey -> do
                  -- FIXME perfect use case of esqueleto here
                  mblobent <- get blobkey
                  blobhash' <-
                    case mblobent of
                      Nothing -> throwPantry "Missing blob"
                      Just (Blob blobhash' _) -> pure blobhash'
                  blobhash <-
                    case blobKeyFromText blobhash' of
                      Nothing -> throwPantry $ "Invalid blob hash found for: " <> displayShow blobkey
                      Just x -> pure x
                  pure $
                    if treeEntryExecutable te
                      then FTEExecutable blobhash
                      else FTENormal blobhash
            pure (sfp, entry)
          pure $ FileTree $ Map.fromList entries
    , pbStorePackageSource = \x y -> flip runSqlPool pool (storePS x y)
    , pbLoadPackageSource = flip runSqlPool pool . loadPS
    }

storePS
  :: PackageSource
  -> PackageInfo
  -> ReaderT SqlBackend (RIO env) ()
storePS (PSArchive (Archive url subdir)) (PackageInfo tree cabal) = do
  tree' <- entityKey <$> getBy_ (UniqueTreeHash $ fileTreeKeyText tree)
  cabal' <- entityKey <$> getBy_ (UniqueBlobHash $ blobKeyText cabal)
  insert_ ArchiveCache
    { archiveCacheUrl = url
    , archiveCacheSubdir = subdir
    , archiveCacheTree = tree'
    , archiveCacheCabal = cabal'
    }

loadPS
  :: PackageSource
  -> ReaderT SqlBackend (RIO env) (Maybe PackageInfo)
loadPS (PSArchive (Archive url subdir)) =
  getBy (UniqueArchiveCache url subdir) >>= traverse go
  where
    go (Entity _ ac) = do
      Tree treehash <- get_ $ archiveCacheTree ac
      treekey <-
        case fileTreeKeyFromText treehash of
          Nothing -> throwPantry $ "Invalid tree hash found: " <> display treehash
          Just x -> pure x
      cabal <- get_ $ archiveCacheCabal ac
      cabalkey <-
        case blobKeyFromText $ blobHash cabal of
          Nothing -> throwPantry $ "Invalid blob hash found: " <> display (blobHash cabal)
          Just x -> pure x
      pure PackageInfo
        { piTree = treekey
        , piCabalFile = cabalkey
        }

get_
  :: ( PersistEntityBackend val ~ SqlBackend
     , PersistEntity val
     )
  => Key val
  -> ReaderT SqlBackend (RIO env) val
get_ key = do
  mres <- get key
  case mres of
    Nothing -> throwPantry "Key lookup failed, that shouldn't happen"
    Just res -> pure res


getBy_
  :: ( PersistEntityBackend val ~ SqlBackend
     , PersistEntity val
     )
  => Unique val
  -> ReaderT SqlBackend (RIO env) (Entity val)
getBy_ uniq = do
  mres <- getBy uniq
  case mres of
    Nothing -> throwPantry "Uniqueness lookup failed, that shouldn't happen"
    Just res -> pure res
