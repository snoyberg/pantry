{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Pantry.SqlBackend
  ( sqlitePantryBackend
  ) where

import Pantry.Import hiding (BlobKey (..))
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
|]

sqlitePantryBackend -- FIXME generalize to allow Postgres too
  :: HasLogFunc env
  => FilePath -- ^ SQLite file
  -> RIO env PantryBackend
sqlitePantryBackend fp = do
  pool <- createSqlitePool (T.pack fp) 1
  runSqlPool (runMigration migrateAll) pool
  pure PantryBackend
    { pbStoreBlob = \key bs -> flip runSqlPool pool $ do
        res <- insertBy $ Blob (keyToText key) bs
        case res of
          Left (Entity _ (Blob _ bs'))
            | bs /= bs' -> throwString "Mismatched blobs!"
            | otherwise -> pure ()
          Right _ -> pure ()
    , pbLoadBlob = \key ->
        fmap (fmap (blobContents . entityVal)) $
        flip runSqlPool pool $
        getBy $ UniqueBlobHash $ keyToText key
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
            mbid <- getBy $ UniqueBlobHash $ keyToText blob
            case mbid of
              Just (Entity bid _) -> pure bid
              Nothing -> throwString $ "Blob key not found in database: " ++ blobKeyString blob
          void $ insertBy TreeEntry
            { treeEntryTree = tid
            , treeEntryPath = fpt
            , treeEntryBlob = mbid
            , treeEntryLinkDest = mlink
            , treeEntryExecutable = exe
            }
    }
    where
      keyToText = T.pack . blobKeyString
