{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SqliteBackend where

import Import hiding (BlobKey (..))
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
    blob BlobId
    path Text
    executable Bool
    UniqueTreeEntry tree path
|]

sqlitePantryBackend
  :: HasLogFunc env
  => FilePath -- ^ SQLite file
  -> RIO env PantryBackend
sqlitePantryBackend fp = do
  pool <- createSqlitePool (T.pack fp) 7
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
        res <- insertBy $ Tree $ T.pack $ fileTreeKeyString key
        let tid = either entityKey id res
        forM_ (Map.toList m) $ \(SafeFilePath fpt, fte) -> do
          mbid <- getBy $ UniqueBlobHash $ keyToText $ fteBlobKey fte
          bid <-
            case mbid of
              Just (Entity bid _) -> pure bid
              Nothing -> throwString $ "Blob key not found in database: " ++ blobKeyString (fteBlobKey fte)
          void $ insertBy $ TreeEntry tid bid fpt (fteExecutable fte)
    }
    where
      keyToText = T.pack . blobKeyString
