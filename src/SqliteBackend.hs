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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Blob
    hash Text
    contents ByteString
    UniqueBlobHash hash
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
    }
    where
      keyToText = T.pack . blobKeyString
