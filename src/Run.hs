{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import TarBlobWriter
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.Conduit.Tar (filePath)
import RIO.Directory (doesFileExist, createDirectoryIfMissing)
import RIO.FilePath ((</>), takeDirectory)
import Data.Word8 (_slash)
import Network.HTTP.Simple
import Conduit
import FileTree (treeFromTarball)
import Data.Conduit.Zlib (ungzip)
import Control.Concurrent.STM.TBMQueue

run :: RIO App ()
run = do
  app <- ask
  let count = 8
  queue <- liftIO $ newTBMQueueIO (count * 8)
  let schedule = atomically . writeTBMQueue queue
      primary = writeTarBlobs (isCabal schedule) $ optionsTarball $ appOptions app
      worker = fix $ \loop -> do
        mnext <- atomically $ readTBMQueue queue
        case mnext of
          Nothing -> pure ()
          Just next -> next *> loop
  concurrently_ primary $ replicateConcurrently_ count worker
  where
    isCabal schedule fi =
      case parseNameVersion $ filePath fi of
        Nothing -> pure False
        Just(name, version) -> True <$ schedule (do
          app <- ask
          let fp = appSdistRoot app </> name </> version </> concat [name, "-", version, ".tar.gz"]
              url = concat
                [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
                , name
                , "-"
                , version
                , ".tar.gz"
                ]
          unlessM (doesFileExist fp) $ do
            req <- parseRequest url
            createDirectoryIfMissing True $ takeDirectory fp
            logInfo $ "Downloading " <> fromString name <> "-" <> fromString version
            withSinkFileCautious fp $ \sink -> withResponse req $ \res ->
              if getResponseStatusCode res == 200
                then runConduit $ getResponseBody res .| sink
                else error $ show (req, void res)
          res <- tryAny $ void $ withSourceFile fp $ \src -> runConduit $ src .| ungzip .| treeFromTarball
          case res of
            Left e -> logError $ "Error making tree for tarball " <> fromString name <> "-" <> fromString version <> ": " <> displayShow e
            Right () -> pure ()
          )

    parseNameVersion :: ByteString -> Maybe (String, String)
    parseNameVersion bs1 = do
      bs2 <- B.stripSuffix ".cabal" bs1
      [name1, version, name2] <- pure $ B.split _slash bs2
      guard $ name1 == name2
      Right name' <- Just $ decodeUtf8' name1
      Right version' <- Just $ decodeUtf8' version
      Just (T.unpack name', T.unpack version')
