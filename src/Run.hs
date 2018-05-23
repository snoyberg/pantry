{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import TarBlobWriter
import qualified RIO.ByteString as B
import Data.Conduit.Tar (filePath)

run :: RIO App ()
run = do
  app <- ask
  writeTarBlobs isCabal $ optionsTarball $ appOptions app
  where
    isCabal fi = ".cabal" `B.isSuffixOf` filePath fi
