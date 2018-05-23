{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import FileBackend
import SqliteBackend
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_pantry

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_pantry.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption
                  ( long "root"
                 <> help "Root directory"
                  )
       <*> strOption
                  ( long "tarball"
                 <> help "Tarball to process"
                  )
       <*> strOption
                  ( long "sqlite"
                 <> help "SQLite database"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    spb <- runRIO lf $ sqlitePantryBackend $ optionsSqlite options
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appPantryBackend = filePantryBackend (optionsRoot options) <> spb
          }
    runRIO app run
