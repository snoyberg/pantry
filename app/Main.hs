{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import RIO
import Pantry
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_pantry
import RIO.Directory (getAppUserDataDirectory)
import RIO.FilePath ((</>))

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsTarball :: !FilePath
  , optionsSqlite :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appPantryBackend :: !PantryBackend
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
instance HasPantryBackend App where
  pantryBackendL = lens appPantryBackend (\x y -> x { appPantryBackend = y })

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
  stackdir <- getAppUserDataDirectory "stack"
  withLogFunc lo $ \lf -> do
    spb <- runRIO lf $ sqlitePantryBackend $ optionsSqlite options
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appPantryBackend = spb
          }
    runRIO app $ updateHackage
      (optionsTarball options)
      (stackdir </> "indices" </> "Hackage" </> "packages")
