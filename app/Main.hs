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
  (options, f) <- simpleOptions
    $(simpleVersion Paths_pantry.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption
                  ( long "sqlite"
                 <> help "SQLite database"
                  )
    )
    (do
        addCommand
          "unpack-archive"
          "Unpack a file from an archive"
          id
          (unpackArchive
             <$> optional (strOption
                  ( long "subdir"
                 <> help "Subdirectory"
                  ))
             <*> strOption
                  ( long "url"
                 <> help "URL to download it from"
                  )
             <*> strOption
                  ( long "dest"
                 <> help "Destination directory"
                  )
          )
        addCommand
          "update-hackage"
          "Download updates from Hackage"
          id
          (updateHackage'
             <$> strOption
                  ( long "tarball"
                 <> help "Tarball to process"
                  )
          )
    )
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    spb <- runRIO lf $ sqlitePantryBackend $ optionsSqlite options
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appPantryBackend = spb
          }
    runRIO app f

updateHackage' :: FilePath -> RIO App ()
updateHackage' tarball = do
  stackdir <- getAppUserDataDirectory "stack"
  updateHackage tarball $ stackdir </> "indices" </> "Hackage" </> "packages"

unpackArchive :: Maybe FilePath -> String -> FilePath -> RIO App ()
unpackArchive (fromMaybe "" -> subdir) url dest = do
  pi'@(PackageInfo treeKey cabal) <- fetchPackageSource $ PSArchive Archive
    { archiveUrl = fromString url
    , archiveSubdir = fromString subdir
    }
  logDebug $ "Tree key: " <> display treeKey
  logDebug $ "Cabal key: " <> display cabal
  unpackPackage pi' dest
