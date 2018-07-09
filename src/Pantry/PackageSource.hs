module Pantry.PackageSource
  ( fetchPackageSource
  ) where

import Pantry.Import
import Pantry.FileTree
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Simple
import qualified RIO.Text as T
import Data.Conduit.Zlib (ungzip)
import qualified RIO.Map as Map

-- FIXME check if it's a tarball, compressed tarball, zip, etc
fetchPackageSource
  :: HasPantryBackend env
  => PackageSource
  -> RIO env PackageInfo
fetchPackageSource ps = do
  mpi <- loadPackageSource ps
  case mpi of
    Just pi' -> pure pi'
    Nothing -> do
      (key, tree) <- inner ps
      pi' <- mkPI key tree
      liftPB $ \pb -> pbStorePackageSource pb ps pi'
      pure pi'

inner
  :: HasPantryBackend env
  => PackageSource
  -> RIO env (FileTreeKey, FileTree)
inner (PSArchive (Archive url subdir)) = do
  req <- parseUrlThrow $ T.unpack url
  tree <- fmap (stripToSubdir subdir . stripCommonPrefix)
        $ httpSink req $ \_res -> ungzip .| treeFromTarball Just
  key <- storeFileTree tree
  pure (key, tree)

mkPI :: FileTreeKey -> FileTree -> RIO env PackageInfo
mkPI key (FileTree tree) = PackageInfo key <$>
    case filter (isRootCabalFile . fst) $ Map.toList tree of
      [] -> throwPantry $ "No cabal file present: " <> displayShow (Map.keys tree)
      [(_, fte)] ->
        case fte of
          FTELink _ -> throwPantry "cabal file cannot be a link"
          FTEExecutable cabal -> pure cabal
          FTENormal cabal -> pure cabal
      cabals -> throwPantry $ "Multiple cabal files present: " <> displayShow (map fst cabals)

stripCommonPrefix :: FileTree -> FileTree
stripCommonPrefix orig@(FileTree m) = fromMaybe orig $ do
    allPairs@((firstFP, _):_) <-
      Just $ map (first unSafeFilePath) $ Map.toList m
    let firstDir = T.takeWhile (/= '/') firstFP
    guard $ not $ T.null firstDir
    let prefix = firstDir <> "/"
    fmap (FileTree . Map.fromList) $ forM allPairs $ \(fp, x) ->
      (, x) <$>
      (T.stripPrefix prefix fp >>= either (const Nothing) Just . mkSafeFilePath)

stripToSubdir :: Text -> FileTree -> FileTree
stripToSubdir subdir (FileTree m) =
  FileTree $ Map.fromList $ mapMaybe go $ Map.toList m
  where
    go (sfp, fte) = do
      t <- fmap (T.dropPrefix "/") $ T.stripPrefix subdir $ unSafeFilePath sfp
      (, fte) <$> either (const Nothing) Just (mkSafeFilePath t)
