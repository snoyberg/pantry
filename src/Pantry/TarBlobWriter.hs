module Pantry.TarBlobWriter
  ( writeTarBlobs
  ) where

import Pantry.Import
import Conduit
import Data.Conduit.Tar
import qualified RIO.ByteString.Lazy as BL

writeTarBlobs
  :: HasPantryBackend env
  => (FileInfo -> RIO env Bool) -- ^ predicate
  -> FilePath -- ^ tarball
  -> RIO env ()
writeTarBlobs predicate fp =
    withSourceFile fp $ \src -> runConduit $ src .| untar worker
  where
    worker fi =
      case fileType fi of
        FTNormal -> whenM (lift $ predicate fi) $ do
          bs <- BL.toStrict <$> sinkLazy
          void $ lift $ storeBlob bs
        _ -> pure ()

