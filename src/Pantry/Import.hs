module Pantry.Import
  ( module RIO
  , module Pantry.Types
  , withWorkers
  , throwPantry
  , PantryException (..)
  ) where

import RIO
import Pantry.Types
import Control.Concurrent.STM.TBMQueue

-- | Run the given action with an accessory function which will run an
-- action in a separate worker thread. Pre-spawn the given number of
-- worker threads. Exceptions anywhere will take down all threads, and
-- backpressure is applied via a bounded queue.
withWorkers :: MonadUnliftIO m => Int -> ((m () -> m ()) -> m a) -> m a
withWorkers count inner = do
  queue <- liftIO $ newTBMQueueIO (count * 8)
  let schedule = atomically . writeTBMQueue queue
      worker = fix $ \loop -> do
        mnext <- atomically $ readTBMQueue queue
        case mnext of
          Nothing -> pure ()
          Just next -> next *> loop
  runConcurrently $
    Concurrently (replicateConcurrently_ count worker) *>
    Concurrently (inner schedule)

newtype PantryException = PantryException Text
  deriving (Show, Typeable)
instance Exception PantryException

throwPantry :: MonadIO m => Utf8Builder -> m a
throwPantry = throwIO . PantryException . utf8BuilderToText
