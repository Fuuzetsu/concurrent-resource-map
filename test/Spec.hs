{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ConcurrentResourceMap
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Typeable
import System.Random

-- | A 'resource' that asserts a number of concurrent initialisations.
-- If it's ever not 0 or 1, something went wrong and we want to fail.
newtype SingleInitResource = SIR (IORef Int)

newSIR :: IO SingleInitResource
newSIR = fmap SIR $ newIORef 0

initSIR :: SingleInitResource -> IO ()
initSIR (SIR r) = atomicModifyIORef' r $ \x -> case x of
  0 -> (1, ())
  n -> error $ "Tried to init SIR when it was non-one, it's: " ++ show n

destroySIR :: SingleInitResource -> IO ()
destroySIR (SIR r) = atomicModifyIORef' r $ \x -> case x of
  1 -> (0, ())
  n -> error $ "Tried to destroy SIR when it was non-one, it's: " ++ show n

checkInitialised :: SingleInitResource -> IO ()
checkInitialised (SIR r) = readIORef r >>= \x -> case x of
  1 -> return ()
  n -> error $ "Checking initialised SIR found non-one value, it's: " ++ show n

checkDestroyed :: SingleInitResource -> IO ()
checkDestroyed (SIR r) = readIORef r >>= \x -> case x of
  0 -> return ()
  n -> error $ "Checking destroyed SIR found non-zero value, it's: " ++ show n

data IntentionalFailure = IntentionalFailure
  deriving (Typeable, Show)
instance Exception IntentionalFailure

newtype SirMap v = M (IntMap.IntMap v)

instance ResourceMap SirMap where
  type Key SirMap = Int
  empty = M IntMap.empty
  delete k (M m) = M (IntMap.delete k m)
  insert k v (M m) = M (IntMap.insert k v m)
  lookup k (M m) = IntMap.lookup k m

main :: IO ()
main = do
  m <- newResourceMap :: IO (ConcurrentResourceMap SirMap SingleInitResource)
  leftThreads <- newTVarIO numThreads
  sirs <- fmap (zip [0 :: Int .. ]) $ replicateM 10 newSIR

  replicateM_ numThreads $ forkIO $ do
    let act [] = return ()
        act ((k, sir):ss) = do
          -- Third of the time, use withInitialisedResource instead.
          withResource <- do
            useWithInitialised <- fmap (== 1) $ randomRIO (1, 3 :: Int)
            pure $ if useWithInitialised
              then \m k i d r -> withInitialisedResource m k d (mapM_ r)
              else withSharedResource
          withResource m k
            (fragileInit sir >> return sir)
            (\r -> smallDelay >> destroySIR r)
            -- Introduce small delay to allow for resource contention
            -- instead of most threads executing instantly creating a fairly
            -- sequenential test scenario.
            --
            -- Further, this tests that we can nest withSharedResource.
            (\r -> smallDelay >> checkInitialised r >> act ss)
            -- Always modify thread exit count when main action finishes.

        actIncr = act sirs `finally` atomically (modifyTVar' leftThreads pred)
        -- Silence intentional failures, no need to pollute test
        -- output.
        silenceIntentional IntentionalFailure = return ()
    actIncr `catch` silenceIntentional

  -- Wait for everything to finish.
  atomically $ readTVar leftThreads >>= check . (== 0)
  forM_ sirs $ \(_, sir) -> checkDestroyed sir
  where
    fragileInit :: SingleInitResource -> IO ()
    fragileInit sir = do
      smallDelay
      -- 25% of the time, throw an exception from initialiser
      shouldFail <- fmap (== 4) $ randomRIO (1, 4 :: Int)
      if shouldFail
        then throwIO IntentionalFailure
        else initSIR sir

    smallDelay :: IO ()
    smallDelay =
      -- Random delay up to 100 µs
      randomRIO (0, 100) >>= threadDelay

    numThreads :: Int
    numThreads = 50000
