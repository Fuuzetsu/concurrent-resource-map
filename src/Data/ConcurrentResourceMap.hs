module Data.ConcurrentResourceMap
  ( ConcurrentResourceMap
  , newResourceMap
  , withInitialisedResource
  , withSharedResource
  ) where

import Control.Exception
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

data Resource r = Uninitialised | Initialised !r

-- | Some resource with a count of the users (threads) using it.
--
-- Internal invariant: if users = 0 then resource = Uninitialised
data CountedResource r = CountedResource
  { users :: !Int
  , resource :: !(Resource r)
  }

-- | A map of shared resources @r@ keyed by @k@.
newtype ConcurrentResourceMap k r = C
  (MVar (Map k (MVar (CountedResource r))))

-- | Create an empty resource map.
newResourceMap :: Ord k => IO (ConcurrentResourceMap k r)
newResourceMap = fmap C $ MVar.newMVar Map.empty

-- | Use a resource that can be accessed concurrently via multiple
-- threads but is only initialised and destroyed on as-needed basis.
-- If number of users falls to 0, the resource is destroyed. If a new
-- user joins and resource is not available, it's created.
--
-- Calls to 'withSharedResource' can even be nested if you need access
-- to resources with different keys in the same map. Calling
-- 'withSharedResource' in a nested matter on same resource key should
-- have no real adverse effects either.
withSharedResource
  :: Ord k
  => ConcurrentResourceMap k r
  -- ^ Resource map. Create with 'newResourceMap'.
  -> k
   -- ^ Key for the resource. This allows you to have many of the same
   -- type of resource but separated: for example, one group of
   -- threads could be holding onto a logging handle to syslog while
   -- another could be holding a handle to a file.
  -> IO r
  -- ^ Initialise resource. Only ran if the resource is not yet
  -- initialised. Does not run in masked context so if you need to
  -- stop async exceptions, you should use 'mask' yourself. If the
  -- action fails (throws an exception), the user fails and we enter
  -- cleanup.
  -> (r -> IO ())
  -- ^ Destroy the resource if it was initialised. Ran by last alive
  -- user when it's exiting. Unlike initialisation, this _is_ ran in
  -- masked context. If this action fails (by throwing an exception
  -- itself), the resource will be assumed to be uninitialised and the
  -- exception will be re-thrown.
  --
  -- Therefore, if your cleanup can fail in a way that you have to
  -- know about/recover from, you should catch exceptions coming out
  -- out 'withSharedResource'. As you get reference to the resource
  -- in the @act@, you're able to store it/monitor it yourself and
  -- decide to take any appropriate actions in the future such as
  -- blocking other threads from running initialisation again until
  -- you've cleaned up the resource yourself.
  -> (r -> IO a)
  -- ^ Run an action with the initialised resource. Note that the
  -- availability of this resource only ensures that the user-given
  -- initialisers/destructors have been ran appropriate number of
  -- times: it of course makes no guarantees as to what the resource
  -- represents. For example, if it's a 'System.Process.ProcessHandle'
  -- or a database connection, there's no guarantee that the process
  -- is alive or that the database connection is still available. For
  -- resources that can dynamically fail, you should implement some
  -- sort of monitoring yourself.
  -> IO a
withSharedResource vm k initResource destroyResource act = bracket
  (addUser vm k)
  (removeUser vm k destroyResource)
  -- Don't leak the internal MVar to the user! This ensures that we
  -- can safely remove it from the resource map when we exit through
  -- 'removeUser'.
  actWithResource
  where
    actWithResource rvar = do
      r <- MVar.modifyMVar rvar $ \cr -> case cr of
        CountedResource { resource = Uninitialised } -> do
          r <- initResource
          return (cr { resource = Initialised r }, r)
        CountedResource { resource = Initialised r } ->
          return (cr, r)
      act r


-- | This is like 'withSharedResource' but will only execute the user
-- action if the resource already exists. This is useful if you create
-- your resources in one place but would like to use them
-- conditionally in another place if they are still alive.
--
-- Action is given Nothing if the resource does not exist or is not
-- initialised.
withInitialisedResource
  :: Ord k
  => ConcurrentResourceMap k r
  -- ^ Resource map. Create with 'newResourceMap'.
  -> k
   -- ^ Key for the resource. This allows you to have many of the same
   -- type of resource but separated: for example, one group of
   -- threads could be holding onto a logging handle to syslog while
   -- another could be holding a handle to a file.
  -> (r -> IO ())
  -- ^ Destroy the resource if it was initialised. Ran by last alive
  -- user when it's exiting. Unlike initialisation, this _is_ ran in
  -- masked context. If this action fails (by throwing an exception
  -- itself), the resource will be assumed to be uninitialised and the
  -- exception will be re-thrown.
  --
  -- Therefore, if your cleanup can fail in a way that you have to
  -- know about/recover from, you should catch exceptions coming out
  -- out 'withSharedResource'. As you get reference to the resource
  -- in the @act@, you're able to store it/monitor it yourself and
  -- decide to take any appropriate actions in the future such as
  -- blocking other threads from running initialisation again until
  -- you've cleaned up the resource yourself.
  -> (Maybe r -> IO a)
  -- ^ Run an action with the resource. Note that the availability of
  -- this resource only ensures that the user-given
  -- initialisers/destructors have been ran appropriate number of
  -- times: it of course makes no guarantees as to what the resource
  -- represents. For example, if it's a 'System.Process.ProcessHandle'
  -- or a database connection, there's no guarantee that the process
  -- is alive or that the database connection is still available. For
  -- resources that can dynamically fail, you should implement some
  -- sort of monitoring yourself.
  -> IO a
withInitialisedResource vm k destroyResource act = bracket
  (addUserIfPresent vm k)
  removeUserIfPresent
  actWithResource
  where
    removeUserIfPresent Nothing = pure ()
    removeUserIfPresent (Just rvar) = removeUser vm k destroyResource rvar

    actWithResource Nothing = act Nothing
    actWithResource (Just rvar) = MVar.readMVar rvar >>= \cr -> case cr of
        CountedResource { resource = Initialised r } -> act (Just r)
        _ -> act Nothing

-- | Adds a user at given key. If it's the first user, creates the
-- underlying map.
--
-- Should be used as initialising action in 'bracket' along with
-- 'removeUser'.
addUser
    :: Ord k
    => ConcurrentResourceMap k r -> k -> IO (MVar (CountedResource r))
addUser (C vm) k = MVar.modifyMVar vm $ \m -> case Map.lookup k m of
  -- We're the first user of this resource, make the counted
  -- resource.
  Nothing -> do
    v <- MVar.newMVar CountedResource { users = 1, resource = Uninitialised }
    return (Map.insert k v m, v)
  -- Other users already exist, increase the count only.
  Just vc -> do
    MVar.modifyMVar_ vc $ \cr ->
      return cr { users = users cr + 1 }
    return (m, vc)


-- | Adds a user at given key but only if the given key already exists..
--
-- Should be used as initialising action in 'bracket' along with
-- 'removeUser'.
addUserIfPresent
    :: Ord k
    => ConcurrentResourceMap k r -> k -> IO (Maybe (MVar (CountedResource r)))
addUserIfPresent (C vm) k = MVar.modifyMVar vm $ \m -> case Map.lookup k m of
  -- We're the first user of this resource, make the counted
  -- resource.
  Nothing -> return (m, Nothing)
  -- Other users already exist, increase the count only.
  Just vc -> do
    MVar.modifyMVar_ vc $ \cr ->
      return cr { users = users cr + 1 }
    return (m, Just vc)

-- | Remove user for the given key. If it's the last user, removes the
-- counted resource from the map completely.
--
-- Should be used as cleanup action in 'bracket' along with 'addUser'.
removeUser
  :: Ord k
  => ConcurrentResourceMap k r
  -> k
  -- ^ The resource from inside the map.
  -> (r -> IO ())
  -- ^ Destroy resource.
  -> MVar (CountedResource r)
  -- ^ Internal ref
  -> IO ()
removeUser (C vm) k destroyResource vc = do
  cr <- MVar.takeMVar vc
  let newCount = users cr - 1
      cr' = cr { users = newCount }
  case cr' of
    -- We're the last ones around, uninitialise.
    CountedResource { users = 0, resource = Initialised r } -> do
      let uninitialise = MVar.putMVar vc cr' { resource = Uninitialised }
          -- Destroy the resource if we can. If we fail, uninitialise it
          -- anyway and re-throw the exception.
          destroy = do
            destroyResource r `onException` uninitialise
            uninitialise
      -- We were the last ones around and whether we managed to
      -- destroy the resource or not, we want to remove the internal
      -- MVar from the resource map if we're still the last ones.
      destroy `finally` cleanFromResourceMap

    -- Resource is uninitialised or there are some other users
    -- around, simply replace the content with updated user counter.
    _ -> MVar.putMVar vc cr'
  where
    cleanFromResourceMap = MVar.modifyMVar_ vm $ \m -> case Map.lookup k m of
      -- The resource is not even in the map anymore. This could
      -- happen if since we decreased the count, a new user came in,
      -- increased the count, finished and cleaned up before we did.
      -- Seems unlikely but not impossible: in this case, there's
      -- nothing left for us to do.
      Nothing -> return m
      Just rvar -> do
        m'cr <- MVar.tryTakeMVar rvar
        case m'cr of
          -- We took the lock and see that there are still no
          -- remaining users. We must be the last ones and must be the
          -- only user holding a useful reference to the MVar as it
          -- can only be created and passed from addUser and it's
          -- internal API. Remove it from the map.
          Just CountedResource { users = 0 } -> return (Map.delete k m)
          -- We were able to take the MVar but there are some other
          -- users around again: we want to keep the original map. Put
          -- the value back and keep original the original mapping.
          Just cr -> do
            MVar.putMVar rvar cr
            return m
          -- We were either unable to take the MVar which means
          -- someone else has started to use it and so, we shouldn't
          -- delete it. Leave it as-is in the map.
          Nothing -> return m
