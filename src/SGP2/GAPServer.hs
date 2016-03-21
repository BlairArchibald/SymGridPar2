-- HdpH binding to call a bunch of GAP servers;
--   requires HaskellGAP-2b binding
--
-- Visibility: public
-- Author: Patrick Maier <P.Maier@hw.ac.uk>
-- Created: 25 Feb 2013
--
-----------------------------------------------------------------------------

module SGP2.GAPServer
  ( -- * configuration parameters for starting GAP
    GAP(..),      -- record collecting all params; insts: Generic, Binary
    defaultGAP,   -- :: GAP
    cmdLineGAP,   -- :: GAP -> String

    -- * start/stop stateless GAP servers
    start,        -- :: GAP -> [Lazy.ByteString] -> Par ()
    stop,         -- :: Par ()
    stopAll,      -- :: Par ()
    barrier,      -- :: Par ()

    -- * calling any one of the stateless GAP server
    call,         -- :: Lazy.ByteString -> Par (IVar (Either GAPError GAPObj))

    -- * handle to stateful GAP server
    GAPHandleST,  -- abstract

    -- * start/stop stateful GAP servers
    startST,      -- :: GAP -> [Lazy.ByteString] -> Par (IVar GAPHandleST)
    stopST,       -- :: GAPHandleST -> Par ()
    killST,       -- :: GAPHandleST -> Par ()
    waitST,       -- :: GAPHandleST -> Par ()

    -- * calling stateful GAP servers
    callST,       -- :: GAPHandleST
                  -- -> Lazy.ByteString
                  -- -> Par (IVar (Either GAPError GAPObj))

    -- * abstract data type of GAP objects
    GAPObj,           -- instances: Eq, Qrd, Show, Generic, NFData, Binary

    -- * data type of GAP errors
    GAPError(..),     -- instances: Eq, Ord, Show, Generic, NFData, Binary

    -- * testers for GAP objects
    isGAPFail,        -- :: GAPObj -> Bool
    isGAPBool,        -- :: GAPObj -> Bool
    isGAPInt,         -- :: GAPObj -> Bool
    isGAPRat,         -- :: GAPObj -> Bool
    isGAPList,        -- :: GAPObj -> Bool
    isGAPNull,        -- :: GAPObj -> Bool
    isGAPString,      -- :: GAPObj -> Bool
    isGAPOpaque,      -- :: GAPObj -> Bool

    -- * abstract class of encoders/decoders for GAP objects
    GAPEnc,           -- abstract class
    toGAPObj,         -- :: (GAPEnc a) => a -> GAPObj
    fromGAPObj,       -- :: (GAPEnc a) => GAPObj -> a

    -- * building lazy byte strings (to pass to call/callST) from GAP objects
    buildGAPObj,      -- :: GAPObj -> Lazy.Builder

    -- * convenience: construct a GAP function call
    mkGAPCallFuncList  -- :: Strict.ByteString -> [GAPObj] -> Lazy.ByteString
  ) where

import Prelude hiding (init)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar,
                                readMVar, withMVar, modifyMVar_)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad (foldM, replicateM, replicateM_)
import qualified Data.ByteString as Strict (ByteString)          -- just for doc
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Builder as Lazy (Builder)  -- just for doc
import System.IO.Unsafe (unsafePerformIO)

import Binding.GAPServerIO
       (GAP(..), defaultGAP, cmdLineGAP,
        GAPServer, startGAP, killGAP, callGAP,
        GAPObj, GAPError(..), isGAPFail, isGAPBool, isGAPInt,
        isGAPRat, isGAPList, isGAPNull, isGAPString, isGAPOpaque,
        GAPEnc, toGAPObj, fromGAPObj, buildGAPObj,
        mkGAPCallFuncList)

import Control.Parallel.HdpH (Par, io, IVar, new, put, stub)


-----------------------------------------------------------------------------
-- Key facts

-- * Actual binding to GAP is in module GAPServerIO.
--
-- * GAP servers come in two flavours: stateful and stateless.
--
-- * Stateful GAP servers are identified by unique handle. Calling a
--   stateful GAP server may block the calling scheduler until the server
--   becomes idle.
--
-- * HdpH can control a bunch of stateless GAP servers. These are not
--   individually identifiable; instead HdpH balances calls to them.
--   Calling a stateless GAP server may block the calling scheduler until
--   a stateless server becomes idle.


-----------------------------------------------------------------------------
-- GAP API for stateful GAP servers (including stateful stub server)

-- data type encapsulating a task for a GAP server
type GAPTask = (Lazy.ByteString, IVar (Either GAPError GAPObj))
               -- 1st component of pair is GAP argument
               -- 2nd component is IVar to take result

-- stateful GAP server handle; abstract outwith this module
data GAPHandleST = GAPHandleST { inst :: GAPServer,
                                 task :: MVar (Maybe GAPTask),
                                 idle :: MVar () }


-- Start one stateful GAP server. Returns an IVar that will contain
-- the server handle when it is ready (ie. up and idle).
startST :: GAP -> [Lazy.ByteString] -> Par (IVar GAPHandleST)
startST gap setup_cmds = do
  ivar <- new
  -- fork stub server (which is free to block)
  stub $ stubServerST gap setup_cmds ivar
  return ivar


-- Stop the stateful GAP server. Blocks until the server is idle.
stopST :: GAPHandleST -> Par ()
stopST hdl = do
  io $ takeMVar (idle hdl)         -- block until server idle
  io $ putMVar (task hdl) Nothing  -- tell stub server to terminate


-- Kill the stateful GAP server immediately. May leave stub server hanging.
killST :: GAPHandleST -> Par ()
killST hdl = io $ killGAP (inst hdl)


-- Block until the stateful GAP server is idle.
waitST :: GAPHandleST -> Par ()
waitST hdl = io $ readMVar (idle hdl)  -- block until server idle


-- Call the stateful GAP server.
-- Will return immediately if the server is currently idle, will block
-- the scheduler otherwise (should therefore not be executed by msg handler).
-- Returns an IVar which will eventually return the result of the call.
callST :: GAPHandleST -> Lazy.ByteString -> Par (IVar (Either GAPError GAPObj))
callST hdl cmd = do
  v <- new
  io $ takeMVar (idle hdl)                 -- block until server idle
  io $ putMVar (task hdl) $ Just (cmd, v)  -- post task
  return v


-- HdpH stub server starting and controlling one stateful GAP server.
-- Any errors during initialisation of the GAP server are deemed fatal
-- for HdpH (will abort the application); however, fatal crashes later
-- on during task execution are simply reported (and don't bring HdpH down).
stubServerST :: GAP -> [Lazy.ByteString] -> IVar GAPHandleST -> Par ()
stubServerST gap setup_cmds hdl_ivar = init
  where
    init = do
      res <- io $ setupGAP gap setup_cmds  -- init GAP server (may block)
      case res of                          -- check result
        Left err  -> error $ "HdpH.GAPServer.stubServerST.start: " ++ show err
        Right srv -> do                    -- result ok:
          task_mv <- io $ newEmptyMVar       -- construct server handle
          idle_mv <- io $ newEmptyMVar
          let hdl = GAPHandleST { inst = srv, task = task_mv, idle = idle_mv }
          put hdl_ivar hdl                   -- release server handle
          loop srv task_mv idle_mv           -- drop into stub server loop
    loop srv task_mv idle_mv = do
      io $ putMVar idle_mv ()              -- signal GAP server idle
      maybe_task <- io $ takeMVar task_mv  -- block waiting for task
      case maybe_task of                   -- check for termination
        Nothing          -> io $ killGAP srv -- terminate GAP server and stub
        Just (cmd, ivar) -> do               -- execute task:
          res <- io $ callGAP srv cmd          -- call GAP (may block)
          put ivar res                         -- release result
          case res of                          -- check for fatal server crash
            Left GAPCrash -> crash task_mv idle_mv    -- yes: go to crash loop
            _             -> loop srv task_mv idle_mv -- no: recurse
    crash task_mv idle_mv = do
      io $ putMVar idle_mv ()              -- signal GAP server idle
      maybe_task <- io $ takeMVar task_mv  -- block waiting for task
      case maybe_task of                   -- check for termination
        Nothing        -> return ()          -- terminate stub
        Just (_, ivar) -> do                 -- 'execute' task:
          put ivar (Left GAPCrash)             -- immediately return error
          crash task_mv idle_mv                -- and recurse


-- Starts one GAP server and runs a list of initialising commands.
-- Returns either a handle to the server or a GAP error; in the latter case
-- the server is killed (even is the GAP error is considered non-fatal).
setupGAP :: GAP -> [Lazy.ByteString] -> IO (Either GAPError GAPServer)
setupGAP gap setup_cmds = do
  start_result <- startGAP gap
  case start_result of
    Left  err -> return (Left err)                -- GAP instance already dead
    Right srv -> foldM runCmd (Right srv) setup_cmds
      where
        runCmd (Left e)  _   = return (Left e)
        runCmd (Right _) cmd = do
          call_result <- callGAP srv cmd
          case call_result of
            Right _ -> return (Right srv)
            Left e -> case e of
              GAPCrash     -> do return (Left e)  -- GAP instance already dead
              GAPSuPeRfail -> do killGAP srv      -- kill GAP instance now
                                 return (Left e)


-----------------------------------------------------------------------------
-- GAP API for stateless GAP servers (including load balancer and stub servers)

-- Start one GAP server. Returns immediately (unless blocked by barrier).
start :: GAP -> [Lazy.ByteString] -> Par ()
start gap setup_cmds = do
  task_mv <- io $ newEmptyMVar
  -- fork new stub server (which is free to block)
  stub $ stubServer gap setup_cmds task_mv
  -- increment server count (optimistically assuming server survives init)
  io $ modifyMVar_ stubCount $ \ k -> return $! k + 1
  return ()


-- Stop one GAP server. Blocks until there is an idle server.
stop :: Par ()
stop =
  io $ modifyMVar_ stubCount $ \ k ->
    if k > 0
      then do { stopN 1; return $! k - 1 }
      else return k


-- Stop all GAP servers. Blocks until all servers are idle.
stopAll :: Par ()
stopAll =
  io $ modifyMVar_ stubCount $ \ k -> do { stopN k; return 0 }


-- Internal function to stop n GAP servers.
-- Assumes that caller holds stubCount lock and there are at least n servers.
stopN :: Int -> IO ()
stopN n = replicateM_ n $ do
            task_mv <- popStubQFront
            putMVar task_mv Nothing


-- Block until all GAP servers are idle.
barrier :: Par ()
barrier =
  io $ withMVar stubCount $ \ k -> do
         task_mvs <- replicateM k popStubQFront
         mapM_ pushStubQBack task_mvs


-- Call one of the running GAP servers.
-- Will return immediately if a GAP server is currently idle, will block
-- the scheduler otherwise (should therefore not be executed by msg handler).
-- Returns an IVar which will eventually return the result of the call.
call :: Lazy.ByteString -> Par (IVar (Either GAPError GAPObj))
call cmd = do
  v <- new
  task_mv <- io $ popStubQFront  -- may block
  io $ putMVar task_mv $ Just (cmd, v)
  return v


-- HdpH stub server starting and controlling one stateless GAP server.
-- Any errors during initialisation of the GAP server are deemed fatal
-- for HdpH (will abort the application); however, fatal crashes later
-- on during task execution result in silent recovery of the GAP server
-- (unless that recovery itself triggers an error, which does bring HdpH down).
stubServer :: GAP -> [Lazy.ByteString] -> MVar (Maybe GAPTask) -> Par ()
stubServer gap setup_cmds task_mv = init
  where
    init = do
      res <- io $ setupGAP gap setup_cmds  -- init GAP server (may block)
      case res of                          -- check result
        Left err  -> error $ "HdpH.GAPServer.stubServer.start: " ++ show err
        Right srv -> do                    -- result ok:
          io $ pushStubQBack task_mv         -- put task_mv on back of stubQ
          loop srv                           -- drop into stub server loop
    loop srv = do
      maybe_task <- io $ takeMVar task_mv     -- wait for task
      case maybe_task of                      -- check for termination
        Nothing          -> io $ killGAP srv  -- terminate GAP server and stub
        Just (cmd, ivar) -> do                -- execute task:
          res <- io $ callGAP srv cmd           -- call GAP (may block)
          put ivar res                          -- release result
          case res of                           -- check for fatal server crash
            Left GAPCrash -> init                 -- yes: restart server
            _             -> do
              io $ pushStubQBack task_mv          -- no: put task_mv back on
              loop srv                            --     stubQ and recurse


-----------------------------------------------------------------------------
-- state

-- Counts the number of GAP servers and acts as lock for start/stop/barrier.
stubCount :: MVar Int
stubCount = unsafePerformIO $ newMVar 0
{-# NOINLINE stubCount #-}   -- required to protect unsafePerformIO hack


stubQ :: Chan (MVar (Maybe GAPTask))
stubQ = unsafePerformIO $ newChan
{-# NOINLINE stubQ #-}   -- required to protect unsafePerformIO hack

-- May block.
popStubQFront :: IO (MVar (Maybe GAPTask))
popStubQFront = readChan stubQ

pushStubQBack :: MVar (Maybe GAPTask) -> IO ()
pushStubQBack = writeChan stubQ
