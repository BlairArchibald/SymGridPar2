-- Binding for GAP.
-- * GAP server runs in separate OS process, communicating via anonymous pipes.
-- * The communication protocol is described in 'GAPServer.g'.

{-# LANGUAGE FlexibleInstances #-}  -- for Rational instance of FromGAPObj
{-# LANGUAGE DeriveGeneric #-}      -- for deriving Binary instance
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Binding.GAPServerIO
  ( -- * version of GAP binding
    version,          -- :: String

    -- * configuration parameters for starting GAP
    GAP(..),          -- record collecting all params; insts: Generic, Binary
    defaultGAP,       -- :: GAP
    cmdLineGAP,       -- :: GAP -> String

    -- * GAP server handle
    GAPServer,        -- abstract type serving as a handle to server

    -- * starting/stopping GAP server
    startGAP,  -- :: GAP -> IO (Either GAPError GAPServer)
    stopGAP,   -- :: GAPServer -> IO ()
    killGAP,   -- :: GAPServer -> IO ()

    -- * blocking call to GAP server
    callGAP,   -- :: GAPServer -> Lazy.ByteString -> IO (Either GAPError GAPObj)

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

    -- * abstract class of encoder/decoders for GAP objects
    GAPEnc,           -- abstract class
    toGAPObj,         -- :: (GAPEnc a) => a -> GAPObj
    fromGAPObj,       -- :: (GAPEnc a) => GAPObj -> a

    -- * building lazy byte strings (to pass to callGAP) from GAP objects
    buildGAPObj,      -- :: GAPObj -> Lazy.Builder

    -- * convenience: construct a GAP function call
    mkGAPCallFuncList  -- :: Strict.ByteString -> [GAPObj] -> Lazy.ByteString
  ) where

import Prelude
import Control.DeepSeq (NFData)
import Control.Monad (when, liftM2, replicateM)
import Data.Binary (Binary)
import Data.Binary.Get (Get, runGet, getByteString,
                        getWord8, getWord16le, getWord32le, getWord64le)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString
       as BS (empty, null, length, tail, pack, unpack, hPut)
import qualified Data.ByteString.Char8 as CBS (pack, unpack)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy
       as LBS (empty, null, length, hGet, hPut, hGetContents,
               fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Builder as Lazy (Builder)
import qualified Data.ByteString.Lazy.Builder
       as LBS (toLazyByteString, hPutBuilder, byteString, char7, string7)
import qualified Data.ByteString.Lazy.Builder.ASCII as LBS (word64Dec)
import Data.Char (chr, ord)
import Data.Functor ((<$>))
import Data.List (foldl', unfoldr)
import Data.Monoid (mempty, (<>))
import Data.Ratio ((%), numerator, denominator)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize (put, putWord8, get, getWord8)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (sizeOf, peek)
import GHC.Generics (Generic)
import System.Process (ProcessHandle, runInteractiveCommand, waitForProcess,
                       terminateProcess)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, BufferMode(..),
                  hClose, hFlush, hGetBuf)


-----------------------------------------------------------------------------
-- GAP config

-- data type specifying parameters for calling GAP
data GAP = GAP
           { rootdir :: String
             -- ^ HaskellGAP root directory, non-empty;
             -- absolute path of directory containing file 'GAPServer.g'.

           , exe :: String
             -- ^ GAP executable, non-empty;
             -- this must either be a command on the OS search path,
             -- or an absolute path, or a relative path from rootdir.

           , flags :: String
             -- ^ GAP command line flags, or empty;
             -- don't use '-q' and '-T' as this will be added automatically.

           , workspace :: String
             -- ^ workspace to load, or empty;
             -- this must be an absolute path, or a relative path from rootdir.

           , rprefix :: Strict.ByteString
             -- ^ reply prefix, non-empty;
             -- best to use only printable 7-bit ASCII characters.
           } deriving (Generic, Show)  -- Show instance for debugging

instance Binary GAP

instance Serialize GAP where
  put gap = do Serialize.put $ rootdir gap
               Serialize.put $ exe gap
               Serialize.put $ flags gap
               Serialize.put $ workspace gap
               Serialize.put $ rprefix gap
  get = do rd <- Serialize.get
           e  <- Serialize.get
           f  <- Serialize.get
           ws <- Serialize.get
           rp <- Serialize.get
           return GAP { rootdir = rd, exe = e, flags = f,
                        workspace = ws, rprefix = rp }

defaultGAP :: GAP
defaultGAP = GAP
             { rootdir   = HGAPDIR
             , exe       = "gap.sh"
             , flags     = "-r -A"
             , workspace = ""
             , rprefix   = CBS.pack "-L]]6T8TRX:9:U10"  -- 16 random chars
             }

-- version string (must be non-empty and not contain characters ^ and $)
version :: String
version = "2b"  -- best to use only printable 7-bit ASCII characters here


-----------------------------------------------------------------------------
-- GAP errors

data GAPError = GAPCrash      -- fatal error (terminates GAP instance)
              | GAPSuPeRfail  -- sth went wrong (but GAP instance stayed alive)
              deriving (Eq, Ord, Show, Generic)

instance Binary GAPError

instance NFData GAPError

instance Serialize GAPError where
  put GAPCrash     = Serialize.putWord8 0
  put GAPSuPeRfail = Serialize.putWord8 1
  get = do tag <- Serialize.getWord8
           case tag of
             0 -> return GAPCrash
             1 -> return GAPSuPeRfail
             _ -> error "panic in instance Serialize GAPError: tag out of range"


-----------------------------------------------------------------------------
-- GAP API (in the IO monad)

-- abstract data type encapsulating connection to a GAP instance
data GAPServer = GAPServer { cmdline :: GAP,
                             stdin   :: Handle,
                             stdout  :: Handle,
                             stderr  :: Handle,
                             pid     :: ProcessHandle }

-- Turn the GAP record into a string callable by most Unix shells.
cmdLineGAP :: GAP -> String
cmdLineGAP gap = command ++ " " ++ options ++ " " ++ script
  where
    script  = rootdir gap ++ "/GAPServer.g"
    command | '/' `notElem` exe gap = exe gap                        -- no path
            | head (exe gap) == '/' = exe gap                        -- abs path
            | otherwise             = rootdir gap ++ "/" ++ exe gap  -- rel path
    options = flags gap ++ " -q -T" ++ wspace
    wspace | null (workspace gap)        = ""
           | head (workspace gap) == '/' = " -L " ++ workspace gap
           | otherwise = " -L " ++ rootdir gap ++ "/" ++ workspace gap

-- Starts a GAP server, returning a handle to the server.
-- Blocks until the server is up and ready to take commands.
-- Aborts immediately if there is a version mismatch; returns an error
-- if the GAP instance disconnects. In both cases, the GAP instance is killed.
startGAP :: GAP -> IO (Either GAPError GAPServer)
startGAP gap = do
  -- fork GAP at the end of a pipe
  -- putStrLn $ "DEBUG//GAPServerIO.startGAP.0: " ++ cmdLineGAP gap
  (h_in, h_out, h_err, ph) <- runInteractiveCommand $ cmdLineGAP gap
  -- set pipe handles to binary mode (no character/newline translation)
  hSetBinaryMode h_in  True
  hSetBinaryMode h_out True
  hSetBinaryMode h_err True
  -- disable buffering on pipe handles
  hSetBuffering h_in (BlockBuffering $ Just 512)  -- bufsize in 'GAPServer.g'
  hSetBuffering h_out NoBuffering
  hSetBuffering h_err NoBuffering
  -- construct GAP server handle
  let gap_server = GAPServer { cmdline = gap,
                               stdin   = h_in,
                               stdout  = h_out,
                               stderr  = h_err,
                               pid     = ph }
  -- read version information
  version_read <- readVersionString h_out
  if BS.null version_read
    then do
      -- version information could not be read: fatal GAP error
      killGAP gap_server
      return $ Left GAPCrash
    else do
      -- on version mismatch kill GAP server and abort
      let gap_version = CBS.unpack $ BS.tail version_read
      when (version /= gap_version) $ do
        killGAP gap_server
        error $ "GAPServerIO.startGAP: version mismatch (" ++
                version ++ "/=" ++ gap_version ++ ")"
      -- post reply prefix
      hPutWord64DecLn h_in $ fromIntegral $ BS.length $ rprefix gap
      BS.hPut h_in $ rprefix gap
      hFlush h_in
      -- block until ready msg can be read
      skipOverReplyPrefix (rprefix gap) h_out
      ready_msg <- hGetLn h_out
      -- putStrLn $ "DEBUG//GAPServerIO.startGAP.1: " ++ show ready_msg
      if BS.null ready_msg
        then do
          -- ready message could not be read: fatal GAP error
          killGAP gap_server
          return $ Left GAPCrash
        else do
          -- return GAP server handle
          return $ Right gap_server


-- Stops the given GAP server.
-- Blocks until GAP server has delivered all output.
stopGAP :: GAPServer -> IO ()
stopGAP srv = do
  -- close server's input stream
  hClose (stdin srv)
  -- drain and close output streams
  last_msg <- LBS.hGetContents (stdout srv)
--  putStrLn $ "DEBUG//GAPServerIO.stopGAP.1: " ++ show last_msg
  hClose (stdout srv)
  hClose (stderr srv)
  -- wait for the server to terminate, then terminate
  exit_code <- waitForProcess (pid srv)
--  putStrLn $ "DEBUG//GAPServerIO.stopGAP.2: " ++ show exit_code
  return ()


-- Stops the given GAP server immediately.
-- Does not block until GAP server has finished.
killGAP :: GAPServer -> IO ()
killGAP srv = do
  -- close server's input and output streams
  hClose (stdin srv)
  hClose (stdout srv)
  hClose (stderr srv)
  -- forcibly terminate server (by sending SIGTERM)
  terminateProcess (pid srv)


-- Call given GAP server to execute given command.
-- Blocks until the server delivers the result of the call, or an error.
callGAP :: GAPServer -> Lazy.ByteString -> IO (Either GAPError GAPObj)
callGAP srv cmd = do
  -- post a command
  hPutWord64DecLn (stdin srv) $ fromIntegral $ LBS.length cmd
  LBS.hPut (stdin srv) cmd
  hFlush (stdin srv)
--  putStrLn $ "DEBUG//GAPServerIO.callGAP.1: " ++ show cmd
  -- skip ahead to just beyond reply prefix (or to EOF, whichever comes first)
  skipOverReplyPrefix (rprefix $ cmdline srv) (stdout srv)
--  putStrLn $ "DEBUG//GAPServerIO.callGAP.2"
  -- read header (64-bit word)
  header <- hGetWord64le (stdout srv)
  case header of
    Nothing -> do
      -- result header could not be read: fatal GAP error
      killGAP srv
      return $ Left GAPCrash
    Just output_bytes -> do
--      putStrLn $ "DEBUG//GAPServerIO.callGAP.3: " ++ show output_bytes
      if output_bytes == 0
        then return $ Left GAPSuPeRfail  -- non-fatal error
        else do
          -- read payload (lazy bytestring)
          output <- LBS.hGet (stdout srv) $ fromIntegral output_bytes
          let bytes_read = LBS.length output
--          putStrLn $ "DEBUG//GAPServerIO.callGAP.4: " ++ show bytes_read
--          putStrLn $ "DEBUG//GAPServerIO.callGAP.5: " ++ show output
          if bytes_read < fromIntegral output_bytes
            then do
              -- payload could not be read in full: fatal GAP error
              killGAP srv
              return $ Left GAPCrash
            else do         
              -- decode payload into GAP data type and return
              let !result = decodeGAPObj output
--              putStrLn $ "DEBUG//GAPServerIO.callGAP.6: " ++ show result
              return $ Right result


-- Blocks and reads a non-empty byte string from given handle. The byte string 
-- is delimted by '^' to the left and '$' to the right, but the returned string
-- excludes the '$'. An empty byte string is returned to signal error
-- (eg. reaching EOF before the second delimiter).
readVersionString :: Handle -> IO Strict.ByteString
readVersionString hdl = do
  let caret  = fromIntegral $ ord '^'
      dollar = fromIntegral $ ord '$'
  prefix <- hGetBytesReversedUntil hdl (== caret)
  case prefix of
    [] -> return BS.empty
    _  -> do bytes <- hGetBytesReversedUntil hdl (== dollar)
             case bytes of
               []        -> return BS.empty
               _:payload -> return $! BS.pack (caret : reverse payload)


-- Auxiliary function, reading bytes from 'hdl' until predicate 'p' is True.
-- Returns the list of bytes read in reverse order (ie. the byte fulfulling
-- p is first); returns the empty list on encountering EOF before 'p' is True.
hGetBytesReversedUntil :: Handle -> (Word8 -> Bool) -> IO [Word8]
hGetBytesReversedUntil hdl p = readUntil []
  where
    readUntil accu = do
      maybe_byte <- hGetWord8 hdl
      case maybe_byte of
        Nothing               -> return []           -- terminate: EOF reached
        Just byte | p byte    -> return (byte:accu)  -- terminate: 'p' is True
                  | otherwise -> readUntil (byte:accu)


-- Blocks and skips ahead reading bytes from given handle until EOF or 
-- until having consumed one full, un-interrupted reply prefix.
skipOverReplyPrefix :: Strict.ByteString -> Handle -> IO ()
skipOverReplyPrefix reply_prefix hdl = skip reply_prefix_bytes
  where
    reply_prefix_bytes = BS.unpack reply_prefix
    skip []           = return ()  -- terminate: reply prefix skipped
    skip (byte:bytes) = do
      maybe_b <- hGetWord8 hdl
      case maybe_b of
        Nothing -> return ()       -- terminate: EOF reached
        Just b  -> do
--          putStrLn $ "DEBUG//GAPServerIO.skipOverReplyPrefix: " ++ show b
          if b == byte
            then skip bytes
            else skip reply_prefix_bytes


-- Write given 64-bit word in decimal ASCII representation to given handle,
-- followed by a newline character.
hPutWord64DecLn :: Handle -> Word64 -> IO ()
hPutWord64DecLn hdl n =
  LBS.hPutBuilder hdl (LBS.word64Dec n <> LBS.char7 '\n')


-- Block and read bytes from given handle until reaching a newline character,
-- and return the bytes (including the trailing newline) as a bytestring;
-- returns an empty bytestring if EOF is reached before a newline.
hGetLn :: Handle -> IO Strict.ByteString
hGetLn hdl = do 
  let newline = fromIntegral $ ord '\n'
  bytes <- hGetBytesReversedUntil hdl (== newline)
  case bytes of
    [] -> return BS.empty
    _  -> return $! BS.pack (reverse bytes)


-- Block and read a single 8-bit byte from given handle;
-- return Just the byte if there is any, Nothing otherwise (ie. upon EOF).
hGetWord8 :: Handle -> IO (Maybe Word8)
hGetWord8 hdl = alloca $ \ buf -> do
  k <- hGetBuf hdl buf sizeOfWord8
  if k < sizeOfWord8
    then return Nothing
    else do
      byte <- peek buf
      return $ Just byte


-- Block and read a 64-bit word (in little endian format) from given handle;
-- return Just the word if there is any, Nothing otherwise (ie. upon EOF).
hGetWord64le :: Handle -> IO (Maybe Word64)
hGetWord64le hdl = do
  buf <- LBS.hGet hdl sizeOfWord64
  if LBS.length buf < sizeOfWord64
    then return Nothing
    else do
      let !word = runGet getWord64le buf
      return $ Just word


-----------------------------------------------------------------------------
-- GAP data types

data GAPObj = GAPFail
            | GAPBool   !Bool
            | GAPInt    !Integer
            | GAPRat    {-# UNPACK #-} !Rational
            | GAPList   !GAPList       -- INV: arg /= GAPNil
            | GAPNull                  -- empty list or empty string
            | GAPString {-# UNPACK #-} !Strict.ByteString  -- INV: arg non-empty
            | GAPOpaque {-# UNPACK #-} !Strict.ByteString  -- INV: arg non-empty
            deriving (Eq, Ord, Generic)

-- NB: type GAPList is not exposed
data GAPList = GAPNil
             | GAPCons !GAPObj !GAPList
             deriving (Eq, Ord, Generic)

instance Binary GAPObj
instance Binary GAPList

instance NFData GAPObj
instance NFData GAPList

instance Serialize GAPObj where
  put (GAPFail)      = Serialize.putWord8 0
  put (GAPBool b)    = Serialize.putWord8 1 >> Serialize.put b
  put (GAPInt i)     = Serialize.putWord8 2 >> Serialize.put i
  put (GAPRat r)     = Serialize.putWord8 3 >> Serialize.put r
  put (GAPList list) = Serialize.putWord8 4 >> Serialize.put list
  put (GAPNull)      = Serialize.putWord8 5
  put (GAPString bs) = Serialize.putWord8 6 >> Serialize.put bs
  put (GAPOpaque bs) = Serialize.putWord8 7 >> Serialize.put bs
  get = do tag <- Serialize.getWord8
           case tag of
             0 -> return GAPFail
             1 -> GAPBool   <$> Serialize.get
             2 -> GAPInt    <$> Serialize.get
             3 -> GAPRat    <$> Serialize.get
             4 -> GAPList   <$> Serialize.get
             5 -> return GAPNull
             6 -> GAPString <$> Serialize.get
             7 -> GAPOpaque <$> Serialize.get
             _ -> error "panic in instance Serialize GAPObj: tag out of range"

instance Serialize GAPList where
  put (GAPNil)           = Serialize.putWord8 0
  put (GAPCons obj list) = Serialize.putWord8 1 >>
                           Serialize.put obj >>
                           Serialize.put list
  get = do tag <- Serialize.getWord8
           case tag of
             0 -> return GAPNil
             1 -> liftM2 GAPCons Serialize.get Serialize.get
             _ -> error "panic in instance Serialize GAPList: tag out of range"


instance Show GAPObj where
  showsPrec _ GAPFail         = showString "fail"
  showsPrec _ (GAPBool True)  = showString "true"
  showsPrec _ (GAPBool False) = showString "false"
  showsPrec _ (GAPInt i)      = shows i
  showsPrec _ (GAPRat r)      = shows (numerator r) . showString " / " .
                                shows (denominator r)
  showsPrec _ (GAPList list)  = shows list
  showsPrec _ (GAPNull)       = showString "[ ]"
  showsPrec _ (GAPString bs)  = let s = escDoublequote (CBS.unpack bs)
                                  in showChar doublequote . showString s .
                                     showChar doublequote
  showsPrec _ (GAPOpaque bs)  = showString (CBS.unpack bs)

instance Show GAPList where
  showsPrec _ (GAPNil)           = showString "[ ]"  -- NB: dead code
  showsPrec _ (GAPCons obj list) = showString "[ " . shows obj . showl list
    where
      showl (GAPNil)             = showString " ]"
      showl (GAPCons obj' list') = showString ", " . shows obj' . showl list'


-- Construct a lazy bytestring builder for the given GAP object. 
-- The resulting builder can be executed by passing it to
-- 'Data.ByteString.Lazy.Builder.toLazyByteString'.
-- NOTE: This builder does not escape double quotes occuring within strings,
--       which could lead to problems.
buildGAPObj :: GAPObj -> Lazy.Builder
buildGAPObj (GAPFail)       = LBS.string7 "fail"
buildGAPObj (GAPBool True)  = LBS.string7 "true"
buildGAPObj (GAPBool False) = LBS.string7 "false"
buildGAPObj (GAPInt i)      = LBS.string7 (show i)
buildGAPObj (GAPRat r)      = LBS.string7 (show (numerator r)) <> slash <>
                              LBS.string7 (show (denominator r))
buildGAPObj (GAPList GAPNil)             = lbrack <> rbrack  -- NB: dead code
buildGAPObj (GAPList (GAPCons obj list)) = lbrack <> buildGAPObj obj <>
                                           buildGAPList list
buildGAPObj (GAPNull)      = lbrack <> rbrack
buildGAPObj (GAPString bs) = dquote <> LBS.byteString bs <> dquote
buildGAPObj (GAPOpaque bs) = space <> LBS.byteString bs <> space

-- Auxiliary function, called by 'buildGAPObj'.
buildGAPList :: GAPList -> Lazy.Builder
buildGAPList (GAPNil)           = rbrack
buildGAPList (GAPCons obj list) = comma <> buildGAPObj obj <> buildGAPList list


-----------------------------------------------------------------------------
-- testers for GAP data types

isGAPFail :: GAPObj -> Bool
isGAPFail GAPFail = True
isGAPFail _       = False

isGAPBool :: GAPObj -> Bool
isGAPBool (GAPBool _) = True
isGAPBool _           = False

isGAPInt :: GAPObj -> Bool
isGAPInt (GAPInt _) = True
isGAPInt _          = False

isGAPRat :: GAPObj -> Bool
isGAPRat (GAPRat _) = True
isGAPRat _          = False

isGAPList :: GAPObj -> Bool
isGAPList (GAPList _) = True
isGAPList GAPNull     = True      -- NB: GAPNull represents empty list.
isGAPList _           = False

isGAPNull :: GAPObj -> Bool
isGAPNull GAPNull = True
isGAPNull _       = False

isGAPString :: GAPObj -> Bool
isGAPString (GAPString _) = True
isGAPString GAPNull       = True  -- NB: GAPNull represents empty string.
isGAPString _             = False

isGAPOpaque :: GAPObj -> Bool
isGAPOpaque (GAPOpaque _) = True
isGAPOpaque _             = False


-----------------------------------------------------------------------------
-- conversion to and from GAP data types
-- NB: Can't access opaque GAP objects.

toGAPObj :: (GAPEnc a) => a -> GAPObj
toGAPObj = toGAP

fromGAPObj :: (GAPEnc a) => GAPObj -> a
fromGAPObj = fromGAP

-- The class GAPEnc is only exported abstractly.
-- Hence all instances have to be declared here.
class GAPEnc a where
  toGAP   :: a -> GAPObj
  fromGAP :: GAPObj -> a

instance GAPEnc GAPObj where
  toGAP   = id
  fromGAP = id

instance GAPEnc Bool where
  toGAP = GAPBool
  fromGAP (GAPBool b) = b
  fromGAP obj         = error_fromGAP "GAPBool" obj

instance GAPEnc Integer where
  toGAP = GAPInt
  fromGAP (GAPInt i) = i
  fromGAP obj        = error_fromGAP "GAPInt" obj

instance GAPEnc Rational where
  toGAP = GAPRat
  fromGAP (GAPRat r) = r
  fromGAP obj        = error_fromGAP "GAPRat" obj

-- NB: toGAP forces input list (due to hyperstrictness of GAPObj constructors)
instance (GAPEnc a) => GAPEnc [a] where
  toGAP [] = GAPNull
  toGAP xs = GAPList $ (foldr GAPCons GAPNil . map toGAP) xs
  fromGAP (GAPNull)      = []
  fromGAP (GAPList list) = (map fromGAP . unfoldr unCons) list where
                             unCons GAPNil        = Nothing
                             unCons (GAPCons o l) = Just (o, l)
  fromGAP obj            = error_fromGAP "GAPNull or GAPList" obj

instance GAPEnc () where
  toGAP () = GAPNull
  fromGAP GAPNull = ()
  fromGAP obj     = error_fromGAP "GAPNull" obj

instance GAPEnc Strict.ByteString where
  toGAP bs | BS.null bs = GAPNull
           | otherwise  = GAPString bs
  fromGAP (GAPNull)      = BS.empty
  fromGAP (GAPString bs) = bs
  fromGAP obj            = error_fromGAP "GAPNull or GAPString" obj

-- NB: toGAP expensive (due to conversion from lazy to strict bytestring)
instance GAPEnc Lazy.ByteString where
  toGAP bs | LBS.null bs = GAPNull
           | otherwise   = GAPString $ LBS.toStrict bs
  fromGAP (GAPNull)      = LBS.empty
  fromGAP (GAPString bs) = LBS.fromStrict bs
  fromGAP obj            = error_fromGAP "GAPNull or GAPString" obj

instance (GAPEnc a) => GAPEnc (Maybe a) where
  toGAP Nothing  = GAPFail
  toGAP (Just x) = toGAP x
  fromGAP GAPFail = Nothing
  fromGAP obj     = Just (fromGAP obj)

instance (GAPEnc a, GAPEnc b) => GAPEnc (a,b) where
  toGAP (x1,x2) = GAPList $ GAPCons (toGAP x1) $ GAPCons (toGAP x2) $ GAPNil
  fromGAP (GAPList (GAPCons o1 (GAPCons o2 GAPNil))) = (fromGAP o1, fromGAP o2)
  fromGAP obj = error_fromGAP "GAPList [_,_]" obj

instance (GAPEnc a, GAPEnc b, GAPEnc c) => GAPEnc (a,b,c) where
  toGAP (x1,x2,x3) = GAPList $ GAPCons (toGAP x1) $ GAPCons (toGAP x2) $ GAPCons (toGAP x3) $ GAPNil
  fromGAP (GAPList (GAPCons o1 (GAPCons o2 (GAPCons o3 GAPNil)))) = (fromGAP o1, fromGAP o2, fromGAP o3)
  fromGAP obj = error_fromGAP "GAPList [_,_,_]" obj

instance (GAPEnc a, GAPEnc b, GAPEnc c, GAPEnc d) => GAPEnc (a,b,c,d) where
  toGAP (x1,x2,x3,x4) = GAPList $ GAPCons (toGAP x1) $ GAPCons (toGAP x2) $ GAPCons (toGAP x3) $ GAPCons (toGAP x4) $ GAPNil
  fromGAP (GAPList (GAPCons o1 (GAPCons o2 (GAPCons o3 (GAPCons o4 GAPNil))))) = (fromGAP o1, fromGAP o2, fromGAP o3, fromGAP o4)
  fromGAP obj = error_fromGAP "GAPList [_,_,_,_]" obj

instance (GAPEnc a, GAPEnc b, GAPEnc c, GAPEnc d, GAPEnc e) => GAPEnc (a,b,c,d,e) where
  toGAP (x1,x2,x3,x4,x5) = GAPList $ GAPCons (toGAP x1) $ GAPCons (toGAP x2) $ GAPCons (toGAP x3) $ GAPCons (toGAP x4) $ GAPCons (toGAP x5) $ GAPNil
  fromGAP (GAPList (GAPCons o1 (GAPCons o2 (GAPCons o3 (GAPCons o4 (GAPCons o5 GAPNil)))))) = (fromGAP o1, fromGAP o2, fromGAP o3, fromGAP o4, fromGAP o5)
  fromGAP obj = error_fromGAP "GAPList [_,_,_,_,_]" obj

error_fromGAP :: String -> GAPObj -> a
error_fromGAP expected found =
  error $ "GAPServerIO.fromGAPObj: expected " ++ expected ++
          ", found " ++ show found


-----------------------------------------------------------------------------
-- utilities

-- Construct a GAP function call (to be passed as a command to 'callGAP')
-- from the given function name and the given list of arguments.
mkGAPCallFuncList :: Strict.ByteString -> [GAPObj] -> Lazy.ByteString
mkGAPCallFuncList func args = LBS.toLazyByteString builder
  where
    builder = LBS.byteString func <> lpar <> buildArgs args <> rpar <> scolon
    buildArgs []   = mempty
    buildArgs objs = foldr1 (\ x y -> x <> comma <> y) $ map buildGAPObj objs


-----------------------------------------------------------------------------
-- GAP-to-Haskell encoding (internal)

-- Decode given bytestring into a (fully evaluated) GAP object.
decodeGAPObj :: Lazy.ByteString -> GAPObj
decodeGAPObj = runGet parseGAPObj

-- Parse one byte tag and return the character; does not check tag!
parseTag :: Get Char
parseTag = chr . fromIntegral <$> getWord8

-- Parse one byte length.
parseShortLength :: Get Word64
parseShortLength = fromIntegral <$> getWord8

-- Parse 8 byte length (in little endian format).
parseLongLength :: Get Word64
parseLongLength = getWord64le

-- Parse 'k' bytes representing a non-negative integer (little endian format).
-- The values of 'k' are 0, 1, 2, 4, and multiples of 8.
parseAbsInt :: Word64 -> Get Integer
parseAbsInt k
  | k == 0            = return 0
  | k == sizeOfWord8  = fromIntegral <$> getWord8
  | k == sizeOfWord16 = fromIntegral <$> getWord16le
  | k == sizeOfWord32 = fromIntegral <$> getWord32le
  | r == 0            = integer <$> replicateM (fromIntegral n) getWord64le
  | otherwise = error $ "GAPServerIO.parseAbsInt: panic (k == " ++ show k ++")"
      where
        (n,r) = k `divMod` sizeOfWord64
        integer = foldl' (\ i w -> i * twoToThe64 + fromIntegral w) 0 . reverse

-- Parse a GAP object, encoded by function 'ToHaskell' in 'GAPServer.g'
parseGAPObj :: Get GAPObj
parseGAPObj = do
  tag <- parseTag
  case tag of
    '!' -> return $ GAPFail
    '1' -> return $ GAPBool True
    '0' -> return $ GAPBool False
    'n' -> do k <- parseShortLength
              n <- negate <$> parseAbsInt k
              return $! GAPInt n
    'p' -> do k <- parseShortLength
              n <- parseAbsInt k
              return $! GAPInt n
    'N' -> do k <- parseLongLength
              n <- negate <$> parseAbsInt k
              return $! GAPInt n
    'P' -> do k <- parseLongLength
              n <- parseAbsInt k
              return $! GAPInt n
    '/' -> do num <- fromGAP <$> parseGAPObj
              denom <- fromGAP <$> parseGAPObj
              let r = num % denom
              return $! GAPRat r
    's' -> do k <- parseShortLength
              bs <- getByteString $ fromIntegral k  -- NB: k > 0 guaranteed
              return $! GAPString bs
    'S' -> do k <- parseLongLength
              bs <- getByteString $ fromIntegral k  -- NB: k > 0 guaranteed
              return $! GAPString bs
    'l' -> do k <- parseShortLength
              objs <- replicateM (fromIntegral k) parseGAPObj
              return $! toGAP objs
    'L' -> do k <- parseLongLength
              objs <- replicateM (fromIntegral k) parseGAPObj  -- NB: k > 0
              return $! toGAP objs
    'o' -> do k <- parseShortLength
              bs <- getByteString $ fromIntegral k  -- NB: k > 0 guaranteed
              return $! GAPOpaque bs
    'O' -> do k <- parseLongLength
              bs <- getByteString $ fromIntegral k  -- NB: k > 0 guaranteed
              return $! GAPOpaque bs
    _ -> error $ "GAPServerIO.parseGAPObj: panic (tag == " ++ show tag ++ ")"


-----------------------------------------------------------------------------
-- escaping double quotes in GAP strings (internal)

escDoublequote :: String -> String
escDoublequote []     = []
escDoublequote (c:cs) | c == doublequote = backslash : c : escDoublequote cs
                      | c == backslash   = c : escBackslashDoublequote cs
                      | otherwise        = c : escDoublequote cs

escBackslashDoublequote :: String -> String
escBackslashDoublequote []     = []
escBackslashDoublequote (c:cs) = c : escDoublequote cs


-----------------------------------------------------------------------------
-- some constants

-- size of 8-, 16-, 32- and 64-bit words (in bytes)
sizeOfWord8, sizeOfWord16, sizeOfWord32, sizeOfWord64 :: (Num a) => a
sizeOfWord8  = fromIntegral $ sizeOf (0 :: Word8)
sizeOfWord16 = fromIntegral $ sizeOf (0 :: Word16)
sizeOfWord32 = fromIntegral $ sizeOf (0 :: Word32)
sizeOfWord64 = fromIntegral $ sizeOf (0 :: Word64)

twoToThe64 :: Integer
twoToThe64 = 2 ^ (64 :: Int)

-- builders used to build lazy bytestrings (send as commands to GAP)
lpar, rpar, lbrack, rbrack, comma, scolon, dquote, slash, space :: Lazy.Builder
lpar   = LBS.char7 '('
rpar   = LBS.char7 ')'
lbrack = LBS.char7 '['
rbrack = LBS.char7 ']'
comma  = LBS.char7 ','
scolon = LBS.char7 ';'
dquote = LBS.char7 '"'
slash  = LBS.char7 '/'
space  = LBS.char7 ' '

-- special characters in GAP strings
backslash, doublequote :: Char
backslash   = '\\'
doublequote = '\"'
