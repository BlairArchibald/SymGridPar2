{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.HdpH
       (RTSConf(..), defaultRTSConf, updateConf,
        Par, runParIO_,
        myNode, allNodes, io, spark, pushTo, new, get, glob, rput,
        Node, IVar, GIVar,
        Thunk(Thunk), Closure, mkClosure,
        toClosure, ToClosure(locToClosure),
        static, StaticToClosure, staticToClosure,
        StaticDecl, declare, register, here)
import qualified Control.Parallel.HdpH as HdpH (declareStatic)

import System.IO (stdout, stderr, hSetBuffering, BufferMode(..))
import System.Environment (getArgs)
import Data.Monoid (mconcat)

import qualified Data.ByteString.Char8 as BS (pack)

import SGP2.GAPServer (startST, stopST, callST, GAPHandleST, GAPObj,
                      fromGAPObj, toGAPObj, mkGAPCallFuncList, defaultGAP)

import qualified Data.ByteString.Lazy as Lazy (ByteString)

initCommands :: [Lazy.ByteString]
initCommands = [
    -- Need a ! here to convince SGP that Read was successful since it doesn't seem to return a proper header.
    "!Read(\"/home/blair/src/haskell/SymGridPar2/test/finiteGeometry.g\");"
  ]

hdphComp :: Par ()
hdphComp = do
  gap <- startST defaultGAP initCommands >>= get
  io $ putStrLn "Server Started"

  io $ putStrLn "Getting candidates"
  ps <- getCandidates gap [1,500]
  io . putStrLn $ "Candidates: " ++ show ps

  io $ putStrLn "Stopping GAP"
  stopST gap

getCandidates:: GAPHandleST -> [Integer] -> Par [Integer]
getCandidates gap pts = do
          ret <- callST gap (mkGAPCallFuncList "getCandidates" [toGAPObj pts]) >>= get
          case ret of
            Left e    -> error  $ "GAP error: " ++ show e
            Right val -> return $ fromGAPObj val

parseOpts :: [String] -> IO (RTSConf, [String])
parseOpts args = do
  either_conf <- updateConf args defaultRTSConf
  case either_conf of
    Left err_msg                 -> error $ "parseOpts: " ++ err_msg
    Right (conf, remaining_args) -> return (conf, remaining_args)

$(return [])
declareStatic :: StaticDecl
declareStatic = mconcat [HdpH.declareStatic]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
  register declareStatic
  (conf, args) <- parseOpts =<< getArgs
  runParIO_ conf hdphComp
