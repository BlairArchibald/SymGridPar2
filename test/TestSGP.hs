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

import SGP2.GAPServer (startST, stopST, callST, GAPHandleST, GAPObj,
                      fromGAPObj, toGAPObj, mkGAPCallFuncList, defaultGAP)

hdphComp :: Par ()
hdphComp = do
  io $ putStrLn "Starting Server"
  gap <- startST defaultGAP [] >>= get
  io $ putStrLn "Server Started"
  ret <- callST gap (mkGAPCallFuncList "Sum" [toGAPObj ([1 .. 10] :: [Integer])]) >>= get
  io $ putStrLn "Sum Called"

  case ret of
    Left e    -> error $ "GAP error: " ++ show e
    Right val -> io . putStrLn $ "Got: " ++ show (fromGAPObj val :: Integer)

  io $ putStrLn "Stopping GAP"
  stopST gap

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

