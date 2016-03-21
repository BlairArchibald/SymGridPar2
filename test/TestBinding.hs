module Main where

import qualified Data.ByteString.Char8 as BS (pack)

import Binding.GAPServerIO (version, GAPServer, GAP(..), defaultGAP,
                            startGAP, stopGAP, callGAP,
                            fromGAPObj, toGAPObj, mkGAPCallFuncList)

sumGAP :: GAPServer -> [Integer] -> IO Integer
sumGAP srv xs = do
  let cmd = mkGAPCallFuncList (BS.pack "Sum") [toGAPObj xs]
  result <- callGAP srv cmd
  case result of
    Left e    -> error $ "GAP error: " ++ show e
    Right obj -> return $ fromGAPObj obj


main :: IO ()
main = do
  putStrLn "Starting GAP"
  Right gap <- startGAP defaultGAP
  putStrLn "GAP Started"

  putStrLn "Performing Sum"
  res <- sumGAP gap [1 .. 10]
  putStrLn $ "Sum [1 .. 10]: "  ++ show res

  putStrLn "Stopping GAP"
  stopGAP gap
  putStrLn "Complete"
