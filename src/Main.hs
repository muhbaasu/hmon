{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever, liftM)
import Data.Aeson (Value, (.=), toJSON, object, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (isPrefixOf)
import Network.WebSockets (Connection, DataMessage(Text), sendDataMessage,
                           runServer, acceptRequest)

cpuInfo :: String -> [[Double]]
cpuInfo = map (map parseNum) . map words . removeSpaces . getCoreLines
    where getCoreLines = takeWhile (not . isPrefixOf "intr") . drop 1 . lines
          removeSpaces = map (dropWhile (/= ' '))
          parseNum s = (read s :: Double)

cpuPercentage :: [Double] -> Double -> Double -> [Double]
cpuPercentage cpu prevIdle prevTotal = idle:total:diffUsage:[]
  where idle = cpu !! 3
        total = sum cpu
        diffIdle = idle - prevIdle
        diffTotal = total - prevTotal
        diffUsage = (100 * (diffTotal - diffIdle) / diffTotal)

readCpuInfo :: IO String
readCpuInfo = readFile "/proc/stat"

delay :: Int
delay = 1000000 -- ^ Milliseconds

recWritePerc :: Chan Value -> [Double] -> [Double] -> IO ()
recWritePerc chan pis pts = do
  infos <- (liftM cpuInfo) $ readCpuInfo
  let results = zipWith (!!) [ zipWith (cpuPercentage cpu) pis pts | cpu <- infos ] [0..]
  let rounded = map (roundWith 2 . (!!2)) results
  putStrLn $ show rounded
  writeChan chan $ object ["cpu" .= toJSON rounded]
  threadDelay delay
  recWritePerc chan (map (!!0) results) (map (!!1) results)

roundWith :: Integer -> Double -> Double
roundWith n f = let i = (round $ f * (10 ^ n)) :: Integer
                    d = (10.0 ^^ n)
                in (fromIntegral i) / d

zeros :: [Double]
zeros = [0,0..0]

cpuMonitor :: Chan Value -> IO ()
cpuMonitor chan = recWritePerc chan zeros zeros

connectionHandler :: Chan Value -> Connection -> IO ()
connectionHandler chan con = forever $ do
  m <- readChan chan
  let msg = encode m
  putStrLn $ "Publishing: " ++ (unpack msg)
  sendDataMessage con $ Text msg

websocketServer :: Chan Value -> IO ()
websocketServer chan = do
  runServer "0.0.0.0" 9001 acceptor

  where acceptor pending = do
          con <- acceptRequest pending
          putStrLn "Accepted new connection."
          connectionHandler chan con
          return ()

main :: IO ()
main = do
  c <- newChan
  _ <- forkIO $ cpuMonitor c
  _ <- forkIO $ websocketServer c
  _ <- getLine
  return ()
