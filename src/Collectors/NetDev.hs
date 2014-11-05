{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Collectors.Network where

import Control.Applicative ((*>))
import Control.Lens.TH (makeClassy)
import Data.Functor ((<$>))
import Data.Text (Text, pack)
import Text.Parsec (manyTill, anyChar, digit, endBy, string
                   , many1, char, sepBy, newline, space)
import Text.Parsec.String

data NetTransmittedMetric = NetTransmittedMetric { _bytesT      :: !Int
                                                 , _packetsT    :: !Int
                                                 , _errsT       :: !Int
                                                 , _dropT       :: !Int
                                                 , _fifoT       :: !Int
                                                 , _collsT      :: !Int
                                                 , _carrierT    :: !Int
                                                 , _compressedT :: !Int }
                                                 deriving Show

$(makeClassy ''NetTransmittedMetric)


data NetReceivedMetric = NetReceivedMetric { _bytesR      :: !Int
                                           , _packetsR    :: !Int
                                           , _errsR       :: !Int
                                           , _dropR       :: !Int
                                           , _fifoR       :: !Int
                                           , _frameR      :: !Int
                                           , _compressedR :: !Int
                                           , _multicastR  :: !Int }
                                           deriving Show

$(makeClassy ''NetReceivedMetric)


data NetDeviceMetric = NetDeviceMetric { _interface   :: !Text
                                       , _received    :: !NetReceivedMetric
                                       , _transmitted :: !NetTransmittedMetric }
                                       deriving Show

$(makeClassy ''NetDeviceMetric)

identifier :: Parser Text
identifier = pack <$> manyTill anyChar (string ":")

toMetric :: [Int] -> (NetReceivedMetric, NetTransmittedMetric)
toMetric ms = (recvdMetric, transMetric)
  where
   (rList, tList) = splitAt 8 ms
   (b:p:e:d:fi:fr:c:m:_) = rList
   (bytes:pkts:errs:drp:fifo:colls:carr:compr:_) = tList
   recvdMetric = NetReceivedMetric b p e d fi fr c m
   transMetric =  NetTransmittedMetric bytes pkts errs drp fifo colls carr compr


metrics :: Parser [Int]
metrics = sepBy number (many1 $ char ' ')

number :: Parser Int
number = read <$> many1 digit

deviceLine :: Parser NetDeviceMetric
deviceLine = do
  i <- identifier
  _ <- many1 space
  (recvd, transm) <- toMetric <$> metrics
  return  $ NetDeviceMetric i recvd transm

parseStat :: Parser [NetDeviceMetric]
parseStat = skipLine  *> skipLine  *> endBy deviceLine newline
          where skipLine = manyTill anyChar newline
