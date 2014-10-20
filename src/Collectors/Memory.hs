{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Collectors.Memory where

import Control.Lens.TH (makeClassy)
import Text.Parsec
import Text.Parsec.String

data MemoryMetric = MemoryMetric { _metricName :: ! [Char]
                                 , _size :: !Int }
                    deriving Show

$(makeClassy ''MemoryMetric)

parseStat :: Parser [MemoryMetric]
parseStat = do
  metrics <- endBy memoryLine newline
  return metrics


memoryLine :: Parser MemoryMetric
memoryLine = do
  name <-  manyTill anyChar $ string ":"
  _ <- many1 space
  memSize <- memorySize
  return $ MemoryMetric name memSize

memorySize :: Parser Int
memorySize = do
  memSize <- many1 digit <?> "size"
  return  $ read memSize

-- Fix this part which is optional, to make it actually work
--memEndLine = do
-- _ <- try option space string "kB"
-- _ <- newline
-- return ()
