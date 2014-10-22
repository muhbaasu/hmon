{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Collectors.Memory where

import Control.Applicative ((<*))
import Control.Lens.TH (makeClassy)
import Data.Functor ((<$>))
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
  _ <- skipMany1 space
  memSize <- memorySize
  return $ MemoryMetric name memSize

memorySize :: Parser Int
memorySize =  read <$> (many1 digit) <* (optional $ string " kB")
