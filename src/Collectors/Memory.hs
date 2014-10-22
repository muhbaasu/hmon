{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

module Collectors.Memory where

import Control.Applicative ((<*))
import Control.Lens.TH (makeClassy)
import Data.Functor ((<$>))
import Data.Text (Text(), pack)
import Text.Parsec
import Text.Parsec.Text

data MemoryMetric = MemoryMetric { _metricName :: !Text
                                 , _size       :: !Int }
                  deriving Show

$(makeClassy ''MemoryMetric)

parseStat :: Parser [MemoryMetric]
parseStat = do
  metrics <- endBy memoryLine newline
  return metrics

memoryLine :: Parser MemoryMetric
memoryLine = do
  name <- manyTill anyChar $ string ":"
  _ <- skipMany1 space
  memSize <- memorySize
  return $ MemoryMetric (pack name) memSize

memorySize :: Parser Int
memorySize = read <$> (many1 digit) <* (optional $ string " kB")
