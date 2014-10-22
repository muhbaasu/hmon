{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

module Collectors.Memory where

import Control.Applicative ((<$>), (<*))
import Control.Lens.TH (makeClassy)
import Data.Text (Text(), pack)
import Text.Parsec
import Text.Parsec.Text

data MemoryMetric = MemoryMetric { _metricName :: !Text
                                 , _size       :: !Int }
                  deriving Show

$(makeClassy ''MemoryMetric)

parseStat :: Parser [MemoryMetric]
parseStat = endBy memoryLine newline

memoryLine :: Parser MemoryMetric
memoryLine = do
  name <- pack <$> manyTill anyChar (string ":") <* skipMany1 space
  memSize <- memorySize
  return $ MemoryMetric name memSize

memorySize :: Parser Int
memorySize = read <$> many1 digit <* optional (string " kB")
