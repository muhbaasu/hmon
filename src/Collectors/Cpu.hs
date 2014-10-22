{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

module Collectors.Cpu where

import Control.Lens.TH (makeClassy)
import Data.Text (Text(), append, empty, pack, singleton)
import Text.Parsec
import Text.Parsec.Text

data CoreMetric = CoreMetric { _coreName :: !Text
                             , _user     :: !Int
                             , _nice     :: !Int
                             , _system   :: !Int
                             , _idle     :: !Int
                             , _iowait   :: !Int
                             , _irq      :: !Int
                             , _softirq  :: !Int }
                deriving Show

$(makeClassy ''CoreMetric)

data StatMetrics = StatMetrics { _total :: !CoreMetric
                               , _cores :: ![CoreMetric] }
                 deriving Show

$(makeClassy ''StatMetrics)

parseStat :: Parser StatMetrics
parseStat = do
  ms <- endBy cpuLine newline
  let t  = head ms
      cs = tail ms
  return $ StatMetrics t cs

cpuLine :: Parser CoreMetric
cpuLine = do
  name <- identifier
  _ <- space
  ms <- metrics
  let (u:n:sy:idl:io:ir:so:_) = ms
  return $ CoreMetric name u n sy idl io ir so

metrics :: Parser [Int]
metrics = sepBy number (char ' ') <?> "metrics"

number :: Parser Int
number = do
  ds <- many1 digit <?> "number"
  return $ read ds

identifier :: Parser Text
identifier = do
  n <- string "cpu" <?> "identifier"
  i <- digit <|> space <?> "identifier"
  return $ (pack n) `append` if i == ' '
                                   then empty
                                   else singleton i
