{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

module Collectors.Cpu where

import Control.Applicative ((<$>), (<*))
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
  (m:ms) <- endBy cpuLine newline
  return $ StatMetrics m ms

cpuLine :: Parser CoreMetric
cpuLine = do
  name <- identifier <* space
  (u:n:sy:idl:io:ir:so:_) <- metrics
  return $ CoreMetric name u n sy idl io ir so

metrics :: Parser [Int]
metrics = sepBy number (char ' ') <?> "metrics"

number :: Parser Int
number = read <$> many1 digit <?> "number"

identifier :: Parser Text
identifier = do
  n <- pack <$> string "cpu" <?> "identifier"
  i <- digit <|> space <?> "identifier"
  return $ n `append` if i == ' '
                      then empty
                      else singleton i
