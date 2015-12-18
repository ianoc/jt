module Jt.DetailedJob (
    DetailedJob(..)
    ) where

import qualified Data.Int as Ints


data DetailedJob = DetailedJob {
  name :: String,
  queue :: String,
  user :: String,
  state :: String,
  startedTime :: Ints.Int64,
  finishedTime :: Ints.Int64,
  flowId :: Maybe String,
  flowStepId :: Maybe String,
  jobId :: String,
  numMappers :: Ints.Int32,
  numReducers :: Ints.Int32
} deriving (Show, Eq)

