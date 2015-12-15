module Jt.Job (
    Job(..)
    ) where

import qualified Data.Int as Ints

data Job = Job {
  name :: String,
  queue :: String,
  user :: String,
  state :: String,
  startedTime :: Ints.Int64,
  finishedTime :: Ints.Int64,
  flowId :: Maybe String,
  idxInFlow :: Maybe Ints.Int16,
  flowSize :: Maybe Ints.Int16,
  jobId :: String,
  applicationId :: String,
  jobUrl :: String
} deriving (Show, Eq)
