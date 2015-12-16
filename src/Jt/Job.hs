module Jt.Job (
    Job(..),
    JobProvider(..)
    ) where

import qualified Data.Int as Ints
import qualified Jt.QueryParameters as QP

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


class JobProvider a where
    jobs :: a -> IO (Either String [Job])
    jobsWithOpts :: QP.QueryParameters -> a -> IO (Either String [Job])
    jobsE :: a -> IO [Job]
    jobsE j = do
        e <- jobs j
        case e of
            Right s -> return s
            Left e' ->  fail e'
