module Jt.Server (
    Server(..),
    HistoryUrl(..),
    AppUrl(..),
    combineEither
    ) where

import qualified Jt.Job as Job
import qualified Jt.History.Listing as History
import qualified Jt.App.Listing as App
import qualified Jt.QueryParameters as QP
import Data.List(nub)

data HistoryUrl = HistoryUrl String deriving (Show, Eq, Ord)

data AppUrl = AppUrl String deriving (Show, Eq, Ord)

data Server = Server { serverName :: String, appUrl :: AppUrl, historyUrl :: HistoryUrl } deriving (Show, Eq, Ord)

combineEither :: Either String [a] -> Either String [a] -> Either String [a]
combineEither (Left e1) (Left e2) = Left $ e1 ++ "\n" ++ e2
combineEither (Left e1) (Right _) = Left e1
combineEither (Right _) (Left e2) = Left e2
combineEither (Right r1) (Right r2) = Right $ r1 ++ r2

instance Job.JobProvider AppUrl where
  jobs (AppUrl url) = App.fetchJobs QP.defaultsQP url
  jobsWithOpts opts (AppUrl url) = App.fetchJobs opts url

instance Job.JobProvider HistoryUrl where
  jobs (HistoryUrl url) = History.fetchJobs QP.defaultsQP url
  jobsWithOpts opts (HistoryUrl url) = History.fetchJobs opts url


instance Job.JobProvider Server where
  jobs s = do
    apps <- Job.jobs $ appUrl s
    historyJobs <- Job.jobs $ historyUrl s
    let allJobs = combineEither apps historyJobs
    return $ fmap nub allJobs
  jobsWithOpts opts s = do
    apps <- Job.jobsWithOpts opts $ appUrl s
    historyJobs <- Job.jobsWithOpts opts $ historyUrl s
    let allJobs = combineEither apps historyJobs
    return $ fmap nub allJobs
