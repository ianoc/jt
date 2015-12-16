module Jt.Server (
    Server(..),
    HistoryUrl(..),
    AppUrl(..)
    ) where

import qualified Jt.Job as Job
import qualified Jt.App as App
import qualified Jt.QueryParameters as QP

data HistoryUrl = HistoryUrl String deriving (Show, Eq, Ord)

data AppUrl = AppUrl String deriving (Show, Eq, Ord)

data Server = Server { serverName :: String, appUrl :: AppUrl, historyUrl :: HistoryUrl } deriving (Show, Eq, Ord)

instance Job.JobProvider AppUrl where
  jobs (AppUrl url) = App.fetchJobs QP.defaultsQP (url ++ "/ws/v1/cluster/apps?states=running,failed,finished&limit=10")
  jobsWithOpts opts (AppUrl url) = App.fetchJobs opts (url ++ "/ws/v1/cluster/apps?states=running,failed,finished")

instance Job.JobProvider Server where
  jobs s = do
    apps <- Job.jobs $ appUrl s
    return apps
  jobsWithOpts opts s = do
    apps <- Job.jobsWithOpts opts $ appUrl s
    return apps

