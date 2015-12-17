-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- module Jt.History (
--     fetchJobs,
--     Server(..)
--     ) where

-- data Server = HistoryServer { url :: String } | AppServer { url :: String } deriving (Show)

-- fetchJobs :: Server -> String -> IO (Maybe [Job])
-- fetchJobs (HistoryServer url) = fetchHistoryJobs
-- fetchJobs (AppServer url) = fetchHistoryJobs


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt.History (
    fetchJobs
    ) where



import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import qualified Jt.Net as Net
import qualified Jt.Job as Job
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Int as Ints
import qualified Data.String.Utils as StringUtils
import Jt.QueryParameters

--   private[JobFetcher] case class JobHistoryResponse(jobs: HistoryJobList)

--   private[JobFetcher] case class HistoryJobList(job: List[HistoryJob])
-- case class HistoryJob(
--   startTime: Long,
--   finishTime: Long,
--   id: String,
--   name: String,
--   queue: String,
--   user: String,
--   state: String,
--   mapsTotal: Int,
--   mapsCompleted: Int,
--   reducesTotal: Int,
--   reducesCompleted: Int,
--   flowIdOpt: Option[String],
--   idxInFlowOpt: Option[Int],
--   flowSizeOpt: Option[Int],
--   clusterOpt: Option[Cluster]) extends BaseJob {
--   override def startedTime = startTime
--   override def finishedTime = finishTime
--   def url: String = s"""${cluster.jobHistory}/jobhistory/job/$jobId"""
-- }

data Jobs = Jobs { job :: [HistoryJob] } deriving (Show, Generic)
data HistoryResponse = HistoryResponse { jobs :: Jobs } deriving (Show, Generic)
data HistoryJob = HistoryJob { id :: String,
    user :: String,
    name :: String,
    queue :: String,
    state :: String,
    startTime :: Ints.Int64,
    finishTime :: Ints.Int64} deriving (Show, Generic)

instance FromJSON HistoryResponse
instance FromJSON Jobs
instance FromJSON HistoryJob

applicationId :: HistoryJob -> String
applicationId (HistoryJob appId _ _ _ _ _ _) = StringUtils.replace "job_" "application_" appId

jobId :: HistoryJob -> String
jobId (HistoryJob appId _ _ _ _ _ _) = StringUtils.replace "application_" "job_" appId

addInfo :: String -> IO (Either String a)  -> IO (Either String a)
addInfo extra idata = do
    d <- idata
    case d of
      Left l -> return $ Left (extra ++ l)
      Right r -> return $ Right r

extractApps :: IO (Either String BL.ByteString) -> IO (Either String HistoryResponse)
extractApps ioData = do
  e <- ioData
  return (do
      bs <- e
      maybeToError bs $ decode bs
      )
  where maybeToError _ (Just a) = Right a
        maybeToError input Nothing
          | (BL.unpack input) == "{\"jobs\":null}" = Right $ HistoryResponse (Jobs [])
          | otherwise = Left ("Unable to decode response:\n" ++ (BL.unpack input))


fetchApps :: QueryParameters -> String -> IO (Either String [HistoryJob])
fetchApps params url = do
    maybeApps <- addInfo ("Url Queried: " ++ url ++ "\n") $ extractApps $ Net.queryUrlWith params url
    let resApps = fmap job $ fmap jobs maybeApps
    return resApps


historyJobToJob :: HistoryJob -> Job.Job
historyJobToJob app@(HistoryJob _ cUser cName cQueue cState cStartedTime cFinishedTime) = Job.Job {
    Job.name = nameM,
    Job.queue = cQueue,
    Job.user = cUser,
    Job.state = cState,
    Job.startedTime = cStartedTime,
    Job.finishedTime = cFinishedTime,
    Job.flowId = flowIdM,
    Job.flowStepId = stepIdM,
    Job.jobId = jobId app,
    Job.applicationId = applicationId app,
    Job.jobUrl = ""
  }
  where
    (Job.JobNameElements flowIdM stepIdM nameM) = Job.parseName cName


fetchJobs :: QueryParameters -> String -> IO (Either String [Job.Job])
fetchJobs queryParameters url = do
  e <-  fetchApps queryParameters url
  return (fmap convertL e)
  where
    convertL lst = fmap historyJobToJob lst

