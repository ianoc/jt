{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt.App.Listing (
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

data Apps = Apps { app :: [App] } deriving (Show, Generic)
data AppsResponse = AppsResponse { apps :: Apps } deriving (Show, Generic)
data App = App { id :: String,
    user :: String,
    name :: String,
    queue :: String,
    state :: String,
    startedTime :: Ints.Int64,
    finishedTime :: Ints.Int64} deriving (Show, Generic)

instance FromJSON AppsResponse
instance FromJSON Apps
instance FromJSON App

applicationId :: App -> String
applicationId (App appId _ _ _ _ _ _) = StringUtils.replace "job_" "application_" appId

jobId :: App -> String
jobId (App appId _ _ _ _ _ _) = StringUtils.replace "application_" "job_" appId

addInfo :: String -> IO (Either String a)  -> IO (Either String a)
addInfo extra idata = do
    d <- idata
    case d of
      Left l -> return $ Left (extra ++ l)
      Right r -> return $ Right r

extractApps :: IO (Either String BL.ByteString) -> IO (Either String AppsResponse)
extractApps ioData = do
  e <- ioData
  return (do
      bs <- e
      maybeToError bs $ decode bs
      )
  where maybeToError _ (Just a) = Right a
        maybeToError input Nothing
              | (BL.unpack input) == "{\"apps\":null}" = Right $ AppsResponse (Apps [])
              | otherwise = Left ("Unable to decode response:\n" ++ (BL.unpack input))

fetchApps :: QueryParameters -> String -> IO (Either String [App])
fetchApps params url = do
    let finalUrl = url ++ "/ws/v1/cluster/apps?states=running,failed,finished"
    maybeApps <- addInfo ("Url Queried: " ++ finalUrl ++ "\n") $ extractApps $ Net.queryUrlWith params finalUrl
    let resApps = fmap app $ fmap apps maybeApps
    return resApps


appToJob :: App -> Job.Job
appToJob app'@(App _ cUser cName cQueue cState cStartedTime cFinishedTime) = Job.Job {
    Job.name = nameM,
    Job.queue = cQueue,
    Job.user = cUser,
    Job.state = cState,
    Job.startedTime = cStartedTime,
    Job.finishedTime = cFinishedTime,
    Job.flowId = flowIdM,
    Job.flowStepId = stepIdM,
    Job.jobId = jobId app',
    Job.applicationId = applicationId app',
    Job.jobUrl = ""
  }
  where
    (Job.JobNameElements flowIdM stepIdM nameM) = Job.parseName cName


fetchJobs :: QueryParameters -> String -> IO (Either String [Job.Job])
fetchJobs queryParameters url = do
  e <-  fetchApps queryParameters url
  return (fmap convertL e)
  where
    convertL lst = fmap appToJob lst

