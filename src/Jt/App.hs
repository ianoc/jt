{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt.App (
    -- fetchJobs,
    -- Server(..)
    fetchJobs
    ) where



import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import Jt.Net
import qualified Jt.Job as Job
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Int as Ints
import qualified Data.String.Utils as StringUtils
import qualified Jt.Job as Job

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


data Server = HistoryServer { url :: String } | AppServer { url :: String } deriving (Show)

applicationId :: App -> String
applicationId (App appId _ _ _ _ _ _) = StringUtils.replace "job_" "application_" appId

jobId :: App -> String
jobId (App appId _ _ _ _ _ _) = StringUtils.replace "application_" "job_" appId


extractApps :: IO (Either String BL.ByteString) -> IO (Either String AppsResponse)
extractApps ioData = do
  e <- ioData
  return (do
      bs <- e
      maybeToError $ decode bs
      )
  where maybeToError (Just a) = Right a
        maybeToError  Nothing = Left "Unable to decode response"

fetchApps :: String -> IO (Either String [App])
fetchApps url = do
    maybeApps <- extractApps $ queryUrl url
    let resApps = fmap app $ fmap apps maybeApps
    return resApps


appToJob :: App -> Job.Job
appToJob app@(App _ cUser cName cQueue cState cStartedTime cFinishedTime) = Job.Job {
    Job.name = cName,
    Job.queue = cQueue,
    Job.user = cUser,
    Job.state = cState,
    Job.startedTime = cStartedTime,
    Job.finishedTime = cFinishedTime,
    Job.flowId = Nothing,
    Job.idxInFlow = Nothing,
    Job.flowSize = Nothing,
    Job.jobId = jobId app,
    Job.applicationId = applicationId app,
    Job.jobUrl = ""
  }


fetchJobs :: String -> IO (Either String [Job.Job])
fetchJobs url = do
  e <-  fetchApps url
  return (fmap convertL e)
  where
    convertL lst = fmap appToJob lst

