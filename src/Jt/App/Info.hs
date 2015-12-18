{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt.App.Info (
    fetchDetailedJob,
    fetchJob
    ) where


import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)
import qualified Jt.Job as Job
import qualified Jt.DetailedJob as DetailedJob
import Jt.QueryParameters
import Jt.Server(AppUrl(..))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Int as Ints
import qualified Data.String.Utils as StringUtils
import qualified Jt.Net as Net

data JobInfoResponse = JobInfoResponse { job :: JobInfo } deriving (Show, Generic)
data JobInfo = JobInfo {
    user :: String
    , name :: String
    , queue :: String
    , state :: String
    , startTime :: Ints.Int64
    , finishTime :: Ints.Int64
    , elapsedTime :: Ints.Int64
    , mapsTotal :: Ints.Int32
    , mapsCompleted :: Ints.Int32
    , reducesTotal :: Ints.Int32
    , reducesCompleted :: Ints.Int32
    , diagnostics :: String
    , uberized :: String
    , mapsPending :: Ints.Int32
    , mapsRunning :: Ints.Int32
    , reducesPending :: Ints.Int32
    , newReduceAttempts :: Ints.Int32
    , runningReduceAttempts :: Ints.Int32
    , failedReduceAttempts :: Ints.Int32
    , killedReduceAttempts :: Ints.Int32
    , successfulReduceAttempts :: Ints.Int32
    , newMapAttempts :: Ints.Int32
    , runningMapAttempts :: Ints.Int32
    , failedMapAttempts :: Ints.Int32
    , killedMapAttempts :: Ints.Int32
    , successfulMapAttempts :: Ints.Int32
    } deriving (Show, Generic)

instance FromJSON JobInfoResponse
instance FromJSON JobInfo

applicationId :: String -> String
applicationId jobId' = StringUtils.replace "job_" "application_" jobId'

jobId :: String -> String
jobId jobId' = StringUtils.replace "application_" "job_" jobId'

addInfo :: String -> Either String a  -> Either String a
addInfo extra (Left l) = Left (extra ++ l)
addInfo extra o = o

extractApps :: IO (Either String BL.ByteString) -> IO (Either String JobInfoResponse)
extractApps ioData = do
  e <- ioData
  return (do
      bs <- e
      maybeToError bs $ decode bs
      )
  where maybeToError _ (Just a) = Right a
        maybeToError input Nothing = Left ("Unable to decode response:\n" ++ (BL.unpack input))

extractNoJob :: Either String JobInfo -> Either String (Maybe JobInfo)
extractNoJob (Left "TooManyRedirects") = Right Nothing
extractNoJob (Right a) = Right (Just a)
extractNoJob (Left a) = Left a

fetchDetailedJob :: String -> QueryParameters -> AppUrl -> IO (Either String (Maybe JobInfo))
fetchDetailedJob jobId' params url = do
    let (AppUrl rawUrl) = url
    let finalUrl = rawUrl ++ "/proxy/" ++ (applicationId jobId') ++ "/ws/v1/mapreduce/jobs/" ++ (jobId jobId')
    jInfoEither <- extractApps $ Net.queryUrlWith params finalUrl
    let resApps = fmap job jInfoEither
    let withNoJob = extractNoJob resApps
    let resApps = addInfo ("Url Queried: " ++ finalUrl ++ "\n") $ withNoJob
    return resApps



jobInfoToJob :: String -> Maybe JobInfo -> Maybe DetailedJob.DetailedJob
jobInfoToJob _ Nothing = Nothing
jobInfoToJob id' (Just info) = Just $ DetailedJob.DetailedJob {
    DetailedJob.name = nameM,
    DetailedJob.queue = queue info,
    DetailedJob.user = user info,
    DetailedJob.state = state info,
    DetailedJob.startedTime = startTime info,
    DetailedJob.finishedTime = finishTime info,
    DetailedJob.flowId = flowIdM,
    DetailedJob.flowStepId = stepIdM,
    DetailedJob.jobId = jobId id',
    DetailedJob.numMappers = mapsTotal info,
    DetailedJob.numReducers = reducesTotal info
  }
  where
    (Job.JobNameElements flowIdM stepIdM nameM) = Job.parseName $ name info


fetchJob :: String -> QueryParameters -> AppUrl -> IO (Either String (Maybe DetailedJob.DetailedJob))
fetchJob jobId' queryParameters url = do
  e <-  fetchDetailedJob jobId' queryParameters url
  return $ convertL e
  where
    convertL lst = fmap (jobInfoToJob jobId') lst

