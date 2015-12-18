module Jt.Command.Details (
  detailsAction,
  detailsCommand
) where

import Jt
import Jt.Command.Utils
import Jt.Server
import Options.Applicative
import qualified Jt.App.Info as AppInfo
import qualified Jt.History.Info as HistoryInfo
import qualified Jt.DetailedJob as DetailedJob
import qualified Jt.QueryParameters as QP

data JobArgs = JobArgs {  jobCluster :: Maybe String
                         , jobJobId :: String
                         , jobTabs :: Bool
                         }

detailsCommand :: Command
detailsCommand = Command { commandName = "details"
                      , commandDesc = "details info on a job"
                      , commandParser = jobParser
                      , commandAction = detailsAction }

jobParser :: Parser JobArgs
jobParser = let
  clusterP = optional(strOption (long "cluster" <> short 'c' <> metavar "CLUSTER" <> help "cluster to operate from"))
  jobP = strOption (long "job" <> short 'j' <> metavar "JOB" <> help "job to show info on")
  tabs = switch (long "tabs" <>
           short 't' <>
           help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in JobArgs <$> clusterP <*> jobP <*> tabs :: Parser JobArgs

toLineSummary :: DetailedJob.DetailedJob -> IO [String]
toLineSummary job = do
  let name' = DetailedJob.name job
  let user' = DetailedJob.user job
  let state' = DetailedJob.state job
  let jobId' = DetailedJob.jobId job
  let mappers' = show $ DetailedJob.numMappers job
  let reducers' = show $ DetailedJob.numReducers job
  startedTime' <- toLocalTimeString $ DetailedJob.startedTime job
  finishedTime' <- toLocalTimeString $ DetailedJob.finishedTime job
  return [name', user', state', jobId', mappers', reducers', startedTime', finishedTime']

headLine :: [String]
headLine = ["Name", "User", "State", "JobId", "Mappers", "Reducers", "StartedTime", "FinishedTime"]


recoverWith :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
recoverWith existing generator = do
    e <- existing
    fnE e
  where
    fnE (Just e) = return $ Just e
    fnE Nothing  = generator

printResults :: Config -> JobArgs -> IO ()
printResults conf sargs = do
    let jobId' = jobJobId sargs
    let server = extractServer conf (jobCluster sargs)
    let queryParameters = QP.QueryParameters []
    let fetchFromApp = AppInfo.fetchJob jobId' queryParameters $ appUrl server
    let fetchFromHistory = HistoryInfo.fetchJob jobId' queryParameters $ historyUrl server
    let historyQueryWithError = fmap failOnLeft fetchFromHistory :: IO (Maybe DetailedJob.DetailedJob)
    let appQueryWithError = fmap failOnLeft fetchFromApp :: IO (Maybe DetailedJob.DetailedJob)

    maybeRes <- recoverWith appQueryWithError historyQueryWithError

    let maybeJob = failOnNothing "Unable to locate job in history or app server" maybeRes

    summarizedJobs <- toLineSummary maybeJob
    let column = if (jobTabs sargs) then tabColumnarize else evenColumnarize
    let shortFn = return . column
    lineSummaries <- shortFn (headLine : [summarizedJobs])
    sequence_ (map putStrLn lineSummaries)


detailsAction :: Config -> JobArgs -> IO ()
detailsAction conf sargs = do
  printResults conf sargs
