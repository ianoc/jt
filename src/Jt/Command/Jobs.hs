module Jt.Command.Jobs (
  jobsAction,
  jobsCommand
) where

import Jt
import qualified Jt.Job as Job
import Jt.Server
import Data.Maybe(fromMaybe)
import Options.Applicative
import qualified Jt.QueryParameters as QP
import Jt.Command.Utils

data JobArgs = JobArgs { jobUser :: Maybe String
                         , jobCluster :: Maybe String
                         , jobLimit :: Maybe Int
                         , showHistory :: Bool
                         , showRM :: Bool
                         , jobTabs :: Bool
                         }

jobsCommand :: Command
jobsCommand = Command { commandName = "jobs"
                      , commandDesc = "List Jobs"
                      , commandParser = jobsParser
                      , commandAction = jobsAction }

jobsParser :: Parser JobArgs
jobsParser = let
  clusterP = optional(strOption (long "cluster" <> short 'c' <> metavar "CLUSTER" <> help "cluster to operate from"))
  userP = optional(strOption (long "user" <> short 'u' <> metavar "USER" <> help "user to list jobs from"))
  limitP = optional(option auto (long "limit" <> short 'l' <> metavar "LIMIT" <> help "limit of jobs to return"))
  history = switch (long "history" <> short 'o' <> help "History: show the history url")
  rm = switch (long "resource-manager" <> short 'a' <> help "resource-manager: show the rm url")
  tabs = switch (long "tabs" <>
           short 't' <>
           help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in JobArgs <$> userP <*> clusterP <*> limitP <*> history <*> rm <*> tabs :: Parser JobArgs


toLineSummary :: Job.Job -> [String]
toLineSummary job = let
  name' = Job.name job
  user' = Job.user job
  state' = Job.state job
  jobId' = Job.jobId job
  startedTime' = show $ Job.startedTime job
  in [name', user', state', jobId', startedTime']

headLine :: [String]
headLine = ["Name", "User", "State", "JobId", "StartedTime"]


printResults :: Config -> JobArgs -> IO ()
printResults conf sargs = do
  let particularSet = showRM sargs || showHistory sargs
  let historyInclude = (not particularSet) || showHistory sargs
  let rmInclude = (not particularSet) || showRM sargs

  let userOption = fromMaybe QP.EmptyParameter $ fmap (\u -> (QP.toQp "user" u)) (jobUser sargs)
  let limitOption = fromMaybe QP.EmptyParameter $ fmap (\u -> (QP.toQp "limit" $ show u)) (jobLimit sargs)

  let maxLimit = fromMaybe 500 $ jobLimit sargs
  let server = extractServer conf (jobCluster sargs)

  let queryParameters = QP.QueryParameters [userOption, limitOption]
  historyJobs <- if historyInclude then Job.jobsWithOpts queryParameters $ historyUrl server else return $ Right []
  rmJobs <- if rmInclude then Job.jobsWithOpts queryParameters $ appUrl server else return $ Right []

  let jobEither = combineEither rmJobs historyJobs
  let jobLimited = fmap (\jobs' -> take maxLimit jobs') jobEither
  let jobs' = failOnLeft jobLimited
  let summarizedJobs = fmap toLineSummary jobs'
  let column = if (jobTabs sargs) then tabColumnarize else evenColumnarize
  let shortFn = return . column
  lineSummaries <- shortFn (headLine : summarizedJobs)
  sequence_ (map putStrLn lineSummaries)

jobsAction :: Config -> JobArgs -> IO ()
jobsAction conf sargs = do
  printResults conf sargs
