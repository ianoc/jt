module Jt.Command.Jobs (
  jobsAction,
  jobCommand
) where

import Data.List(intercalate, sort, transpose, length)
import qualified Data.Map.Strict as Map
import Jt
import qualified Jt.Job as Job
import Jt.Server
import Data.Maybe(fromMaybe)
import Options.Applicative
import qualified Jt.QueryParameters as QP

data JobArgs = JobArgs { jobUser :: Maybe String
                         , jobCluster :: String
                         , jobLimit :: Maybe Int
                         , showHistory :: Bool
                         , showRM :: Bool
                         , jobTabs :: Bool
                         }

jobCommand :: Command
jobCommand = Command { commandName = "jobs"
                      , commandDesc = "List Jobs"
                      , commandParser = jobParser
                      , commandAction = jobsAction }

jobParser = let
  clusterP = strOption (long "cluster" <> short 'c' <> metavar "CLUSTER" <> help "cluster to operate from")
  userP = optional(strOption (long "user" <> short 'u' <> metavar "USER" <> help "user to list jobs from"))
  limitP = optional(option auto (long "limit" <> short 'l' <> metavar "LIMIT" <> help "limit of jobs to return"))
  history = switch (long "history" <> short 'o' <> help "History: show the history url")
  rm = switch (long "resource-manager" <> short 'a' <> help "resource-manager: show the rm url")
  tabs = switch (long "tabs" <>
           short 't' <>
           help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in JobArgs <$> userP <*> clusterP <*> limitP <*> history <*> rm <*> tabs :: Parser JobArgs


tabColumnarize :: [[String]] -> [String]
tabColumnarize = map (intercalate "\t")

{-| Pad the i^th string with enough space to make columns line up
  >>> evenColumnarize [["yo", "man"], ["foo", "bar"], ["bazbaz", "baby"]]
  ["yo     man ","foo    bar ","bazbaz baby"]
-}
evenColumnarize :: [[String]] -> [String]
evenColumnarize rows = let
  columns = transpose rows
  widths = map (\c -> maximum (map length c)) columns
  padTo t str = let
    sz = length str
    pads = t - sz
    tail = replicate pads ' '
    in str ++ tail
  resCol = map (\(w, c) -> map (padTo w) c) (zip widths columns)
  in map (intercalate " ") (transpose resCol)

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

  let maybeServer = cfgLookup (jobCluster sargs) conf
  let server = fromMaybe (err ("Unable to find cluster: " ++ (jobCluster sargs))) maybeServer
  let queryParameters = QP.QueryParameters [userOption, limitOption]
  jobEither <- jobsWithOpts queryParameters server
  let jobs = failOnLeft jobEither
  let summarizedJobs = fmap toLineSummary jobs
  let column = if (jobTabs sargs) then tabColumnarize else evenColumnarize
  let shortFn = return . column
  lineSummaries <- shortFn (headLine : summarizedJobs)
  sequence_ (map putStrLn lineSummaries)

jobsAction :: Config -> JobArgs -> IO ()
jobsAction conf sargs = do
  printResults conf sargs
