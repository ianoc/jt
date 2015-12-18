module Jt.Command.Counters (
  countersAction,
  countersCommand
) where

import Jt
import Jt.Command.Utils
import Jt.Server
import Options.Applicative
import qualified Jt.History.Counters as HistoryCounters
import qualified Jt.App.Counters as AppCounters
import Jt.Counter(Counter(..))
import qualified Jt.QueryParameters as QP

data JobCounterArgs = JobCounterArgs
  {
     jobCounterCluster :: Maybe String
   , jobCounterTabs :: Bool
   , jobCounterJobId :: String
   , jobCounterGroup :: Maybe String
   , jobCounterName :: Maybe String
   }

countersCommand :: Command
countersCommand = Command { commandName = "counters"
                      , commandDesc = "Get counters on a job"
                      , commandParser = countersParser
                      , commandAction = countersAction }

countersParser :: Parser JobCounterArgs
countersParser = let
  clusterP = optional(strOption (long "cluster" <> short 'c' <> metavar "CLUSTER" <> help "cluster to operate from"))
  groupP = optional(strOption (long "group" <> short 'g' <> metavar "GROUP" <> help "filter for group"))
  nameP = optional(strOption (long "name" <> short 'n' <> metavar "NAME" <> help "filter for group"))
  jobP = argument str (metavar "JOB" <> help "job to show info on")
  tabs = switch (long "tabs" <>
           short 't' <>
           help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in JobCounterArgs <$> clusterP <*> tabs <*> jobP <*> groupP <*> nameP :: Parser JobCounterArgs


toLineSummary :: Counter -> [String]
toLineSummary (Counter grpName' name' redCntrV' mapCntrV' totCntrV') = let
  mapCntrV'' = show mapCntrV'
  redCntrV'' = show redCntrV'
  totCntrV'' = show totCntrV' in
  [grpName', name', mapCntrV'', redCntrV'', totCntrV'']

headLine :: [String]
headLine = ["Group Name", "Name", "Mapper Counter Value", "Reducer Counter Value", "Total Counter Value"]


recoverWith :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
recoverWith existing generator = do
    e <- existing
    fnE e
  where
    fnE (Just e) = return $ Just e
    fnE Nothing  = generator

grpFilter :: Maybe String -> Counter -> Bool
grpFilter (Just f) (Counter grp _ _ _ _)  = f == grp
grpFilter Nothing  _                      = True

nameFilter :: Maybe String -> Counter -> Bool
nameFilter (Just f) (Counter _ nme _ _ _)  = f == nme
nameFilter Nothing  _                      = True


countersAction :: Config -> JobCounterArgs -> IO ()
countersAction conf sargs = do
    let jobId'                = jobCounterJobId sargs
    let server                = extractServer conf (jobCounterCluster sargs)
    let queryParameters       = QP.QueryParameters []
    let fetchFromApp          = AppCounters.fetchCounters jobId' queryParameters $ appUrl server
    let fetchFromHistory      = HistoryCounters.fetchCounters jobId' queryParameters $ historyUrl server
    let historyQueryWithError = fmap failOnLeft fetchFromHistory :: IO (Maybe [Counter])
    let appQueryWithError     = fmap failOnLeft fetchFromApp     :: IO (Maybe [Counter])

    maybeRes <- recoverWith appQueryWithError historyQueryWithError

    let allCounters = failOnNothing "Unable to locate job in history or app server" maybeRes :: [Counter]
    let nameFilteredCntrs = filter (nameFilter (jobCounterName sargs)) allCounters
    let grpFilteredCntrs = filter (grpFilter (jobCounterGroup sargs)) nameFilteredCntrs

    let summaries = fmap toLineSummary grpFilteredCntrs
    let column = if (jobCounterTabs sargs) then tabColumnarize else evenColumnarize
    let shortFn = return . column
    lineSummaries <- shortFn (headLine : summaries)
    sequence_ (map putStrLn lineSummaries)

