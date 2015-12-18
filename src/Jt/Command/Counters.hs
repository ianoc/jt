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
   , jobCounterTabs    :: Bool
   , jobCounterJobId   :: String
   , jobCounterGroup   :: Maybe String
   , jobCounterName    :: Maybe String
   , jobMapV           :: Bool
   , jobReduceV        :: Bool
   , jobTotalV         :: Bool
   , jobQuiet          :: Bool
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
  mapP    = switch (long "mappers" <> short 'm' <> help "Mapper value")
  reduceP = switch (long "reducers" <> short 'r' <> help "Reducers value")
  totalP  = switch (long "total" <> short 't' <> help "Total value")
  quietP  = switch (long "quiet" <> short 'q' <> help "Don't print the group or name on each line or the header")
  tabs = switch (long "tabs" <>
           help "Use tabs for columns. Useful with sort -t $'\t' -k3 | column -t -n $'\t'")
  in JobCounterArgs <$> clusterP <*> tabs <*> jobP <*> groupP <*> nameP <*> mapP <*> reduceP <*> totalP <*> quietP :: Parser JobCounterArgs


toLineSummary :: Bool -> Bool -> Bool -> Bool -> Counter -> [String]
toLineSummary quietP' mapVInc' redVInc' totVInc' (Counter grpName' name' redCntrV' mapCntrV' totCntrV') = let
  mapCntrV'' = if mapVInc' then [show mapCntrV'] else []
  redCntrV'' = if redVInc' then [show redCntrV'] else []
  totCntrV'' = if totVInc' then [show totCntrV'] else []
  grpN = if quietP' then [] else [grpName', name']
  in
  grpN ++ mapCntrV'' ++ redCntrV'' ++ totCntrV''

headLine :: Bool -> Bool -> Bool -> Bool -> [String]
headLine False mapVInc' redVInc' totVInc' = let
    mapCntrV'' = if mapVInc' then ["Mapper Counter Value"] else []
    redCntrV'' = if redVInc' then ["Reducer Counter Value"] else []
    totCntrV'' = if totVInc' then ["Total Counter Value"] else []
  in
    ["Group Name", "Name"] ++ mapCntrV'' ++ redCntrV'' ++ totCntrV''
headLine True _ _ _ = []


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
    let mapVInclude            = jobMapV sargs
    let reduceVInclude         = jobReduceV sargs
    let totVInclude            = jobTotalV sargs
    let quietP                 = jobQuiet sargs

    let allInclude             = not (totVInclude || reduceVInclude || mapVInclude)
    let totInc'                = totVInclude    || allInclude
    let mapVInc'               = mapVInclude    || allInclude
    let redVInc'               = reduceVInclude || allInclude

    let jobId'                 = jobCounterJobId sargs
    let server                 = extractServer conf (jobCounterCluster sargs)
    let queryParameters        = QP.QueryParameters []
    let fetchFromApp           = AppCounters.fetchCounters jobId' queryParameters $ appUrl server
    let fetchFromHistory       = HistoryCounters.fetchCounters jobId' queryParameters $ historyUrl server
    let historyQueryWithError  = fmap failOnLeft fetchFromHistory :: IO (Maybe [Counter])
    let appQueryWithError      = fmap failOnLeft fetchFromApp     :: IO (Maybe [Counter])

    maybeRes <- recoverWith appQueryWithError historyQueryWithError

    let allCounters = failOnNothing "Unable to locate job in history or app server" maybeRes :: [Counter]
    let nameFilteredCntrs = filter (nameFilter (jobCounterName sargs)) allCounters
    let grpFilteredCntrs = filter (grpFilter (jobCounterGroup sargs)) nameFilteredCntrs

    let summaries = fmap (toLineSummary quietP mapVInc' redVInc' totInc') grpFilteredCntrs
    let column = if (jobCounterTabs sargs) then tabColumnarize else evenColumnarize
    let shortFn = return . column
    lineSummaries <- shortFn ((headLine quietP mapVInc' redVInc' totInc') : summaries)
    let filteredSummaries = filter (\a -> length a > 0) $ lineSummaries
    sequence_ (map putStrLn filteredSummaries)

