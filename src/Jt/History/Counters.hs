{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt.History.Counters (
    fetchCounters,
    Counter
    ) where


import Jt.Counter(Counter, rawFetchCounters)
import Jt.QueryParameters(QueryParameters)
import Jt.Server(HistoryUrl(..))
import qualified Jt.Utils as Utils

fetchCounters :: String -> QueryParameters -> HistoryUrl -> IO (Either String (Maybe [Counter]))
fetchCounters jobId' queryParameters url = do
    let (HistoryUrl rawUrl) = url
    let finalUrl = rawUrl ++ "/ws/v1/history/mapreduce/jobs/" ++ (Utils.toJobId jobId') ++ "/counters"
    rawFetchCounters queryParameters finalUrl

