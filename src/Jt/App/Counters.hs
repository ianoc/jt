{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt.App.Counters (
    fetchCounters,
    Counter
    ) where


import Jt.Counter(Counter, rawFetchCounters)
import Jt.QueryParameters(QueryParameters)
import Jt.Server(AppUrl(..))
import qualified Jt.Utils as Utils

fetchCounters :: String -> QueryParameters -> AppUrl -> IO (Either String (Maybe [Counter]))
fetchCounters jobId' queryParameters url = do
    let (AppUrl rawUrl) = url
    let finalUrl = rawUrl ++ "/proxy/" ++ (Utils.toApplicationId jobId') ++ "/ws/v1/mapreduce/jobs/" ++ (Utils.toJobId jobId') ++ "/counters"
    rawFetchCounters queryParameters finalUrl

