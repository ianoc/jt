{-# LANGUAGE DeriveGeneric #-}
module Jt.Counter (
  Counter(..),
  rawFetchCounters
) where

import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import Jt.QueryParameters
import qualified Data.Int as Ints
import qualified Jt.Net as Net

data CountersResponse = CountersResponse   { _raw_hadoop_field_jobCounters  :: CountersResponse2 } deriving (Show, Generic)
data CountersResponse2 = CountersResponse2 { _raw_hadoop_field_counterGroup :: [RawCounterGroup] } deriving (Show, Generic)
data RawCounterGroup = RawCounterGroup {
  _raw_hadoop_field_counterGroupName    :: String,
  _raw_hadoop_field_counter             :: [RawCounter]
} deriving (Show, Generic)

data RawCounter = RawCounter {
  _raw_hadoop_field_reduceCounterValue  :: Ints.Int64,
  _raw_hadoop_field_mapCounterValue     :: Ints.Int64,
  _raw_hadoop_field_totalCounterValue   :: Ints.Int64,
  _raw_hadoop_field_name                :: String
} deriving (Show, Generic)


instance FromJSON CountersResponse where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 18 })

instance FromJSON CountersResponse2 where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 18 })

instance FromJSON RawCounterGroup where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 18 })

instance FromJSON RawCounter where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 18 })

data Counter = Counter {
  groupName           :: String,
  name                :: String,
  reduceCounterValue  :: Ints.Int64,
  mapCounterValue     :: Ints.Int64,
  totalCounterValue   :: Ints.Int64
} deriving (Show, Eq, Ord)

counterGroupToCounters :: RawCounterGroup -> [Counter]
counterGroupToCounters (RawCounterGroup grpName rawCntrs) = fmap rawCntr2Cntr rawCntrs
    where rawCntr2Cntr (RawCounter redCntrV mapCntrV totCntrV fName) = Counter grpName fName redCntrV mapCntrV totCntrV

grps2Cntrs :: [RawCounterGroup] -> [Counter]
grps2Cntrs x = collapse' x []
  where
    collapse' []      accum  = accum
    collapse' (x' :xs) accum  = collapse' xs $ accum ++ (counterGroupToCounters x')

rawFetchCounters :: QueryParameters -> String -> IO (Either String (Maybe [Counter]))
rawFetchCounters queryParameters finalUrl = do
    Net.fetchJsonUrl queryParameters finalUrl (grps2Cntrs._raw_hadoop_field_counterGroup._raw_hadoop_field_jobCounters)

