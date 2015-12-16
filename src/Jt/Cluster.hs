{-# LANGUAGE OverloadedStrings #-}


module Jt.Cluster (
    fetchJobs,
    Server(..)
    ) where

import qualified Jt.App as App
import Jt.QueryParameters

data Cluster = Cluster { historyUrl :: String, appUrl :: String } deriving (Show, Eq)


fetchJobs :: Cluster -> QueryParameters -> [Job]
fetchJobs cluster queryParameters = App.fetchJobs queryParameters

-- -- import qualified Jt.App as App
-- -- import qualified Jt.Job as Job


-- -- data Server = HistoryServer { url :: String } | AppServer { url :: String }




-- module Jt.Server (
--     -- fetchJobs,
--     -- Server(..)
--     fetchJobs
--     ) where



-- import Data.Aeson (FromJSON, ToJSON, decode)
-- import GHC.Generics (Generic)
-- import Jt.Net
-- import qualified Jt.Job as Job
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Int as Ints
-- import qualified Data.String.Utils as StringUtils
-- import qualified Jt.Job as Job


-- fetchJobs :: Server -> IO (Maybe [Job])
-- -- fetchJobs (HistoryServer url) = fetchHistoryJobs
-- fetchJobs (AppServer url) = App.fetchJobs url
