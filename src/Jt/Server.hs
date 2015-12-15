-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- module Jt.Server (
--     fetchJobs,
--     Server(..)
--     ) where

-- import qualified Jt.App as App
-- import qualified Jt.Job as Job


-- data Server = HistoryServer { url :: String } | AppServer { url :: String } deriving (Show)

-- fetchJobs :: Server -> IO (Maybe [Job])
-- -- fetchJobs (HistoryServer url) = fetchHistoryJobs
-- fetchJobs (AppServer url) = App.fetchJobs url