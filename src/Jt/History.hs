-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- module Jt.History (
--     fetchJobs,
--     Server(..)
--     ) where

-- data Server = HistoryServer { url :: String } | AppServer { url :: String } deriving (Show)

-- fetchJobs :: Server -> String -> IO (Maybe [Job])
-- fetchJobs (HistoryServer url) = fetchHistoryJobs
-- fetchJobs (AppServer url) = fetchHistoryJobs