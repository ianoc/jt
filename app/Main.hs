module Main where

-- import Control.Monad
-- import qualified Jt.App as App
-- import Jt
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import System.Environment(getArgs)
-- import qualified Jt.QueryParameters as QueryParameters

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let firstArg = take 1 args
--   cfgs <- readConfig
--   forM cfgs print
--   forM cfgs (\server -> do
--       resp <- userJobs "exp" server
--       case resp of
--             Right r -> void $ mapM print r
--             Left l -> putStrLn l
--     )
--   return ()


import Control.Monad(sequence_)
import Jt
import Jt.Command.All(allCommands)


main :: IO ()
main = do
  conf <- readConfig
  toAction conf allCommands