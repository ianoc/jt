module Main where

import Control.Monad
import qualified Jt.App as App
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  let firstArg = take 1 args
  forM firstArg (\a -> do
      resp <- App.fetchJobs a
      case resp of
            Right r -> void $ mapM print r
            Left l -> putStrLn l
    )
  return ()
