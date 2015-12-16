module Jt.Command.Show (
  showCommand
) where

import Jt.Server
import Jt
import Options.Applicative
import Text.Printf
import Control.Monad
{-
  jt show --history --rm
-}
data ShowArgs = ShowArgs { showVerbose :: Bool
                         , showHistory :: Bool
                         , showRM :: Bool
                         }

showCommand :: Command
showCommand = Command { commandName = "show"
                      , commandDesc = "Show clusters"
                      , commandParser = showParser
                      , commandAction = showAction }

showParser = let
  history = switch (long "history" <> help "History: show the history url")
  rm = switch (long "resource-manager" <> help "resource-manager: show the rm url")
  verbose = switch (long "verbose" <> short 'v' <> help "Verbose: show full body and all comments")
  in ShowArgs <$> verbose <*> history <*> rm

showAction :: Config -> ShowArgs -> IO ()
showAction servers sargs = do
  let particularSet = showRM sargs || showHistory sargs
  let historyInclude = (not particularSet) || showHistory sargs
  let rmInclude = (not particularSet) || showRM sargs
  forM servers (\server -> do
      printf "%s\t" $ serverName server
      when historyInclude $ do
        printf "%s\t" $ historyUrlStr server
      when rmInclude $ do
         printf "%s\t" $ rmUrlStr server
      printf "\n"
    )
  return ()
  where
      historyUrlStr (Server _ _ (HistoryUrl url)) = url
      rmUrlStr      (Server _ (AppUrl url) _) = url
