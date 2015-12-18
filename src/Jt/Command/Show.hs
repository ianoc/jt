module Jt.Command.Show (
  showCommand
) where

import Jt.Server
import Jt
import Options.Applicative
import Text.Printf
import Control.Monad

data ShowArgs = ShowArgs { showHistory :: Bool
                         , showRM :: Bool
                         }

showCommand :: Command
showCommand = Command { commandName = "show"
                      , commandDesc = "Show clusters"
                      , commandParser = showParser
                      , commandAction = showAction }

showParser :: Parser ShowArgs
showParser = let
  history = switch (long "history" <> help "History: show the history url")
  rm = switch (long "resource-manager" <> help "resource-manager: show the rm url")
  in ShowArgs <$> history <*> rm

showAction :: Config -> ShowArgs -> IO ()
showAction cfg sargs = do
  let particularSet = showRM sargs || showHistory sargs
  let historyInclude = (not particularSet) || showHistory sargs
  let rmInclude = (not particularSet) || showRM sargs
  _ <- forM servers' (\server -> do
      printf "%s\t" $ serverName server
      when historyInclude $ printf "%s\t" $ historyUrlStr server
      when rmInclude $ printf "%s\t" $ rmUrlStr server
      printf "\n"
    )
  return ()
  where
      servers' = configServers cfg
      historyUrlStr (Server _ _ (HistoryUrl url)) = url
      rmUrlStr      (Server _ (AppUrl url) _) = url
