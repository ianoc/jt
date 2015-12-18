{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Jt (
    -- * Modules
    readConfig,
    jobs,
    jobsWithOpts,
    Command(..),
    toAction,
    failOnLeft,
    failOnNothing,
    cfgLookup,
    err,
    Config(..)
    ) where

import Data.List.Split(splitOn)
import Data.Text(unpack)
import Data.Typeable
import Options.Applicative
import Filesystem.Path(parent)
import qualified Turtle as T
import Jt.Server
import Jt.Job
import qualified Data.Map.Strict as Map
import Control.Exception
{-
  This is any information we need to make a query
-}

findServer :: T.FilePath -> IO (Maybe T.FilePath)
findServer init' = do
  let here = init' T.</> ".hadoop_cluster.conf"
  gitDir <- T.testfile here
  case gitDir of True  -> return (Just here)
                 False -> let p = parent init'
                          in if p == init' then return Nothing else findServer p

findServerFromRoots :: [T.FilePath] -> IO (Maybe T.FilePath)
findServerFromRoots [] = return Nothing
findServerFromRoots (x : xs) = do
        cfg <- findServer x
        maybeCfg cfg
        where maybeCfg (Just p) = return $ Just p
              maybeCfg Nothing = findServerFromRoots xs

lineToServer :: String -> Server
lineToServer line = Server name' (AppUrl appUrl') (HistoryUrl historyUrl')
    where [name', appUrl', historyUrl'] = splitOn " " line

pathToServer :: T.FilePath -> IO [Server]
pathToServer path = do
    conf <- T.strict (T.input path)
    let lines' = lines $ unpack conf :: [String]
    return (fmap lineToServer lines')


readConfig :: IO Config
readConfig = do
  initd <- T.pwd
  home <- T.home
  mdir <- findServerFromRoots [initd, home]
  case mdir of (Just config_path) -> do
                               s <- pathToServer config_path
                               return $ cfgFromServerList s
               Nothing -> fail ("\nYou must have a .hadoop_cluster.conf file in the parent of the pwd, or home directory,\n" <>
                                "with a file at .hadoop_cluster.conf containing\n" <>
                                "\"Name\" \"RM URL\" and \"History Url\" where these urls correspond to the RM and history urls of your clusters. One per line\n" <>
                                "Example:\n" <>
                                "tstA http://tstA.example.com:50030 http://tstA.example.com:8080\n")
data Config = Config {
  configServers :: Map.Map String Server,
  defaultServer :: Server
}


cfgLookup :: String -> Config -> Maybe Server
cfgLookup k (Config m _) = Map.lookup k m


data FailOnLeftException = FailOnLeftException String deriving (Show, Typeable)
instance Exception FailOnLeftException


failOnLeft :: Either String b -> b
failOnLeft (Right e) = e
failOnLeft (Left str')  = throw $ FailOnLeftException str'

data FailOnNothingException = FailOnNothingException String deriving (Show, Typeable)
instance Exception FailOnNothingException

failOnNothing :: String -> Maybe a -> a
failOnNothing _      (Just e) = e
failOnNothing errMsg Nothing  = throw $ FailOnNothingException errMsg

data ForcedException = ForcedException String deriving (Show, Typeable)
instance Exception ForcedException

err :: String -> a
err msg = throw $ ForcedException msg

data FailOnBadConfig = FailOnBadConfig String deriving (Show, Typeable)
instance Exception FailOnBadConfig

cfgFromServerList :: [Server] -> Config
cfgFromServerList [] = throw $ FailOnBadConfig "Unable to parse any servers from config."
cfgFromServerList servers@(x : _) = Config (Map.fromList pairList) x
  where pairList = fmap server2Pair servers
        server2Pair srv = (serverName srv, srv)

{-
 This is the struture we fit each subcommand into
 -}
data Command = forall a . Command {
  commandName :: String,
  commandDesc :: String,
  commandParser :: Parser a,
  commandAction :: Config -> a -> IO ()
}

{-
  Prepares the subcommand
-}
subcom :: Config -> Command -> Mod CommandFields (IO ())
subcom conf Command { commandName = name', commandDesc = desc, commandParser = parser, commandAction = act } = let
  toAct = act conf
  actionParser = helper <*> (toAct <$> parser)
  in command name' (info actionParser (progDesc desc <> header desc))

{-
  Called by the main function to run one of the commands
-}
toAction :: Config -> [Command] -> IO ()
toAction conf commands = let
  subcommands = map (subcom conf) commands
  subc = subparser (mconcat subcommands)
  in do
    action' <- execParser (info (helper <*> subc) (fullDesc <> header "jt: a command line job tracker tool"))
    action'
