module Jt.Job (
    Job(..),
    JobProvider(..),
    parseName,
    JobNameElements(..),
    ) where

import qualified Data.Int as Ints
import qualified Jt.QueryParameters as QP
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BL

data JobNameElements  = JobNameElements {
  flowIdElement :: Maybe String,
  stepIdElement :: Maybe String,
  nameElement :: String
} deriving (Show, Eq)


jobNameParser :: Parser JobNameElements
jobNameParser = do
  _ <- char '['
  stepId  <- count 32 anyChar
  _ <- char '/'
  flowId' <- count 32 anyChar
  _ <- char ']'
  _ <- space
  nameRemaining <- takeTill (\c -> c == '/')
  return $ JobNameElements (Just flowId') (Just stepId) $ BL.unpack nameRemaining

parseName :: String -> JobNameElements
parseName input = either defaultName id parsedResponse
    where parsedResponse = parseOnly jobNameParser $ BL.pack input
          defaultName _ = JobNameElements Nothing Nothing input

data Job = Job {
  name :: String,
  queue :: String,
  user :: String,
  state :: String,
  startedTime :: Ints.Int64,
  finishedTime :: Ints.Int64,
  flowId :: Maybe String,
  flowStepId :: Maybe String,
  jobId :: String,
  applicationId :: String,
  jobUrl :: String
} deriving (Show, Eq)


class JobProvider a where
    jobs :: a -> IO (Either String [Job])
    jobsWithOpts :: QP.QueryParameters -> a -> IO (Either String [Job])
    jobsE :: a -> IO [Job]
    jobsE j = do
        e <- jobs j
        case e of
            Right s -> return s
            Left e' ->  fail e'
