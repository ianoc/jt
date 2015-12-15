{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Jt (
    -- * Modules
    hello,
    queryJt,
    App(..)
    ) where



import Data.Aeson
import qualified Data.Int as Ints
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.HTTP
import Network.URI
import Data.Char (intToDigit)
import Network.HTTP
import Network.URI
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Network.BufferType
import Network.TCP
import Network.Stream


data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)

$(deriveJSON defaultOptions ''Coord)

hello :: BL.ByteString
hello =
  let
    req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
    reply = Coord 123.4 20
  in
    (encode reply) `BL.append` (encode req)


err :: String -> IO a
err msg = do
      hPutStrLn stderr msg
      exitFailure

get :: HStream ty => URI -> IO ty
get uri = do
    eresp <- simpleHTTP (request uri)
    resp <- handleE (err . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

request :: HStream ty => URI -> HTTPRequest ty
request uri = req
  where
   req = Request{ rqURI = uri
                , rqMethod = GET
        , rqHeaders = []
        , rqBody = nullVal
        }
   nullVal = buf_empty bufferOps


handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

queryUrl :: String -> Maybe (IO BL.ByteString)
queryUrl url = fmap get (parseURI url)


-- * http://hadoop-dw2-rm.smf1.twitter.com:50030/ws/v1/cluster/apps?states=running,failed,finished&limit=1
-- {"apps":{"app":[{"id":"application_1450140097802_31660","user":"exp","name":"[FCB00FFF555A40E4ACF310D18BCEC597/31CB2279F78540B9AF2A412CB4DD98F6] com.twitter.experiments.scalding.BatchPartialHomunculiClientEventLoginsJob/(2/3)","queue":"root.exp","state":"FINISHED","finalStatus":"SUCCEEDED","progress":100.0,"trackingUI":"History","trackingUrl":"http://hadoop-dw2-rm.smf1.twitter.com:50030/proxy/application_1450140097802_31660/jobhistory/job/job_1450140097802_31660","diagnostics":"","clusterId":1450140097802,"applicationType":"MAPREDUCE","applicationTags":"","totalMemory":9428795392,"reservedMemory":-1048576,"totalCores":3,"startedTime":1450211354124,"finishedTime":1450211702261,"elapsedTime":348137,"amContainerLogs":"http://smf1-bom-31-sr1.prod.twitter.com:50060/node/containerlogs/container_1450140097802_31660_01_000001/exp","amHostHttpAddress":"smf1-bom-31-sr1.prod.twitter.com:50060","allocatedMB":-1,"allocatedVCores":-1,"runningContainers":-1,"memorySeconds":0,"vcoreSeconds":0,"preemptedResourceMB":0,"preemptedResourceVCores":0,"numNonAMContainerPreempted":0,"numAMContainerPreempted":0}]}}[tw-ioconnell jt (master)]$ curl "http://hadoop-dw2-rm.smf1.twitter.com:50030/ws/v1/cluster/apps?states=running,failed,finished&limit=1"

data Apps = Apps { app :: [App] } deriving (Show)
data AppsResponse = AppsResponse { apps :: Apps } deriving (Show)
data App = App { id :: String,
    user :: String,
    name :: String,
    queue :: String,
    state :: String,
    startedTime :: Ints.Int64,
    finishedTime :: Ints.Int64} deriving (Show)

$(deriveJSON defaultOptions ''AppsResponse)
$(deriveJSON defaultOptions ''Apps)
$(deriveJSON defaultOptions ''App)

extractApps :: Maybe (IO BL.ByteString) -> IO (Either String AppsResponse)
extractApps (Just ioData) = fmap maybeToError $ fmap decode ioData
                    where maybeToError (Just a) = Right a
                          maybeToError  Nothing = Left "Unable to decode response"
extractApps Nothing = return $ Left "Unable to parse URL"

queryJt :: IO (Either String [App])
queryJt = do
    maybeApps <- extractApps $ queryUrl "http://hadoop-dw2-rm.smf1.twitter.com:50030/ws/v1/cluster/apps?states=running,failed,finished&limit=10"
    let resApps = fmap app $ fmap apps maybeApps
    return resApps
          -- extract arg = decode arg :: Apps

