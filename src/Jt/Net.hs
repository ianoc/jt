{-# LANGUAGE OverloadedStrings #-}

module Jt.Net (
    queryUrl
    ) where

import Control.Monad
import Data.Char (intToDigit)
import Network.BufferType
import Network.HTTP
import Network.HTTP
import Network.Stream
import Network.TCP
import Network.URI
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Int as Ints
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


responseCodeHandler :: HStream ty => Response ty -> Either String ty
responseCodeHandler resp = case rspCode resp of
                  (2,0,0) -> Right (rspBody resp)
                  _ -> Left (httpError resp)
    where
      showRspCode (a,b,c) = map intToDigit [a,b,c]
      httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

    -- return (do
    --   x <- resp
    --   responseCodeHandler x)

-- wreq



get :: HStream ty => URI -> IO (Either String ty)
get uri = do
    eresp <- simpleHTTP (request uri)
    let resp = handleE eresp
    return $ resp >>= responseCodeHandler

request :: HStream ty => URI -> HTTPRequest ty
request uri = req
  where
   req = Request{ rqURI = uri
                , rqMethod = GET
        , rqHeaders = []
        , rqBody = nullVal
        }
   nullVal = buf_empty bufferOps


handleE :: Show b => Either b a -> Either String a
handleE (Left e) = Left $ show e
handleE (Right v) = Right v

queryUrl :: String -> IO (Either String BL.ByteString)
queryUrl url = extractRes $ parseURI url
  where
    extractRes (Just a) = get a
    extractRes Nothing = return $ Left "Unable to parse query string"
