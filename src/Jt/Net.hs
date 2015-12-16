{-# LANGUAGE OverloadedStrings #-}

module Jt.Net (
    queryUrl,
    queryUrlWith
    ) where

import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception as E
import Network.HTTP.Client(HttpException)
import qualified Jt.QueryParameters as QP

queryUrlWith :: QP.QueryParameters -> String -> IO(Either String BL.ByteString)
queryUrlWith params url = fmap handleErr runUrl
    where handleErr (Right a) = Right $ a ^. responseBody
          handleErr (Left e) = Left $ show (e :: HttpException)
          options' = qParamsToOptions params
          runUrl = E.try (getWith options' url)

qParamsToOptions :: QP.QueryParameters -> Options
qParamsToOptions (QP.QueryParameters parameters) = merger defaults parameters
    where merger existing [] = existing
          merger existing (QP.EmptyParameter:xs) = merger existing xs
          merger existing ((QP.QueryParameter k v):xs) = merger (existing & param k .~ [v] ) xs

queryUrl :: String -> IO(Either String BL.ByteString)
queryUrl = queryUrlWith QP.defaultsQP

