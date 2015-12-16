module Jt.QueryParameters (
    QueryParameters(..),
    QueryParameter(..),
    defaultsQP,
    addOpt,
    addQp,
    toQp
    ) where
import Data.Text

data QueryParameter = QueryParameter { key :: Text, value :: Text } | EmptyParameter deriving (Show, Eq)
data QueryParameters = QueryParameters { parameters :: [QueryParameter] } deriving (Show, Eq)

defaultsQP :: QueryParameters
defaultsQP = addOpt (QueryParameters []) "limit" "10"

addOpt :: QueryParameters -> String -> String -> QueryParameters
addOpt qParams newK newV = addQp qParams $ toQp newK newV

addQp :: QueryParameters -> QueryParameter -> QueryParameters
addQp (QueryParameters(existing)) qp@(QueryParameter _ _) = QueryParameters(qp : existing)
addQp qps@(QueryParameters(_)) EmptyParameter = qps

toQp :: String -> String -> QueryParameter
toQp newK newV = QueryParameter (pack newK) (pack newV)
