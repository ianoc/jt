module Jt.Utils (
    toApplicationId
  , toJobId
  , addInfo
    ) where
import qualified Data.String.Utils as StringUtils

toApplicationId :: String -> String
toApplicationId jobId' = StringUtils.replace "job_" "application_" jobId'

toJobId :: String -> String
toJobId jobId' = StringUtils.replace "application_" "job_" jobId'


addInfo :: String -> Either String a  -> Either String a
addInfo extra (Left l) = Left (extra ++ l)
addInfo extra o = o
