module Jt.Utils (
    toApplicationId
  , toJobId
    ) where
import qualified Data.String.Utils as StringUtils

toApplicationId :: String -> String
toApplicationId jobId' = StringUtils.replace "job_" "application_" jobId'

toJobId :: String -> String
toJobId jobId' = StringUtils.replace "application_" "job_" jobId'
