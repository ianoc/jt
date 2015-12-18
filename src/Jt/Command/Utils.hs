module Jt.Command.Utils (
  tabColumnarize,
  evenColumnarize,
  toLocalTimeString,
  extractServer
) where

import Data.List(intercalate, transpose)
import Data.Maybe(fromMaybe)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Data.Time.LocalTime(getCurrentTimeZone, utcToLocalTime)
import Jt
import Jt.Server(Server(..))
import qualified Data.Int as Ints

tabColumnarize :: [[String]] -> [String]
tabColumnarize = map (intercalate "\t")


{-| Pad the i^th string with enough space to make columns line up
  >>> evenColumnarize [["yo", "man"], ["foo", "bar"], ["bazbaz", "baby"]]
  ["yo     man ","foo    bar ","bazbaz baby"]
-}
evenColumnarize :: [[String]] -> [String]
evenColumnarize rows = let
  columns = transpose rows
  widths = map (\c -> maximum (map length c)) columns
  padTo t str = let
    sz = length str
    pads = t - sz
    tail' = replicate pads ' '
    in str ++ tail'
  resCol = map (\(w, c) -> map (padTo w) c) (zip widths columns)
  in map (intercalate " ") (transpose resCol)

toLocalTimeString :: Ints.Int64 -> IO String
toLocalTimeString timestamp64 = fmap printOut tzIO
  where
      tzIO = getCurrentTimeZone
      timestamp = fromIntegral timestamp64
      printOut tz = show (utcToLocalTime tz time)
      time = posixSecondsToUTCTime (fromInteger (timestamp `div` 1000))

extractServer :: Config -> Maybe String -> Server
extractServer cfg                 (Just q) = fromMaybe (err ("Unable to find cluster: " ++ q)) $ cfgLookup q cfg
extractServer (Config _ defaultV) Nothing  = defaultV
