module Data.FeedBack.Show where

import Data.FeedBack.Base

showValue :: Value -> String
showValue (Int i) = show i
showValue (Real r) = show (fromRational r :: Double)
showValue (Quoted s) = show s
showValue (Ident s) = s

showChunk :: String -> RawChunk -> String
showChunk name evs = concat
  [ "[", name, "]\n"
  , "{\n"
  , concatMap showLine evs
  , "}\n" ] where
  showLine (lv, rvs) = concat
    ["  ", showValue lv, " = ", unwords (map showValue rvs), "\n"]
