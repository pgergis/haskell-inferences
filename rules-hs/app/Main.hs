module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BC

import Data.Maybe
import Data.String
import System.Environment

import Inference

string2bytes :: String -> BS.ByteString
string2bytes x = fromString x :: BS.ByteString

main :: IO ()
main = do args <- getArgs
          let ruleJson = fromJust $ (decode (string2bytes $ args !! 0) :: Maybe [Rule])
          let inputJson = fromJust $ (decode (string2bytes $ args !! 1) :: Maybe [Type])
          BC.putStrLn $ encode $ inferoutputs (pretreat ruleJson) inputJson
