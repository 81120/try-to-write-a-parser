module Main where

import JsonParser (Json, json)
import Parse (parse)
import System.IO (
  IOMode (ReadMode),
  hGetContents,
  openFile,
 )

readJsonFile :: String -> IO String
readJsonFile filePath = do
  filePtr <- openFile filePath ReadMode
  hGetContents filePtr

parseJson :: String -> Json
parseJson jsonStr =
  fst . head $ parse json jsonStr

main :: IO ()
main = do
  jsonStr <- readJsonFile "test.json"
  print $ parseJson jsonStr
