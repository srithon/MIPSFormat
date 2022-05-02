module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Lib

main :: IO ()
main = do
  contents <- getContents
  let text = T.pack contents
  TO.putStrLn $ format text
