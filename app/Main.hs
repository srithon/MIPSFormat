module Main where

import Data.Text (pack)
import Lib

main :: IO ()
main = do
  contents <- getContents
  let text = pack contents
  debugCategorize text
