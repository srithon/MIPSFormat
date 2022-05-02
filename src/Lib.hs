-- Need to put at top of file, not inside module
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( debugCategorize,
  )
where

import Data.Char (isSpace)
import qualified Data.Text as T

directives = [".data", ".text"]

-- read the file, determine the length of the longest instruction
data ParseSymbol = Directive T.Text | Label T.Text | Instruction T.Text | Comment T.Text deriving (Show)

categorizeLine :: T.Text -> [ParseSymbol]
categorizeLine s
  | code `elem` directives = Directive code : commentSection
  | T.null code = commentSection
  -- MISTAKE: put this before the null check
  | T.last code == ':' = Label code : commentSection
  | otherwise = Instruction code : commentSection
  where
    (rawCode, comment) = T.breakOn "#" s
    code = T.dropWhileEnd isSpace $ T.dropWhile isSpace rawCode
    containsComment = not $ T.null comment
    commentSection = [Comment comment | containsComment]

debugCategorize :: T.Text -> IO ()
debugCategorize = mapM_ (println . categorizeLine) . T.lines
  where
    println x = do
      print x
      putStrLn ""
