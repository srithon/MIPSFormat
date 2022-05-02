-- Need to put at top of file, not inside module
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( debugCategorize,
    format,
  )
where

import Data.Char (isSpace)
import qualified Data.Text as T
import Debug.Trace
import Text.Printf

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

indent t l = T.replicate l " " `T.append` t

indentBasic c = case c of
  Instruction t -> Instruction $ indentText t
  Comment t -> Comment $ indentText t
  x -> x
  where
    indentText t = indent t 4

fixComments :: Int -> ParseSymbol -> ParseSymbol -> T.Text
fixComments commentStartColumn (Instruction code) (Comment comment) = T.intercalate "\n" $ T.append codePadded firstCommentLine : restFixedComments
  where
    maxColumnWidth = 80
    instructionLength = T.length code
    -- how much space to add after the instruction
    codePadLength = commentStartColumn - instructionLength
    -- instruction with spaces added at the end
    codePadded = T.append code $ T.replicate codePadLength " "
    -- how long the comment text is
    totalCommentLength = T.length comment
    -- how much text we can have between the max column width and the comment start column
    -- subtract 2 for "# "
    maxCommentLengthPerLine = maxColumnWidth - commentStartColumn - 2
    comments = T.chunksOf maxCommentLengthPerLine comment
    (firstCommentLine : restComments) = comments
    restFixedComments = map (\c -> indent (T.append "# " c) commentStartColumn) restComments
fixComments _ _ _ = error ""

format :: T.Text -> T.Text
format text = T.intercalate "\n" wrappedComments
  where
    categorized = map categorizeLine $ T.lines text
    basicIndented =
      map
        ( \l ->
            if not $ null l
              then let (x : xs) = l in indentBasic x : xs
              else [Instruction ""]
        )
        categorized
    maxInstructionLength :: Int
    maxInstructionLength =
      -- NOTE: maximum, not max
      maximum $
        map
          ( \(t : ts) -> case t of
              Instruction k -> T.length k
              _ -> 0
          )
          basicIndented
    -- need to indent to one tabstop after the max instruction length
    commentStartColumn = (maxInstructionLength `div` 4) * 4 + 4
    wrappedComments :: [T.Text]
    wrappedComments =
      map
        ( \x ->
            if length x == 1
              then
                let [j] = x
                 in case j of
                      Instruction x -> x
                      Label x -> x
                      Directive x -> x
                      -- TODO: wrap comments
                      Comment x -> x
              else let [code, comment] = x in fixComments commentStartColumn code comment
        )
        basicIndented

debugCategorize :: T.Text -> IO ()
debugCategorize = mapM_ (println . categorizeLine) . T.lines
  where
    println x = do
      print x
      putStrLn ""
