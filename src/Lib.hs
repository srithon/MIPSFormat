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

-- Given a line, splits it into a list of ParseSymbol's, and gives the leading indentation of the line as well
categorizeLine :: T.Text -> (Int, [ParseSymbol])
categorizeLine s = (indentationCount, classification ++ commentSection)
  where
    (rawCode, comment) = T.breakOn "#" s
    indentationCount = T.length $ T.takeWhile isSpace rawCode
    code = T.dropWhileEnd isSpace $ T.drop indentationCount rawCode
    containsComment = not $ T.null comment
    commentSection = [Comment comment | containsComment]
    classification
      | code `elem` directives = [Directive code]
      -- if there is no code at all, then yield nothing
      | T.null code = []
      -- MISTAKE: put this before the null check
      | T.last code == ':' = [Label code]
      | otherwise = [Instruction code]

-- Prepends `l` spaces to t
indent t l = T.replicate l " " `T.append` t

-- Given a ParseSymbol, indents instructions
indentBasic c = case c of
  Instruction t -> Instruction $ indentText t
  x -> x
  where
    indentText t = indent t 4

indentInstructionArgs :: ParseSymbol -> ParseSymbol
indentInstructionArgs c = case c of
  Instruction t ->
    Instruction $
      let instWords = T.words t
          padWords :: [T.Text] -> [T.Text]
          padWords [] = []
          -- pads all but the last word
          padWords (x : xs)
            | null xs = [x]
            | otherwise =
              let len = T.length x
                  spacesToPad = 8 - len
               in T.append x (T.replicate spacesToPad " ") : padWords xs
          paddedWords = padWords instWords
       in T.concat paddedWords
  x -> x

-- Given a start column, an instruction and a comment on the same line, indents
-- the code accordingly and wraps it to the max column width
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
            -- MISTAKE: checking if the tuple is null rather than the list within the tuple
            if not $ null $ snd l
              then let (indentationCount, x : xs) = l in (indentationCount, (indentBasic . indentInstructionArgs) x : xs)
              else (0, [Instruction ""])
        )
        categorized
    maxInstructionLength :: Int
    maxInstructionLength =
      -- NOTE: maximum, not max
      maximum $
        map
          ( \(_, t : ts) -> case t of
              Instruction k -> T.length k
              _ -> 0
          )
          basicIndented
    -- need to indent to one tabstop after the max instruction length
    commentStartColumn = (maxInstructionLength `div` 4) * 4 + 4
    wrappedComments :: [T.Text]
    wrappedComments =
      map
        ( \(indentationCount, x) ->
            if length x == 1
              then
                let [j] = x
                 in case j of
                      Instruction x -> x
                      Label x -> x
                      Directive x -> x
                      -- TODO: wrap comments
                      Comment x ->
                        -- if starts with more than 4 indentation, then move comments to end and wrap
                        -- otherwise, indents by 4 and wraps
                        if indentationCount > 4
                          then fixComments commentStartColumn (Instruction "") j
                          else fixComments 4 (Instruction "") j
              else let [code, comment] = x in fixComments commentStartColumn code comment
        )
        basicIndented

debugCategorize :: T.Text -> IO ()
debugCategorize = mapM_ (println . categorizeLine) . T.lines
  where
    println x = do
      print x
      putStrLn ""
