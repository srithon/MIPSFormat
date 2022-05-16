module Main where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Lib
import Options.Applicative

data AppOptions = AppOptions
  {debug :: Bool}

appOptions :: Parser AppOptions
appOptions =
  AppOptions
    <$> switch
      ( long "debug"
          <> short 'd'
          <> help "If specified, prints out debug information rather than the formatted code."
      )

run :: AppOptions -> IO ()
run opts = do
  contents <- getContents
  let text = T.pack contents
  if debug opts
    then debugCategorize text
    else TO.putStrLn $ format text

main :: IO ()
main = do
  run =<< execParser opts
  where
    opts =
      info
        -- add help message to parser
        (helper <*> appOptions)
        ( fullDesc
            <> progDesc "A basic, opinionated formatter for MIPS assembly."
        )
