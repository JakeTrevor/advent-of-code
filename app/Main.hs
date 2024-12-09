module Main (main) where

import D1 (d1)
import D2 (d2)
import D3 (d3)
import D4 (d4)
import D5 (d5)
import D6 (d6)
import D7 (d7)
import D8 (d8)
import Lib (DaySolution)
import Options.Applicative

days :: [DaySolution]
days = [(const "d0.1", const "d0.2"), d1, d2, d3, d4, d5, d6, d7, d8]

data Config = MkConfig
  { year :: Int,
    day :: Int,
    secondExercise :: Bool,
    useReal :: Bool,
    dataDir :: String
  }
  deriving (Show)

getLeaf :: Config -> String
getLeaf cfg = if useReal cfg then "/real.data" else "/test.data"

getDay :: Config -> String
getDay = ("day-" ++) . show . day

configParser :: Parser Config
configParser =
  MkConfig
    <$> option auto (long "year" <> short 'y' <> metavar "YEAR" <> help "The year this challenge comes from; defaults to this year" <> value 24)
    <*> option auto (long "day" <> short 'd' <> metavar "DAY" <> help "The day to run the exercise for")
    <*> switch (short 'e' <> help "Run the code for exercise 2")
    <*> switch (short 'x' <> help "Run the real data set")
    <*> strOption (long "inputDir" <> short 'i' <> metavar "INDIR" <> help "Data Directory" <> value "./data/")

parser :: ParserInfo Config
parser =
  info
    (configParser <**> helper)
    ( fullDesc
        <> header "Jakes Advent of code 2024"
    )

runSolution :: Config -> (String, String) -> String
runSolution cfg =
  let getter = if secondExercise cfg then snd else fst
   in getter $ days !! day cfg

main :: IO ()
main = do
  config <- execParser parser
  let file = dataDir config ++ getDay config ++ getLeaf config
  contents <- readFile file
  let solution = runSolution config (file, contents)
  putStr solution
  putStr "\n"
