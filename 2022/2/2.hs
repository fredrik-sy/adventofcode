import Data.List (sortBy)
import Data.String
import Data.Text
import System.IO

readScore :: Text -> Int
readScore value
  | value == pack "A X" = 3
  | value == pack "A Y" = 4
  | value == pack "A Z" = 8
  | value == pack "B X" = 1
  | value == pack "B Y" = 5
  | value == pack "B Z" = 9
  | value == pack "C X" = 2
  | value == pack "C Y" = 6
  | otherwise = 7

splitLine :: Text -> [Text]
splitLine = splitOn (pack "\n")

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let score = sum (Prelude.map readScore (splitLine (strip (pack contents))))
  print score
  hClose handle
