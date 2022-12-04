import Data.List (sortBy)
import Data.String
import Data.Text
import System.IO

readInt :: Text -> Int
readInt value = read (unpack value) :: Int

sumGroup :: [Text] -> Int
sumGroup value = sum (Prelude.map readInt value)

mapGroup :: [[Text]] -> [Int]
mapGroup = Prelude.map sumGroup

splitLine :: Text -> [Text]
splitLine = splitOn (pack "\n\n")

splitNumber :: Text -> [Text]
splitNumber = splitOn (pack "\n")

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let sortDesc = sortBy (flip compare)
  let values = sortDesc (mapGroup (Prelude.map splitNumber (splitLine (strip (pack contents)))))
  print (Prelude.head values)
  print (sum (Prelude.take 3 values))
  hClose handle
