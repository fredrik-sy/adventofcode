import Data.Char (digitToInt, isAsciiLower, ord)
import Data.HashSet qualified as HashSet
import Data.Text (Text, pack, splitOn, tail, unpack)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

readInt :: String -> Int
readInt value = read value :: Int

splitHyphen :: String -> [Int]
splitHyphen value = map (readInt . unpack) (splitOn (pack "-") (pack value))

splitComma :: String -> [[Int]]
splitComma value = map (splitHyphen . unpack) (splitOn (pack ",") (pack value))

splitLine :: String -> [String]
splitLine value = map unpack (splitOn (pack "\n") (pack value))

containsFully :: [Int] -> [Int] -> Bool
containsFully left right = head left <= head right && left !! 1 >= right !! 1

compareAssignmentPairs :: [[Int]] -> Bool
compareAssignmentPairs value = containsFully (head value) (value !! 1) || containsFully (value !! 1) (head value)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let fullyAssignmentPairs = map (compareAssignmentPairs . splitComma) (splitLine contents)
  print (sum (map fromEnum fullyAssignmentPairs))
  hClose handle
