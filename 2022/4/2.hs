import Data.Char (digitToInt, isAsciiLower, ord)
import Data.HashSet (HashSet)
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

toHashSet :: [Int] -> HashSet Int
toHashSet value = HashSet.fromList [(head value) .. value !! 1]

splitHyphen :: String -> [Int]
splitHyphen value = map (readInt . unpack) (splitOn (pack "-") (pack value))

splitComma :: String -> [HashSet Int]
splitComma value = map (toHashSet . splitHyphen . unpack) (splitOn (pack ",") (pack value))

splitLine :: String -> [String]
splitLine value = map unpack (splitOn (pack "\n") (pack value))

containsPartial :: HashSet Int -> HashSet Int -> Bool
containsPartial left right = HashSet.size (HashSet.intersection left right) > 0

compareAssignmentPairs :: [HashSet Int] -> Bool
compareAssignmentPairs value = containsPartial (head value) (value !! 1) || containsPartial (value !! 1) (head value)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let fullyAssignmentPairs = map (compareAssignmentPairs . splitComma) (splitLine contents)
  print (sum (map fromEnum fullyAssignmentPairs))
  hClose handle
