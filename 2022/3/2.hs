import Data.Char (digitToInt, isAsciiLower, ord)
import Data.HashSet qualified as HashSet
import Data.List.Split (chunksOf)
import Data.Text (Text, pack, splitOn, unpack)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

commonChar :: (String, String, String) -> Char
commonChar (upper, middle, lower) = head (HashSet.toList (HashSet.intersection (HashSet.fromList middle) (HashSet.intersection (HashSet.fromList upper) (HashSet.fromList lower))))

splitBounds :: [String] -> (String, String, String)
splitBounds value = (head value, value !! 1, value !! 2)

splitLine :: String -> [String]
splitLine value = map unpack (splitOn (pack "\n") (pack value))

toPriority :: Char -> Int
toPriority letter = if isAsciiLower letter then ord letter - 96 else ord letter - 38

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let chunks = chunksOf 3 (splitLine contents)
  let priorities = map ((toPriority . commonChar) . splitBounds) chunks
  print (sum priorities)
  hClose handle
