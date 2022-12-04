import Data.Char (digitToInt, isAsciiLower, ord)
import Data.HashSet qualified as HashSet
import Data.Text (Text, pack, splitOn, unpack)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

commonChar :: (String, String) -> Char
commonChar (upper, lower) = head (HashSet.toList (HashSet.intersection (HashSet.fromList upper) (HashSet.fromList lower)))

splitUpperLower :: String -> (String, String)
splitUpperLower value = splitAt (div (length value) 2) value

splitLine :: String -> [String]
splitLine value = map unpack (splitOn (pack "\n") (pack value))

toPriority :: Char -> Int
toPriority letter = if isAsciiLower letter then ord letter - 96 else ord letter - 38

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = map splitUpperLower (splitLine contents)
  let priorities = map (toPriority . commonChar) lines
  print (sum priorities)
  hClose handle
