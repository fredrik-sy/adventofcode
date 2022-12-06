import System.IO

readInput :: IO String
readInput = do
  handle <- openFile "input.txt" ReadMode
  hGetContents handle

unique :: String -> Bool
unique a = case a of
  [] -> True
  (chr : str) -> chr `notElem` str && unique str

substring :: String -> Int -> Int -> String
substring a idx len = take len (drop idx a)

findMarker :: String -> Int -> Int -> Int
findMarker a len idx
  | idx >= length a = 0
  | unique (substring a idx len) = idx + len
  | otherwise = findMarker a len (idx + 1)

main :: IO ()
main = do
  datastream <- readInput
  print (findMarker datastream 4 0)
  print (findMarker datastream 14 0)