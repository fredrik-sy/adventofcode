import Data.Char (digitToInt, isAsciiLower, ord)
import Data.HashSet qualified as HashSet
import Data.List.Split (splitWhen)
import Data.Text (Text, pack, splitOn, tail, unpack)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

data Instruction = Instruction
  { count :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

type Stack = String

readInstruction :: String -> Instruction
readInstruction value =
  let (a : b : c : d : e : f : g) = splitWhen (== ' ') value
   in Instruction {count = read b, from = read d, to = read f}

splitLine :: String -> [String]
splitLine value = map unpack (splitOn (pack "\n") (pack value))

splitMultiLine :: String -> [String]
splitMultiLine value = map unpack (splitOn (pack "\n\n") (pack value))

extract :: [[String]] -> ([Stack], [Instruction])
extract value = (head value, map readInstruction (value !! 1))

pop :: Instruction -> [Stack] -> ([Stack], Stack)
pop instruction stacks = (before ++ [remaining] ++ after, crates)
  where
    (before, current : after) = splitAt (from instruction - 1) stacks
    (crates, remaining) = splitAt (count instruction) current

push :: Instruction -> Stack -> [Stack] -> [Stack]
push instruction crates stacks = before ++ [new] ++ after
  where
    (before, current : after) = splitAt (to instruction - 1) stacks
    new = crates ++ current

move :: Instruction -> [Stack] -> [Stack]
move instruction stacks = push instruction crates nextStacks
  where
    (nextStacks, crates) = pop instruction stacks

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let (stacks, instructions) = extract (map splitLine (splitMultiLine contents))
  let result = foldr move stacks (reverse instructions)
  print result
  print (map head (filter (not . null) result))
  hClose handle
