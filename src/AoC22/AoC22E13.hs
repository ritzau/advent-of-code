module AoC22E13 (distressSignal) where

import Data.Char (isDigit)
import Data.List (break, elemIndex, sort, span)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)
import Helpers (printHeader, printResult, readData)

data Item = Number Int | List [Item]
  deriving (Eq)

instance Ord Item where
  compare a b = compareItems a b

instance Show Item where
  show i = showItem i

showItem (Number a) = show a
showItem (List a) = show a

distressSignal :: IO ()
distressSignal = do
  printHeader "2022 Day 13: Distress Signal"

  sampleContent <- readData "data-s22e13-sample.txt"
  content <- readData "data-s22e13.txt"

  let sampleInput = parseInput sampleContent
  let input = parseInput content

  printResult "Magic sum of sample" 13 $ sumOfIndicesOfMatchingPairs sampleInput
  printResult "Magic sum" 5330 $ sumOfIndicesOfMatchingPairs input

  printResult "Decoder key of sample" 140 $ decoderKey sampleContent
  printResult "Decoder key" 27648 $ decoderKey content

sumOfIndicesOfMatchingPairs pairs =
  let indexedComparisons = zip [1 ..] $ map (uncurry compareItems) pairs
      matchingPairs = filter (\(_, cmp) -> cmp == LT) indexedComparisons
   in sum $ map fst matchingPairs

decoderKey content =
  let div1 = List [List [Number 2]]
      div2 = List [List [Number 6]]
      result = sort (parseInput2 content ++ [div1, div2])
      ind1 = 1 + fromJust (div1 `elemIndex` result)
      ind2 = 1 + fromJust (div2 `elemIndex` result)
   in ind1 * ind2

parseInput :: String -> [(Item, Item)]
parseInput = processPairs . lines
  where
    processPairs [] = []
    processPairs ls =
      let (p, ps) = break null ls
       in processPair p : processPairs (drop 1 ps)

    processPair [p1, p2] = (parseItem p1, parseItem p2)
    processPair _ = undefined

parseInput2 :: String -> [Item]
parseInput2 = map parseItem . filter (not . null) . lines

parseItem :: String -> Item
parseItem cs =
  let (item, rest) = parseItem' cs
   in if null rest then item else undefined
  where
    parseItem' [] = undefined
    parseItem' cs'@(c : cs)
      | c == '[' = parseListItem [] cs
      | isDigit c = parseNumItem cs'
      | otherwise = undefined

    parseListItem [] (']' : cs) = (List [], cs)
    parseListItem ls cs =
      let (item, rest) = parseItem' cs
       in parseListItem' (ls ++ [item]) rest
      where
        parseListItem' ls (']' : cs) = (List ls, cs)
        parseListItem' ls (',' : cs) =
          let (item, rest) = parseItem' cs
           in parseListItem' (ls ++ [item]) rest
        parseListItem' _ _ = undefined

    parseNumItem cs =
      let (n, rest) = span isDigit cs
       in (Number $ read n, rest)

compareItems :: Item -> Item -> Ordering
compareItems (Number a) (Number b) = compare a b
compareItems na@(Number a) (List b) = compareLists [na] b
compareItems (List a) nb@(Number b) = compareLists a [nb]
compareItems (List a) (List b) = compareLists a b

compareLists [] (_ : _) = LT
compareLists (_ : _) [] = GT
compareLists [] [] = EQ
compareLists (a : as) (b : bs) = case compareItems a b of
  EQ -> compareLists as bs
  o -> o
