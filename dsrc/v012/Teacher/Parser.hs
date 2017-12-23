{-# LANGUAGE LambdaCase #-}
module Teacher.Parser (
  readDeck
) where

--import Control.Applicative
import Data.Foldable (asum)
import Control.Arrow (first, (&&&))
import Data.List.Split (splitOn)

import Teacher.Types

readDeck path = fmap f (readFile path)
  where
    f = linesOf .| grouped isBoundary .|
        fmap parsed .| justList
        -- &&& id

isBoundary :: LINE -> Bool
isBoundary [] = False
isBoundary line = head line `elem` boundaries

parsed :: List LINE -> Maybe Card
parsed group = firstSuccess parsers group

-- parsers :: [List LINE -> Maybe (Format, Card)]
parsers :: List (List LINE -> Maybe Card)
parsers = fmap parser formats

--parser :: Format -> List LINE -> Maybe Card
parser format [] = Nothing
parser format ("":rest) = Nothing
parser format (first:rest) =
    if head first `isBoundaryOf` format
      then let
             unmarkedFirst = boundarySpaced first
             messyCard = linesToCard format (unmarkedFirst : rest)
           -- in Just (format, tidied messyCard)
           in Just (tidied messyCard)
      else Nothing


boundarySpaced :: LINE -> LINE
boundarySpaced "" = error "should not be called with empty LINE"
boundarySpaced line = spaceHead (tail line)


linesToCard :: Format -> List LINE -> Card
linesToCard One (first:_)
    = splitOn "/" first
    & fmap (rectoVerso unspaced) & fmap oneFace
linesToCard Two (first:rest) = oneFace first : face rest : []
linesToCard Many lines =
    grouped isFaceOfMany lines & fmap manyFaceParser
linesToCard ManyOnes lines = fmap oneFace lines

manyFaceParser :: List LINE -> Face
manyFaceParser
  = when (not' null)
  $ when (head .| isFaceOfMany) (overHead boundarySpaced)


oneFace a = [a]
face a = a

---------- values


type List a = [a]
type LINE = String

data Format = One | Many | ManyOnes | Two deriving (Show, Bounded, Enum)

type Face = List LINE

formats :: List Format
formats = [minBound..maxBound]

boundary :: Format -> Char
boundary One = ':'
boundary Many = '*'
boundary ManyOnes = '='
boundary Two = '@'

isBoundaryOf :: Char -> Format -> Bool
isBoundaryOf c format =
    boundary format == c

boundaries = fmap boundary formats



isFaceOfMany "" = False
isFaceOfMany line = head line == '/'

-- MAIN TOOLS


grouped :: (a -> Bool) -> [a] -> [[a]]
grouped _ [] = []
grouped isBoundary tokens@(tok:tokTail) =
    let
      (groupTail, rest) = cleaveWhile (not' isBoundary) tokTail
      hereGroup = tok : groupTail
    in hereGroup : grouped isBoundary rest

firstSuccess :: [a -> Maybe b] -> a -> Maybe b
firstSuccess fs x = asum (fmap (asArg x) fs)



-- GENERAL TOOLS

when :: (a -> Bool) -> (a -> a) -> (a -> a)
when condition g a = if condition a then g a else a

fmapWhen :: Functor f => (a -> Bool) -> (a -> a) -> f a -> f a
fmapWhen condition g fa = fmap (\a -> if condition a then g a else a) fa

robot :: a -> [a]
robot a = a :[]

cleaveWhile :: (a -> Bool) -> List a -> (List a, List a)
cleaveWhile _ [] = ([], [])
cleaveWhile predicate xxs@(x:xs)
  | predicate x = first (asHead x) (cleaveWhile predicate xs)
  | otherwise   = ([], xxs)

not' :: (a -> Bool) -> (a -> Bool)
not' f a = not (f a)

asArg :: x -> (x -> y) -> y
asArg x f = f x

farg :: Functor f => f a -> (a -> b) -> f b
farg = flip fmap

headOf :: [a] -> a -> [a]
headOf = flip (:)

spaceHead :: String -> String
spaceHead = asHead ' '

overHead :: (a -> a) -> [a] -> [a]
overHead _ [] = []
overHead f (head:rest) = f (head) : rest

rectoVerso :: ([a] -> [a]) -> [a] -> [a]
rectoVerso f = f .| reverse .| f .| reverse

unspaced :: String -> String
unspaced = dropWhile isWhite

isWhite :: Char -> Bool
isWhite = \case
  ' ' -> True
  '\n' -> True
  '\t' -> True
  _ -> False

emptyLinesRemoved :: [String] -> [String]
emptyLinesRemoved = dropWhile isWhiteline

isWhiteline :: String -> Bool
isWhiteline = all isWhite


tidied :: List (List LINE) -> List (List LINE)
tidied = fmap (rectoVerso emptyLinesRemoved) --for each face