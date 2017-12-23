import Control.Applicative
import Data.Foldable (asum)
import Control.Arrow (first)

grouped :: (a -> Bool) -> [a] -> [[a]]
grouped _ [] = []
grouped isBoundary tokens@(tok:tokTail) =
    let
      (groupTail, rest) = cleaveWhile (not' isBoundary) tokTail
      hereGroup = tok : groupTail
    in hereGroup : grouped isBoundary rest

cleaveWhile :: (a -> Bool) -> List a -> (List a, List a)
cleaveWhile _ [] = ([], [])
cleaveWhile predicate xxs@(x:xs)
  | predicate x = first (heady x) (cleaveWhile predicate xs)
  | otherwise   = ([], xxs)


not' :: (a -> Bool) -> (a -> Bool)
not' f a = not (f a)

isBoundary :: Sline -> Bool
isBoundary [] = False
isBoundary line = head line `elem` boundaries


io path = fmap f (readFile path)
  where
    f = linesOf .| grouped isBoundary .| inAndOut (fmap parsed)

---------- values


type List a = [a]
type Sline = String

data Format = One | Many | ManyOnes | Two deriving (Show, Bounded, Enum)

type Face = List Sline
type Card = List Face

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



parsed group = tryAll parsers group

--parser :: Format -> List Sline -> Maybe Card
parser format [] = Nothing
parser format ("":rest) = Nothing
parser format (first:rest) =
    if head first `isBoundaryOf` format
      then Just (format, _parser format (tail first : rest))
      else Nothing

parsers :: [List Sline -> Maybe (Format, Card)]
parsers = fmap parser formats

_parser :: Format -> List Sline -> Card
_parser One (first:_) = [first] : []
_parser Two (first:rest) = [first] : rest : []
_parser Many lines = grouped isFaceOfMany lines
_parser ManyOnes lines = fmap robot lines

robot :: a -> [a]
robot a = a :[]

isFaceOfMany "" = False
isFaceOfMany line = head line == '/'

-------------- tools

inAndOut :: (a -> b) -> a -> (a, b)
inAndOut f a = (a, f a)

tryAll :: [a -> Maybe b] -> a -> Maybe b
tryAll fs x = asum (fmap (asArg x) fs)

asArg :: x -> (x -> y) -> y
asArg x f = f x

(.|) :: (a -> b) -> (b -> c) -> (a -> c)
(.|) = flip (.)
infixl 9 .|

linesOf = lines

farg :: Functor f => f a -> (a -> b) -> f b
farg = flip fmap

--elemOf = flip elem

heady :: a -> [a] -> [a]
heady = (:)

headset :: (a -> a) -> [a] -> [a]
headset f [] = []
headset f (x:xs) = f x : xs

