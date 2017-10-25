{-# LANGUAGE RankNTypes #-}
module Teacher.Types where

import Control.Monad (MonadPlus(..))
import Data.List (intercalate)
import System.Random

(&) = flip ($)
(<>) = (++)
(===) :: Eq a => a -> a -> Bool
(===) = (==)
ord :: Ord a => a -> a -> Bool
ord = (<)
trailWhen p l = dropWhile (not . p) l
takeAndRest n xs = (take n xs, drop n xs)
must :: MonadPlus m => (a -> Bool) -> a -> m a
must p a = if p a then pure a else mzero
diff :: Eq a => a -> a -> Bool
diff = (/=)
linesOf = lines
fromLines = unlines
fromLines' :: [[Char]] -> [Char]
fromLines' = fuse '\n'
fuse a [] = []
fuse a l = foldr1 f l
  where f s s' = s <> [a] <> s'
(.|) = flip (.)
infixl 9 .|
(=>>) :: Functor f => f a -> (a -> b) -> f b
(=>>) = flip (<$>)
justList :: [Maybe a] -> [a]
justList [] = []
justList (Just a: rest) = a : justList rest
justList (Nothing: rest) = justList rest
joinWith = intercalate
undef = undefined

constM :: Applicative f => a -> b -> f a
constM a _ = pure a

glueWith :: a -> [[a]] -> [a]
glueWith x [] = []
glueWith x [xs] = xs
glueWith x (xs:r) = xs ++ (x : glueWith x r)

asHead :: a -> [a] -> [a]
asHead x xs = x:xs

type Lines = [String]

data CmdQandA = Skip | GiveAns | Mistake | Next deriving (Show, Enum, Bounded)
data CmdGen = Quit | QuitSave | SayScore deriving (Show, Enum, Bounded)
data Step = Answer | Question deriving Show

type Score = (Int, Int, Int, Int)

type Card = [[String]]
type Deck = [Card]

type QandA = ([String], [String])
type Lesson = [QandA]
{-
newtype LessonMaterial = LessonMaterial {
  mainPile :: Deck,
  bottomPile :: Deck
}
-}
