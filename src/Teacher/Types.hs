module Teacher.Types where

import Control.Monad (MonadPlus(..))
import Data.List (intercalate)
import System.Random

(&) = flip ($)
(<>) = (++)
(===) :: Eq a => a -> a -> Bool
(===) = (==)
trailWhen p l = dropWhile (not . p) l
must :: MonadPlus m => (a -> Bool) -> a -> m a
must p a = if p a then pure a else mzero
diff :: Eq a => a -> a -> Bool
diff = (/=)
linesOf = lines
fromLines = unlines
fromLines' :: [[Char]] -> [Char]
fromLines' = fuse '\n'
fuse a l = foldr1 f l
  where f s s' = s <> [a] <> s'
(./) = flip (.)
(=>>) :: Functor f => f a -> (a -> b) -> f b
(=>>) = flip (<$>)
justList :: [Maybe a] -> [a]
justList [] = []
justList (Just a: rest) = a : justList rest
justList (Nothing: rest) = justList rest
joinWith = intercalate

constM :: Applicative f => a -> b -> f a
constM a _ = pure a

data CmdCard = Skip | ShowAnswer | Mistake | Next deriving Show
data CmdGen = Quit | QuitSave | ShowScore deriving Show
data Step = Answer | Question

type Content = String
type Card = (Content, Content, Maybe Content)
type Deck = [Card]
type WithMistakes a = (a, Deck)

type Score = (Int, Int, Int)

data Config = Config {
  maybeFlip :: Deck -> Deck,
  maybeRandom :: StdGen -> Deck -> Deck,
  maybeLoop :: IO () -> IO (),
  maybeVerbose :: IO () -> IO ()
}
