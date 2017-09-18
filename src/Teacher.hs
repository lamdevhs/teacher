{-# LANGUAGE LambdaCase #-}
module Teacher (teaching, input) where

-- base
import Control.Arrow ( (|||) )
import qualified Control.Arrow as Arr
import qualified Data.Tuple as Tuple (swap)

-- not base
import Data.List.Split (splitOn)

(&) = flip ($)
(./) = flip (.)
(=>>) :: Functor f => f a -> (a -> b) -> f b
(=>>) = flip (<$>)
justList = undefined :: [Maybe a] -> [a]

data CmdCard = Skip | Reveal | Mistake | NoMistake
data CmdGen = Quit | QuitSave
data Step = Answer | Question

type Content = String
type Card = (Content, Content, Maybe Content)
type Deck = [Card]
type WithMistakes a = (a, Deck)

type Score = (Int, Int, Int)

data Config m = Config {
  maybeFlip :: Deck -> Deck,
  maybeRandom :: Deck -> Deck,
  maybeLoop :: m () -> m (),
  maybeVerbose :: m () -> m ()
}

-- main = getArgs >>= input >>= either error teaching

input :: [String] -> m (Either String (Deck, Config m))
input args = maybe maybeFiles
    (pure $ Left "Input error (no files given)")
    $ \files -> mapM readingDeck files =>> \decks ->
      -- Left e -> pure (Left e)
      -- Right decks -> 
      Right (concat decks, Config {
        maybeFlip = hasOpt "flip" flipDeck id,
        maybeRandom = hasOpt "norand" id randomDeck,
        maybeLoop = hasOpt "noloop" (pure () & const) id,
        maybeVerbose = hasOpt "verbose" id (pure () & const)
        
        })
  where
    opt = ("--" <>)
    maybeFiles = must (len ./ diff 0) $
      tail $ trailWhen (=== opt "decks") args
    hasOpt key ifyes ifnot =
      if opt key `elem` args then ifyes else ifnot
    flipDeck deck = Arr.first (fmap Tuple.swap) deck

readingDeck :: FilePath -> m Deck
readingDeck path = getContents path =>> asDeck

asDeck :: String -> Deck
asDeck file
    =  linesOf
    ./ splitOn "???"
    ./ fmap (splitOn "%%%" ./ maybeCard)
    ./ justList
  where
    maybeCard (a: b: rest) = Just (a, b, concat rest)
    maybeCard _ = Nothing

teaching :: (Deck, Config m) -> m ()
teaching (deck, cfg) =
    processed deck
    & walk emptyMistakes emptyStats (maybeVerbose cfg) (maybeLoop doLoop)
  where
    emptyMistakes = []
    emptyStats = (0,0,0)
    processed deck = deck & maybeFlip cfg & maybeRandom cfg
    doLoop = teaching (deck, cfg)

printing = undefined
newMistake = undefined
readingChar = undefined :: m Char

walk
  :: (m () -> m ())
  -> m ()
  -> Score
  -> WithMistakes Deck
  -> m ()
walk maybeVerbose maybeLoop score ([],[]) = printing score >> maybeLoop
walk maybeVerbose maybeLoop score ([],mistakes) = rec (mistakes, [])
walk maybeVerbose maybeLoop score (deck, mistakes) =
    let
      (card : rest) = deck
      ((first, second), maybeN) = card
    in
      maybeVerbose (printing score) >>
      showFirst first >> listening Question $ \case
        Reveal -> showSecond [second, maybeN] >> listening Answer $ \case
          Mistake -> rec (newMistake score) (rest, card:mistakes)
          NoMistake -> rec score (rest, mistakes)
        Skip -> rec score (rest, mistakes)
  where
    rec = walk maybeVerbose maybeLoop
    
    show_ = undefined
    showFirst = show_
    showSecond = justList ./ concat ./ show_
    
    listening step cont = (readingChar =>> charToCmd step) >>=
        maybe (listening step cont) (doGenCmd undefined ||| cont)
    doGenCmd _ Quit = pure ()
    doGenCmd _ QuitSave = putStrLn "\nfilename:"
        >> getLine >>= writeFile (fromDeck $ deck ++ mistakes)

charToCmd :: Step -> Char -> Maybe (Either CmdGen CmdCard)
charToCmd _ 'q' = Left Quit & Just
charToCmd _ 'z' = Left QuitSave & Just
charToCmd Question 'a' = Right Reveal & Just
charToCmd Question 's' = Right Skip & Just
charToCmd Answer 'm' = Right Mistake & Just
charToCmd Answer 'n' = Right NoMistake & Just
charToCmd _ _ = Nothing
-- potential extension: more than two data per card
-- reusability <=> no arbitrary conceptual limitations
-- create interface that would allow creation of decks via hs functions
-- option when giving several files: even if random, do not mix decks together
