{-# LANGUAGE LambdaCase #-}
-- module Teacher (teaching, input) where
module Teacher where

-- base
import Control.Arrow ( (|||) )
import qualified Control.Arrow as Arr
import qualified Data.Tuple as Tuple (swap)


import System.Random

--to delete

import Teacher.Types
import Teacher.Parser


-- main = getArgs >>= input >>= either error teaching

input :: [String] -> IO (Either String (Deck, Config))
input args = maybe' maybeFiles
    (pure $ Left "Input error (no files given)")
    $ \files -> do
      mapM readingDeck files =>> \decks ->
        Right (concat decks, Config {
          maybeFlip = hasOpt "flip" flipDeck id,
          maybeRandom = hasOpt "norand" (\_ d -> d) randomDeck,
          maybeLoop = hasOpt "noloop" (constM ()) id,
          maybeVerbose = hasOpt "verbose" id (constM ())
          
        })
  where
    maybe' m n j = maybe n j m
    opt = ("--" <>)
    maybeFiles :: Maybe [FilePath]
    maybeFiles = must (length ./ diff 0) $
      tail $ trailWhen (=== opt "decks") args
    hasOpt key ifyes ifnot =
      if opt key `elem` args then ifyes else ifnot
    flipDeck deck = fmap (\(a, b, c) -> (b, a, c)) deck


teaching :: (Deck, Config) -> IO ()
teaching (deck, cfg) = do
    g <- newStdGen
    let processed = maybeFlip cfg ./ maybeRandom cfg g
    print $ fmap (\(a, b, c) -> a) $ processed deck
    (withMistakes $ processed deck)
      & walk (maybeVerbose cfg) (maybeLoop cfg doLoop) emptyStats
  where
    withMistakes d = (d,[])
    emptyStats = (0,0,0)
    doLoop = teaching (deck, cfg)

printing :: Show a => a -> IO ()
printing = putStrLn . show

ttt [] = [[]]
ttt (x:xs) = xs: ttt xs

shuffle :: [Int] -> [a] -> [a]
shuffle rs [] = []
shuffle rs [x] = [x]
shuffle rs xs = h : shuffle is (end <> beg)
    -- reversing end and beg avoids constantly ending
    -- the shuffling with (foot xs)
  where
    (beg, h:end) = takeAndRest (processed i) xs
    (i:is) = flip trailWhen rs
      (processed ./ diff (ln - 1))
    processed i = mod i ln
    ln = length xs
    takeAndRest n xs = (take n xs, drop n xs)
    trailWhen p [] = []
    trailWhen p (x:xs)
      | p x = x:xs
      | otherwise = trailWhen p xs

randomDeck :: StdGen -> Deck -> Deck
randomDeck rg deck = shuffle (randoms rg :: [Int]) deck
  
newMistake (n, p, q) = (n + 1, p, q)
-- readingChar = undefined :: IO Char

walk
  :: (IO () -> IO ())
  -> IO ()
  -> Score
  -> WithMistakes Deck
  -> IO ()
walk maybeVerbose maybeLoop score ([],[]) = printing score >> maybeLoop
walk maybeVerbose maybeLoop score ([],mistakes) = walk maybeVerbose maybeLoop score (mistakes, [])
walk maybeVerbose maybeLoop score (deck, mistakes) =
    let
      (card : rest) = deck
      (first, second, maybeN) = card
    in do
      maybeVerbose (printing score)
      showFirst first & putStrLn
      listening Question $ \case
        ShowAnswer -> do
          showSecond second maybeN & putStrLn
          listening Answer $ \case
            Mistake -> rec (newMistake score) (rest, card:mistakes)
            Next -> rec score (rest, mistakes)
        Skip -> rec score (rest, mistakes)
  where
    rec = walk maybeVerbose maybeLoop
    
    
    listening step cont = getChar >>= charToCmd step >>=
        maybe (listening step cont) (doGenCmd loop ||| cont)
      where loop = listening step cont
    doGenCmd cont ShowScore = printing score >> cont
    doGenCmd _ Quit = pure ()
    doGenCmd _ QuitSave = do
        putStrLn "filename:"
        getLine >>= writeFile (fromDeck $ deck <> mistakes)

charToCmd :: Step -> Char -> IO (Maybe (Either CmdGen CmdCard))
charToCmd step c = do
    maybe (putStrLn " -- *Unrecognized*") (either verbose verbose) cmd
    pure cmd
  where
    cmd = parsed step c
    verbose v = putStrLn (" -- " <> show v)
    parsed _ 'q'        = Left Quit & Just
    parsed _ 'z'        = Left QuitSave & Just
    parsed _ 'c'        = Left ShowScore & Just
    parsed Question 'n' = Right ShowAnswer & Just
    parsed Question 's' = Right Skip & Just
    parsed Answer 'm'   = Right Mistake & Just
    parsed Answer 'n' = Right Next & Just
    parsed _ _ = Nothing
-- potential extension: more than two data per card
-- reusability <=> no arbitrary conceptual limitations
-- create interface that would allow creation of decks via hs functions
-- option when giving several files: even if random, do not mix decks together
