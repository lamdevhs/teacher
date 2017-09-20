{-# LANGUAGE LambdaCase #-}
-- module Teacher (teaching, input) where
module Teacher where

-- base
import Control.Arrow ( (|||) )
import qualified Control.Arrow as Arr
import qualified Data.Tuple as Tuple (swap)
import Data.List (intercalate)

-- not base
import Data.List.Split (splitOn)


--to delete
import Control.Monad (MonadPlus(..))

(&) = flip ($)
(<>) = (++)
(===) :: Eq a => a -> a -> Bool
(===) = (==)
trailWhen p l = dropWhile (not . p) l
len = length
must :: MonadPlus m => (a -> Bool) -> a -> m a
must p a = if p a then pure a else mzero
diff :: Eq a => a -> a -> Bool
diff = (/=)
linesOf = lines
fromLines = unlines
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

data CmdCard = Skip | ShowAnswer | Mistake | NoMistake deriving Show
data CmdGen = Quit | QuitSave deriving Show
data Step = Answer | Question

type Content = String
type Card = (Content, Content, Maybe Content)
type Deck = [Card]
type WithMistakes a = (a, Deck)

type Score = (Int, Int, Int)

data Config = Config {
  maybeFlip :: Deck -> Deck,
  maybeRandom :: Deck -> Deck,
  maybeLoop :: IO () -> IO (),
  maybeVerbose :: IO () -> IO ()
}

-- main = getArgs >>= input >>= either error teaching

input :: [String] -> IO (Either String (Deck, Config))
input args = maybe' maybeFiles
    (pure $ Left "Input error (no files given)")
    $ \files -> mapM readingDeck files =>> \decks ->
      -- Left e -> pure (Left e)
      -- Right decks -> 
      Right (concat decks, Config {
        maybeFlip = hasOpt "flip" flipDeck id,
        maybeRandom = hasOpt "norand" id randomDeck,
        maybeLoop = hasOpt "noloop" (constM ()) id,
        maybeVerbose = hasOpt "verbose" id (constM ())
        
        })
  where
    maybe' m n j = maybe n j m
    opt = ("--" <>)
    maybeFiles :: Maybe [FilePath]
    maybeFiles = must (len ./ diff 0) $
      tail $ trailWhen (=== opt "decks") args
    hasOpt key ifyes ifnot =
      if opt key `elem` args then ifyes else ifnot
    flipDeck deck = fmap (\(a, b, c) -> (b, a, c)) deck

readingDeck :: FilePath -> IO Deck
readingDeck path = readFile path =>> asDeck

cardSeparator = "???"
sideSeparator = "%%%"

asDeck :: String -> Deck
asDeck
    =  linesOf
    ./ tidy
    ./ splitOn [cardSeparator]
    ./ fmap (splitOn [sideSeparator] ./ fmap fromLines' ./ maybeCard)
    ./ justList
  where
    maybeCard (a: b: rest) = if null rest
      then Just (a, b, Nothing)
      else Just (a, b, Just $ fromLines' rest)
    maybeCard _ = Nothing
    tidy = filter (diff "")

fromDeck :: Deck -> String
fromDeck
    =  fmap fromCard
    ./ joinWith cardSeparator
  where
    fromCard (x, y, Nothing) = joinWith sideSeparator [x, y]
    fromCard (x, y, Just z) = joinWith sideSeparator [x, y, z]

teaching :: (Deck, Config) -> IO ()
teaching (deck, cfg) =
    (withMistakes $ processed deck)
      & walk (maybeVerbose cfg) (maybeLoop cfg doLoop) emptyStats
  where
    withMistakes d = (d,[])
    emptyStats = (0,0,0)
    processed deck = deck & maybeFlip cfg & maybeRandom cfg
    doLoop = teaching (deck, cfg)

printing :: Show a => a -> IO ()
printing = putStrLn . show
randomDeck = reverse
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
      showFirst first
      listening Question $ \case
        ShowAnswer -> do
          showSecond [Just second, maybeN]
          listening Answer $ \case
            Mistake -> rec (newMistake score) (rest, card:mistakes)
            NoMistake -> rec score (rest, mistakes)
        Skip -> rec score (rest, mistakes)
  where
    rec = walk maybeVerbose maybeLoop
    
    show_ = putStrLn
    showFirst = show_
    showSecond = justList ./ fromLines' ./ show_
    
    listening step cont = getChar >>= charToCmd step >>=
        maybe (listening step cont) (doGenCmd undefined ||| cont)
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
    parsed Question 'a' = Right ShowAnswer & Just
    parsed Question 's' = Right Skip & Just
    parsed Answer 'm'   = Right Mistake & Just
    parsed Answer 'n' = Right NoMistake & Just
    parsed _ _ = Nothing
-- potential extension: more than two data per card
-- reusability <=> no arbitrary conceptual limitations
-- create interface that would allow creation of decks via hs functions
-- option when giving several files: even if random, do not mix decks together
