{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Teacher.Parser (
  readDeck
) where

import Data.List.Split (splitOn)

import Teacher.Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Foldable (asum)


-- type a :*: b = (a, b)

type RawDeck = [(Lines, RawCardParser)]


readDeck :: FilePath -> IO Deck
readDeck path = readFile path =>> deckFromStr

deckFromStr :: String -> Deck
deckFromStr str =
    linesOf str
    & rawDeck
    & cookedDeck

rawDeck :: Lines -> RawDeck
rawDeck lines =
  runDeckParser
   (asDeckParser deckParser)
   lines 
   (anyBP bpList)

cookedDeck :: RawDeck -> Deck
cookedDeck
  =  fmap (uncurry (&))
  .| justList



newtype DeckParser a = DeckParser
  (ReaderT (BoundaryParser DeckParser)
    (StateT DeckParserState
      Identity)
   a)
  deriving (Functor, Applicative, Monad)
data DeckParserState = DeckParserState {
  pjInput :: Lines,
  pjRawCard :: Lines,
  pjInCardParser :: InCardParser DeckParser,
  pjRawCardParser :: RawCardParser
}

type BoundaryParser m =
  String -> Maybe (InCardParser m, RawCardParser)

type InCardParser m
  = String
  -> m RawDeck
  -> m RawDeck
type RawCardParser
  = Lines
  -> Maybe Card
  

runDeckParser :: DeckParser a -> Lines -> BoundaryParser DeckParser -> a
runDeckParser (DeckParser dp) input boundaryParser =
    runIdentity (evalStateT (runReaderT dp boundaryParser) s)
  where
    s = DeckParserState {
      pjInput = input,
      pjRawCard = [],
      pjInCardParser = garbageICP,
      pjRawCardParser = garbageRCP
    }

modInput :: Lines -> DeckParserState -> DeckParserState
modInput ls s = s { pjInput = ls }

modRawCard :: Lines -> DeckParserState -> DeckParserState
modRawCard rc s = s { pjRawCard = rc }

modRawCardParser :: RawCardParser -> DeckParserState -> DeckParserState
modRawCardParser rcp s = s { pjRawCardParser = rcp }

modInCardParser :: InCardParser DeckParser -> DeckParserState -> DeckParserState
modInCardParser icp s = s { pjInCardParser = icp }

getDPS :: (DeckParserState -> a) -> DeckParser a
getDPS getter = DeckParser $ lift (get =>> getter)

setDPS
  :: (a -> DeckParserState -> DeckParserState)
  -> a -> DeckParser ()
setDPS setter value = DeckParser $ lift (modify (setter value))

class DeckParserClass m where
  getInput :: m Lines
  setInput :: Lines -> m ()

  getRawCard :: m Lines
  setRawCard :: Lines -> m ()

  getInCardParser :: m (InCardParser m)
  setInCardParser :: (InCardParser m) -> m ()

  getRawCardParser :: m RawCardParser
  setRawCardParser :: RawCardParser -> m ()

  getBoundaryParser :: m (BoundaryParser m)

instance DeckParserClass DeckParser where
  getInput = getDPS pjInput
  setInput = setDPS modInput

  getRawCard = getDPS pjRawCard
  setRawCard = setDPS modRawCard

  getInCardParser = getDPS pjInCardParser
  setInCardParser = setDPS modInCardParser

  getRawCardParser = getDPS pjRawCardParser
  setRawCardParser = setDPS modRawCardParser
  
  getBoundaryParser = DeckParser ask

withLine
  :: (DeckParserClass m, Monad m)
  => (String -> m RawDeck)
  -> m RawDeck
withLine cont = getInput >>= \case
    [] -> whenBoundaryReached $ pure []
    l:ls -> setInput ls >> cont l

deckParser
  :: (DeckParserClass m, Monad m)
  => m RawDeck
deckParser = withLine $ \line -> do
    boundaryP <- getBoundaryParser
    let
      whenSameCard = parseCard line
      whenNewCard (newInCardP, newRawCardP) =
        whenBoundaryReached $ do
          setInCardParser newInCardP
          setRawCardParser newRawCardP
          setRawCard []
          parseCard line
    maybe
      whenSameCard
      whenNewCard
      (boundaryP line)

parseCard
  :: (DeckParserClass m, Monad m)
  => String
  -> m RawDeck
parseCard line = do
  inCardP <- getInCardParser
  inCardP line deckParser

whenBoundaryReached
  :: (DeckParserClass m, Monad m)
  => m RawDeck
  -> m RawDeck
whenBoundaryReached cont = do
  rawCard <- getRawCard
  rawCardP <- getRawCardParser
  fmap (asHead (rawCard, rawCardP)) cont

asDeckParser :: DeckParser a -> DeckParser a
asDeckParser = id

anyBP
  :: (DeckParserClass m, Monad m)
  => [BoundaryParser m]
  -> BoundaryParser m
anyBP = sequence .| fmap asum

----
-- * BoundaryParser's
onelineBP :: (DeckParserClass m, Monad m) => BoundaryParser m
onelineBP (':' : _) = Just (onelineICP, onelineRCP)
onelineBP _ = Nothing

multilineBP :: (DeckParserClass m, Monad m) => BoundaryParser m
multilineBP ('*' : _) = Just (multilineICP, multilineRCP)
multilineBP _ = Nothing

multionelineBP :: (DeckParserClass m, Monad m) => BoundaryParser m
multionelineBP ('=' : _) = Just (multionelineICP, multionelineRCP)
multionelineBP _ = Nothing

bpList = [onelineBP, multilineBP, multionelineBP]

----
-- * InCardParser's
alwaysKeepICP :: (DeckParserClass m, Monad m) => InCardParser m
alwaysKeepICP line cont =
  cont =>> overHead (mapOverRawCard $ asHead line)
  where mapOverRawCard f (lines, x) = (f lines, x)

keepOnceICP :: (DeckParserClass m, Monad m) => InCardParser m
keepOnceICP = \line cont -> do
  setRawCard [line]
  whenBoundaryReached $ do
    setInCardParser garbageICP
    setRawCardParser garbageRCP
    setRawCard []
    cont

garbageICP :: (DeckParserClass m, Monad m) => InCardParser m
garbageICP = \_ cont -> cont
  
onelineICP :: (DeckParserClass m, Monad m) => InCardParser m
onelineICP = keepOnceICP

multilineICP :: (DeckParserClass m, Monad m) => InCardParser m
multilineICP = alwaysKeepICP

multionelineICP :: (DeckParserClass m, Monad m) => InCardParser m
multionelineICP = alwaysKeepICP

----
-- * RawCardParser's
garbageRCP :: RawCardParser
garbageRCP = const Nothing

onelineRCP :: RawCardParser
onelineRCP = \case
  [':':rest] -> rest
      &( splitOn "/"
      .| fmap tidy
      .| Just)
  _ ->  error "DEBUG: onelineRCP: wrong input"

multilineRCP :: RawCardParser
multilineRCP [] = error "DEBUG: multilineRCP: wrong input 2"
multilineRCP (first:rest) = case first of
  '*':cs -> rest
      &( processed
      .| overHead (asHead cs)
      .| fmap (fmap tidy .| fromLines .| tidy)
      .| Just)
  _ -> error "DEBUG: multilineRCP: wrong input"
  where
    processed [] = [[]]
    processed (l:ls) = case l of
      '/':tail -> [] : overHead (asHead tail) (processed ls)
      line -> overHead (asHead line) (processed ls)

multionelineRCP :: RawCardParser
multionelineRCP (('=':tail):rest)
    = (tail:rest)
    &( rectoVerso noEmptyLines
    .| fmap tidy
    .| Just)
multionelineRCP _ = error "DBG: multionelineRCP: wrong input"


-- TOOLS
overHead :: (a -> a) -> [a] -> [a]
overHead _ [] = []
overHead f (head:rest) = f (head) : rest

rectoVerso :: ([a] -> [a]) -> [a] -> [a]
rectoVerso f = f .| reverse .| f .| reverse

tidy :: String -> String
tidy = rectoVerso unspace

unspace :: String -> String
unspace "" = ""
unspace all@(c:rest) = case c of
  ' ' -> unspace rest
  '\n' -> unspace rest
  '\t' -> unspace rest
  _ -> all

noEmptyLines :: [String] -> [String]
noEmptyLines [] = []
noEmptyLines all@(l:rest) = case l of
  "" -> noEmptyLines rest
  _ -> all
