module Teacher.Parser where

import Teacher.Types
-- not base
import Data.List.Split (splitOn)


class DeckParser m where
  getInput :: m Lines
  setInput :: Lines -> m ()

newtype MyParser a = MyParser (StateT MyState Identity a)

data MyState = MyState {
  _input :: Lines
}

state_setInput x s = s { _input = x }

instance DeckParser MyParser where
  getInput = lift get =>> _input
  setInput lines = lift (modify $ state_setInput lines)

popLine :: DeckParser m, Monad m => m (Maybe Line)
popLine = getInput >>= \case
  [] -> pure Nothing
  l:ls -> setInput ls >> pure (Just l)

withLine :: DeckParser m, Monad m => -> (Line -> m RawOutput) -> m RawOutput
withLine cont =
    popLine >>= maybe (pure []) cont

deckFromFile :: FilePath -> IO Deck
deckFromFile path = readFile path =>> deckFromStr

oneCard = ...
  afterQuestion
afterQuestion --> showAnswer --> afterAnswer

deckFromStr :: String -> Deck
deckFromStr str
    = linesOf
    .| initState
    .| runState parser
    .| fmap (tidy .| cardFromStr)
    .| justList
  where
    initState lines = MyState {
      _input = lines,
      _boundaryParser = ...,
    }
    tidy = ...

type LineP
    = (MonadState PState m,
       MonadReader (Line -> Maybe (RawCard, LineP)) m)
    => String
    -> m [RawCard]
    -> m [RawCard]

type RCardP = MonadState Lines m => m (Maybe Card)
newtype RawCard = RawCard (Lines, RCardP)

data PState = PState {
  _rawDeck :: Lines,
  _rawCard :: RawCard,
  _linep :: LineP
}

class DSL m where
  initp


boundaryParser :: Line -> Either Line (Line, LineParser)


p :: DeckParser m, Monad m => m RawOutput
p = eachLine $ either
      ifSameCard
      ifNewCard
-- where
eachLine cont = withLine $ \line ->
  initp <- askInitp
  cont (boundaryParser line)
ifSameCard :: Line -> Parser m
ifSameCard line = do
    lineParser <- getLineParser
    let continuation = p
    lineParser line p
ifNewCard :: (Line, LineParser) -> Parser m
ifNewCard (line, lineParser) = do
    rawCard <- getRawCard
    setRawCard [line]
    setLineParser lineParser
    fmap (cons rawCard) p
type LineParser :: Line -> Parser m -> Parser m

withLine
    :: (MonadState PState m,
        MonadReader (Line -> Maybe (RawCard, LineP)) m)
    -> (String -> m [RawCard]) -> m [RawCard]
withLine c = get >>= \case
  [] -> pure []
  (x:xs) -> set srest xs >> c x


srest
    :: PState -> Lines -> PState
srest state rest = state { _rawDeck = rest }

slinep state x = state { _linep = x }
srawCard state x = state { _rawCard = x }

caseMaybe, caseEither, caseEmpty (Applicative)
linep
  :: (MonadState PState m,
     MonadReader (Line -> Maybe (RawCard, LineP)) m)
  => String -> m [RawCard]
  -> m [RawCard]
linep l cont = 
  state <- get
  _linep state l cont
      

fromDeck :: Deck -> String
fromDeck
    =  fmap fromCard
    .| concat
    .| fromLines
  where
    fromCard [] = joinWith sideSepF [x, y, z]
