module Teacher.Parser where

import Teacher.Types
-- not base
import Data.List.Split (splitOn)

deckFromFile :: FilePath -> IO Deck
deckFromFile path = readFile path =>> deckFromStr

oneCard = ...
  afterQuestion
afterQuestion -> showAnswer -> afterAnswer

deckFromStr :: String -> Deck
deckFromStr
    =  linesOf
    .| runState parser
    .| fmap (tidy .| cardFromStr)
    .| justList
  where
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

p = withLine $ \l ->
    initp <- ask
    case initp l of
      Just (rawCard, linep) -> do
        rcard <- currentRawCard
        set srawCard
        set slinep
        fmap (cons rcard) p
      Nothing -> linep l p

withLine
    :: (MonadState PState m,
        MonadReader (Line -> Maybe (RawCard, LineP)) m)
    -> (String -> m [RawCard]) -> m [RawCard]
withLine c = get >>= \case
  [] -> pure []
  (x:xs) -> set srest xs >> c x

set setter x = do
  state <- get
  put $ setter state x

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
