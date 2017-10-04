module Teacher.Parser where

import Teacher.Types
-- not base
import Data.List.Split (splitOn)


newtype RawCard = RawCard (m (Maybe Card), Lines)

f
  :: MonadState s m
  => (s -> Bool)
  -> m a
  -> m [a]
f pp ma = get >>= \s ->
    if pp s
      then pure []
      else liftA2 (:) ma (f ma pp)

h
  :: MonadState s m
  => (s -> Bool)
  -> (s -> s)
  -> m (Maybe a)
  -> m (Maybe a)
h pp changed ma = get >>= \s ->
    if pp s
      then pure Nothing
      else ma >>= \case
        Nothing -> put (changed s) >> h ma changed
        just_a -> pure just_a

type Subparser m = m (Maybe Card)
type P m = (Line -> P m) -> Lines -> m [(Lines, Subparser)]

newtype DeckParserS = DeckParserS (RawCard, [RawCard])

class (MonadState S m) => DeckParser m where
    headOfRawCard :: Line -> m ()
  

p
  :: MonadState Lines m
  => LineTest
  -> Subparser m
  -> P m
--  -> (Line -> P m)
--  -> Lines
--  -> m [(Lines, Subparser)]
p testLine subparser initParsers acc = do
    l <- behead
    case initParsers l of
      Just (line, p) ->
          recNext p [line]
      Nothing -> case testLine acc l of
        Stop -> recNext garbageParser []
        Skip -> recCont acc
        Keep -> recCont (acc ++ [l])
  where
    recCont = p testLine subparser initParsers
    recNext p newAcc =
      fmap ((acc, subparser):) $
        p initParsers newAcc

p initp linep acc = get >>= \case
  [] -> pure [acc]
  line:_ -> case initp line of
    Just (newAcc, newLinep) -> fmap (asHead acc) $
      p initp newLinep newAcc
    Nothing -> linep (p initp) acc l

skipLinep p acc l = p garbagep acc
keepLinep p acc l = p keepLinep (first (++ [l]) acc)
stopLinep p acc l = fmap (asHead acc') $ p skipLinep emptyAcc
  where acc' = first (++ [l]) acc

emptyAcc = ([], pure Nothing)

garbageParser = p (alwaysSkip) (pure Nothing)

fromFile :: File -> Deck
fromFile file
    = runState pa (linesOf file)
    ./ fmap f
    ./ justList
  where
    pa = p initParsers alwaysSkip alwaysFail []
    f (ls, cardParser) = runState cardParser ls

-- don't forget to filter out the stuff parsed by alwaysSkip

allParsers list = foldr1 malt list
alwaysFail = pure Nothing

alwaysSkip _ _ = Skip
oneLine _ _ = Stop
manyLines _ _ = Keep

g :: MonadState [x] m => m a -> m [a]
g ma = f null ma

k :: MonadState Lines m => [m (Maybe RawCard)] -> m (Maybe RawCard)
k ps = h null tail $ foldr1 malt ps

RawCard = (Lines, m Card)
type T = supertest -> m (Maybe RawCard)
  maxsize: Int }

w :: MonadState Lines m => [Config] -> [m (MaybeRawCard)]
w cfgL = fmap (f supertest) cfgL
  where
    supertest = foldr1 (liftA2 (&&)) allTests
      where allTests = fmap test cfgL
    f supertest cfg

malt :: Monad m, Alternative f => m (f a) -> m (f a) -> m (f a)
malt = liftA2 (<|>)

type RawCardParser = MonadState Lines m => m (Maybe RawCard)

oneliner :: MonadState Lines m => (m (Maybe RawCardParser)) -> (m (Maybe RawCard))
oneliner allTests = get >>= \case
    [] -> pure Nothing
    (l:ls) -> case l of
      '#':cs -> pure . Just (l, onelinerP)
      _ -> 







cardSepF = "???"
sideSepF = "%%%"


subparts
  :: (  t
     -> (e -> Maybe t)
     -> [e]
     -> (Maybe (t, [e]), [e])
     )
  -> (e -> Maybe t)
  -> [e]
  -> [(t, [e])]
subparts [] = []
subparts ofType diagnose l = let
    (maybe_te, rest) = onesubpart l
    in
    maybe (subparts rest) (: subparts rest)
      maybe_te
  where
    onesubpart [] = (Nothing, []) -- a priori dead path
    onesubpart (e:es) =
      case diagnose e of
        Nothing -> onesubpart es
        Just t -> ofType t diagnose (e:es)

asDeck = subparts oneCard diagnoseLine

oneCard :: CardFormat -> (String -> Maybe CardFormat)
  -> Lines
  -> (Maybe (CardFormat, Lines), Lines)
oneCard 

unspace :: String -> String
unspace s = fromBothEnds unspaceHead s

fromBothEnds :: ([a] -> [a]) -> [a] -> [a]
fromBothEnds f l = f l ./ reverse ./ f ./ reverse

unspaceHead :: String -> String
unspaceHead [] = []
unspaceHead l@(c:r) = case c of
  ' ' -> cont
  '\t' -> cont
  '\n' -> cont
  _ -> l
  where cont = unspaceHead r

data LineStart
  = OneLiner
  | Multiliner
  | MultiOneliner

diagnoseLine :: String -> (Maybe LineStart, String)

takeCard :: Lines -> (Maybe (Lines, CardFormat), Lines)
takeCard [] = (Nothing, [])
takeCard (l: rest) =
    case diagnoseLine l of
      (Nothing, _) -> first (first $ asHead l) (takeCard rest)
      (Just _, _) -> ([], rest)

splitCards :: Lines -> [RawCard]

asDeck :: Lines -> Deck
asDeck lines = rec [] lines =>> justList
  where
    rec deck [] = deck
    rec deck ls@(l: rest) =
        case diagnoseLine l of
          (Just OneLiner, ls) -> 
    
    case l of
      ',': ',': trail -> rec (oneliner trail: deck) rest
      '=': _ -> rec (multiOneliner (take 3 ls) : deck) (drop 2 rest)
      '*': _ -> let
        (maybeCard, remains) = multiliner ls
        in
        rec (maybeCard: deck) remains

oneliner :: String -> Maybe Card
oneliner line =
  case splitOn ",," line of
    one:two:rest ->
      Just (unspace one, unspace two, unspace $ glueWith ' ' rest)
    _ -> Nothing

multiOneliner :: Lines -> Maybe Card
multiOneliner (xl: yl: []) = Just (unspace xl, unspace yl, "")
multiOneliner (xl: yl: zl) = case zl of
    NotCardStart -> Just (unspace xl, unspace yl, unspace zl)
multiliner :: Lines -> (Maybe Card, Lines)

  

asDeck :: String -> Deck
asDeck
    =  linesOf
    ./ tidy
    ./ splitOn [cardSepF]
    ./ fmap (splitOn [sideSepF] ./ fmap fromLines' ./ maybeCard)
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
    ./ joinWith cardSepF
  where
    fromCard (x, y, Nothing) = joinWith sideSepF [x, y]
    fromCard (x, y, Just z) = joinWith sideSepF [x, y, z]

_N = 10
cardSep = "\n" <> replicate _N '=' <> "\n"
postCardSep = "\n" <> replicate _N '-'

showFirst :: String -> String
showFirst c = cardSep <> c
showSecond :: String -> Maybe String -> String
showSecond c mc2 =
  maybe c (\c2 -> fromLines' [c,c2]) mc2
  <> postCardSep
