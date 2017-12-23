{-# LANGUAGE CPP #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

-- module Teacher (teaching, input) where
module Teacher where

-- #define DRAFT

-- base
import Control.Arrow ( (|||) , (&&&), second)
import Control.Applicative ( (<|>) )
-- import qualified Control.Arrow as Arr
-- import qualified Data.Tuple as Tuple (swap)

import System.Random

import Teacher.Types
import Teacher.Parser (readDeck)

-- main = getArgs >>= input >>= either error teach


-- STATE 
data LessonState = LessonState {
  lessonContent :: Deck,
  piles :: (Deck {-remainder-}, Deck {-mistakes-}),
  mistakesNb :: Int,
  total :: Int,
  skippedNb :: Int,
  _inTheEnd :: T Forever OnOff,
  _nextCard :: T RandomPick OnOff,
  _verbosity :: T Verbosity OnOff,
  _processed :: T QuestionIx Int
}

data T tag content = T { t :: content } deriving Show
data OnOff = On | Off deriving Show
data Forever
data RandomPick
data Verbosity
data QuestionIx

instance Functor (T tag) where
  fmap f (T a) = T (f a)

toggle :: T g OnOff -> T g OnOff
toggle = fmap switch

switch :: OnOff -> OnOff
switch On = Off
switch Off = On

taggedOption :: (state -> T g a) -> (a -> r) -> state -> r
taggedOption getter behavior state =
  t (getter state) & behavior


inTheEnd = taggedOption _inTheEnd forever
nextCard = taggedOption _nextCard randomPick
verbosity = taggedOption _verbosity verbose
processed = taggedOption _processed card2QandA

-- LessonState.inTheEnd
forever :: OnOff -> IO () -> IO ()
forever On = id
forever Off = always quit

quit = putStrLn "bye!"

-- LessonState.nextCard
randomPick :: OnOff -> Deck -> IO (Card, Deck)
randomPick On  = randPick
randomPick Off = pure . pop

randPick :: [a] -> IO (a, [a])
randPick [] = error "empty list"
randPick xs = fmap (randomR range .| fst .| flip popAt xs) newStdGen
  where
    range = (0, length xs - 1)

pop :: [a] -> (a, [a])
pop [] = error "empty list"
pop (x:xs) = (x, xs)

randPop :: [a] -> (a, [a])
randPop = undefined

--LessonState.verbosity
verbose :: OnOff -> IO () -> IO ()
verbose On  = id
verbose Off = always pass

pass :: Applicative f => f ()
pass = pure ()

always :: a -> b -> a
always x _ = x

card2QandA :: Int -> Card -> ([String], [String])
card2QandA ix card = popAt ix card & second concat

popAt :: Int -> [a] -> (a, [a])
popAt ix xs
    | length xs <= ix
      = error "index out of range"
    | otherwise
      = let (bef, rest) = takeAndRest ix xs
        in  (head rest, bef ++ tail rest)


tellStats :: LessonState -> IO ()
tellStats state = putStrLn
    ("mistakes: "<> show m <> "/" <> show t <> ", skipped: " <> show s)
  where
    m = mistakesNb state
    t = total state
    s = skippedNb state

printProgress :: LessonState -> IO ()
printProgress state = putStrLn
    ("[" <> replicate done '+'
         <> replicate remaining '-'
         <> "] "
         <> show done <> "/" <> show lessonLen)
  where
    lessonLen = length (lessonContent state)
    remaining = length (piles state & fst)
    done = lessonLen - remaining



start :: LessonState -> IO ()
start s = teach state
  where state = s { piles = (lessonContent s, []) }

teach :: LessonState -> IO ()
teach s{-state-} = case piles s of
    ([], []) -> inTheEnd s (start s)
    ([], mistakes) -> do
      announceMistakes
      let s' = s { piles = (mistakes, []) }
      teach s'
    (nexts, mistakes) -> do
      (card, rest) <- nextCard s nexts
      let
        (question, answer) = processed s card
      --printProgress s
      verbosity s (tellStats s)
      printWithMargin question
      listen Question s $ \s'{-newstate-} -> \case
        Skip ->
          let state = s' {
                piles = (rest, mistakes),
                skippedNb = skippedNb s' + 1 }
          in  teach state
        Reveal -> do
          printWithMargin answer
          listen Answer s' $ \s''{-newstate-} -> \case
            Mistake ->
              let state = s'' {
                    piles = newPiles,
                    mistakesNb = mistakesNb s'' + 1 }
                  newPiles = (rest, card : mistakes)
              in teach state
            Next ->
              let state = s'' { piles = (rest, mistakes) }
              in  teach state

announceMistakes :: IO ()
announceMistakes = putStrLn "End of Main Pile. Now, Mistakes."

printWithMargin :: Lines -> IO ()
printWithMargin = fmap (withMargin ' ') .| fuse '\n' .| putStrLn

withMargin :: Char -> String -> String
withMargin c str = replicate marginSize c <> str

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound..maxBound]

marginSize = maximum lengths
  where
    lengths = length <$> (fmap show locals <> fmap show generals)
    locals = enumAll :: [LocalCmd]
    generals = enumAll :: [GeneralCmd]

data Step = Question | Answer deriving Show
data LocalCmd = Skip | Reveal | Mistake | Next deriving (Show, Enum, Bounded)




-- LISTENING
listen :: Step
       -> LessonState
       -> (LessonState -> LocalCmd -> IO ())
       -> IO ()
listen step s{-state-} cont = do
    char <- getChar
    let result = parseCmd char
    feedback result
    result & maybe whenInvalid whenValid
  where
    parseCmd char =
        (generalCmdParser char =>> Left) <|>
        (localCmdParser step char =>> Right)
    whenInvalid = loop s
    whenValid =
        generalAction s loop ||| cont s
    loop s = listen step s cont

feedback :: Maybe (Either GeneralCmd LocalCmd) -> IO ()
feedback = show_ .| asHead lineEraser .| putStrLn
  where
    show_ = maybe invalidCommand (either show show)
invalidCommand = "Invalid"
lineEraser = '\r'


data GeneralCmd = Quit | TellStats deriving (Show, Enum, Bounded)

localCmdParser :: Step -> Char -> Maybe LocalCmd
localCmdParser Question 'n' = Reveal & Just
localCmdParser Question 's' = Skip & Just
localCmdParser Answer   'm' = Mistake & Just
localCmdParser Answer   'n' = Next & Just
localCmdParser _         _  = Nothing

generalCmdParser :: Char -> Maybe GeneralCmd
generalCmdParser = \case
  'q' -> Just Quit
  '!' -> Just TellStats
  _   -> Nothing

generalAction :: LessonState -> (LessonState -> IO ()) -> GeneralCmd -> IO ()
generalAction s cont TellStats = tellStats s >> cont s
generalAction _ _ Quit = quit

caseNothing :: Maybe b -> b -> b
caseNothing (Just x) _ = x
caseNothing Nothing  x = x


class Operations v where
  add :: Int -> Int -> v
  mul :: Int -> Int -> v

instance Operations [Char] where
  add = showOp '+'
  mul = showOp 'x'

instance Operations Int where
  add = (+)
  mul = (*)

showOp :: Char -> Int -> Int -> String
showOp o i j = show i <> (' ' : o : ' ' : show j)

card :: [a -> [String]] -> a -> Card
card makers a = fmap ($ a) makers

deck :: [a -> [String]] -> [a] -> Deck
deck makers = fmap (card makers)

result :: Int -> Int
result = id
oper :: String -> String
oper = id

addDeck = deck [tupled add .| oper .| bot, tupled add .| result .| show .| bot] couples
  where
    swap (x, y) = (y, x)
    couples = cps <> fmap swap cps
    cps = (,) <$> [1..999] <*> [1..999]

dftState content = LessonState {
  lessonContent = content,
  piles = (content, []),
  mistakesNb = 0,
  total = 0,
  skippedNb = 0,
  _inTheEnd = T On,
  _nextCard = T On, -- !:!!!!!!! to change
  _verbosity = T Off,
  _processed = T 0
}



#ifdef DRAFT





shuffled' :: [Int] -> [a] -> [a]
shuffled' rs [] = []
shuffled' (r:rs) xs =  newHead : shuffled' rs (end <> beg)
    -- reversing end and beg avoids constantly ending
    -- the shuffling with (last xs)
  where
    (newHead, beg, end) = case takeAndRest (processed r) xs of
        (beg, []) -> (last beg, init beg, [])
        (beg, end) -> (head end, beg, tail end)
      -- ^ because takeAndRest could return (beg, [])
    processed n = mod n ln
    ln = length xs

shuffled :: StdGen -> [a] -> [a]
shuffled rg list = shuffled' (randoms rg :: [Int]) list

















 | ChangeOption ...




  ... change options










#endif

---------------





{-


-- ideas better hs syntax:
      

listen : Step -> state -> cont -> IO ()
   where state = LessonState a
         cont = state -> LocalCmd -> IO ()
* step state/s cont = do
    char <- getChar
    let result = parseCmd char
    feedback result
    result & maybe whenInvalid whenValid
  where
    parseCmd char = caseNothing ARG
      generalCmdParser char =>> Left
      localCmdParser step char =>> Right
    whenInvalid = loop s
    whenValid = ||| ARG
       generalAction s loop
       cont s
    loop s = listen localParser s cont

generalAction
  : state -> listenAgain -> IO ()
    state = LessonState
    listenAgain = state -> IO ()
* state/s cont TellScore = tellScore s >> cont s
* _       _    Quit = quit

-}


-- ############################# old stuff:

#ifdef OLDSTUFF

teach :: 

input :: [String] -> IO (Either String (Lesson, Config))
input args = maybeFiles & maybe
    (pure $ Left "Input error (no files given)")
    (\files -> do
      putStrLn $ show args
      mapM readDeck files >>= \decks ->
        let
          notShuffled gen lesson = lesson
          config = Config {
            _maybeRandom = ifHasOpt "norand" notShuffled shuffled, -- use a typeclass CheckBox {on, off}
            _maybeLoop = ifHasOpt "noloop" (constM ()) id,
            _maybeVerbose = ifHasOpt "verbose" id (constM ())
            }
        in
        pure $ Right (deckToLesson questionIx $ concat decks, config)
    )
  where
    opt = ("--" <>)
    maybeFiles :: Maybe [FilePath]
    maybeFiles = must (length .| diff 0) $ -- problem: if i use tail and there is no --decks in arg...
      tail $ trailWhen (=== opt "decks") args
    ifHasOpt key ifyes ifnot =
      if opt key `elem` args then ifyes else ifnot
    questionIx = ifHasOpt "qindex"
        (extractIndex args) -- then
        0 -- otherwise
      where
        extractIndex :: [String] -> Int
        extractIndex args = args & (trailWhen (=== opt "qindex")
              .| tail
              .| head
              .| read)


----
-- * deckToLessonContent
deckToLesson :: Int -> Deck -> Lesson
deckToLesson index =
  fmap (newHeadAt index .| cardToQandA)

newHeadAt :: Int -> [a] -> [a]
newHeadAt index list
  | ord index 0 || ord lastIndex index =
    list
  | otherwise =
      let (taken, rest) = takeAndRest index list
      in  (head rest) : (taken ++ tail rest)
  where lastIndex = length list - 1

cardToQandA :: Card -> QandA
cardToQandA (question:ans) = (question, concat ans)
-- cardToQandA (question:ans) = (question, "")


----
-- * Randomness

  
testA = do
  g <- newStdGen
  print $ shuffled g [1,2,3,4,5]

test = sequence_ (replicate 5 testA)

data R = R {
  _f :: forall m . Applicative m => m () -> m ()
}

bar :: (forall m . Applicative m => m () -> m ()) -> IO ()
bar f = f (putStrLn "bar")

foo :: R -> IO ()
foo r = bar $ _f r


teaching :: (Lesson, Config) -> IO ()
teaching (lesson, config) =
  runTLW walk lesson config

class (Monad m, MonadIO m) => LessonWalk m where
  doMaybeStartOver :: m ()
  doMaybeVerbose :: m ()
  mistakesAsNewDeck :: m ()
  mistakesNum :: m Int
  lessonLength :: m Int
  popNextQA :: m (Maybe QandA)
  sayStr :: String -> m ()
  sayScore :: m ()
  recordMistake :: QandA -> m ()
  recordNoMistakes :: m ()
  recordSkip :: m ()


instance MonadIO m => LessonWalk (TLessonWalk m) where
  doMaybeStartOver = (TLW $ lift $ fmap _maybeStartOver ask) >>= id
  doMaybeVerbose = (TLW $ lift $ fmap _maybeVerbose' ask)
    >>= \maybeSay -> maybeSay sayScore
  mistakesAsNewDeck = TLW $ do
    state <- get
    let mistakes = _mistakes state
    put $ state { _lesson = mistakes, _mistakes = [] }
  mistakesNum = TLW $ fmap (_mistakes .| length) get
  lessonLength = TLW $ fmap (_lesson .| length) get
  popNextQA = TLW $ do
    state <- get
    let lesson = _lesson state
    case _lesson state of
       [] -> pure Nothing
       next:rest -> do
         put $ state { _lesson = rest }
         pure (Just next)
  sayStr msg = TLW (liftIO $ putStrLn msg)
  sayScore = TLW (fmap _score get) >>=
    show .| sayStr
  recordMistake mistake = TLW $ do
    state <- get
    let mistakes = _mistakes state
    put $ state { _score = scoreMistake (_score state),
                  _mistakes = mistake : mistakes }
  recordNoMistakes = TLW $ do
    state <- get
    put $ state { _score = scoreNoMistakes (_score state) }
  recordSkip = TLW $ do
    state <- get
    put $ state { _score = scoreSkip (_score state) }

scoreTick
  (total, progress, mistakes, skipped) = 
  (total, progress + 1, mistakes, skipped)
scoreMistake
  (total, progress, mistakes, skipped) = 
  scoreTick (total, progress, mistakes + 1, skipped)
scoreSkip
  (total, progress, mistakes, skipped) = 
  scoreTick (total, progress, mistakes, skipped + 1)
scoreNoMistakes = scoreTick


newtype TLessonWalk monadIO a
 = TLW
     (StateT TLWState
       (ReaderT (TLWConfig monadIO)
         monadIO)
      a) deriving (Functor, Applicative, Monad)

data Config = Config {
  _maybeRandom :: forall a . StdGen -> [a] -> [a],
  _maybeLoop :: forall m . Applicative m => m () -> m (),
  _maybeVerbose :: forall m . Applicative m => m () -> m ()
}

data TLWConfig monadIO = TLWConfig {
  _maybeStartOver :: TLessonWalk monadIO (),
  _maybeVerbose' :: forall m . Applicative m => m () -> m ()
}



data TLWState = TLWState {
  _score :: Score,
  _mistakes :: Lesson,
  _lesson :: Lesson
}


runTLW
  :: (MonadIO m)
  => TLessonWalk m a
  -> Lesson
  -> Config
  -> m a
runTLW (TLW tlw) lesson config = do
  g <- liftIO $ newStdGen
  runReaderT (evalStateT tlw (tlwState $ _maybeRandom config g lesson)) tlwConfig
  where
    tlwConfig = TLWConfig {
      _maybeStartOver = _maybeLoop config $ liftIO $ teaching (lesson, config),
      _maybeVerbose' = _maybeVerbose config
    }

tlwState :: Lesson -> TLWState
tlwState lesson = TLWState {
  _score = (length lesson, 0, 0, 0),
  _lesson = lesson,
  _mistakes = []
}


instance MonadTrans TLessonWalk where
  lift = TLW . lift . lift

instance (MonadIO m) => MonadIO (TLessonWalk m) where
  liftIO = lift . liftIO
  
getNextQA :: LessonWalk m => m (Maybe QandA)
getNextQA = lessonLength >>= \case
  0 -> mistakesNum >>= \case
    0 -> pure Nothing
    _ -> do
      mistakesAsNewDeck
      popNextQA
  _ -> popNextQA



walk :: LessonWalk m => m ()
walk = withQA $ \qa -> do
  doMaybeVerbose
  sayQ qa
  listenAfter Question $ \case
    GiveAns -> do
      sayA qa
      listenAfter Answer $ \case
            Mistake -> recordMistake qa >> walk
            Next -> recordNoMistakes >> walk
    Skip -> recordSkip >> walk

withQA :: LessonWalk m => (QandA -> m ()) -> m ()
withQA cont = getNextQA >>= \case
  Nothing -> sayScore >> sayEnd >> doMaybeStartOver
  Just qa -> cont qa

listenAfter :: LessonWalk m => Step -> (CmdQandA -> m ()) -> m ()
listenAfter step cont =
    liftIO (putChar '?' >> getChar) >>= charToCmd step >>= \maybeCmd -> do
        sayCmd maybeCmd
        maybe loop (doGenCmd loop ||| cont) maybeCmd
      where loop = listenAfter step cont

doGenCmd :: LessonWalk m => m () -> CmdGen -> m ()
doGenCmd cont SayScore = sayScore >> cont
doGenCmd _ Quit = sayBye
-- doGenCmd _ QuitSave = saveProgress >> sayBye

charToCmd :: LessonWalk m => Step -> Char -> m (Maybe (Either CmdGen CmdQandA))
charToCmd step c = pure (parsed step c)
  where
    parsed _ 'q'        = Left Quit & Just
--    parsed _ 'z'        = Left QuitSave & Just
    parsed _ 'c'        = Left SayScore & Just
    parsed Question 'n' = Right GiveAns & Just
    parsed Question 's' = Right Skip & Just
    parsed Answer 'm'   = Right Mistake & Just
    parsed Answer 'n' = Right Next & Just
    parsed _ _ = Nothing
-- potential extension: more than two data per card
-- reusability <=> no arbitrary conceptual limitations
-- create interface that would allow creation of decks via hs functions
-- option when giving several files: even if random, do not mix decks together


sayCmd :: LessonWalk m => Maybe (Either CmdGen CmdQandA) -> m ()
sayCmd = maybe unknownCmd (either show show)
  -- .| ("--------------- " <>)
  .| asHead lineEraser
  .| sayStr

lineEraser = '\r'

sepBefQ =
   "\n-.-.-.-.-.-.-.-.-.-.-.-\n\n"

sayQ
  :: LessonWalk m => QandA -> m ()
sayQ = fst
   .| fmap mindMargin
   .| asHead sepBefQ
   .| fuse '\n'
   .| sayStr
sayA
  :: LessonWalk m => QandA -> m ()
sayA = snd .| fmap mindMargin .| fuse '\n'.| sayStr

{-
sayQorA :: LessonWalk m => Step -> QandA -> m ()
sayQorA step qa = liftIO $ do
    putStrLn $ replicate n '_' <> show step
    putStrLn which 
    putStrLn $ replicate n '-'
  where
    n = 10
    which = qa & case step of
      Question -> fst
      Answer -> snd
-}

sayEnd
  :: LessonWalk m => m ()
sayEnd = sayStr "End of Lesson\n@-@-@-@-@-@-@-@-@-@"

sayBye :: LessonWalk m => m ()
sayBye = sayStr "Bye!"

unknownCmd = "Unknown"

maxSizeCmdName :: Int
maxSizeCmdName = maximum [f allCmdGen, f allCmdQandA, length unknownCmd]
  where allCmdGen = [minBound..maxBound] :: [CmdGen]
        allCmdQandA = [minBound..maxBound] :: [CmdQandA]
        f :: Show a => [a] -> Int
        f = fmap (show .| length) .| maximum

mindMargin :: String -> String
mindMargin = (replicate maxSizeCmdName ' ' <>)


#endif
{-

config.lessonFocus :: Card -> (Question = Face, [Faces])
askQuestion :: Question -> IO ()
giveAnswer :: [Faces] -> IO ()

data LessonState a = LessonState {
  piles :: (Deck a{-remainder-}, Deck a {-mistakes-}),
  inTheEnd :: IO () -> IO (),
  nextCard :: Deck -> (Card, Deck),
  verbosity :: IO () -> IO (),
  score :: Score,
  processed :: Card a -> (a, [a]) {-question, answer-}
}


idea: order :: Deck -> (next, rest)

lesson =
  . randomization
  . occurenceOpt = occurence cfg (redo)
  . 

-> walkDeck could have a value that would represent the IO of redoing the deck/lesson,
   and from there, could use occurence cfg over it --would allow the option of
   modifying the option in the middle of a lesson... that could imply we wudn't need to put any options:
   all options could be changed from within the lesson...
   though to have a complete input is preferrable

startLesson =
  lesson config initScore (deck, mistakes) (deck,mistakes)

lesson :: Config -> Score -> (Deck, Mistakes) -> OriginalInput -> IO ()
lesson config score ([], []) originalInput = tellEndOfDeck >>
  maybeLoop config (lesson config score originalInput originalInput)
lesson config score 

lesson s@tate = case remains s of
    ([], []) -> maybeEnd...
    ([], mistakes) -> lesson $ s { remains = (mistakes, []) }
    (pile, mistakes) -> do
      let
        card : rest = nextCard s (pile s)
        (question, answer) = focus s (card)
      verbosity s (print $ score s)
      askQuestion question
      listen questionCmd s $ \newS@tate -> \case
          Cmd1 -> do
             giveAnswer answer
          Cmd2 -> do ...

listen :: (Char -> Maybe Command)
       -> (Command -> IO ())
       -> IO ()
listen localParser s@tate cont@inuation = do
  char <- getChar
  caseNothing (generalParser s rec char) $
    caseNothing (localParser char =>> cont s)
                (reportInvalid >> rec)
  where
    rec s = listen localParser s cont

questionCmd = fromList [('s', Skip), ('a', Answer)]

...

justified partialFunction = \input -> ioUnsafePerform $ catch
    (pure $ Just $ partialFunction input)
    (_ -> pure Nothing)
    

answerCmd :: Char -> Maybe Cmd
answerCmd = \case ->
    'm' -> Just Mistake
    _   -> Nothing

generalParser :: LessonState -> (LessonState -> IO ()) -> Char -> Maybe (IO ())
generalParser s@tate cont = \case
  's' -> Just (sayScore >> cont)
  'q' -> Just quitTeacher
  restartLesson@'r' -> Just (cont $ s { remains = fullLesson s })
  resetScore@'S' -> Just (cont $ s { score = initScore })
  _  -> Nothing

how to implement the toggling of options with one common general command if
all that is saved in the state are functions? maybe using enums wouldn't be
that big of a deal...

-> setup a help option to present all commands, their description, and when they are effective



§§§§§§§§§§§§§§§
force parser in accepting only decks with cards having always the same number of faces, a une face  pres (celle quii serait toujours visible dans la réponse)
use a zipper to go thru the deck?

"loopOpt, verboseOpt, randOpt" instead of maybeLoop...
or "mayLoop", "maySay", "mayShuffle"

generalParser :: Char -> Maybe (IO ())
localParser :: Char -> Cmd
cont :: Cmd -> IO ()


listen :: (Char -> Maybe Command)
       -> (Command -> IO ())
       -> IO ()
listen localParser cont = do
  c <- getChar
  caseNothing (generalParser c again) $
    caseNothing (localParser c =>> cont)
                (reportInvalid >> again)
  where
    again = listen localParser cont

questionCmd :: Char -> Maybe Cmd
questionCmd s = Just Skip
...

answerCmd :: Char -> Maybe Cmd
answerCmd 'm' = Just Mistake

generalParser :: Char -> IO () -> Maybe (IO ())
generalParser 't' cont = Just (sayScore >> cont)
generalParser 'q' _    = Just quitTeacher
generalParser _ cont = Nothing



walk
  :: (IO () -> IO ())
  -> IO ()
  -> Score
  -> WithMistakes Deck
  -> IO ()
walk maybeVerbose maybeLoop score ([],[]) = print score >> maybeLoop
walk maybeVerbose maybeLoop score ([],mistakes) = walk maybeVerbose maybeLoop score (mistakes, [])
walk maybeVerbose maybeLoop score (deck, mistakes) =
    let
      card : rest = deck
      (first, second, maybeN) = card
    in do
      maybeVerbose (print score)
      showFirst first & putStrLn
      listen questionCmd $ \case
        ShowAnswer -> do
          showSecond second maybeN & putStrLn
          listen answerCmd $ \case
            Mistake -> rec (newMistake score) (rest, card:mistakes)
            Next -> rec score (rest, mistakes)
        Skip -> rec score (rest, mistakes)
  where
    rec = walk maybeVerbose maybeLoop
    
    
-}


{-
charToCmd :: LessonWalk m => Step -> Char -> m ()
charToCmd _ 'q' = sayBye
charToCmd _ 'z' = saveProgress >> sayBye
charToCmd step 'c' = do
  sayScore
  listenAfter step
charToCmd Question 'n' = giveAnswer
charToCmd Question 's' = skipAnswer
charToCmd Answer 'm' = recordMistake
charToCmd Answer 'm' = nextQuestion
charToCmd step _ = do
  sayUnrecognized
  listenAfter step
-}
