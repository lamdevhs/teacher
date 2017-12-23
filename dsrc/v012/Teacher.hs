{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
-- module Teacher (teaching, input) where
module Teacher where

-- base
import Control.Arrow ( (|||) , (&&&) )
import qualified Control.Arrow as Arr
import qualified Data.Tuple as Tuple (swap)

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader


import System.Random

--to delete

import Teacher.Types
import Teacher.Parser (readDeck)


-- main = getArgs >>= input >>= either error teaching

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

{-

config.lessonFocus :: Card -> (Question = Face, [Faces])
askQuestion :: Question -> IO ()
giveAnswer :: [Faces] -> IO ()

data LessonConfig = LessonConfig {
  verbosity = terse | verbose
  endOfLesson = restart = id | terminate = const quit
  order = random | asGiven
  lessonFocus

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

caseNothing :: Maybe b -> b -> b
caseNothing (Just x) _ = x
caseNothing Nothing  x = x

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
