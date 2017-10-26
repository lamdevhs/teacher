module Main where

import Teacher (input, teaching)
import Teacher.Parser
import Teacher.Types
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import Control.Monad ((>=>))

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getArgs >>= input >>= either error teaching



-- tests, drafty area

testParser = mapM_ (readDeck >=> print) files
testTeacher = pureArgs >>= input >>= either error teaching


pureArgs :: IO [String]
pureArgs = pure
  ( ""
  -- : "--norand"
  -- : "--noloop"
  : "--flip"
  -- : "--verbose"
  : "--decks"
  -- : [])
  : files )

files = "test/deck1" : "test/deck2" : []
