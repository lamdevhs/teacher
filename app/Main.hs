module Main where

import Teacher (input, teaching)
import Teacher.Parser
import Teacher.Types
import System.Environment (getArgs)
import Control.Monad ((>=>))

main :: IO ()
main = getArgs >>= input >>= either error teaching



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
