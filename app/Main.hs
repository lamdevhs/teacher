module Main where

import Teacher
import System.Environment (getArgs)

main :: IO ()
-- main = getArgs >>= input >>= either error teaching
main = pureArgs >>= input >>= either error teaching

test = pureArgs >>= input >>= either error f
  where
    f (d, c) = pure c


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
