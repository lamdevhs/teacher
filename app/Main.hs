module Main where

import Teacher
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= input >>= either error teaching
