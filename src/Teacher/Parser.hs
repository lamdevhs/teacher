module Teacher.Parser where

import Teacher.Types
-- not base
import Data.List.Split (splitOn)

readingDeck :: FilePath -> IO Deck
readingDeck path = readFile path =>> asDeck

cardSepF = "???"
sideSepF = "%%%"

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

showFirst :: Content -> String
showFirst c = cardSep <> c
showSecond :: Content -> Maybe Content -> String
showSecond c mc2 =
  maybe c (\c2 -> fromLines' [c,c2]) mc2
  <> postCardSep
