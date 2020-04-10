{-# LANGUAGE RecordWildCards #-}

module Lib where

import Text.RawString.QQ (r)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Q, Exp)
import Debug.Trace

indentR :: QuasiQuoter
indentR = QuasiQuoter
  { quoteExp = withoutIndent
  , quotePat = quotePat
  , quoteType = quoteType
  , quoteDec = quoteDec
  }
  where
    QuasiQuoter{..} = r

    withoutIndent :: String -> Q Exp
    withoutIndent string =
      let
        lines' = lines string
        fullIndents = linesIndents $ filter (not . null) lines'
        commonIndent = minimum fullIndents

        in quoteExp $ if null lines' then "" else (unlines . reverse . (dropWhile null) . reverse . map (dropIndent commonIndent)) lines'

lineIndent :: String -> Int
lineIndent line = foldr
  (\char acc -> case char of
    '\t' -> acc + tabSize
    ' ' -> acc + 1
    _ -> acc
  )
  0
  (takeWhile isWhiteSpace line)

linesIndents :: [String] -> [Int]
linesIndents lines' = map lineIndent $ dropWhile null lines'

isWhiteSpace char = char == '\t' || char == ' '
tabSize = 2

dropIndent :: Int -> String -> String
dropIndent 0 line = line
dropIndent _ "" = ""
dropIndent commonIndent [x] = if isWhiteSpace x then "" else [x]
dropIndent commonIndent line@(x:xs)
  | x == '\t' = dropIndent (commonIndent - tabSize) xs
  | x == ' ' = dropIndent (commonIndent - 1) xs
  | otherwise = line
