{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib (indentR)

main :: IO ()
main = print [indentR|
    {
      test
        two
          three
    }|]
