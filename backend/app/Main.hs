module Main where

import Lib

main :: IO ()
main = loadDotenvConfig >> runApp
