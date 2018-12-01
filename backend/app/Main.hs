module Main where

import Lib

import Control.Monad (void)
import Configuration.Dotenv
import Configuration.Dotenv.Types

main :: IO ()
main = do
  void $ loadFile defaultConfig
  runApp
