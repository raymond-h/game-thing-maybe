module Main where

import Lib

import Control.Monad (void)
import Configuration.Dotenv
import Configuration.Dotenv.Types

main :: IO ()
main = do
  -- we don't care if the file is missing
  (void $ loadFile defaultConfig) `onMissingFile` return ()
  runApp
