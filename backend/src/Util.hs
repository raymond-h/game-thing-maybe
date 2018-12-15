module Util where

import Data.Bifunctor
import qualified Data.Map.Strict as M

toMultiMap :: Ord f => [(f, e)] -> M.Map f [e]
toMultiMap = M.fromListWith (<>) . wrapSecond . reverse
  where
    wrapSecond :: [(f, e)] -> [(f, [e])]
    wrapSecond = map . second $ pure
