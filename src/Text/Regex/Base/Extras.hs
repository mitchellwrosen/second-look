module Text.Regex.Base.Extras
    ( matchAllTextOnly ) where

import Data.Array (elems)
import Text.Regex.Base.RegexLike (matchAllText)

matchAllTextOnly regex = concatMap (map fst . elems) . matchAllText regex
