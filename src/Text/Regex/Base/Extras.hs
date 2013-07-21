module Text.Regex.Base.Extras
    ( matchAllTextOnly ) where

import Data.Array (Array, elems)
import Text.Regex.Base.RegexLike (MatchText, makeRegex, matchAllText)

matchAllTextOnly regex = concatMap (map fst . elems) . matchAllText regex
