module Control.Monad.Extras
    ( mapMaybeM ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

mapMaybeM :: (Functor m, Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs
