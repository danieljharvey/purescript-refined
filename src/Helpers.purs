module Data.Refined.Internal where
 
import Prelude
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))

fromInt :: forall a. (Ring a) => Int -> a
fromInt i
  = a
  where
    (Additive a) = power (Additive one) i

-- | Helper version of function because i am too lazy to find the package
data These a b
  = This a
  | That b
  | These a b

derive instance eqThese 
  :: (Eq a, Eq b) 
  => Eq (These a b)
derive instance ordThese 
  :: (Ord a, Ord b) 
  => Ord (These a b)

