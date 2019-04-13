module Data.Refined ( module Data.Refined.Error
                    , module Data.Refined.Internal
                    , module Data.Refined.Predicate
                    , Refined(..)
                    , refine
                    , unsafeRefine
                    , unrefine
                    ) where

import Data.Refined.Error (RefinedError(..))
import Data.Refined.Internal (These(..), fromInt) 
import Data.Refined.Predicate (class Predicate, And, EqualTo, From, FromTo,
GreaterThan, IdPred, LessThan, Negative, NonEmpty, NonNegative, NonZero, Not,
  NotEqualTo, Or, Positive, SizeEqualTo, SizeGreaterThan, SizeLessThan, To,
  ZeroToOne, validate)

import Prelude
import Data.Either (Either)
import Data.Typelevel.Undefined (undefined)
import Data.Bifunctor (lmap)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)

newtype Refined p x
  = Refined x

derive newtype instance eqRefined 
  :: (Eq x) => Eq (Refined p x)
derive newtype instance showRefined 
  :: (Show x) => Show (Refined p x)
derive instance genericRefined 
  :: Generic (Refined p x) _

-- for decoding we first decode the thing inside, then run our predicate on it
instance decodeJsonRefined 
  :: (DecodeJson x, Show x, Predicate p x) 
  => DecodeJson (Refined p x) where
  decodeJson a = do
     val <- decodeJson a
     (refineStr val :: Either String (Refined p x))

-- for encoding we just want to strip away the outside layer and use whatever
-- is inside
derive newtype instance encodeJsonRefined 
  :: (EncodeJson x) => EncodeJson (Refined p x)

-- used by decode json for it's errors
refineStr 
  :: forall p x. (Predicate p x)
  => (Show x) 
  => x 
  -> Either String (Refined p x)
refineStr x = lmap show (refine x)

-- the regular way in which one would turn a value into a Refined value
refine 
  :: forall p x. (Predicate p x) 
  => x 
  -> Either (RefinedError x) (Refined p x)
refine x = do
  Refined <$> validate (undefined :: p) x

-- the Bad Boy way to make a Refined (for testing etc)
unsafeRefine 
  :: forall p x. (Predicate p x) 
   => x 
   -> (Refined p x)
unsafeRefine x = Refined x

-- Let's get that value out and use it for something useful
unrefine 
  :: forall p x. Refined p x 
   -> x
unrefine (Refined x) = x

