module Data.Refined where

import Prelude

import Data.Either (Either(..))

import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Num.Reps (D0, D1)
import Data.Typelevel.Undefined (undefined)
import Data.Foldable (class Foldable, length)
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple(..))
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))

data RefinedError a
  = AndError (These (RefinedError a) (RefinedError a))
  | OrError (RefinedError a) (RefinedError a)
  | SizeEqualToError Int a
  | SizeGreaterThanError Int a
  | SizeLessThanError Int a
  | NotError
  | LessThanError Int a
  | GreaterThanError Int a
  | FromError Int a
  | ToError Int a
  | FromToError Int Int a
  | EqualToError Int a
  | NotEqualToError Int a

derive instance eqRefinedError 
  :: (Eq a) => Eq (RefinedError a)

instance showRefinedError :: (Show a) => Show (RefinedError a) where
  show (AndError (These a b)) 
    = "And (" <> show a <> " && " <> show b <> ")"
  show (AndError (This a))    
    = show a
  show (AndError (That b))    
    = show b
  show (OrError a b)          
    = "Or (" <> show a <> " && " <> show b <> ")"
  show (SizeEqualToError n a) 
    = show a <> "'s length should equal " <> show n <> " but does not."
  show (SizeGreaterThanError n a)
    = show a <> "'s length should be greater than " <> show n <> " but is not."
  show (SizeLessThanError n a)
    = show a <> "'es length should be less than " <> show n <> " but is not."
  show (NotError)
    = "RefinedError with Not"
  show (LessThanError n a)
     = show a <> " should be less than " <> show n <> " but is not."
  show (GreaterThanError n a)
     = show a <> " should be greater than " <> show n <> " but is not."
  show (FromError n a)
     = show a <> " should be equal to or greater than " <> show n <> " but is not."
  show (ToError n a)
     = show a <> " should be equal to or less than " <> show n <> " but is not."
  show (FromToError m n a)
     = show a <> " should be equal to or greater than " <> show m <> " and equal to or less than " <> show n <> " but is not."
  show (EqualToError n a)
     = show a <> " should be equal to " <> show n <> " but is not."
  show (NotEqualToError n a)
     = show a <> " should not be equal to " <> show n <> " but it is."

fromInt :: forall a. (Ring a) => Int -> a
fromInt i
  = a
  where
    (Additive a) = power (Additive one) i

class Predicate p x where
  validate :: p -> x -> Either (RefinedError x) x

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

-- helper because i am too lazy to find the package
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

--- the useful things

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

-- let's get that value out
unrefine 
  :: forall p x. Refined p x 
   -> x
unrefine (Refined x) = x

---

-- | A 'Predicate' for which everything is valid
data IdPred

instance predicateIdPred 
  :: Predicate IdPred x
  where
    validate _ x = Right x

---

-- | A 'Predicate' ensuring that both given Predicates are valid
data And l r

instance predicateAnd 
  :: (Predicate l x, Predicate r x) 
  => Predicate (And l r) x where
    validate _ x
      = case Tuple first second of
          (Tuple (Left a) (Left b))   -> Left (AndError (These a b))
          (Tuple (Left a) (Right _))  -> Left (AndError (This a))
          (Tuple (Right _) (Left b))  -> Left (AndError (That b))
          (Tuple (Right _) (Right _)) -> Right x
      where
        first
          = validate (undefined :: l) x
        second
          = validate (undefined :: r) x

---

-- | A 'Predicate' ensuring that one of two given Predicates is valid 
data Or l r

instance predicateOr 
  :: (Predicate l x, Predicate r x) 
  => Predicate (Or l r) x where
    validate _ x
      = case Tuple first second of
          (Tuple (Left a) (Left b))   -> Left (OrError a b)
          _                           -> Right x
      where
        first
          = validate (undefined :: l) x
        second
          = validate (undefined :: r) x

---

-- | A 'Predicate' ensuring that the length of a foldable is equal to a given int
data SizeEqualTo a

instance predicateSizeEqualTo 
  :: (Predicate a x, Foldable t, Nat n) 
  => Predicate (SizeEqualTo n) (t a) where
    validate _ x
      = case length x == val of
          true  -> Right x
          false -> Left (SizeEqualToError val x)
      where
        val :: Int
        val = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the length of a foldable is greater than a given int
data SizeGreaterThan a

instance predicateSizeGreaterThan 
  :: (Predicate a x, Foldable t, Nat n) 
  => Predicate (SizeGreaterThan n) (t a) where
    validate _ x
      = case length x > val of
          true  -> Right x
          false -> Left (SizeGreaterThanError val x)
      where
        val 
          = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the length of a foldable is less than a given int
data SizeLessThan a

instance predicateSizeLessThan 
  :: (Predicate a x, Foldable t, Nat n) 
  => Predicate (SizeLessThan n) (t a) where
    validate _ x
      = case length x < val of
          true  -> Right x
          false -> Left (SizeLessThanError val x)
      where
        val 
          = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the opposite of another Predicate passes
data Not a

instance predicateNot 
  :: (Predicate a x) 
  => Predicate (Not a) x where
  validate _ x
    = case validate (undefined :: a) x of
        Left _ -> Right x
        _      -> Left NotError

---

-- | A 'Predicate' ensuring that the value is less than a given int
data LessThan n

instance predicateLessThan 
  :: (Nat n, Ord x, Ring x) 
  => Predicate (LessThan n) x where
  validate _ x
    = case x < (fromInt val) of
        true  -> Right x
        false -> Left (LessThanError val x)
    where
      val 
        = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the value is greater than a given int
data GreaterThan n

instance predicateGreaterThan 
  :: (Nat n, Ord x, Ring x) 
  => Predicate (GreaterThan n) x
  where
    validate _ x
      = case x > (fromInt val) of
          true -> Right x
          false -> Left (GreaterThanError val x)
      where
        val 
          = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the value is equal to or greater than a given int
data From n

instance predicateFrom 
  :: (Nat n, Ord x, Ring x) 
  => Predicate (From n) x where
  validate _ x
    = case x >= (fromInt val) of
        true  -> Right x
        false -> Left (FromError val x)
    where
      val 
        = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the value is up and including a given int
data To n

instance predicateTo 
  :: (Nat n, Ord x, Ring x) 
  => Predicate (To n) x where
  validate _ x
    = case x <= (fromInt val) of
        true  -> Right x
        false -> Left (ToError val x)
    where
      val 
        = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the value is between two given ints (inclusive)
data FromTo m n

instance predicateFromTo 
  :: (Nat n, Nat m, Ord x, Ring x) 
  => Predicate (FromTo m n) x where
  validate _ x
    = case (x >= fromInt lower) && (x <= fromInt upper) of
        true  -> Right x
        false -> Left (FromToError lower upper x)
    where
      lower 
        = toInt (undefined :: m)
      upper
        = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the value is equal to a given int
data EqualTo n

instance predicateEqualTo 
  :: (Nat n, Eq x, Ring x) 
  => Predicate (EqualTo n) x where
  validate _ x
    = case x == (fromInt val) of
        true -> Right x
        false -> Left (EqualToError val x)
    where
      val 
        = toInt (undefined :: n)

---

-- | A 'Predicate' ensuring that the value is not equal to a given int
data NotEqualTo n

instance predicateNotEqualTo 
  :: (Nat n, Eq x, Ring x) 
  => Predicate (NotEqualTo n) x where
  validate _ x
    = case x /= (fromInt val) of
        true -> Right x
        false -> Left (NotEqualToError val x)
    where
      val 
        = toInt (undefined :: n)

-- | A 'Predicate' ensuring that the value is greater than zero.
type Positive = GreaterThan D0

-- | A 'Predicate' ensuring that the value is less than or equal to zero.
type NonPositive = To D0

-- | A 'Predicate' ensuring that the value is less than zero.
type Negative = LessThan D0

-- | A 'Predicate' ensuring that the value is greater than or equal to zero.
type NonNegative = From D0

-- | An inclusive range of values from zero to one.
type ZeroToOne = FromTo D0 D1

-- | A 'Predicate' ensuring that the value is not equal to zero.
type NonZero = NotEqualTo D0

-- | A 'Predicate' ensuring that the 'Foldable' is non-empty.
type NonEmpty = SizeGreaterThan D0
