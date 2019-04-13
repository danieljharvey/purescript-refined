module Data.Refined.Predicate where

import Data.Refined.Error (RefinedError(..))
import Data.Refined.Internal (These(..), fromInt) 

import Prelude
import Data.Either (Either(..))
import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Num.Reps (D0, D1)
import Data.Typelevel.Undefined (undefined)
import Data.Foldable (class Foldable, length)
import Data.Tuple (Tuple(..))

class Predicate p x where
  validate :: p -> x -> Either (RefinedError x) x

-- | A 'Predicate' for which everything is valid
data IdPred

instance predicateIdPred 
  :: Predicate IdPred x
  where
    validate _ x = Right x

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

-- | A 'Predicate' ensuring that the opposite of another Predicate passes
data Not a

instance predicateNot 
  :: (Predicate a x) 
  => Predicate (Not a) x where
  validate _ x
    = case validate (undefined :: a) x of
        Left _ -> Right x
        _      -> Left NotError

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
