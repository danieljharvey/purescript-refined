module Data.Refined.Error where

import Data.Refined.Internal (These(..))

import Prelude

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

