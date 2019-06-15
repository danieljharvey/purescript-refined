module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class
import Data.Array
import Data.Either
import Data.Maybe
import Control.Monad.Free (Free)
import Control.Monad.State
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Typelevel.Num.Reps (D1, D9)
import Data.Typelevel.Undefined (undefined)
import Data.Traversable
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Core (fromNumber, fromString, toNumber)

import Data.Refined

type TestDecodeShape
  = { idPred   :: Refined IdPred Number
    , from    :: Refined (From D9) Number
    }

decodeTestShape :: String -> Either String TestDecodeShape
decodeTestShape = decodeJson <<< fromString

main :: Effect Unit
main = runTest tests

tests :: Free TestF Unit
tests = do
  suite "Refined" do
    test "Fail to validate a number that is over LessThan" do
      let ans = validate (undefined :: (LessThan D9)) 10.0 
      Assert.equal ans (Left (LessThanError 9 10.0))

    test "Successfully validate number that is LessThan" do
      let ans = validate (undefined :: (LessThan D9)) 8.0
      Assert.equal ans (Right 8.0)

    test "Fail to validate a number that is under GreaterThan" do
      let ans = validate (undefined :: (GreaterThan D9)) 8.0 
      Assert.equal ans (Left (GreaterThanError 9 8.0))

    test "Successfully validate number that over GreaterThan" do
      let ans = validate (undefined :: (GreaterThan D9)) 100.0
      Assert.equal ans (Right 100.0)

    test "Fails From" do
      let ans = validate (undefined :: (From D9)) 8.9 
      Assert.equal ans (Left (FromError 9 8.9))

    test "Succeeds From" do
      let ans = validate (undefined :: (From D9)) 9.0
      Assert.equal ans (Right 9.0)

    test "Fails To" do
      let ans = validate (undefined :: (To D9)) 9.1 
      Assert.equal ans (Left (ToError 9 9.1))

    test "Succeeds To" do
      let ans = validate (undefined :: (To D9)) 8.9
      Assert.equal ans (Right 8.9)

    test "Validates positive" do
      let ans = validate (undefined :: Positive) 10.0
      Assert.equal ans (Right 10.0)

    test "Validates not positive" do
      let ans = validate (undefined :: Positive) (-10.0)
      Assert.equal ans (Left (GreaterThanError 0 (-10.0)))

    test "And rejects correctly" do
      let ans = validate (undefined :: And (From D1) (To D9)) 11.0
      Assert.equal ans (Left (AndError (That (ToError 9 11.0))))

    test "And works correctly" do
      let ans = validate (undefined :: And (From D1) (To D9)) 8.0
      Assert.equal ans (Right 8.0)  

    test "Or rejects correctly" do
      let ans = validate (undefined :: Or (From D9) (To D1)) 8.0
      Assert.equal ans (Left (OrError (FromError 9 8.0) (ToError 1 8.0)))

    test "Or works correctly" do
      let ans = validate (undefined :: Or (From D9) (To D1)) 10.0
      Assert.equal ans (Right 10.0)   

    test "Not rejects correctly" do
      let ans = validate (undefined :: Not (From D9)) 10.0
      Assert.equal ans (Left NotError)

    test "Not works correctly" do
      let ans = validate (undefined :: Not (From D9)) 8.0
      Assert.equal ans (Right 8.0) 

    test "Equal rejects correctly" do
      let ans = validate (undefined :: EqualTo D9) 8.0
      Assert.equal ans (Left (EqualToError 9 8.0))

    test "Equal works correctly" do
      let ans = validate (undefined :: EqualTo D9) 9.0
      Assert.equal ans (Right 9.0)

    test "Whole thing works" do
      let ans = (refine 8.0 :: Either (RefinedError Number) (Refined Positive Number))
      Assert.equal ans (Right (Refined 8.0))

    test "We can get the value back out..." do
      let ans = (refine 8.0 :: Either (RefinedError Number) (Refined Positive Number))
      Assert.equal (unrefine <$> ans) (Right 8.0) 

  suite "Json decoding refined" do
     test "Decoding anything works" do
       let ans = (decodeJson (fromString "hello") :: Either String String)
       Assert.equal (ans) (Right "hello")

     test "Works with IdPred" do
       let ans = (decodeJson (fromNumber 8.0) :: Either String (Refined IdPred Number))
       Assert.equal (Right 8.0) (unrefine <$> ans)

     test "From D9 fails" do
       let ans = (decodeJson (fromNumber 8.0) :: Either String (Refined (From D9) Number))
       Assert.assert "Should fail the predicate" (isLeft ans)    

     test "From D9 succeeds" do
       let ans = (decodeJson (fromNumber 10.0) :: Either String (Refined (From D9) Number))
       Assert.equal (Right 10.0) (unrefine <$> ans)

  suite "Json encoding Refined" do
     test "Encoding a value is fine" do
        let val = (unsafeRefine 8.0 :: Refined Positive Number)
        let encoded = encodeJson val
        Assert.assert "poo" (toNumber encoded == (Just 8.0 :: Maybe Number))

     test "Decoding several values is good" do
        let original = { idPred: (unsafeRefine 1.0  :: Refined IdPred Number)
                       , from : (unsafeRefine 10.0 :: Refined (From D9) Number)
                       }
        let decoded = (decodeJson (encodeJson original) :: Either String TestDecodeShape)
        Assert.equal decoded (Right original)

     test "Decoding broken values does not work" do
        let original = { idPred: (unsafeRefine 1.0  :: Refined IdPred Number)
                       , from : (unsafeRefine 1.0 :: Refined (From D9) Number)
                       }
        let decoded = (decodeJson (encodeJson original) :: Either String TestDecodeShape)
        Assert.assert "Should not be able to decode this as invalid" (isLeft decoded)

  suite "Generate Arbitrary instances" do
     test "Generate IdPred values that pass the refinement" do
        values <- liftEffect (randomSample (arbitrary :: Gen (Refined IdPred Number)))
        let check = \(val :: Refined IdPred Number) -> do
             let raw = unrefine val
                 newVal :: Either (RefinedError Number) (Refined IdPred Number)
                 newVal = refine raw
             Assert.assert "Has created valid data" 
              (isRight newVal)
        traverse_ check values

     test "Generate From values that pass the refinement" do
        values <- liftEffect (randomSample (arbitrary :: Gen (Refined (From D9) Int)))
        let check = \val -> do
             let raw = unrefine val
                 newVal :: Either (RefinedError Int) (Refined (From D9) Int)
                 newVal = refine raw
             Assert.assert "Has created valid data" 
              (isRight newVal)
        traverse_ check values
 
     test "Generate To values that pass the refinement" do
        values <- liftEffect (randomSample (arbitrary :: Gen (Refined (To D9) Int)))
        let check = \val -> do
             let raw = unrefine val
                 newVal :: Either (RefinedError Int) (Refined (To D9) Int)
                 newVal = refine raw
             Assert.assert "Has created valid data" 
              (isRight newVal)
        traverse_ check values
  
     test "Generate And values that pass the refinement" do
        values <- liftEffect (randomSample (arbitrary :: Gen (Refined (And (From D1) (To D9)) Int)))
        let check = \val -> do
             let raw = unrefine val
                 newVal :: Either (RefinedError Int) (Refined (And (From D1) (To D9)) Int)
                 newVal = refine raw
             Assert.assert "Has created valid data" 
              (isRight newVal)
        traverse_ check values

