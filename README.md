# Purescript Refined

A Purescript port of [Refined](http://hackage.haskell.org/package/refined).

Allows one to create types that restrain the values inside, and automatically generate validators for them.

For instance:

```haskell
type DiceRoll = Refined (FromTo D1 D6) Int

invalidDiceRoll :: Either RefinedError DiceRoll
invalidDiceRoll = refine 8
-- invalidDiceRoll = Left (FromToError 1 6 8)

validDiceRoll :: Either RefinedError DiceRoll
validDiceRoll = refine 5
-- validDiceRoll = Right DiceRoll

diceRollToInt :: DiceRoll -> Int
diceRollToInt = unrefine
```

This package also contains `EncodeJson` and `DecodeJson` instances for Argonaut so that JSON data can be validated.

Documentation can be found on [Pursuit](https://pursuit.purescript.org/packages/purescript-refined/0.1.2)
