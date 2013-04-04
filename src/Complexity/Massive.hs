{-# LANGUAGE FlexibleInstances #-}
module Complexity.Massive (
  module Complexity.Massive
) where

import Language.Python.Common.SrcLocation (SrcSpan)

data Mass
  = Func String [Mass]
  | Simple Float
  deriving (Eq, Show)

class Massive a where
  mass :: Float -> a -> [Mass]

instance Massive a => Massive [a] where
  mass coef = concatMap (mass coef)

instance (Massive a, Massive b) => Massive (a, b) where
  mass coef (x1, x2) = concatMass2 coef x1 x2

instance Massive a => Massive (Maybe a) where
  mass coef Nothing = []
  mass coef (Just a) = mass coef a

concatMass2 :: (Massive a, Massive b) => Float -> a -> b -> [Mass]
concatMass2 coef x1 x2 = concat [mass coef x1, mass coef x2]

concatMass3 :: (Massive a, Massive b, Massive c) => Float -> a -> b -> c -> [Mass]
concatMass3 coef x1 x2 x3 = concat [mass coef x1, mass coef x2, mass coef x3]

concatMass4 :: (Massive a, Massive b, Massive c, Massive d) => Float -> a -> b -> c -> d -> [Mass]
concatMass4 coef x1 x2 x3 x4 = concat [mass coef x1, mass coef x2, mass coef x3, mass coef x4]
