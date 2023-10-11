module Derivator where

import FunctionTypes

class Derivable a where
  derivative :: a -> a

instance (Floating a, Enum a) => Derivable (Function a) where
  derivative (Polynomial cs) = if null (tail cs) then Constant 0 else Polynomial (zipWith (*) [1 ..] (tail cs))
  derivative (Exponential cs) = Product [Constant (log cs), Exponential cs]
  derivative (Constant cs) = Constant 0
  derivative (Sum fs) = Sum $ map derivative fs
  derivative (Product fs) = case length fs of
    0 -> Constant 0
    1 -> derivative $ head fs
    _ -> Sum (map (\i -> Product $ take (i - 1) fs ++ [derivative (fs !! (i - 1))] ++ drop i fs) [1 .. length fs])
  derivative (Chain f1 f2) = Product [Chain (derivative f1) f2, derivative f2]
  derivative (ConstantVar var) = Constant 0
