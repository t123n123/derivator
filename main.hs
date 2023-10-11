module Main where

import Derivator
import Formating
import FunctionTypes

square = Polynomial [0, 0, 1]

negative = Polynomial [0, -1]

negativeSquare = Polynomial [0, 0, -1]

identity = Polynomial [0, 1]

e = exp 1

f1 = Product [identity, Chain (Exponential e) (Sum [square, Chain negativeSquare (ConstantVar "y")])]

f2 = Product [Constant 0.05, Sum [square, Chain square (ConstantVar "y")]]

f = Sum [f1, f2]

main = do
  putStrLn $ toLatexDoc $ toLatex $ simplifyFunction $ derivative $ simplifyFunction $ derivative f
