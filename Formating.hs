module Formating where

import Data.Text (pack, replace, unpack)
import FunctionTypes

replaceString :: String -> String -> String
replaceString string substring = unpack $ replace (pack "x") (pack substring) (pack string)

parens str = "(" ++ str ++ ")"

braces str = "{" ++ str ++ "}"

showPolynomialTerm coef power
  | coef == 0 = ""
  | power == 0 = show coef
  | coef == 1 = if power == 1 then "x" else "x^" ++ braces (show power)
  | power == 1 = show coef ++ "* x"
  | otherwise = show coef ++ "* x^" ++ braces (show power)

instance (Num a, Eq a, Show a) => Show (Function a) where
  show (Polynomial cs) = foldl1 (\x y -> x ++ "+" ++ y) $ filter (not . null) $ zipWith (\x y -> if y == 0 then "" else show y ++ " * x^" ++ show x) [0 ..] cs
  show (Exponential c) = show c ++ "^x"
  show (Constant a) = show a
  show (Sum fs) = foldl1 (\x y -> x ++ "+" ++ y) (map show fs)
  show (Product fs) = foldl1 (\x y -> x ++ "*" ++ y) (map show fs)
  show (Chain f1 f2) = replaceString (show f1) (show f2)
  show (ConstantVar s) = s

toLatex :: (Num a, Eq a, Show a) => Function a -> String
toLatex (Polynomial cs) = foldl1 (\x y -> x ++ "+" ++ y) $ filter (not . null) $ zipWith showPolynomialTerm cs [0 ..]
toLatex (Exponential c) = show c ++ "^{x}"
toLatex (Constant a) = show a
toLatex (Sum fs) = foldl1 (\x y -> x ++ "+" ++ y) (map toLatex fs)
toLatex (Product fs) = foldl1 (\x y -> x ++ "*" ++ y) (map toLatexTerm fs)
toLatex (Chain f1 f2) = replaceString (toLatex f1) (toLatex f2)
toLatex (ConstantVar s) = s

toLatexTerm (Polynomial cs) = if length cs > 2 then parens $ toLatex (Polynomial cs) else toLatex (Polynomial cs)
toLatexTerm (Sum fs) = if length fs > 2 then parens $ toLatex (Sum fs) else toLatex (Sum fs)
toLatexTerm (Chain f1 f2) = replaceString (toLatexTerm f1) (toLatex f2)
toLatexTerm x = toLatex x

toLatexDoc function = "\\documentclass{article} \n \\begin{document} \n \\thispagestyle{empty} \n $" ++ function ++ "$ \n \\end{document} \n"
