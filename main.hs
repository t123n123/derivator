import Data.Text (pack, replace, unpack)

data Function a
  = Polynomial [a]
  | Exponential a
  | Constant a
  | Sum (Function a) (Function a)
  | Product (Function a) (Function a)
  | Chain (Function a) (Function a)
  | ConstantVar String

class Derivable a where
  derivative :: a -> a

substitute :: String -> String -> String
substitute string substring = unpack $ replace (pack "x") (pack substring) (pack string)

isZero :: (Num a, Eq a) => Function a -> Bool
isZero (Constant a) = a == 0
isZero (Sum f1 f2) = (isZero f1) && (isZero f2)
isZero (Product f1 f2) = (isZero f1) || (isZero f2)
isZero (Chain f1 f2) = isZero f1
isZero (Polynomial cs) = null cs || (length cs == 1 && cs == [0])
isZero _ = False

isOne :: (Num a, Eq a) => Function a -> Bool
isOne (Constant a) = a == 1
isOne (Product f1 f2) = (isOne f1) && (isOne f2)
isOne (Chain f1 f2) = isOne f1
isOne (Exponential c) = c == 1
isOne (Polynomial cs) = (length cs == 1 && cs == [1])
isOne _ = False

instance (Eq a, Floating a, Enum a, Show a) => Show (Function a) where
  show f@(Polynomial cs) = if isZero f then "" else if isOne f then "1" else foldl (\x y -> if null x then y else if null y then x else x ++ " + " ++ y) "" (zipWith (\x y -> if y == 0.0 then "" else show y ++ " * x^" ++ show x) [0 ..] cs)
  show f@(Exponential c) = if isZero f then "" else show c ++ "^{x}"
  show f@(Constant a) = if isZero f then "" else show a
  show f@(Sum f1 f2) = if isZero f then "" else if isZero f1 then show f2 else if isZero f2 then show f1 else "(" ++ show f1 ++ ") + (" ++ show f2 ++ ")"
  show f@(Product f1 f2) = if isZero f then "" else if isOne f1 then show f2 else if isOne f2 then show f1 else "(" ++ show f1 ++ ") * (" ++ show f2 ++ ")"
  show f@(Chain f1 f2) = if isZero f then "" else substitute (show f1) ("(" ++ show f2 ++ ")")
  show f@(ConstantVar s) = if isZero f then "" else s

instance (Floating a, Enum a) => Derivable (Function a) where
  derivative (Polynomial cs) = if (null (tail cs)) then Constant 0 else Polynomial (zipWith (*) [1 ..] (tail cs))
  derivative (Exponential cs) = Product (Constant (log cs)) (Exponential cs)
  derivative (Constant cs) = Constant 0
  derivative (Sum f1 f2) = Sum (derivative f1) (derivative f2)
  derivative (Product f1 f2) = Sum (Product (derivative f1) f2) (Product f1 (derivative f2))
  derivative (Chain f1 f2) = Product (Chain (derivative f1) f2) (derivative f2)
  derivative (ConstantVar var) = Constant 0
