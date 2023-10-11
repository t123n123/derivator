module FunctionTypes where

data Function a
  = Polynomial [a]
  | Exponential a
  | Constant a
  | Sum [Function a]
  | Product [Function a]
  | Chain (Function a) (Function a)
  | ConstantVar String
  | Division (Function a) (Function a)

isZero :: (Num a, Eq a) => Function a -> Bool
isZero (Constant a) = a == 0
isZero (Sum fs) = all isZero fs
isZero (Product fs) = any isZero fs
isZero (Chain f1 f2) = isZero f1
isZero (Polynomial cs) = null cs || cs == [0]
isZero (Division f g) = isZero f
isZero _ = False

isOne :: (Num a, Eq a) => Function a -> Bool
isOne (Constant a) = a == 1
isOne (Product fs) = all isOne fs
isOne (Chain f1 f2) = isOne f1
isOne (Exponential c) = c == 1
isOne (Polynomial cs) = cs == [1]
isOne _ = False

isConstant (Constant a) = True
isConstant (Polynomial cs) = length cs < 2
isConstant (Sum fs) = all isConstant fs
isConstant (Product fs) = all isConstant fs
isConstant (Chain f g) = isConstant f || isConstant g
isConstant (ConstantVar var) = False

simplifyFunction :: (Num a, Eq a) => Function a -> Function a
simplifyFunction (Sum fs) = case length fs of
  0 -> Constant 0
  1 -> simplifyFunction $ head fs
  _ ->
    let sfs = map simplifyFunction fs
     in Sum $ filter (not . isZero) sfs
simplifyFunction (Product fs) = case length fs of
  0 -> Constant 1
  1 -> simplifyFunction $ head fs
  _ ->
    let sfs = map simplifyFunction fs
     in if any isZero sfs
          then Constant 0
          else Product $ filter (not . isOne) sfs
simplifyFunction (Polynomial cs) = case length cs of
  0 -> Constant 0
  1 -> Constant $ head cs
  _ -> Polynomial cs
simplifyFunction (Chain f g) = Chain (simplifyFunction f) (simplifyFunction g)
simplifyFunction (Division f g) = Division (simplifyFunction f) (simplifyFunction g)
simplifyFunction f = f
