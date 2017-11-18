{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Data.Foldable as F
import Data.Monoid

data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show

eval :: Expr -> Int
eval (Const a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

count :: Expr -> Int
count (Const a) = 1
count (Add a b) = count a + count b
count (Mul a b) = count a + count b

opt1 :: Expr -> Expr
opt1 = o where
  o (Add a (Const 0)) = o a
  o (Add (Const 0) a) = o a
  o (Mul a b) = Mul (o a) (o b)
  o e = e

opt2 :: Expr -> Expr
opt2 = o where
  o (Mul a (Const 1)) = o a
  o (Mul (Const 1) a) = o a
  o (Add a b) = Add (o a) (o b)
  o e = e

opt :: Expr -> Expr
opt = opt2 . opt1

fact n = if n == 0 then 1 else n * fact (n - 1)

fix f = let x = f x in x

fact' n = fix f n where f x m = if m == 0 then 1 else m * x (m - 1)

e = Add (Mul (Const 1) (Const 2)) (Const 0)

data ExprF r = ConstF Int
             | AddF r r
             | MulF r r
             deriving (Show, Functor, Foldable)

newtype Fix f = Fix { unFix :: f (Fix f) }

type Expr' = Fix ExprF

deriving instance Show (f (Fix f)) => Show (Fix f)

cst a   = Fix (ConstF a)
add a b = Fix (AddF a b)
mul a b = Fix (MulF a b)

e' = add (mul (cst 1) (cst 2)) (cst 0)

-- ef = Fix $ AddF (Fix $ MulF (Fix $ ConstF 1) (Fix $ ConstF 2)) (Fix $ ConstF 0)

type Algebra a = ExprF a -> a

cata :: (ExprF a -> a) -> Expr' -> a
cata alg = alg . fmap (cata alg) . unFix

eval' :: Expr' -> Int
eval' = cata alg where
  alg :: ExprF Int -> Int
  alg (ConstF a) = a
  alg (AddF a b) = a + b
  alg (MulF a b) = a * b

count' :: Expr' -> Int
count' = cata alg where
  alg :: ExprF Int -> Int
  alg (ConstF a) = 1
  alg (AddF a b) = a + b
  alg (MulF a b) = a + b

count'' :: Expr' -> Int
count'' = getSum . cata alg where
  alg :: ExprF (Sum Int) -> Sum Int
  alg (ConstF _) = 1
  alg e = F.fold e

opt1' :: ExprF Expr' -> Expr'
opt1' (AddF a (Fix (ConstF 0))) = a
opt1' (AddF (Fix (ConstF 0)) a) = a
opt1' e = Fix e

opt2' :: ExprF Expr' -> Expr'
opt2' (MulF a (Fix (ConstF 1))) = a
opt2' (MulF (Fix (ConstF 1)) a) = a
opt2' e = Fix e

optSlow :: Expr' -> Expr'
optSlow = cata opt2' . cata opt1'

optFast :: Expr' -> Expr'
optFast = cata (opt2' . unFix . opt1')

main :: IO ()
main = do
  putStrLn . show . optFast $ e'
