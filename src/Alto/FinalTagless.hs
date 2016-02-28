{-# LANGUAGE NoMonomorphismRestriction #-}

module Alto.FinalTagless where

import Prelude hiding (compare)

data Eval ret = Eval ret
data Stringify ret = Stringify (String)

class Expr repr where
    pint :: Int -> repr Int
    pboolean :: Bool -> repr Bool
    padd :: repr Int -> repr Int -> repr Int
    pcompare :: (Eq a) =>
               repr a -> repr a -> repr Bool

instance Expr Eval where
    pint = Eval
    pboolean = Eval
    padd (Eval l) (Eval r) = Eval $ l + r
    pcompare (Eval l) (Eval r) = Eval $ l == r

instance Expr Stringify where
    pint = Stringify . show
    pboolean = Stringify . show
    padd (Stringify l) (Stringify r) =
        Stringify $ "(" ++ l ++ " + " ++ r ++ ")"
    pcompare (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " == " ++ r ++ ")"

class Mult repr where
    pmul :: repr Int -> repr Int -> repr Int

instance Mult Eval where
    pmul (Eval l) (Eval r) = Eval $ l * r

instance Mult Stringify  where
    pmul (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " * " ++ r ++ ")"

