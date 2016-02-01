{-# LANGUAGE NoMonomorphismRestriction #-}

module Alto.FinalTagless where

import Prelude hiding (compare)

data Eval ret = Eval ret
data Stringify ret = Stringify (String)

class Expr repr where
    int :: Int -> repr Int
    boolean :: Bool -> repr Bool
    add :: repr Int -> repr Int -> repr Int
    compare :: (Eq a) =>
               repr a -> repr a -> repr Bool

instance Expr Eval where
    int = Eval
    boolean = Eval
    add (Eval l) (Eval r) = Eval $ l + r
    compare (Eval l) (Eval r) = Eval $ l == r

instance Expr Stringify where
    int = Stringify . show
    boolean = Stringify . show
    add (Stringify l) (Stringify r) =
        Stringify $ "(" ++ l ++ " + " ++ r ++ ")"
    compare (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " == " ++ r ++ ")"

class Mult repr where
    mul :: repr Int -> repr Int -> repr Int

instance Mult Eval where
    mul (Eval l) (Eval r) = Eval $ l * r

instance Mult Stringify  where
    mul (Stringify l) (Stringify r) = 
        Stringify $ "(" ++ l ++ " * " ++ r ++ ")"

runEval :: Eval a -> a
runEval (Eval ret) = ret

runStringify :: (Show a) => Stringify a -> String
runStringify (Stringify ret) = ret

expr :: (Mult repr, Expr repr) =>
    repr Bool
expr = int 90 `compare` (int 3 `mul` (int 10 `add` int 20))

test :: IO ()
test = do
    putStrLn $ (runStringify expr) ++ " : " ++ (show $ (runEval expr))
