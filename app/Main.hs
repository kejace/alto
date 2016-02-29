module Main where

import Alto.Contracts
import Alto.ExampleModel
import Alto.FinalTagless
import Alto.Model

import Sparklines

import Text.PrettyPrint.ANSI.Leijen

-----

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon = 30 :: TimeStep

-----

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

c1 :: Contract
c1 = zcb t1 10 USD

c1s = "\nc1 :: Contract\nc1 = zcb t1 10 USD"

c11 :: Contract
c11 = european (mkDate 20)
          (zcb (mkDate 20) 0.4 USD `cAnd`
           zcb (mkDate 30) 9.3 USD `cAnd`
           zcb (mkDate 40) 109.3 USD `cAnd`
           give (zcb (mkDate 12) 100 USD))

c11s = "\nc11 :: Contract  \nc11 = european (mkDate 20) \n       (zcb (mkDate 20) 0.4 USD `cAnd` \n        zcb (mkDate 30) 9.3 USD `cAnd` \n        zcb (mkDate 40) 109.3 USD `cAnd` \n        give (zcb (mkDate 12) 100 USD))"

------

runEval :: Eval a -> a
runEval (Eval ret) = ret

runStringify :: (Show a) => Stringify a -> String
runStringify (Stringify ret) = ret

expr :: (Mult repr, Expr repr) =>
    repr Bool
expr = pint 90 `pcompare` (pint 3 `pmul` (pint 10 `padd` pint 20))

------

main :: IO ()
main = do
        putStrLn $ "\nContract haskell: " ++ c1s
        putStrLn $ "Contract output"
        putStrLn $ show $ c1
        putStrLn $ "Contract stringified"
        putStrLn $ show $ evalSX c1
        putStrLn $ "Contract evaluated"
        drawSparkLineWithStats $ expectedValuePr $ evalX c1

        putStrLn $ "\nContract haskell: " ++ c11s
        putStrLn $ "Contract output"
        putStrLn $ show $ c11
        putStrLn $ "Contract stringified"
        putStrLn $ show $ evalSX c11
        putStrLn $ "Contract evaluated"
        drawSparkLineWithStats $ expectedValuePr $ evalX c11

        putStrLn $ "\nFT:"
        putStrLn $ (runStringify expr) ++ " : " ++ (show $ (runEval expr))

        return ()
