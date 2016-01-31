module Main where

import Alto

-----

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon = 30 :: TimeStep

-----

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

c1 :: Contract
c1 = zcb t1 10 USD

c11 :: Contract
c11 = european (mkDate 20)
          (zcb (mkDate 20) 0.4 USD `cAnd`
           zcb (mkDate 30) 9.3 USD `cAnd`
           zcb (mkDate 40) 109.3 USD `cAnd`
           give (zcb (mkDate 12) 100 USD))

pr1 :: PR Double
pr1 = evalX c11

------

------

someFunc :: IO ()
someFunc = putStrLn $ show $ c11

someMore :: IO ()
someMore = putStrLn $ show $ expectedValuePr pr1

main :: IO ()
main = do
        someFunc
        someMore
        test
        return ()
