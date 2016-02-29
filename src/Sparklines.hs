module Sparklines where

import Data.Char (chr)
--import Data.List.Split (splitOneOf)
 
toSparkLine :: [Double] -> [Char]
toSparkLine xs = map cl xs
    where
        top = maximum xs
        bot = minimum xs
        range = top - bot
        cl x = chr $ 0x2581 + round ((x - bot) / range * 7)
 
makeSparkLine :: [Double] -> (String, Stats)
makeSparkLine xs = (toSparkLine xs, stats xs)
   -- where parsed = map read $ filter (not . null) $ splitOneOf " ," xs
 
data Stats = Stats { minValue, maxValue, rangeOfValues :: Double,
    numberOfValues :: Int }
 
instance Show Stats where
    show (Stats mn mx r n) = "min: " ++ show mn ++ "; max: " ++ show mx ++
        "; range: " ++ show r ++ "; no. of values: " ++ show n
 
stats :: [Double] -> Stats
stats xs = Stats { minValue = mn, maxValue = mx,
    rangeOfValues = mx - mn, numberOfValues = length xs }
    where
        mn = minimum xs
        mx = maximum xs
 
drawSparkLineWithStats :: [Double] -> IO ()
drawSparkLineWithStats xs = putStrLn sp >> print st
    where (sp, st) = makeSparkLine xs

 