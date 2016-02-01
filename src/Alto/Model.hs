module Alto.Model
  where

import Alto.Contracts

data Model = Model {
   modelStart :: Date,
   disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
   exch       :: Currency -> Currency -> PR Double,
   absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
   rateModel  :: Currency -> PR Double
   }

exampleModel :: CalendarTime -> Model
exampleModel modelDate = Model {
   modelStart = (modelDate,0),
   disc       = disc,
   exch       = exch,
   absorb     = absorb,
   rateModel  = rateModel
   }
   where

    rates :: Double -> Double -> PR Double
    rates rateNow delta = PR $ makeRateSlices rateNow 1
     where
       makeRateSlices rateNow n = (rateSlice rateNow n) : (makeRateSlices (rateNow-delta) (n+1))
       rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]
    rateModels = [(CHF, rates 7   0.8)
                ,(EUR, rates 6.5 0.25)
                ,(GBP, rates 8   0.5)
                ,(KYD, rates 11  1.2)
                ,(USD, rates 5   1)
                ,(ZAR, rates 15  1.5)
                ]
    rateModel k =
     case lookup k rateModels of
       Just x -> x
       Nothing -> error $ "rateModel: currency not found " ++ (show k)

    disc :: Currency -> (PR Bool, PR Double) -> PR Double
    disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
         where
           discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
           discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
             if and bRv -- test for horizon
               then [pRv]
               else let rest@(nextSlice:_) = discCalc bs ps rs
                        discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                        thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
                                      bRv pRv discSlice
                    in thisSlice : rest
           prevSlice :: RV Double -> RV Double
           prevSlice [] = []
           prevSlice (_:[]) = []
           prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

    absorb :: Currency -> (PR Bool, PR Double) -> PR Double
    absorb k (PR bSlices, PR rvs) =
         PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                      bSlices rvs

    exch :: Currency -> Currency -> PR Double
    exch k1 k2 = PR (konstSlices 1)
