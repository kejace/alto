module Contracts
 where

import Data.List
import Numeric
import Control.Monad
import Data.Unique

data Currency = USD | GBP | EUR | SEK | BTC | ETH | ZAR | KYD | CHF deriving (Eq, Show)

type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0

data Contract =
     Zero
   | One  Currency
   | Give Contract
   | And  Contract Contract
   | Or   Contract Contract
   | Cond    (Obs Bool)   Contract Contract
   | Scale   (Obs Double) Contract
   | When    (Obs Bool)   Contract
   | Anytime (Obs Bool)   Contract
   | Until   (Obs Bool)   Contract
   deriving Show

newtype PR a = PR { unPr :: [RV a] } deriving Show
type RV a = [a]
newtype Obs a = Obs (Date -> PR a)

instance Show a => Show (Obs a) where
   show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

zero :: Contract
zero = Zero

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

cAnd :: Contract -> Contract -> Contract
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d

instance Num a => Num (Obs a) where
   fromInteger i = konst (fromInteger i)
   (+) = lift2 (+)
   (-) = lift2 (-)
   (*) = lift2 (*)
   abs = lift abs
   signum = lift signum

instance Eq a => Eq (Obs a) where
   (==) = undefined

(==*) :: Ord a => Obs a -> Obs a -> Obs Bool
(==*) = lift2 (==)

konst :: a -> Obs a
konst k = Obs (\t -> bigK k)

lift :: (a -> b) -> Obs a -> Obs b
lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))

date :: Obs Date
date = Obs (\t -> PR $ timeSlices [t])

at :: Date -> Obs Bool
at t = date ==* (konst t)

(%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%=)  = lift2 (==)
(%>=) = lift2 (>=)
(%>)  = lift2 (>)

european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)
american :: (Date, Date) -> Contract -> Contract
american (t1, t2) u = anytime (between t1 t2) u
between :: Date -> Date -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))

bigK :: a -> PR a
bigK x = PR (konstSlices x)

konstSlices x = nextSlice [x]
    where nextSlice sl = sl : (nextSlice (x:sl))

datePr :: PR Date
datePr = PR $ timeSlices [time0]

timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b tru fal -> if b then tru else fal)

liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
zipWithAll f as@(_:_) []       = as
zipWithAll f []       bs@(_:_) = bs
zipWithAll _ _        _        = []

instance Num a => Num (PR a) where
   fromInteger i = bigK (fromInteger i)
   (+) = lift2PrAll (+)
   (-) = lift2PrAll (-)
   (*) = lift2PrAll (*)
   abs = liftPr  abs
   signum = liftPr signum

instance Ord a => Ord (PR a) where
   max = lift2Pr max

instance Eq a => Eq (PR a) where
   (PR a) == (PR b) = a == b

evalO :: Obs a -> PR a
evalO (Obs o) = o time0

expectedValue :: RV Double -> RV Double -> Double
expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

expectedValuePr :: PR Double -> [Double]
expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
   where
     probabilities :: [RV Integer] -> [RV Double]
     probabilities (sl:sls) = map (\n -> (fromInteger n) / (fromInteger (sum sl))) sl : probabilities sls
     pathCounts :: [RV Integer]
     pathCounts = paths [1] where paths sl = sl : (paths (zipWith (+) (sl++[0]) (0:sl)))


