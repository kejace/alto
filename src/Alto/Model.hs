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

evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model modelDate disc exch absorb rateModel) k = eval 
   where eval Zero           = bigK 0
         eval (One k2)       = exch k k2
         eval (Give c)       = -(eval c)
         eval (o `Scale` c)  = (evalO o) * (eval c)
         eval (c1 `And` c2)  = (eval c1) + (eval c2)
         eval (c1 `Or` c2)   = max (eval c1) (eval c2)
         eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
         eval (When o c)     = disc   k (evalO o, eval c)
         eval (Anytime _ _ ) = error "Anytime is not implemented"
 --      eval (Anytime o c)  = snell  k (evalO o, eval c)
         eval (Until o c)    = absorb k (evalO o, eval c)

evalS :: Model -> Currency -> Contract -> String
evalS m@(Model modelDate disc exch absorb rateModel) k = eval 
  where eval Zero           = "0"
        eval (One k2)       = "exch(" ++ (show k) ++ ", " ++ (show k2) ++ ")"
        eval (Give c)       = "-" ++ (eval c)
        eval (o `Scale` c)  = "scale(" ++ show ((head $ unPr $ evalO o)) ++ ", " ++ (eval c) -- this is dubious
        eval (c1 `And` c2)  = (eval c1) ++ " && " ++ (eval c2)
        eval (c1 `Or` c2)   = (eval c1) ++ " || " ++ (eval c2)
        eval (Cond o c1 c2) = error "Cond not implemented"
        eval (When o c)     = "when(" ++ (show o) ++ ", " ++ (evalS m k c) ++ ")"
        eval (Anytime _ _ ) = error "Anytime is not implemented"
        eval (Until o c)    = error "Absorb not implemented"

-- instance Show a => Show (Obs a) where
--    show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

-- zcb t x k = cWhen (at t) (scale (konst x) (one k))