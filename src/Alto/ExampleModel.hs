module Alto.ExampleModel
  where

import Alto.Contracts
import Alto.Model

xm :: Model
xm = exampleModel ()

evalX :: Contract -> PR Double
evalX = evalC xm USD

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
