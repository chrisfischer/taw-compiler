module ComparisonTest where

import Ast
import AstGen
import Interpreter
import Test.QuickCheck


-----------------------------
-- HARNESS ------------------
-----------------------------

-----------------------------
-- PROPERTIES ---------------
-----------------------------

-- Binop
isBinop :: Exp -> Bool
isBinop Bop{} = True
isBinop _     = False

prop_test :: Show a => Gen a -> Property
prop_test gen = 
  forAll gen $ \i -> True

vCheck :: Show a => Gen a -> IO ()
vCheck gen = verboseCheck (prop_test gen)
{--
prop_equal_binop :: Property
prop_equal_binop =
  forAll genExp $ \exp ->
    isBinop exp ==>
      evalBop exp == evalBop exp
--}
{--
prop_binop_comm :: Property
prop_binop_comm = forAll genExp $ \exp ->
  isBinop exp ==>
    let (Bop binop e1 e2) = exp in
    evalBop exp == evalBop (Bop binop e2 e1)
--}


