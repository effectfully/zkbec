module Zkbec.Compiler.Pretty where

import           Zkbec.Language.Var

mulShow :: String -> String -> String
mulShow l r = l ++ " * " ++ r

eqShow :: String -> String -> String
eqShow l r = l ++ " = " ++ r

approxEqShow :: String -> String -> String
approxEqShow l r = l ++ " =~ " ++ r

assignShow :: String -> String -> String
assignShow l r = l ++ " := " ++ r

unifyShow :: String -> String -> String
unifyShow l r = l ++ " =?= " ++ r

parens :: String -> String
parens s = "(" ++ s ++ ")"

-- Ignores indices when there is a name.
showVarWeird :: Var -> String
showVarWeird var@(Var (Unique _) ""  ) = show var
showVarWeird     (Var (Unique _) name) = name
