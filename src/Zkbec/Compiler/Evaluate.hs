module Zkbec.Compiler.Evaluate where

import           Zkbec.Compiler.Compile
import           Zkbec.Compiler.LinComb
import           Zkbec.Compiler.VarNames

import           Data.Field
import           Zkbec.Language.Core
import           Zkbec.Language.Environment
import           Zkbec.Language.Var

evalR1CS :: (Eq a, Field a) => Env a -> [R1C a] -> a
evalR1CS = go . insertUnique (Unique (-1)) one where
    go _    []                      = error "empty R1CS"
    go vals (R1C l r v kind : r1cs) =
        case kind of
            R1Assignment
                | null r1cs -> vV
                | otherwise -> go (insertVar v vV vals) r1cs
            R1Constraint
                | null r1cs -> error "an R1CS ends by a constraint"
                | vV == vV' -> go vals r1cs
                | otherwise -> error "a constraint is not satisfied"
        where
            lV = evalLinComb vals l
            rV = evalLinComb vals r
            vV = lV `mul` rV
            vV' = unsafeLookupVar v vals

evalNamedR1CS :: (Eq a, Field a) => Env a -> InEnv String [R1C a] -> a
evalNamedR1CS vals = evalR1CS vals . _inEnvVal

evalCompileExpr :: (Eq a, Field a) => Env a -> Expr -> a
evalCompileExpr env = evalNamedR1CS env . toR1CS
