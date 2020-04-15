module Compiler.Evaluation
    ( test_evalCompileExpr
    ) where

import           Zkbec.Prelude

import           Compiler.FakeIntField      ()

import           Zkbec.Examples.ExprGroup
import           Zkbec.Examples.Utils

import           Zkbec.Compiler.Evaluate

import           Zkbec.Language.Core
import           Zkbec.Language.Environment
import           Zkbec.Language.Evaluator
import           Zkbec.Language.Generator

import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.QuickCheck

data CheckResult
    = CheckUnknown
    | CheckFailure
    | CheckSuccess Bool

instance Semigroup CheckResult where
    -- Due to the fact that compilation to R1CS can remove "redundant" operations (and thus
    -- variables), we can get evaluation success with R1CS when we get evaluation failure
    -- with normal evaluation.
    CheckUnknown   <> ch             = ch
    CheckFailure   <> _              = CheckFailure
    -- Since @if@ is lazy, we can get evaluation success with normal evaluation and evaluation
    -- failure with evaluation-via-compilation, because the former can ignore some branches
    -- (that may contain free variables) and the latter cannot.
    ch             <> CheckUnknown   = ch
    _              <> CheckFailure   = CheckFailure
    CheckSuccess b <> CheckSuccess c
        | b == c    = CheckSuccess b
        | otherwise = CheckFailure

instance Monoid CheckResult where
    mempty = CheckUnknown

checkResultToBool :: CheckResult -> Bool
checkResultToBool CheckUnknown     = True
checkResultToBool CheckFailure     = False
checkResultToBool (CheckSuccess _) = True

eitherToCheckResult :: Either e Bool -> CheckResult
eitherToCheckResult (Left  _) = CheckUnknown
eitherToCheckResult (Right b) = CheckSuccess b

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True
intToBool _ = error "an Int can't be converted to Bool"

pureTry :: a -> Either SomeException a
pureTry = unsafePerformIO . try . evaluate

checkEvalCompileExpr :: ExprWithEnv -> Bool
checkEvalCompileExpr (ExprWithEnv expr env) =
    evalExpr env expr == intToBool (evalCompileExpr (fromEnum <$> env) expr)

test_evalCompileExprSuccess :: TestTree
test_evalCompileExprSuccess =
    testProperty "evalCompileExpr" $ withMaxSuccess 10000 checkEvalCompileExpr

-- It is unfortunate that when one evaluation engine fails, it doesn't mean that the other should
-- fail as well. See the comments inline.
checkEvalCompileExprsTry :: Env Bool -> [Expr] -> Bool
-- This does not short-circuit, but we do not really care about performance here.
checkEvalCompileExprsTry env = checkResultToBool . foldMap step where
    step expr = lhs <> rhs where
        lhs = eitherToCheckResult . pureTry $ intToBool (evalCompileExpr (fromEnum <$> env) expr)
        rhs = eitherToCheckResult . pureTry $ evalExpr env expr

test_evalCompileExprGroup :: TestTree
test_evalCompileExprGroup =
    testGroup "evalCompileExprGroup" $ map check exprGroups where
        check (ExprGroup name body) =
            testProperty name $ \env -> runAsForAll body $
                checkEvalCompileExprsTry env . concatMap _exprSameConstrsExpr

test_evalCompileExpr :: TestTree
test_evalCompileExpr =
    testGroup "evalCompileExpr"
        [ test_evalCompileExprSuccess
        , test_evalCompileExprGroup
        ]
