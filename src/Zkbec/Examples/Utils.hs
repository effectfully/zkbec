{-# LANGUAGE UndecidableSuperClasses #-}

module Zkbec.Examples.Utils
    ( Holds (..)
    , GenerateAs
    , ExprSameConstrs (..)
    , ExprGroup (..)
    , CompilationResult (..)
    , runGenerateAs
    , runAsPseudoRandom
    , runAsForAll
    , reqConstant
    , reqPosIntBelow
    ) where

import           Zkbec.Compiler.Utils

import           Data.Field
import           Zkbec.Language.Parser ()

import           Control.Monad.Free
import           Control.Monad.Cont
import           Control.Monad.State
import           Data.Functor
import           Data.Functor.Identity
import           System.Random
import           Test.QuickCheck

class (c a, d a) => And c d a
instance (c a, d a) => And c d a

data Holds constr a where
    Holds :: constr a => Holds constr a

type GenerateAs constr = Free (HRequestF (Holds constr) Identity)

data ExprSameConstrs expr = ExprSameConstrs
    { _exprSameConstrsName :: String
    , _exprSameConstrsExpr :: [expr]
    } deriving (Functor)

data ExprGroup constr expr = ExprGroup
    { _exprGroupName :: String
    , _exprGroupBody :: GenerateAs constr [ExprSameConstrs expr]
    } deriving (Functor)

data CompilationResult f
    = CompilationSuccess (AField f)
    | CompilationNoEvaluationSuccess (AField f)
    | CompilationFailure
    deriving (Show, Functor)

runGenerateAs :: Monad m => (forall a. constr a => m a) -> GenerateAs constr b -> m b
runGenerateAs gen = runHRequestsM $ \Holds -> Identity <$> gen

runAsPseudoRandom :: GenerateAs Random b -> b
runAsPseudoRandom b = evalState (runGenerateAs randomM b) $ mkStdGen 42

runAsForAll :: Testable prop => GenerateAs (Arbitrary `And` Show) b -> (b -> prop) -> Property
runAsForAll b k = runCont (runGenerateAs (cont $ forAll arbitrary) b) (property . k)

reqConstant :: constr a => GenerateAs constr a
reqConstant = runIdentity <$> hrequest Holds

reqPosIntBelow :: constr Int => Int -> GenerateAs constr Int
reqPosIntBelow upper = reqConstant <&> \i -> abs i `rem` upper
