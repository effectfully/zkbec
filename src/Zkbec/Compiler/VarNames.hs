module Zkbec.Compiler.VarNames where

import           Zkbec.Prelude

import           Zkbec.Language.Environment
import           Zkbec.Language.Var

import           Control.Lens
import qualified Data.IntMap.Strict         as IntMap

type VarNames = Env String

data InEnv b a = InEnv
    { _inEnvEnv :: Env b
    , _inEnvVal :: a
    } deriving (Functor, Foldable, Traversable)

instance Bifunctor InEnv where
    bimap g f (InEnv env x) = InEnv (g <$> env) $ f x

instance Applicative (InEnv b) where
    pure = InEnv mempty
    InEnv env1 f <*> InEnv env2 x = InEnv (env1 <> env2) $ f x

class HasName a where
    _Name :: Lens' a String

data Occur
    = Free
    | Bound

data Scoped a = Scoped
    { _scopedOccur :: Occur
    , _scopedValue :: a
    } deriving (Functor, Foldable, Traversable)

instance HasName String where
    _Name = id

instance HasName a => HasName (Scoped a) where
    _Name = visitExtract _scopedValue . _Name

-- TODO: introduce @Name@ upstream and use it.
type ScopedName = Scoped String

type ScopedNames = Env ScopedName

singleScopedName :: Occur -> Var -> ScopedNames
singleScopedName occur (Var uniq name) = insertUnique uniq (Scoped occur name) mempty

varNamesToVars :: VarNames -> [Var]
varNamesToVars = map (uncurry $ Var . Unique) . IntMap.toList . unEnv

varsToVarNames :: Foldable f => f Var -> VarNames
varsToVarNames = toEnvBy $ \(Var uniq name) -> insertUnique uniq name
