module Zkbec.Compiler.LinComb where

import           Zkbec.Prelude

import           Zkbec.Compiler.Pretty
import           Zkbec.Compiler.VarNames

import           Data.Field
import           Zkbec.Language.Environment
import           Zkbec.Language.Var
import           Zkbec.Language.Parser

import           Control.Lens
import qualified Data.IntMap.Strict   as IntMap

infixl 6 `addLinComb`, `subLinComb`
infixl 7 `mulLinCombByVal`

-- | The key of a constant is @-1@.
newtype LinComb f = LinComb
    { unLinComb :: IntMap f
    } deriving (Eq, Functor, Generic)
      deriving newtype (Hashable)

instance (Eq f, IsNegative f, TextField f, HasName a) => Show (InEnv a (LinComb f)) where
    show (InEnv env (LinComb xs)) =
        case map format $ IntMap.toList xs of
            []                       -> "0"
            (sign0, val0) : signVals -> concat
                $ ((if sign0 == "-" then "- " else "") ++ val0)
                : map (\(sign, val) -> concat [" ", sign, " ", val]) signVals
        where
            format (i, x)
                | i == -1 && isNegative x = ("-", showField $ neg x)
                | i == -1                 = ("+", showField x)
                | x == one                = ("+", v)
                | x == neg one            = ("-", v)
                | isNegative x            = ("-", showField (neg x) `mulShow` v)
                | otherwise               = ("+", showField  x      `mulShow` v)
                where
                    uniq = Unique i
                    -- In the @field@ development there must always be a name for each unique,
                    -- but this is not true for the @boolean@ development, so we have to handle
                    -- the non-existing name case (and return the dummy empty string).
                    v = show . Var uniq $ maybe "" (view _Name) $ lookupUnique uniq env

emptyLinComb :: LinComb f
emptyLinComb = LinComb mempty

-- | O(n)
makeLinComb :: (Eq f, Field f) => IntMap f -> LinComb f
makeLinComb = LinComb . IntMap.filter (/= zer)

negLinComb :: Field f => LinComb f -> LinComb f
negLinComb = fmap neg

addLinComb :: (Eq f, Field f) => LinComb f -> LinComb f -> LinComb f
addLinComb (LinComb xs) (LinComb ys) = makeLinComb $ IntMap.unionWith add xs ys

subLinComb :: (Eq f, Field f) => LinComb f -> LinComb f -> LinComb f
subLinComb l r = l `addLinComb` negLinComb r

valLinComb :: (Eq f, Field f) => f -> LinComb f
valLinComb = makeLinComb . IntMap.singleton (-1)

uniqLinComb :: Field f => Unique -> LinComb f
uniqLinComb uniq = LinComb $ IntMap.singleton (unUnique uniq) one

varLinComb :: Field f => Var -> LinComb f
varLinComb = uniqLinComb . _varUniq

groundLinComb :: (Eq f, Field f) => Either f Var -> LinComb f
groundLinComb = either valLinComb varLinComb

singletonLinComb :: Var -> f -> LinComb f
singletonLinComb var = LinComb . IntMap.singleton (unUnique $ _varUniq var)

-- | O(log n)
linCombAsVal :: Field f => LinComb f -> Maybe f
linCombAsVal (LinComb xs) = case IntMap.toList xs of
    []        -> Just zer
    [(-1, x)] -> Just x
    _         -> Nothing

-- | O(log n)
linCombAsUniq :: (Eq f, Field f) => LinComb f -> Maybe Unique
linCombAsUniq (LinComb xs) = case IntMap.toList xs of
    [(i, o)] | i /= -1 && o == one -> Just $ Unique i
    _                              -> Nothing

mulLinCombByVal :: (Eq f, Field f) => f -> LinComb f -> LinComb f
mulLinCombByVal n c
    | n == zer  = LinComb IntMap.empty
    | otherwise = fmap (n `mul`) c

evalLinComb :: Field f => Env f -> LinComb f -> f
evalLinComb (Env vals) (LinComb coeffs) =
    IntMap.foldlWithKey' step zer coeffs where
        step acc var coeff = acc `add` (coeff `mul` (vals IntMap.! var))
