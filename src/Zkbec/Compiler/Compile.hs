module Zkbec.Compiler.Compile where

import           Zkbec.Prelude

import           Zkbec.Compiler.LinComb
import           Zkbec.Compiler.Pretty
import           Zkbec.Compiler.Utils
import           Zkbec.Compiler.VarNames

import           Data.Field
import           Zkbec.Language.Core
import           Zkbec.Language.Environment
import           Zkbec.Language.Parser
import           Zkbec.Language.Var

import qualified Data.IntMap.Strict         as IntMap
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

infixl 7 `mulLinComb`

data R1Kind
    = R1Assignment
    | R1Constraint

-- | > l * r = v
data R1C a = R1C
    { _r1cLeftOperand  :: LinComb a
    , _r1cRightOperand :: LinComb a
    , _r1cVar          :: Var
    , _r1cKind         :: R1Kind
    }

instance (Eq a, IsNegative a, TextField a) => Show (InEnv String (R1C a)) where
    show (InEnv varNames (R1C lo ro v kind)) =
        case kind of
            R1Assignment -> rhs `eqShow` lhs
            R1Constraint -> lhs `unifyShow` rhs
        where
            vnsAnd = InEnv varNames
            lhs = parens (show $ vnsAnd lo) `mulShow` parens (show $ vnsAnd ro)
            rhs = showVarWeird v

appendFieldToAssignment :: Text -> Text
appendFieldToAssignment text
    | "=?=" `Text.isInfixOf` text = Text.replace "=?=" "==" text
    | otherwise                   = Text.append "field " text

zoKratesShowR1C :: (Eq a, IsNegative a, TextField a) => InEnv String (R1C a) -> String
zoKratesShowR1C
    = Text.unpack
    . appendFieldToAssignment
    . Text.replace "(-" "(0 -"
    . Text.replace "_" "var"
    . Text.pack
    . show

zoKratesShowR1Cs :: (Eq a, IsNegative a, TextField a) => InEnv String [R1C a] -> String
zoKratesShowR1Cs = unlines . map zoKratesShowR1C . sequence

-- -- An alternative representation that allows to save a few constraints,
-- -- but is harder to deal with.
-- data Result a
--     = ResultVal a
--     | ResultLinComb (LinComb a)
--     | ResultMulGate (LinComb a) (LinComb a) (LinComb a)
--     -- b ? x : y = r ~> b * (y - x) = y - r
--     -- b `xor` c = r ~> c * 2b      = b + c - r
--
-- (if @c@ in the latter rule evaluates to @d * e = q@ and @b@ evaluates to @f * g = s@ then
-- we keep those constraints and the result is
--
-- > d * e = q
-- > f * g = s
-- > q * 2s = s + q - r
--
-- But @s + q - r@ is a pretty big expression and @q@ and @s@ can be big as well, do we want
-- to assign them to variables to properly handle the recursive case?
-- But this seems to defeat the entire purpose of the optimization.
-- On the other hand if we inline them, we can get some things optimized away.
-- Or we can get a blowup.
--
-- So it seems the most sensible strategy here is to inline all the linear combinations indeed,
-- simplify everything and then perform CSE.

type Compile c a = SupplyT (State c) a

-- We only need 'VarNames' in order to get the maximum index in that map in order to start
-- producing fresh variables. This is arguably not a good design, probably we just shouldn't leave
-- the 'SupplyT' transformer, but it is annoying.
runCompile :: Monoid c => VarNames -> Compile c a -> (a, c)
runCompile varNames a = flip runState mempty . runSupplyT $ do
    supplyFromAtLeast . freeUniqueIntMap $ unEnv varNames
    a

linCombVars :: VarNames -> LinComb a -> VarNames
linCombVars (Env names) (LinComb xs) = Env $ names `IntMap.intersection` xs

varVars :: VarNames -> Var -> VarNames
varVars (Env varNames) (Var (Unique uniq) name) =
    Env $ IntMap.singleton uniq name `IntMap.intersection` varNames

r1cVars :: VarNames -> R1C a -> VarNames
r1cVars varNames (R1C l r v _) = fold
    [ linCombVars varNames l
    , linCombVars varNames r
    , varVars varNames v
    ]

assignMul :: String -> LinComb a -> LinComb a -> Compile [R1C a] Var
assignMul name l r = do
    lrV <- freshVar name
    tell [R1C l r lrV R1Assignment]
    return lrV

mulLinComb :: (Eq a, Field a) => LinComb a -> LinComb a -> Compile [R1C a] (LinComb a)
mulLinComb l r = case (linCombAsVal l, linCombAsVal r) of
    (Just x , _      ) -> return $ mulLinCombByVal x r
    (Nothing, Just y ) -> return $ mulLinCombByVal y l
    (Nothing, Nothing) -> varLinComb <$> assignMul "" l r

compile :: (Eq a, Field a) => Expr -> Compile [R1C a] (LinComb a)
compile (EConst b)         = return . valLinComb $ if b then one else zer
compile (EVar var)         = return $ varLinComb var
compile (EIf b x y)        = do
    -- b ? x : y = y - b * (y - x)
    bC  <- compile b
    xC  <- compile x
    yC  <- compile y
    xyC <- bC `mulLinComb` (yC `subLinComb` xC)
    return $ yC `subLinComb` xyC
compile (EAppUnOp op x)    = case op of
    Not -> (valLinComb one `subLinComb`) <$> compile x
compile (EAppBinOp op x y) = case op of
    Or  -> do
        -- x `or` y = x + y - x * y
        xC  <- compile x
        yC  <- compile y
        xyC <- xC `mulLinComb` yC
        return $ xC `addLinComb` yC `subLinComb` xyC
    And -> do
        -- x `and` y = x * y
        xC <- compile x
        yC <- compile y
        xC `mulLinComb` yC
    Xor -> do
        -- x `xor` y = x + y - 2 * x * y
        xC  <- compile x
        yC  <- compile y
        xyC <- xC `mulLinComb` yC
        return $ xC `addLinComb` yC `subLinComb` (two `mulLinCombByVal` xyC)

toR1CS :: (Eq a, Field a) => Expr -> InEnv String [R1C a]
toR1CS expr =
    let varNames = Env $ exprVarNames expr in
        InEnv varNames . snd . runCompile varNames $ do
            yC <- compile expr
            _ <- assignMul "out" (valLinComb one) yC
            constrs <- get
            let usedVarNames = foldMap (r1cVars varNames) constrs
                ensureBool uniq s = do
                   let v  = Var uniq s
                       lc = varLinComb v
                   modify (R1C lc lc v R1Constraint :)
            traverse_ (uncurry ensureBool) $ toUniques usedVarNames

toNamedR1CS :: (Eq a, Field a) => Expr -> [InEnv String (R1C a)]
toNamedR1CS = sequence . toR1CS
