module Zkbec.Examples.ExprGroup
    ( exprGroups
    ) where

import           Zkbec.Examples.Utils

import           Zkbec.Language.Core
import           Zkbec.Language.Var

a, b, c, d, e :: Var
a = Var (Unique 0) "a"
b = Var (Unique 1) "b"
c = Var (Unique 2) "c"
d = Var (Unique 3) "d"
e = Var (Unique 4) "e"

exprGroups :: [ExprGroup constr Expr]
exprGroups =
    [ ExprGroup "true" $ pure
        [ ExprSameConstrs "main"
            [ EConst True
            , EAppUnOp Not $ EConst False
            , EAppBinOp Or (EConst True) (EVar a)
            , EAppBinOp Or (EVar a) (EConst True)
            ]
        , ExprSameConstrs "if-id-id-not" [EIf (EVar a) (EVar a) (EAppUnOp Not (EVar a))]
        , ExprSameConstrs "xor-not-same" [EAppBinOp Xor (EVar a) (EAppUnOp Not (EVar a))]
        , ExprSameConstrs "or-not-l" [EAppBinOp Or (EAppUnOp Not (EVar a)) (EVar a)]
        , ExprSameConstrs "or-not-r" [EAppBinOp Or (EVar a) (EAppUnOp Not (EVar a))]
        ]
    , ExprGroup "false" $ pure
        [ ExprSameConstrs "main"
            [ EConst False
            , EAppUnOp Not $ EConst True
            , EAppBinOp And (EConst False) (EVar a)
            , EAppBinOp And (EVar a) (EConst False)
            ]
        , ExprSameConstrs "if-id-not-id" [EIf (EVar a) (EAppUnOp Not (EVar a)) (EVar a)]
        , ExprSameConstrs "xor-same" [EAppBinOp Xor (EVar a) (EVar a)]
        , ExprSameConstrs "and-not-l" [EAppBinOp And (EAppUnOp Not (EVar a)) (EVar a)]
        , ExprSameConstrs "and-not-r" [EAppBinOp And (EVar a) (EAppUnOp Not (EVar a))]
        ]
    , ExprGroup "id" $ pure
        [ ExprSameConstrs "main"
            [ EVar a
            , EAppUnOp Not (EAppUnOp Not (EVar a))
            , EAppBinOp Or (EConst False) (EVar a)
            , EAppBinOp Or (EVar a) (EConst False)
            , EAppBinOp And (EConst True) (EVar a)
            , EAppBinOp And (EVar a) (EConst True)
            , EIf (EConst True) (EVar a) (EVar b)
            , EIf (EConst False) (EVar b) (EVar a)
            , EIf (EVar a) (EConst True) (EConst False)
            , EIf (EVar b) (EVar a) (EVar a)
            , EIf (EVar a) (EVar a) (EVar a)
            ]
        , ExprSameConstrs "or-same" [EAppBinOp Or (EVar a) (EVar a)]
        , ExprSameConstrs "and-same" [EAppBinOp And (EVar a) (EVar a)]
        , ExprSameConstrs "if-if-if"
            [ EIf (EVar b)
                (EIf (EVar b) (EVar a) (EVar c))
                (EIf (EVar b) (EVar c) (EVar a))
            ]
        ]
    , ExprGroup "not" $ pure
        [ ExprSameConstrs "main"
            [ EAppUnOp Not $ EVar a
            , EIf (EVar a) (EConst False) (EConst True)
            , EAppUnOp Not (EIf (EVar a) (EConst True) (EConst False))
            ]
        ]
    , ExprGroup "De-Morgan-or" $ pure
        [ ExprSameConstrs "not-or"
            [ EAppUnOp Not (EAppBinOp Or (EVar a) (EVar b))
            ]
        , ExprSameConstrs "and-not"
            [ EAppBinOp And (EAppUnOp Not (EVar a)) (EAppUnOp Not (EVar b))
            ]
        ]
    , ExprGroup "De-Morgan-and" $ pure
        [ ExprSameConstrs "not-and"
            [ EAppUnOp Not (EAppBinOp And (EVar a) (EVar b))
            ]
        , ExprSameConstrs "or-not"
            [ EAppBinOp Or (EAppUnOp Not (EVar a)) (EAppUnOp Not (EVar b))
            ]
        ]
    , ExprGroup "if" $ pure
        [ ExprSameConstrs "main" [EIf (EVar a) (EVar b) (EVar c)]
        ]
    , ExprGroup "if-and" $ pure
        [ ExprSameConstrs "main" [EIf (EAppBinOp And (EVar a) (EVar b)) (EVar c) (EVar d)]
        ]
    , ExprGroup "if-or" $ pure
        [ ExprSameConstrs "main" [EIf (EAppBinOp Or (EVar a) (EVar b)) (EVar c) (EVar d)]
        ]
    , ExprGroup "if-if" $ pure
        [ ExprSameConstrs "main" [EIf (EIf (EVar a) (EVar b) (EVar c)) (EVar d) (EVar e)]
        ]
    ]
