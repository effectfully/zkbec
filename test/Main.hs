module Main where

import           Compiler.Evaluation (test_evalCompileExpr)
import           Compiler.Golden     (test_ExprGroupsConstrs)
import           Language.Textual    (test_printerParserRoundtrip)

import           Test.Tasty

test_all :: TestTree
test_all =
    testGroup "all"
        [ test_printerParserRoundtrip
        , test_evalCompileExpr
        , test_ExprGroupsConstrs
        ]

main :: IO ()
main = defaultMain test_all
