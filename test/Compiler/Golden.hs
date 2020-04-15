{-# LANGUAGE TypeApplications #-}

module Compiler.Golden
    ( test_ExprGroupsConstrs
    ) where

import           Compiler.FakeIntField    ()
import           Compiler.Utils

import           Zkbec.Examples.ExprGroup
import           Zkbec.Examples.Utils

import           Zkbec.Compiler.Compile

import           Zkbec.Language.Core
import           Zkbec.Language.Printer

import           Data.ByteString.Lazy     (ByteString)
import           Data.String
import           System.Directory
import           System.FilePath
import           System.Random
import           Test.Tasty
import           Test.Tasty.Golden

formatR1CS :: Expr -> ByteString
formatR1CS = fromString . unlines . map show . sequence . toR1CS @Int

test_ExprGroupConstrs :: ExprGroup Random Expr -> TestTree
test_ExprGroupConstrs (ExprGroup groupName groupBody) =
    testGroup groupName . map checkGroup $ runAsPseudoRandom groupBody where
        folder = "test" </> "Compiler" </> "golden" </> groupName
        checkGroup (ExprSameConstrs r1CsName exprs) =
            withIO (createDirectoryIfMissing True folder) $
                testConcurrentGroup r1CsName $ map checkSameR1Cs exprs where
                checkSameR1Cs expr =
                    goldenVsString
                        (toStringWithIDs expr)
                        (folder </> r1CsName <> ".golden")
                        (return $ formatR1CS expr)

test_ExprGroupsConstrs :: TestTree
test_ExprGroupsConstrs =
    testGroup "ExprGroupsConstrs" $
        map test_ExprGroupConstrs exprGroups
