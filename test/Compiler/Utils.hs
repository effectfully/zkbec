{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Utils
    ( short
    , removeDirectoryRecursiveIfExists
    , findFilesBy
    , withIO
    , testConcurrentGroup
    ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           System.Directory
import           System.FilePath.Glob
import           Test.Tasty

short :: String -> String
short str
    | length str <= 65 = str
    | otherwise        = "<...>"

removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists dir = do
    exists <- doesDirectoryExist dir
    when exists $ removeDirectoryRecursive dir

findFilesBy :: String -> FilePath -> IO [FilePath]
findFilesBy = globDir1 . compile

withIO :: IO () -> TestTree -> TestTree
withIO io = withResource io mempty . const

-- Manual locking to prevent race conditions in tests that run concurrently.
testConcurrentGroup :: String -> [TestTree] -> TestTree
testConcurrentGroup groupName trees =
    withResource (newMVar ()) mempty $ \getLock ->
        let testSingle tree =
                withResource
                    (getLock >>= takeMVar)
                    (\_ -> getLock >>= flip putMVar ())
                    (\_ -> tree)
            in testGroup groupName $ map testSingle trees
