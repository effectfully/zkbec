{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.FakeIntField where

import           Data.Field
import           Zkbec.Language.Parser

instance Field Int where
    zer = 0
    neg = negate
    add = (+)
    sub = (-)
    one = 1
    inv = error "'Int' doesn't support 'inv'"
    mul = (*)

instance TextField Int where
    parseField = error "You don't need this"

instance IsNegative Int
