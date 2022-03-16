module TestFragments where

import Parser (option)
import Fragments (int, comma)

testOption = do x <- option int
                y <- comma
                z <- option int
                pure (x, z)