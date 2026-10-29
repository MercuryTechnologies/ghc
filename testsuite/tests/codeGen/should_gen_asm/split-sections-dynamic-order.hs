-- `-split-sections -dynamic` and `-dynamic -split-sections` should have the
-- same effect: -dynamic used to unset a -split-sections given before it.
module SplitSectionsDynamicOrder (foo) where

{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = x * 2 + 1
