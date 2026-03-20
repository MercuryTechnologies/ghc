#!/usr/bin/env runhaskell
-- Generates benchmark modules with progressively larger types
-- that derive Generic, then compiles each and reports timing.

module Main where

import System.IO

-- Generate a sum type with N nullary constructors
genSumType :: String -> Int -> String
genSumType name n = unlines $
  [ "{-# LANGUAGE DeriveGeneric #-}"
  , "module " ++ name ++ " where"
  , "import GHC.Generics"
  , ""
  , "data " ++ name ++ " ="
  ] ++
  [ "    " ++ sep i ++ "C" ++ show i
  | i <- [0..n-1]
  ] ++
  [ "  deriving Generic"
  ]
  where
    sep 0 = "  "
    sep _ = "| "

-- Generate a record type with N fields
genRecordType :: String -> Int -> String
genRecordType name n = unlines $
  [ "{-# LANGUAGE DeriveGeneric #-}"
  , "module " ++ name ++ " where"
  , "import GHC.Generics"
  , ""
  , "data " ++ name ++ " = " ++ name
  ] ++
  [ "  { field" ++ show i ++ " :: Int"
  | i <- [0..0]
  ] ++
  [ "  , field" ++ show i ++ " :: Int"
  | i <- [1..n-1]
  ] ++
  [ "  }"
  , "  deriving Generic"
  ]

-- Generate a sum type where each constructor has M fields
genSumRecord :: String -> Int -> Int -> String
genSumRecord name nCons nFields = unlines $
  [ "{-# LANGUAGE DeriveGeneric #-}"
  , "module " ++ name ++ " where"
  , "import GHC.Generics"
  , ""
  , "data " ++ name ++ " ="
  ] ++
  concat
  [ [ "    " ++ sep i ++ "C" ++ show i
      ++ concat [" Int" | _ <- [1..nFields]]
    ]
  | i <- [0..nCons-1]
  ] ++
  [ "  deriving Generic"
  ]
  where
    sep 0 = "  "
    sep _ = "| "

main :: IO ()
main = do
  -- Sum types: 10, 25, 50, 100, 200 constructors
  mapM_ (\n -> writeFile ("Sum" ++ show n ++ ".hs") (genSumType ("Sum" ++ show n) n))
    [10, 25, 50, 100, 200]

  -- Record types: 10, 25, 50, 100, 200 fields
  mapM_ (\n -> writeFile ("Rec" ++ show n ++ ".hs") (genRecordType ("Rec" ++ show n) n))
    [10, 25, 50, 100, 200]

  -- Sum-of-records: NxM constructors x fields
  mapM_ (\(c,f) -> writeFile ("SumRec" ++ show c ++ "x" ++ show f ++ ".hs")
                              (genSumRecord ("SumRec" ++ show c ++ "x" ++ show f) c f))
    [(10,5), (25,5), (50,5), (10,10), (25,10), (50,10)]

  putStrLn "Generated all benchmark modules."
