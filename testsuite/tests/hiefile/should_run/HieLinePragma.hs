-- LINE pragmas can make the spans of different declarations overlap (see the
-- Note on mergeAsts in GHC.Iface.Ext.Utils). The .hie file must still record
-- every occurrence of each identifier, once: mergeAsts used to drop some nodes
-- (e.g. quuuuuux's type signature) and duplicate others.
module Main where

import TestUtils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sort)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)

{-# LINE 30 "HieLinePragma.hs" #-}
foozball :: Int
foozball = 0

{-# LINE 30 "HieLinePragma.hs" #-}
bar, quuuuuux :: Int
bar = 1
quuuuuux = 2

{-# LINE 31 "HieLinePragma.hs" #-}
zap :: Int -> Int
zap x = x + foozball + bar + quuuuuux

{-# LINE 50 "HieLinePragma.hs" #-}
uses :: DynFlags -> HieAST a -> [(String, String)]
uses df (Node (SourcedNodeInfo m) sp kids) =
  [ (occNameString (nameOccName n), render df sp ++ " " ++ render df ci)
  | ni <- M.elems m
  , (Right n, details) <- M.toList (nodeIdentifiers ni)
  , ci <- S.toList (identInfo details) ]
  ++ concatMap (uses df) kids

main :: IO ()
main = do
  (df, hf) <- readTestHie "HieLinePragma.hie"
  let found = sort (concatMap (uses df) (M.elems (getAsts (hie_asts hf))))
  mapM_ (\(n, u) -> putStrLn (n ++ " " ++ u))
    [ x | x@(n, _) <- found, n `elem` ["foozball", "bar", "quuuuuux", "zap"] ]
