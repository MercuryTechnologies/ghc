-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

{-
Note [HPC init via Cmm]
~~~~~~~~~~~~~~~~~~~~~~~~
HPC (Haskell Program Coverage) requires two things per module:

1. A static array of StgWord64 tick boxes, referenced by generated code
   (mkTickBox) to count coverage hits at runtime.

2. A call to hs_hpc_module() at program startup to register the module
   with the RTS so that tick counts are written to the .tix file on exit.

Previously, both the tick array and the registration were generated as C code
via hpcInitCode in GHC.HsToCore.Coverage. The C stub was compiled by gcc,
which added significant build overhead (especially in nix environments where
the gcc wrapper has ~1.5s startup cost per invocation).

Now, both the tick array and the registration are generated as Cmm:

- The tick array is emitted as CmmData with zero-initialized slots.

- The registration function is emitted as a CmmProc that:
  (a) Does a CmmUnsafeForeignCall to hs_hpc_module() — this generates proper
      C calling convention code (argument setup + CALL instruction)
  (b) Ends with a CmmCall (tail jump) to hs_hpc_return(), a trivial C function
      in the RTS that simply executes a RET instruction.

- An .init_array entry points to the registration function so that the
  dynamic linker calls it at program startup.

The tail-call trick works because:
- CmmUnsafeForeignCall generates: sub rsp (alignment), setup args, CALL, add rsp
- After the call, rsp is restored to its entry value
- CmmCall generates: JMP hs_hpc_return
- hs_hpc_return does: RET, which pops the return address pushed by the
  .init_array caller and returns to it

This eliminates the need for a per-module C compiler invocation for HPC.
-}

module GHC.StgToCmm.Hpc ( mkTickBox, initHpc ) where

import GHC.Prelude

import GHC.Platform

import GHC.Cmm
import GHC.Cmm.Graph
import qualified GHC.Cmm.Graph as CmmGraph
import GHC.Cmm.CLabel
import GHC.Cmm.Utils

import GHC.Data.FastString

import GHC.StgToCmm.Monad
import GHC.StgToCmm.Config
import GHC.StgToCmm.Lit ( newByteStringCLit )

import GHC.Types.Basic ( FunctionOrData(IsFunction) )
import GHC.Types.ForeignCall ( CCallConv(CCallConv) )
import GHC.Types.HpcInfo
import GHC.Cmm.Node ( CmmTickScope(GlobalScope) )

import GHC.Unit.Module

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8


mkTickBox :: Platform -> Module -> Int -> CmmAGraph
mkTickBox platform mod n
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64 NaturallyAligned
                                , CmmLit (CmmInt 1 W64)
                                ])
  where
    tick_box = cmmIndex platform W64
                        (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                        n

-- | Emit Cmm declarations for HPC: tick array, init function, and .init_array
-- entry.  See Note [HPC init via Cmm].
initHpc :: FCode ()
initHpc = do
  cfg <- getStgToCmmConfig
  let this_mod = stgToCmmThisModule cfg
      platform = stgToCmmPlatform cfg
  case stgToCmmHpcInfo cfg of
    NoHpcInfo {} -> return ()
    HpcInfo { hpcInfoTickCount = tickCount, hpcInfoHash = hashNo } -> do

      -- 1. Emit the tick array: StgWord64 ticks[tickCount] = {0, ...}
      let ticks_lbl = mkHpcTicksLabel this_mod
          zero64    = CmmInt 0 W64
          ticks_data = CmmData (Section Data ticks_lbl)
                               (CmmStaticsRaw ticks_lbl
                                 [ CmmStaticLit zero64 | _ <- [1..tickCount] ])
      emitDecl ticks_data

      -- 2. Emit the module name as a NUL-terminated string
      let mod_name = moduleNameString (moduleName this_mod)
          full_name
            | moduleUnit this_mod == mainUnit = mod_name
            | otherwise = unitString (moduleUnit this_mod) ++ "/" ++ mod_name
      name_lit <- newByteStringCLit (BS8.pack (full_name ++ "\0"))

      -- 3. Emit the init function that calls hs_hpc_module() and tail-calls
      --    hs_hpc_return()
      let init_lbl = mkInitializerStubLabel this_mod (fsLit "hpc")

          -- Foreign call target: hs_hpc_module(char*, StgWord32, StgWord32, StgWord64*)
          hs_hpc_module_lbl = mkForeignLabel (fsLit "hs_hpc_module")
                                Nothing ForeignLabelInExternalPackage IsFunction
          call_target = ForeignTarget (mkLblExpr hs_hpc_module_lbl)
                          (ForeignConvention CCallConv
                            [AddrHint, NoHint, NoHint, AddrHint]
                            []
                            CmmMayReturn)

          -- Arguments to hs_hpc_module
          args = [ CmmLit name_lit                        -- modName (char*)
                 , CmmLit (CmmInt (fromIntegral tickCount) (wordWidth platform))  -- modCount
                 , CmmLit (CmmInt (fromIntegral hashNo) (wordWidth platform))     -- modHashNo
                 , CmmLit (CmmLabel ticks_lbl)            -- tixArr (StgWord64*)
                 ]

          -- Tail-call target: hs_hpc_return()
          hs_hpc_return_lbl = mkForeignLabel (fsLit "hs_hpc_return")
                                Nothing ForeignLabelInExternalPackage IsFunction

          -- Build the procedure body:
          --   ccall hs_hpc_module(name, count, hash, arr);
          --   jump hs_hpc_return();
          body = mkUnsafeCall call_target [] args
                 CmmGraph.<*> mkLast (CmmCall { cml_target    = mkLblExpr hs_hpc_return_lbl
                                     , cml_cont      = Nothing
                                     , cml_args_regs = []
                                     , cml_args      = 0
                                     , cml_ret_args  = 0
                                     , cml_ret_off   = 0
                                     })

      -- Emit the procedure with no info table, no arguments, no stack layout
      emitProcWithStackFrame NativeDirectCall Nothing init_lbl [] []
                             (body, GlobalScope) False

      -- 4. Emit .init_array entry pointing to the init function
      let init_arr_lbl = mkInitializerArrayLabel this_mod
          init_array = CmmData (Section InitArray init_arr_lbl)
                               (CmmStaticsRaw init_arr_lbl
                                 [ CmmStaticLit (CmmLabel init_lbl) ])
      emitDecl init_array
