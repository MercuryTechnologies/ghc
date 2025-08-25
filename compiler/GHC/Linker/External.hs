-- | External ("system") linker
module GHC.Linker.External
  ( LinkerConfig(..)
  , runLink
  )
where

import qualified System.Directory as SysDir
import System.FilePath (pathSeparator, takeFileName)
import Control.Monad (filterM, (>=>))
import qualified Data.Set as Set
import Data.List (uncons)
import Data.Maybe (isJust)
import GHC.Prelude
import GHC.Utils.TmpFs
import GHC.Utils.Logger
import GHC.Utils.Error
import GHC.Utils.CliOption
import GHC.SysTools.Process
import GHC.Linker.Config

-- | Run the external linker
runLink :: Logger -> TmpFs -> LinkerConfig -> [Option] -> IO ()
runLink logger tmpfs cfg args = traceSystoolCommand logger "linker" $ do
  let all_args = linkerOptionsPre cfg ++ args ++ linkerOptionsPost cfg

  -- on Windows, mangle environment variables to account for a bug in Windows
  -- Vista
  mb_env <- getGccEnv all_args

  let gcc_lib_path_opts = filter isGccLibPathOpt (asLinkOptions all_args)
      non_lib_path_opts = removeAllLibPathOpts (asLinkOptions all_args)
      maybe_lib_path_list = sequence $ filter isJust (gccLibPathOptFilePath <$> gcc_lib_path_opts)
      lib_paths = case maybe_lib_path_list of
        Just l -> Set.toList $ Set.fromList l
        Nothing -> [] -- this isn't actually reachable

  (TempDir consolidated_lib_dir) <- makeConsolidatedLibDir logger tmpfs (linkerTempDir cfg) lib_paths
  let new_lib_path_opts = [ GccOption (FileOption "-L" consolidated_lib_dir)
                          , LdOption (Option "-rpath")
                          , LdOption (Option consolidated_lib_dir)
                          ]

  let modified_opts = new_lib_path_opts ++ non_lib_path_opts
  runSomethingResponseFile logger tmpfs (linkerTempDir cfg) (linkerFilter cfg)
    "Linker" (linkerProgram cfg) (renderLinkOptions modified_opts) mb_env

data LinkOption = GccOption Option
                | LdOption Option
  deriving ( Eq )

isGccLibPathOpt :: LinkOption -> Bool
isGccLibPathOpt opt = case opt of
  GccOption (FileOption "-L" _ ) -> True
  _ -> False

removeAllLibPathOpts :: [LinkOption] -> [LinkOption]
removeAllLibPathOpts opts = case uncons opts of
  Just (LdOption (Option "-rpath"), _ : rest) -> rest
  Just (LdOption (Option "-rpath-link"), _ : rest) -> rest
  Just (GccOption (FileOption "-L" _), rest) -> rest
  Just (opt, rest) -> opt : removeAllLibPathOpts rest
  Nothing -> []

gccLibPathOptFilePath :: LinkOption -> Maybe String
gccLibPathOptFilePath opt = case opt of
  GccOption (FileOption _ path) -> Just path
  _ -> Nothing

asLinkOptions :: [Option] -> [LinkOption]
asLinkOptions opts = case uncons opts of
  Just (Option "-Xlinker", opt : rest) -> (LdOption opt) : asLinkOptions rest
  Just (opt, rest) -> (GccOption opt) : asLinkOptions rest
  Nothing -> []

makeConsolidatedLibDir :: Logger -> TmpFs -> TempDir -> [FilePath] -> IO TempDir
makeConsolidatedLibDir logger tmpfs temp_dir paths = do
  libdir <- newTempSubDir logger tmpfs temp_dir
  contents <- mconcat <$> sequence (fmap SysDir.listDirectory paths)
  linkable_files <- filterM (SysDir.doesDirectoryExist >=> pure . not) contents
  symlinks <- mapM (createSymlinkInDir libdir) linkable_files
  addFilesToClean tmpfs TFL_GhcSession symlinks -- can this be TFL_CurrentModule?
  pure $ TempDir libdir

createSymlinkInDir :: FilePath -> FilePath -> IO FilePath
createSymlinkInDir dest_dir target = do
  let link_name = dest_dir ++ (pathSeparator : takeFileName target)
  resolved_target <- resolvePath target
  SysDir.createFileLink resolved_target link_name
  pure link_name

resolvePath :: FilePath -> IO FilePath
resolvePath fp = do
  isSymlink <- SysDir.pathIsSymbolicLink fp
  case isSymlink of
    True -> SysDir.getSymbolicLinkTarget fp >>= resolvePath
    False -> pure fp

renderLinkOptions :: [LinkOption] -> [Option]
renderLinkOptions opts = mconcat (fmap renderLinkOption opts)

renderLinkOption :: LinkOption -> [Option]
renderLinkOption opt = case opt of
  GccOption o -> [o]
  LdOption o -> [Option "-Xlinker", o]
