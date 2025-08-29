{-# LANGUAGE BlockArguments #-}

-- | External ("system") linker
module GHC.Linker.External
  ( LinkerConfig(..)
  , runLink
  )
where

import qualified System.Directory as SysDir
import System.FilePath (pathSeparator, takeFileName, dropFileName)
import Control.Monad (filterM, (>=>))
import qualified Data.Set as Set
import Data.List (uncons, intercalate, isPrefixOf, (\\))
import Data.Maybe (isJust)
import GHC.Prelude
import GHC.Utils.TmpFs
import GHC.Utils.Logger
import GHC.Utils.Error
import GHC.Utils.CliOption
import GHC.SysTools.Process
import GHC.Linker.Config
import System.IO (hPutStrLn, stderr)

-- some options are intended for GCC and others
-- will be passed through to the linker
data DirectedOption = GccOption Option
                    | LdOption Option
  deriving ( Eq, Show )

data LinkOption = GccL String
                | RPath String
                | RPathLink String
                | OtherLinkOption DirectedOption
  deriving ( Eq, Show )

asLinkOptions :: [Option] -> [LinkOption]
asLinkOptions = directedOptionsAsLinkOptions . asDirectedOptions
  where
    asDirectedOptions :: [Option] -> [DirectedOption]
    asDirectedOptions opts = case uncons opts of
      Just (Option "-Xlinker", opt : rest) -> LdOption opt : asDirectedOptions rest
      Just (opt, rest) -> GccOption opt : asDirectedOptions rest
      Nothing -> []

    directedOptionsAsLinkOptions :: [DirectedOption] -> [LinkOption]
    directedOptionsAsLinkOptions dopts = case uncons dopts of
      Just (LdOption (Option "-rpath"), LdOption (Option path) : rest) ->
        RPath path : directedOptionsAsLinkOptions rest

      Just (LdOption (Option "-rpath-link"), LdOption (Option path) : rest) ->
        RPathLink path : directedOptionsAsLinkOptions rest

      Just (GccOption (FileOption "-L" path), rest) ->
        GccL path : directedOptionsAsLinkOptions rest

      Just (GccOption (Option s), rest) ->
        if "-L" `isPrefixOf` s
        then GccL (drop 2 s) : directedOptionsAsLinkOptions rest
        else OtherLinkOption (GccOption (Option s)) : directedOptionsAsLinkOptions rest

      Just (dopt, rest) -> OtherLinkOption dopt : directedOptionsAsLinkOptions rest

      Nothing -> []

renderLinkOptions :: [LinkOption] -> [Option]
renderLinkOptions = renderDirectedOptions . renderLinkOptionsToDirectedOptions
  where
    renderLinkOptionToDirectedOptions :: LinkOption -> [DirectedOption]
    renderLinkOptionToDirectedOptions (GccL s) = [GccOption (Option ("-L" ++ s))]
    renderLinkOptionToDirectedOptions (RPath s) = [LdOption (Option "-rpath"), LdOption (Option s)]
    renderLinkOptionToDirectedOptions (RPathLink s) = [LdOption (Option "-rpath-link"), LdOption (Option s)]
    renderLinkOptionToDirectedOptions (OtherLinkOption dopt) = [dopt]

    renderLinkOptionsToDirectedOptions :: [LinkOption] -> [DirectedOption]
    renderLinkOptionsToDirectedOptions lopts = mconcat $ renderLinkOptionToDirectedOptions <$> lopts

    renderDirectedOption :: DirectedOption -> [Option]
    renderDirectedOption dopt = case dopt of
      GccOption o -> [o]
      LdOption o -> [Option "-Xlinker", o]

    renderDirectedOptions :: [DirectedOption] -> [Option]
    renderDirectedOptions dopts = mconcat $ renderDirectedOption <$> dopts

-- | Run the external linker
runLink :: Logger -> TmpFs -> LinkerConfig -> [Option] -> IO ()
runLink logger tmpfs cfg args = traceSystoolCommand logger "linker" $ do
  let all_args = linkerOptionsPre cfg ++ args ++ linkerOptionsPost cfg

  -- on Windows, mangle environment variables to account for a bug in Windows
  -- Vista
  mb_env <- getGccEnv all_args
  modified_opts <- if argStringLength all_args > 1000*1000
                   then consolidateOptions logger tmpfs cfg all_args
                   else pure all_args

  hPutStrLn stderr "original link options:"
  let original_option_strings = show <$> asLinkOptions all_args
      original_option_log_string = intercalate "\n" original_option_strings
  hPutStrLn stderr original_option_log_string
  hPutStrLn stderr "^^^ original link options"

  hPutStrLn stderr "modified link options:"
  let option_strings = show <$> asLinkOptions modified_opts
      option_log_string = intercalate "\n" option_strings
  hPutStrLn stderr option_log_string
  hPutStrLn stderr "^^^ modified link options"

  runSomethingResponseFile logger tmpfs (linkerTempDir cfg) (linkerFilter cfg)
    "Linker" (linkerProgram cfg) modified_opts mb_env
  where
    argStringLength :: [Option] -> Int
    argStringLength opts = sum (length . showOpt <$> opts) + length opts

consolidateOptions :: Logger -> TmpFs -> LinkerConfig -> [Option] -> IO [Option]
consolidateOptions logger tmpfs cfg args = do
  let
    link_opts = asLinkOptions args
    lib_path_opts = filter isLibPathOpt link_opts
    consolidatable_opts = filter hasAbsolutePath lib_path_opts
    nonconsolidatable_opts = link_opts \\ consolidatable_opts
    maybe_path_list = sequence $ filter isJust (libPathOptFilePath <$> consolidatable_opts)
    consolidatable_lib_paths = case maybe_path_list of
                                 Just l -> l
                                 Nothing -> []
  (TempDir consolidated_lib_dir) <- makeConsolidatedLibDir logger tmpfs (linkerTempDir cfg) consolidatable_lib_paths
  let new_lib_path_opts = [ GccL consolidated_lib_dir
                          , RPath consolidated_lib_dir
                          ]
  pure $ renderLinkOptions (new_lib_path_opts ++ nonconsolidatable_opts)
  where
    isLibPathOpt :: LinkOption -> Bool
    isLibPathOpt (OtherLinkOption _) = False
    isLibPathOpt _ = True

    hasAbsolutePath :: LinkOption -> Bool
    hasAbsolutePath (GccL path) = not . isRelative $ path
    hasAbsolutePath (RPath path) = not . isRelative $ path
    hasAbsolutePath (RPathLink path) = not . isRelative $ path
    hasAbsolutePath _ = False

    libPathOptFilePath :: LinkOption -> Maybe FilePath
    libPathOptFilePath (GccL path) = Just path
    libPathOptFilePath (RPath path) = Just path
    libPathOptFilePath (RPathLink path) = Just path
    libPathOptFilePath _ = Nothing

makeConsolidatedLibDir :: Logger -> TmpFs -> TempDir -> [FilePath] -> IO TempDir
makeConsolidatedLibDir logger tmpfs temp_dir paths = do
  let dedupedPaths = Set.toList $ Set.fromList paths
  libdir <- newTempSubDir logger tmpfs temp_dir
  dirPaths <- filterM SysDir.doesDirectoryExist dedupedPaths
  resolvedDirPaths <- mapM resolvePath dirPaths
  contents <- mconcat <$> mapM filesInDir resolvedDirPaths
  linkable_files <- filterM (SysDir.doesDirectoryExist >=> pure . not) contents
  symlinks <- mapM (createSymlinkInDir libdir) linkable_files
  addFilesToClean tmpfs TFL_GhcSession symlinks
  pure $ TempDir libdir
  where
    filesInDir :: FilePath -> IO [FilePath]
    filesInDir dir = do
      fps <- SysDir.listDirectory dir
      pure $ (\fp -> dir ++ (pathSeparator : fp)) <$> fps

createSymlinkInDir :: FilePath -> FilePath -> IO FilePath
createSymlinkInDir dest_dir target = do
  let link_name = dest_dir ++ (pathSeparator : takeFileName target)
  alreadyExists <- SysDir.doesPathExist link_name
  if alreadyExists
    then pure link_name
    else do
      resolved_target <- resolvePath target
      SysDir.createFileLink resolved_target link_name
      hPutStrLn stderr $ mconcat [ "creating symlink ("
                                 , link_name
                                 , " -> "
                                 , resolved_target
                                 , ")"
                                 ]
      pure link_name

isRelative :: FilePath -> Bool
isRelative (c : _) = pathSeparator /= c
isRelative _ = False

resolvePath :: FilePath -> IO FilePath
resolvePath fp = do
  isSymlink <- SysDir.pathIsSymbolicLink fp
  if isSymlink
  then do
    linkDest <- SysDir.getSymbolicLinkTarget fp
    let target = if isRelative linkDest
                 then dropFileName fp ++ linkDest
                 else linkDest
    resolvePath target
  else pure fp
