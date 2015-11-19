{-# LANGUAGE RecordWildCards, ScopedTypeVariables, ViewPatterns #-}
module Main (main) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Data.Maybe
import Data.List
import System.IO
import System.Directory
import System.Process (readProcess)
import System.FilePath

import Options.Applicative
import qualified Options.Applicative as O
import Data.Version

{-
  ___        _   _
 / _ \ _ __ | |_(_) ___  _ __  ___
| | | | '_ \| __| |/ _ \| '_ \/ __|
| |_| | |_) | |_| | (_) | | | \__ \
 \___/| .__/ \__|_|\___/|_| |_|___/
      |_|
-}

data Opts = Opts {
    cli_dirlist_file :: FilePath
  , cli_filelist_file :: FilePath
  , cli_hasktags_args1 :: String
  , cli_ghc_pkgs_args :: String
  , cli_use_stack :: Tristate
  , cli_use_sandbox :: Tristate
  , cli_hasktags_args2 :: [String]
  } deriving(Show)

data Tristate = ON | OFF | AUTO
  deriving(Eq, Ord, Show, Read)

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption (
        long "dir-list" <>
        short 'd' <>
        metavar "FILE" <>
        value "" <>
        help "File containing directory list to process" )
  <*> strOption (
        long "file-list" <>
        short 'f' <>
        metavar "FILE" <>
        value "" <>
        help "File containing Haskell sources to process" )
  <*> strOption (
        long "hasktags-args" <>
        metavar "OPTS" <>
        value "-c -x" <>
        help "Arguments to pass to hasktags")
  <*> strOption (
        long "ghc-pkg-args" <>
        metavar "OPTS" <>
        value "" <>
        help "Arguments to pass to ghc-pkgs")
  <*> option auto (
        long "use-stack" <>
        value AUTO <>
        help "Execute ghc-pkg via stack")
  <*> option auto (
        long "include-sandbox" <>
        value AUTO <>
        help "(!UNIMPLEMENTED!) Include .cabal-sandbox package databases")
  <*> many (argument str (metavar "OPTS" <> help "More hasktags options"))

exename, versionId :: String
exename = "haskdogs"
versionId = "0.4.0"

version :: Parser (a -> a)
version = infoOption (exename ++ " version " ++ versionId)
                     (long "version" <> help "Show version number")

opts = info (helper <*> version <*> optsParser)
      ( fullDesc <> header (exename ++ " - Recursive hasktags-based TAGS generator for a Haskell project" ))

{-
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

-}

main :: IO()
main = do

  Opts {..} <- execParser opts

  let
    -- Directory to unpack sources into
    getDataDir :: IO FilePath
    getDataDir = do
      x <- (</> ".haskdogs") <$> getHomeDirectory
      createDirectoryIfMissing False x
      return x

    cli_verbose = True

    vprint a
      | cli_verbose = eprint a
      | otherwise = return ()

    eprint = hPutStrLn stderr

    runp nm args inp = do
      vprint $ nm ++ " " ++ unwords args
      readProcess nm args inp

    -- Run GNU which tool
    checkapp :: String -> IO ()
    checkapp appname = do
      (runp "which" [appname] [] >> return ()) `onException`
        (eprint ("Please Install \"" ++ appname ++ "\" application"))

    hasapp :: String -> IO Bool
    hasapp appname = do
        vprint $ "Cheking for " ++ appname ++ " with GNU which"
        (runp "which" [appname] [] >> return True) `catch`
          (\(e::SomeException) -> vprint ("GNU which falied to find " ++ appname) >> return False)

  cwd <- getCurrentDirectory
  datadir <- getDataDir
  has_stack <- hasapp "stack"

  let

    readLinedFile f =
      lines <$> (hGetContents =<< (
        if (f=="-")
          then return stdin
          else openFile f ReadMode))

    readDirFile :: IO [FilePath]
    readDirFile
      | null cli_dirlist_file && null cli_filelist_file = return ["."]
      | null cli_dirlist_file = return []
      | otherwise = readLinedFile cli_dirlist_file

    readSourceFile :: IO [FilePath]
    readSourceFile
      | null cli_filelist_file = return []
      | otherwise = readLinedFile cli_filelist_file

    cli_hasktags_args = (words cli_hasktags_args1) ++ cli_hasktags_args2

    runp_ghc_pkgs args = go cli_use_stack where
      go ON = runp "stack" (["exec", "ghc-pkg", "--"] ++ args) []
      go OFF = runp "ghc-pkg" args []
      go AUTO = if has_stack then go ON else go OFF

    cabal_or_stack = go cli_use_stack where
      go ON = "stack"
      go OFF = "cabal"
      go AUTO = if has_stack then go ON else go OFF

    -- Finds *hs in dirs, but filter-out Setup.hs
    findSources :: [FilePath] -> IO [FilePath]
    findSources dirs =
      filter (not . isSuffixOf "Setup.hs") . lines <$>
      runp "find" (dirs ++ words "-type f -and ( -name *\\.hs -or -name *\\.lhs -or -name *\\.hsc )") []

    grepImports :: String -> Maybe String
    grepImports line = case words line of
        ("import":"qualified":x:_) -> Just x
        ("import":x:_) -> Just x
        _ -> Nothing

    -- Produces list of imported modules for file.hs given
    findModules :: [FilePath] -> IO [String]
    findModules files = (catMaybes . map grepImports . lines) <$> runp "cat" files []

    -- Maps import name to haskell package name
    iname2module :: String -> IO (Maybe String)
    iname2module iname = do
        mod <- (listToMaybe . words) <$> (runp_ghc_pkgs ["--simple-output", "find-module", iname])
        vprint $ "Import " ++ iname ++ " resolved to " ++ (fromMaybe "NULL" mod)
        return mod

    inames2modules :: [String] -> IO [FilePath]
    inames2modules is = nub . sort . catMaybes <$> forM (nub is) iname2module

    -- Unapcks haskel package to the sourcedir
    unpackModule :: FilePath -> IO (Maybe FilePath)
    unpackModule ((datadir</>) -> p) = do
        exists <- doesDirectoryExist (datadir</>p)
        case exists of
          True ->  do
            vprint $ "Already unpacked " ++ p
            return (Just p)
          False -> do
            bracket_ (setCurrentDirectory datadir) (setCurrentDirectory cwd) $
              ( runp cabal_or_stack ["unpack", p] [] >> return (Just p)
              ) `catch`
              (\(_ :: SomeException) ->
                eprint ("Can't unpack " ++ p) >> return Nothing
              )

    unpackModules :: [FilePath] -> IO [FilePath]
    unpackModules ms = catMaybes <$> mapM unpackModule ms

    gentags :: IO ()
    gentags = do
      checkapp "hasktags"
      files <- do
        dirs <- readDirFile
        ss_local <- (++) <$> readSourceFile <*> findSources dirs
        when (null ss_local) $ do
          fail $ "haskdogs were not able to find any sources in " <> (intercalate ", " dirs)
        ss_l1deps <- findModules ss_local >>= inames2modules >>= unpackModules >>= findSources
        return $ ss_local ++ ss_l1deps
      runp "hasktags" (cli_hasktags_args ++ files) []
      return ()

  {- _real_main_ -}
  gentags


