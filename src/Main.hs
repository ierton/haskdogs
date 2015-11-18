{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, NoImplicitPrelude, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative
import ClassyPrelude.Conduit hiding ((<>), (<$>))
import Data.Conduit.Shell (run, proc, conduit, ($|), Segment, ProcessException(..), CmdArg(..))
import Data.Conduit.Shell.PATH (test, cd, which, mkdir)
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (nub)
import System.IO (openFile, IOMode(..))
import System.Directory (getHomeDirectory, getCurrentDirectory)

import Options.Applicative
import qualified Options.Applicative as O
import Text.Printf

import Data.Version

default (ByteString)

{-
  ___        _   _
 / _ \ _ __ | |_(_) ___  _ __  ___
| | | | '_ \| __| |/ _ \| '_ \/ __|
| |_| | |_) | |_| | (_) | | | \__ \
 \___/| .__/ \__|_|\___/|_| |_|___/
      |_|
-}

data Opts = Opts {
    cli_verbose :: Bool
  , cli_dirlist_file :: FilePath
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
  <$> flag False True (
        long "verbose" <>
        short 'v' <>
        help "Be verbose" )
  <*> strOption (
        long "dir-list" <>
        short 'd' <>
        metavar "FILE" <>
        value "" <>
        help "File containing directory list to process" )
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
version = infoOption (printf "%s version %s" exename versionId)
                     (long "version" <> help "Show version number")

opts = info (helper <*> version <*> optsParser)
      ( fullDesc
     <> header (exename ++ " - Recursive hasktags-based TAGS generator for a Haskell project" ))

data RuntimeChecks = RTC {
    chk_has_stack :: Bool
  , chk_has_cabal :: Bool
  } deriving(Show)

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

    vprint  :: (IOData a, MonadIO m) => a -> m ()
    vprint a
      | cli_verbose = eprint a
      | otherwise = return ()

    eprint :: (IOData a, MonadIO m) => a -> m ()
    eprint = hPutStrLn stderr

    -- Run GNU which tool
    checkapp :: String -> IO ()
    checkapp appname = do
      run (which appname) `onException`
        eprint ("Please Install \"" ++ appname ++ "\" application")

    hasapp :: String -> IO Bool
    hasapp appname = do
        vprint $ "Cheking for " ++ appname ++ " with GNU which"
        (run (which appname) >> return True) `catch`
          (\(e::SomeException) -> vprint ("GNU which falied to find " ++ appname) >> return False)

  RTC {..} <-  (RTC <$> (hasapp "stack") <*> (hasapp "cabal"))

  let

    readDirFile :: IO [FilePath]
    readDirFile
      | null cli_dirlist_file = return ["."]
      | otherwise =
          lines <$> (hGetContents =<< (
            if (cli_dirlist_file=="-")
              then return stdin
              else openFile cli_dirlist_file ReadMode))

    cli_hasktags_args = (words cli_hasktags_args1) ++ cli_hasktags_args2

    proc' :: CmdArg a => String -> [a] -> Segment ()
    proc' a l = proc a (map (unpack . toTextArg) l)

    ghc_pkgs :: (CmdArg a, IsString a) => [a] -> Segment ()
    ghc_pkgs args = hm cli_use_stack where
      hm ON = proc' "stack" (["exec", "ghc-pkg", "--"] ++ args)
      hm OFF = proc' "ghc-pkg" args
      hm AUTO = if chk_has_stack then hm ON else hm OFF

    cabal_or_stack = hm cli_use_stack where
      hm ON = "stack"
      hm OFF = "cabal"
      hm AUTO = if chk_has_stack then hm ON else hm OFF

    runOutput :: Segment () -> IO L.ByteString
    runOutput cmd = do
        chunks <- run (cmd $| conduit C.consume)
        return $ L.fromChunks chunks

    -- Finds *hs in dirs, but filter-out Setup.hs
    findSources :: [FilePath] -> IO [FilePath]
    findSources dirs = do
        x <- run (proc_find $| conduit (C.lines =$= C.map B.unpack =$= C.consume))
        return (filter (not . isSuffixOf "Setup.hs") x)
      where
        proc_find = proc' "find" $ dirs <> words "-type f -and ( -name *\\.hs -or -name *\\.lhs )"

    grepImports :: ByteString -> Maybe ByteString
    grepImports line = case B.words line of
        ("import":"qualified":x:_) -> Just x
        ("import":x:_) -> Just x
        _ -> Nothing

    -- Produces list of imported modules for file.hs given
    findModules :: [FilePath] -> IO [ByteString]
    findModules files = do
        runResourceT $ forM_ files sourceFile $$
                 ( C.lines
               =$= C.mapMaybe grepImports
               =$= C.consume
                 )

    -- Maps import name to haskell package name
    iname2module :: ByteString -> IO (Maybe ByteString)
    iname2module name = do
        out <- runOutput $
            ghc_pkgs ["--simple-output", "find-module", name]
        let x = do -- Maybe monad
            l <- lastMay (L.lines out)
            lbs <- headMay (L.words l)
            return $ L.toStrict lbs
        vprint $ "Import " ++ B.unpack name ++ " resolved to " ++ (maybe "NULL" B.unpack x)
        return x

    inames2modules :: [ByteString] -> IO [FilePath]
    inames2modules is = map B.unpack . nub . sort . catMaybes <$> forM is (iname2module)

    testdir :: FilePath -> IO a -> IO a -> IO a
    testdir dir fyes fno = do
        (ret :: Either ProcessException ()) <- try $ run $ test "-d" dir
        either (const fno) (const fyes) ret

    -- Unapcks haskel package to the sourcedir
    unpackModule :: FilePath -> IO (Maybe FilePath)
    unpackModule p = do
        srcdir <- sourcedir
        let fullpath = srcdir </> p
        testdir fullpath
            (do
                vprint $ "Already unpacked " ++ p
                return (Just fullpath)
            )
            (do
                cd srcdir
                (run (proc' cabal_or_stack ["unpack", p]) >> return (Just fullpath)) `catch` (\
                  (e :: ProcessException) -> do
                    eprint ("Can't unpack " ++ p)
                    return Nothing)
                <*
                (cd srcdir)
            )

    unpackModules :: [FilePath] -> IO [FilePath]
    unpackModules ms = catMaybes <$> mapM unpackModule ms


    -- Directory to unpack sources into
    sourcedir :: IO FilePath
    sourcedir = do
        home <- getHomeDirectory
        return $ home </> ".haskdogs"

    gentags :: IO ()
    gentags = do
      checkapp "hasktags"
      dirs <- readDirFile
      d <- sourcedir
      testdir d (return ()) (run $ mkdir "-p" d)
      files <- do
        ss_local <- findSources dirs
        when (null ss_local) $ do
          fail $ "haskdogs were not able to find any sources in " <> (intercalate ", " dirs)
        ss_l1deps <- findModules ss_local >>= inames2modules >>= unpackModules >>= findSources
        return $ ss_local ++ ss_l1deps
      run $ proc' "hasktags" (cli_hasktags_args ++ files)

  {- _real_main_ -}
  gentags


