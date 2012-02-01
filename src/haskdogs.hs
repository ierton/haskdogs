import HSH
import Data.List
import Control.Applicative
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import System.FilePath

find_in_dirs dirs p = ("find", dirs ++ ["-name", p])

ghc_pkg_find m = ("ghc-pkg", ["find-module", m])

cabal_unpack p = ("cabal", ["unpack", p])

-- Finds *hs in current dir, recursively
findSources :: [String] -> IO [String]
findSources [] = return []
findSources d = run $ find_in_dirs d "*hs"

-- Produces list of imported modules for file.hs given
findImports :: [String] -> IO [String]
findImports s = run $ catFrom s -|- extractImports

extractImports = nub . sort . filter (/=[]) . map (grepImports . words)

grepImports ("import":"qualified":x:_) = x
grepImports ("import":x:_) = x
grepImports _ = []

-- Maps import name to haskel package name
iname2module :: String -> IO String
iname2module m = run $ ghc_pkg_find m -|- egrep "^ +[a-zA-Z]" -|- map (head . words) -|- highver
    where highver [] = []
          highver s = last (lines s)

inames2modules :: [String] -> IO [String]
inames2modules is = forM is (iname2module) >>= return . nub . sort . filter (/=[])

testdir dir fyes fno = do
    ret <- run ("test",["-d", dir])
    case ret of
        ExitSuccess -> fyes
        _ -> fno

-- Unapcks haskel package to the sourcedir
unpackModule p = do
    srcdir <- sourcedir
    let fullpath = srcdir </> p
    testdir fullpath 
        (do 
            putStrLn $ "Already unpacked " ++ p
            return fullpath
        ) 
        (do 
            cd srcdir
            ec <- tryEC (runIO (cabal_unpack p))
            case ec of
                Left _ -> return ""
                Right _ -> return fullpath
        )

unpackModules ms = filter (/="") <$> mapM unpackModule ms

which :: String -> IO (String, IO (String,ExitCode))
which n = run ("which", [n])

checkapp appname = do
    (_,ec) <- which appname >>= return . snd >>= id
    case ec of
        ExitSuccess -> return ()
        _ -> do
            putStrLn $ "Please Install \"" ++ appname ++ "\" application"
            exitWith ec

-- Directory to unpack sources into
sourcedir = glob "~" >>= return . (</> ".haskdogs") . head

gentags dirs flags = do
    checkapp "cabal"
    checkapp "ghc-pkg"
    checkapp "hasktags"
    d <- sourcedir
    testdir d (return ()) (run ("mkdir",["-p",d]))
    files <- bracketCD "." $ do
        ss_local <- findSources dirs
        ss_l1deps <- findImports ss_local >>= inames2modules >>= unpackModules >>= findSources
        return $ ss_local ++ ss_l1deps
    runIO $ ("hasktags", flags ++ files)

help = do
    putStrLn "haskdogs: generates tags file for haskell project directory"
    putStrLn "Usage:"
    putStrLn "    haskdogs [FLAGS]"
    putStrLn "        FLAGS will be passed to hasktags as-is followed by"
    putStrLn "        a list of files. Defaults to -c."
    return ()

amain [] = gentags ["."] ["-c"]
amain ("-d" : dirfile : flags) = do
    file <- if (dirfile=="-") then return stdin else openFile dirfile ReadMode
    dirs <- lines <$> hGetContents file
    gentags dirs $ ["-c"] ++ flags
amain ("-h":_) = help
amain ("-?":_) = help
amain ("--help":_) = help
amain flags = gentags ["."] flags

main :: IO()
main = getArgs >>= amain

