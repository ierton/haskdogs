import HSH
import Data.List
import Control.Monad
import System.Exit
import System.Environment
import System.FilePath

find_in_dirs dirs p = ("find", dirs ++ ["-name", p])

ghc_pkg_find m = ("ghc-pkg", ["find-module", m])

cabal_unpack p = ("cabal", ["unpack", p])

-- Finds *hs in current dir, recursively
findSources :: [String] -> IO [String]
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
                Left _ -> return []
                Right _ -> return fullpath
        )

unpackModules ms = mapM unpackModule ms >>= return . filter (/=[])

-- Directory to unpack sources into
sourcedir = glob "~" >>= return . (</> ".haskdogs") . head

gentags flags = do
    d <- sourcedir
    testdir d (return ()) (run ("mkdir",["-p",d]))
    files <- bracketCD "." $ do
        ss_local <- findSources ["."]
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

amain [] = gentags ["-c"]
amain ("-h":_) = help
amain ("-?":_) = help
amain ("--help":_) = help
amain flags = gentags flags

main :: IO()
main = getArgs >>= amain

