-- #!/usr/bin/runhaskell

import HSH
import Data.List
import Control.Monad
import System.Exit
import System.FilePath

p :: String -> (String, [String])
p s = let ws = words s in (head ws, tail ws)

find_in_dirs dirs p = ("find", dirs ++ ["-name", p])

ghc_pkg_find m = ("ghc-pkg", ["find-module", m])

cabal_unpack p = ("cabal", ["unpack", p])

findSources :: [String] -> IO [String]
findSources d = run $ find_in_dirs d "*hs"

findImports :: [String] -> IO [String]
findImports s = run $ catFrom s -|- extractImports

extractImports = nub . sort . filter (/=[]) . map (grepImports . words)

grepImports ("import":"qualified":x:_) = x
grepImports ("import":x:_) = x
grepImports _ = []

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

main :: IO ()
main = do
    checkapp "cabal"
    checkapp "ghc-pkg"
    checkapp "hasktags"
    d <- sourcedir
    testdir d (return ()) (run ("mkdir",["-p",d]))
    cwd <- run "pwd" >>= return . head
    ss_local <- findSources ["."]
    ss_l1deps <- findImports ss_local >>= inames2modules >>= unpackModules >>= findSources
    cd cwd
    runIO $ ("hasktags", ["-c"] ++ ss_local ++ ss_l1deps)


