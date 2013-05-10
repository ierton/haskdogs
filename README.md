HaskDogs
========

Haskdogs is a shellscript-like tool which creates tag file for entire
haskell project directory. It takes into account first-level dependencies by
recursively scanning imports and adding matching packages to the final
tag list.

As a result, programmer can use his/her text editor supporting tags (vim, for
example) to jump directly to definition of any standard or foreign function
he/she uses.

Note, that haskdogs relies on some GNU programs as well as on Unix shell
commands such as 'cd', 'mkdir' and so on. Also it would run 'cabal' and ghc-pkg'
in order to obtain package information.

INSTALL
-------

Check the dependencies. Currently they are: cabal, ghc, hasktags, GNU find,
which and shell.

0. cabal install hasktags haskdogs
1. git clone https://github.com/ierton/haskdogs
2. cd haskdogs
3. cabal configure && cabal install
4. export PATH="$HOME/.cabal/bin:$PATH"

RUNNING
-------

1. Make sure yoy have installed hasktags and put it in PATH.

2. cd to your Haskell project dir

    $ cd $HOME/my-haskell-project

3. run haskdogs (cmdline args will be passed to hasktags followed by a filelist generated)

    $ haskdogs


--
Sergey 
<ierton@gmail.com>


