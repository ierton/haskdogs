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
commands such as 'cd', 'mkdir' and so on. Also it would run 'stack' and ghc-pkg'
in order to obtain package information.

INSTALL
-------

Check the dependencies. Currently they are: stack, hasktags, GNU find,
which and shell.

Please follow stack's documentation(https://github.com/commercialhaskell/stack) to install stack.

	$ stack install hasktags haskdogs

Make sure that PATH contains path to your stack binaries directory ($HOME/.local/bin by default).

RUNNING
-------

1. Make sure yoy have installed hasktags and put it in PATH.

2. cd to your Haskell project dir

	$ cd $HOME/my-haskell-project

3. Run haskdogs without arguments to generate tags file in Vim-compatible format

	$ haskdogs

Emacs users would probably want to add -e option to build Emacs-compatible TAGS.

VIM HINT
--------

Hasdogs (and underlying Hasktags) use simple scanning algorithm so it may become
confused facing functions with identical names. In this case Hasktags includes
all of them in the output file so user has to decide which tag to jump to. Vim
offers :tag and :ts commands to deal with such situations but it is somewhat
cumbersome to type them every time.

To speedup things a bit I use the following vim binding. It iterates over all
same tags quickly with just one C-] command.

    " Cyclic tag navigation {{{
	let g:rt_cw = ''
	function! RT()
		let cw = expand('<cword>')
		try
			if cw != g:rt_cw
				execute 'tag ' . cw
				call search(cw,'c',line('.'))
			else
				try
					execute 'tnext'
				catch /.*/
					execute 'trewind'
				endtry
				call search(cw,'c',line('.'))
			endif
			let g:rt_cw = cw
		catch /.*/
			echo "no tags on " . cw
		endtry
	endfunction
	map <C-]> :call RT()<CR>
    " }}}

Just copy the code above to your ~/.vimrc and reload the vim.

--
Sergey
<grrwlf@gmail.com>


