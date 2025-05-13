if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
Plug 'vim-airline/vim-airline'
call plug#end()

" General
	syntax on
	set notermguicolors
	set title
	set autoindent
	set clipboard+=unnamedplus
	set wildmode=longest,list,full
	set nohlsearch
	set ignorecase
	set tabstop=4
	set softtabstop=4
	set shiftwidth=4
	set incsearch
	set smartcase
	set encoding=utf-8
	set linebreak
	set wrap
	set noshowcmd
	set cursorline
	set number relativenumber
	set viewoptions-=options
	set cursorline

" Colors back in the old days...
	hi Normal ctermbg=none guibg=none
	hi Visual		ctermfg=3	ctermbg=0	cterm=bold
	hi Comment		ctermfg=4	ctermbg=none	cterm=none
	hi Type			ctermfg=2	ctermbg=none	cterm=bold
	hi Title		ctermfg=5	ctermbg=none	cterm=bold
	hi PreProc		ctermfg=5	ctermbg=none	cterm=italic
	hi Statement	ctermfg=3	ctermbg=none	cterm=none
	hi LineNr		ctermfg=0	ctermbg=none	cterm=none
	hi Constant		ctermfg=1	ctermbg=none	cterm=none
	hi CursorLineNr	ctermfg=3	ctermbg=none	cterm=none
	hi String		ctermfg=1	ctermbg=none	cterm=none
	hi Identifier	ctermfg=6	ctermbg=none	cterm=none
	hi Todo			ctermfg=0	ctermbg=3	cterm=none
	hi markdownCodeBlock	ctermfg=5	ctermbg=none	cterm=none

" Set Map leader
	let mapleader = ","
" Have a spell check to your document
	map <leader>s :setlocal spell spelllang=en_us <enter>
" Use a script to compile a specific file in a certain way
	map <leader>c :w! \| !compiler "%" <enter>
" Open the output of your file
	map <leader>a :silent !alchemize "%" <enter>
" Format groff documents
	map <leader>g :silent !fmtgroff "%" <enter>
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Save state on exit
	" autocmd BufWinLeave *.* mkview
	" autocmd BufWinEnter *.* silent! loadview

" Automatically read the file type after write
	autocmd BufWritePost * filetype detect
" Read these particular files correctly:
	autocmd BufRead,BufNewFile *.tex set filetype=tex
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile .Xresources,.Xdefaults,xresources,xdefaults set filetype=xdefaults
" Remove any trailing whitespaces
	autocmd BufWritePre * :%s/\s\+$//e

" Recompile suckless programs when saving (Uncomment if you don't need this.)
	autocmd BufWritePost ~/.local/src/dwm/config.h,~/.local/src/st/config.h,~/.local/src/dmenu/config.h,~/.local/src/dwmblocks/config.h !sudo make install
	" autocmd BufWritePost ~/.local/src/dwmblocks/config.h !cd ~/.local/src/dwmblocks/; sudo make install && { pkill -x dwmblocks; setsid -f dwmblocks }
" Restart dunst after its config file is updated
	autocmd BufWritePost ~/.config/dunst/dunstrc !kill $(pidof dunst) && setsid dunst
" Run xrdb whenever Xdefaults or Xresources are updated
	autocmd BufWritePost .Xresources,.Xdefaults,xresources,xdefaults !xrdb %
