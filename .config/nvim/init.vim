" Install the junegunn/vim plugin to manage plugins

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

filetype plugin on
call plug#begin()
Plug 'junegunn/goyo.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'VebbNix/lf-vim'
Plug 'ap/vim-css-color'
Plug 'vimwiki/vimwiki'
call plug#end()

colorscheme default
set nocompatible
set bg=light
syntax on
let g:airline_theme='ravenpower'

set title
set autoindent
set clipboard=unnamedplus
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
" set number relativenumber

" Set Map leader
	let mapleader = ","
" Use goyo plugin to focus more while editing
	map <leader>g :Goyo <enter>
" Have a spell check to your document
	map <leader>s :setlocal spell spelllang=en_us <enter>
" Use a script to compile a specific file in a certain way
	map <leader>c :w! \| !compiler "%" <enter>
" Open the output of your file
	map <leader>a :silent !alchemize "%" <enter>
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Change back to original colorscheme after leaving Goyo
	autocmd User GoyoLeave set bg=light
" Automatically read the file type after write
        autocmd BufWritePost * filetype detect
" Read these particular files correctly:
	autocmd BufRead,BufNewFile *.tex set filetype=tex
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile .Xresources,.Xdefaults,xresources,xdefaults set filetype=xdefaults
" Remove any trailing whitespaces
	autocmd BufWritePre * :%s/\s\+$//e

" Recompile suckless programs when saving (Uncomment if you don't need this.)
	autocmd BufWritePost ~/.local/src/dwm/config.h,~/.local/src/st/config.h,~/.local/src/dmenu/config.h,~/.local/src/surf/config.h !sudo make install
" Restart dwmblocks automatically after compilation
        autocmd BufWritePost ~/.local/src/dwmblocks/config.h !doas make install && kill $(pgrep -x dwmblocks); setsid -f dwmblocks
" Restart dunst after its config file is updated
	autocmd BufWritePost ~/.config/dunst/dunstrc !kill $(pgrep -x dunst); dunst &
" Run xrdb whenever Xdefaults or Xresources are updated
	autocmd BufWritePost .Xresources,.Xdefaults,xresources,xdefaults !xrdb %

highlight Visual ctermfg=3 ctermbg=0 cterm=none
