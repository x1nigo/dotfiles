if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
Plug 'vim-airline/vim-airline'
Plug 'ap/vim-css-color'
call plug#end()

" General
	syntax on
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
	set number relativenumber
	set viewoptions-=options

" Colorscheme
	colorscheme vim
	set bg=light
" Set this to enable your terminal's actual colors. Otherwise, rely on vim's colorscheme.
	set notermguicolors

" Custom Colors (set notermguicolors to enable); Enter ":hi" or ":highlight" for more information.
"	hi Title ctermfg=5 ctermbg=none cterm=none
	hi LineNr ctermfg=3 ctermbg=none cterm=none
	hi Comment ctermfg=4 ctermbg=none cterm=none
	hi Statement ctermfg=3 ctermbg=none cterm=none

" Set Map leader
	let mapleader = ","
	map <leader>, /<++><enter>
" Have a spell check to your document
	map <leader>s :setlocal spell spelllang=en_us <enter>
" Use a script to compile a specific file in a certain way
	map <leader>c :w! \| !compiler "%" <enter>
" Open the output of your file
	map <leader>a :silent !alchemize "%" <enter>
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Save state on exit
	" autocmd BufWinLeave *.* mkview
	" autocmd BufWinEnter *.* silent! loadview

" Automatically read the file type after write
	autocmd BufWritePost * filetype detect
" Read these particular files correctly:
	autocmd BufRead,BufNewFile *.tex set filetype=tex
	autocmd BufWinLeave *.tex !rm -f *.log *.aux
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile .Xresources,.Xdefaults,xresources,xdefaults set filetype=xdefaults
" Remove any trailing whitespaces
	autocmd BufWritePre * :%s/\s\+$//e

" Recompile suckless programs when saving (Uncomment if you don't need this.)
	autocmd BufWritePost ~/.local/src/dwm/config.h,~/.local/src/st/config.h,~/.local/src/dmenu/config.h,~/.local/src/dwmblocks/blocks.h !sudo make install
" Restart dunst after its config file is updated
	autocmd BufWritePost ~/.config/dunst/dunstrc !killall -q dunst; setsid dunst >/dev/null 2>&1
" Run xrdb whenever Xdefaults or Xresources are updated
	autocmd BufWritePost .Xresources,.Xdefaults,xresources,xdefaults !xrdb %
