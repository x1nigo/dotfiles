" Install the junegunn/vim plugin to manage plugins
if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

" Change cursor back after exiting vim/nvim:
au VimEnter,VimResume * set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
  \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
  \,sm:block-blinkwait175-blinkoff150-blinkon175

au VimLeave,VimSuspend * set guicursor=a:ver20

" Plugins
	filetype plugin indent on
	call plug#begin()
	Plug 'junegunn/goyo.vim'
	Plug 'vim-airline/vim-airline'
	Plug 'VebbNix/lf-vim'
	Plug 'ap/vim-css-color'
	call plug#end()

" Colorscheme
	colorscheme default
	set bg=light
	syntax on

" Some basics
	set title
	set autoindent
	set clipboard=unnamedplus
	set wildmode=longest,list,full
	set nohlsearch
	set ignorecase
	set incsearch
	set smartcase
	set encoding=utf-8
	set linebreak
	set wrap
	set number relativenumber
	set noshowcmd
	set conceallevel=2

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
	autocmd BufRead,BufNewFile Xresources,Xdefaults,xresources,xdefaults set filetype=xdefaults
" Remove any trailing whitespaces
	autocmd BufWritePre * :%s/\s\+$//e
" Recompile suckless programs when saving (Uncomment if you don't need this.)
	autocmd BufWritePost ~/.local/src/dwm/config.h,~/.local/src/st/config.h,~/.local/src/dmenu/config.h !sudo make clean install
" Restart dwmblocks automatically after compilation
        autocmd BufWritePost ~/.local/src/dwmblocks/config.h !sudo make clean install && {killall -q dwmblocks; setsid -f dwmblocks}
" Run xrdb whenever Xdefaults or Xresources are updated.
	autocmd BufWritePost Xresources,Xdefaults,xresources,xdefaults !xrdb %

" Colors and Theming
" 0:black 1:red  2:green 3:yellow 4:blue 5:magenta 6:cyan 7:white
hi Title		ctermfg=5	ctermbg=none	cterm=bold
hi Comment		ctermfg=4	ctermbg=none	cterm=none
hi Constant		ctermfg=1	ctermbg=none	cterm=none
hi Special		ctermfg=4	ctermbg=none	cterm=none
hi Identifier		ctermfg=6	ctermbg=none	cterm=none
hi PreProc		ctermfg=5	ctermbg=none	cterm=none
hi String		ctermfg=1	ctermbg=none	cterm=none
hi Number		ctermfg=1	ctermbg=none	cterm=none
hi Function		ctermfg=6	ctermbg=none	cterm=none
hi Visual		ctermfg=3	ctermbg=0	cterm=bold
hi SpellBad		ctermfg=1	ctermbg=none	cterm=italic,underline
hi SpellCap		ctermfg=4	ctermbg=none	cterm=italic,underline
hi SpellRare		ctermfg=6	ctermbg=none	cterm=italic,underline
hi SpellLocal		ctermfg=2	ctermbg=none	cterm=italic,underline
hi markdownCodeBlock 	ctermfg=5	ctermbg=none	cterm=none
