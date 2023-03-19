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

au VimLeave,VimSuspend * set guicursor=a:n-v-c:block-blinkon250-Cursor

" Plugins
filetype plugin indent on
call plug#begin()
Plug 'junegunn/goyo.vim'
Plug 'vim-airline/vim-airline'
Plug 'VebbNix/lf-vim'
call plug#end()

" Colorscheme
colorscheme default
set bg=light

" Syntax
syntax on

" Basics:
set title

" Indentation options:
set autoindent

" Clipboard
set clipboard=unnamedplus

" Search options:
set hlsearch
set ignorecase
set incsearch
set smartcase

" Text rendering options:
set encoding=utf-8
set linebreak
set wrap

" User interface:
set wildmenu
set number relativenumber
set noshowcmd

" Miscellaneous
set confirm
set conceallevel=2

" Mappings
	let mapleader = ","
	map <leader>g :Goyo <enter>
	map <leader>s :setlocal spell spelllang=en_us <enter>
	map <leader>c :!compiler "%" <enter>

" Change back to original colorscheme after leaving Goyo
	autocmd User GoyoLeave set bg=light

" Automatically read the file type after write
        autocmd BufWritePost * filetype detect

" Recompile suckless software automatically
"        autocmd BufWritePost config.h,config.def.h !sudo make clean install

" Restart dwmblocks automatically after compilation
"        autocmd BufWritePost blocks.h !sudo make clean install && {killall -q dwmblocks; setsid -f dwmblocks}

" Recompile LaTeX documents automatically
"        autocmd BufWritePost *\.tex !pdflatex "%"
"        autocmd BufWritePost *\.tex !xelatex "%"

" Automatically save folding
	autocmd BufWrite,VimLeave *\.md mkview
	autocmd BufRead *\.md silent loadview

" Colors and Theming
" 0 -> black
" 1 -> red  
" 2 -> green
" 3 -> yellow
" 4 -> blue 
" 5 -> magenta
" 6 -> cyan 
" 7 -> white
highlight Title            ctermfg=5    ctermbg=none  cterm=bold,underline
highlight Comment          ctermfg=4    ctermbg=none  cterm=bold
highlight Constant         ctermfg=1    ctermbg=none  cterm=none
highlight Special          ctermfg=4    ctermbg=none  cterm=bold
highlight Identifier       ctermfg=6    ctermbg=none  cterm=none
highlight PreProc          ctermfg=5    ctermbg=none  cterm=bold
highlight String           ctermfg=1    ctermbg=none  cterm=none
highlight Number           ctermfg=1    ctermbg=none  cterm=bold
highlight Function         ctermfg=6    ctermbg=none  cterm=none
highlight Visual           ctermfg=3    ctermbg=0     cterm=bold
highlight SpellBad         ctermfg=1    ctermbg=none  cterm=italic,underline
highlight SpellCap         ctermfg=4    ctermbg=none  cterm=italic,underline
highlight SpellRare        ctermfg=6    ctermbg=none  cterm=italic,underline
highlight SpellLocal       ctermfg=2    ctermbg=none  cterm=italic,underline
