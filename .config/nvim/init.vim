" install vim-airline if not yet already:
if !empty(glob('$HOME/.config/nvim/bundle/vim-airline'))
else
        echom "Installing vim-airline...(1/2)"
        silent !git clone https://github.com/vim-airline/vim-airline/ ~/.config/nvim/bundle/vim-airline
endif

if !empty(glob('$HOME/.config/nvim/autoload/pathogen.vim'))
else
        echom "Installing vim-airline...(2/2)"
        silent !mkdir -p ~/.config/nvim/autoload ~/.config/nvim/bundle && curl -LSso ~/.config/nvim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
endif

" change cursor back after exiting vim/nvim:
au VimEnter,VimResume * set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
  \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
  \,sm:block-blinkwait175-blinkoff150-blinkon175

au VimLeave,VimSuspend * set guicursor=a:n-v-c:block-blinkon250-Cursor

" syntax and plugins:
execute pathogen#infect()
syntax on
filetype plugin indent on
colorscheme default

" basics:
set title
set bg=light

" indentation options:
set autoindent
set expandtab

" search options:
set hlsearch
set ignorecase
set incsearch
set smartcase

" text rendering options:
set encoding=utf-8
set linebreak
set wrap

" user interface:
set wildmenu
set number relativenumber
set noshowcmd

" code folding:
set foldmethod=manual
set foldnestmax=5

" miscellaneous
set confirm

" Automatically read the file type after write
        autocmd BufWritePost * filetype detect

" Recompile suckless software automatically
        autocmd BufWritePost config.h,config.def.h !sudo make clean install

" Restart dwmblocks automatically after compilation
        autocmd BufWritePost blocks.h !sudo make clean install && {killall -q dwmblocks; setsid -f dwmblocks}

" Recompile LaTeX documents automatically
"        autocmd BufWritePost *\.tex !pdflatex "%"
        autocmd BufWritePost *\.tex !xelatex "%"

" Use compiler script for other programs
"        autocmd BufWritePost *\.h,*\.ms,*\.py,*\.tex !compiler %

" Colors and Theming
highlight Comment          ctermfg=4    ctermbg=none  cterm=bold
highlight Constant         ctermfg=1    ctermbg=none  cterm=none
highlight Special          ctermfg=4    ctermbg=none  cterm=bold
highlight Identifier       ctermfg=6    ctermbg=none  cterm=none
highlight PreProc          ctermfg=5    ctermbg=none  cterm=bold
highlight String           ctermfg=4    ctermbg=none  cterm=none
highlight Number           ctermfg=1    ctermbg=none  cterm=bold
highlight Function         ctermfg=1    ctermbg=none  cterm=none
highlight Visual           ctermfg=3    ctermbg=0     cterm=bold
