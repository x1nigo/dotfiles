# x1nigo's dotfiles
These are my personal dotfiles, which include scripts to my other respositories.

## Requirements
- Make sure to add the `.scripts` folder in your $PATH.
- Don't forget to `chmod +x` the actual scripts.
- Most of my scripts use `dmenu` so make sure you have that as well.
- You need to install vim-airline from https://github.com/vim-airline/vim-airline. I use the
[Pathogen](https://github.com/tpope/vim-pathogen) plugin manager. If you don't want to do
that, then just remove these lines from `vimrc`:
```
execute pathogen#infect()
let g:airline#extensions#tabline#enabled = 1
```
