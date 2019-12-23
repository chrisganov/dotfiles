"""""""""""""""""""""""""Basic Setup""""""""""""""""""""""""""
filetype off
filetype plugin indent on
let &t_EI = "\<Esc>]50;CursorShape=0\x7"
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let mapleader = ","
set belloff=all
set encoding=utf-8
set expandtab
set formatoptions=tcqrn1
set listchars=tab:▸\ ,eol:¬
set mouse=a
set nocompatible
set noshiftround
set number
set ruler
set shiftwidth=2
set softtabstop=2
set tabstop=2
set textwidth=79
set ttyfast
set wrap
syntax on

""""""""""""""""""""""""""""Key Maps"""""""""""""""""""""""""""""""
map f w
nnoremap <S-Up> :m-2<CR>
nnoremap <S-Down> :m+<CR>
inoremap <S-Up> <Esc>:m-2<CR>
inoremap <S-Down> <Esc>:m+<CR>
vnoremap <Leader>s :sort<CR> 
