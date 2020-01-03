"""""""""""""""""""""""""Basic Setup""""""""""""""""""""""""""
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
set spell
set shiftwidth=2
set smarttab
set autoindent
set cindent
set softtabstop=2
set tabstop=2
set textwidth=79
set ttyfast
set wrap
set hlsearch
set incsearch
set redrawtime=10000
set lazyredraw
set backspace=indent,eol,start

function EnterOrIndentTag()
  let line = getline(".")
  let col = getpos(".")[2]
  let before = line[col-2]
  let after = line[col-1]

  if before == ">" && after == "<"
    return "\<Enter>\<C-o>O"
  endif
   return "\<Enter>"
endfunction

inoremap <expr> <Enter> EnterOrIndentTag()

filetype indent off
""""""""""""""""""""""""""""Key Maps"""""""""""""""""""""""""""""""
map f w
nnoremap <S-Up> :m-2<CR>
nnoremap <S-Down> :m+<CR>
inoremap <S-Up> <Esc>:m-2<CR>
inoremap <S-Down> <Esc>:m+<CR>
vnoremap <Leader>s :sort<CR>
map <leader>m :bn<cr>
map <leader>n :bp<cr>
map <leader>d :bd<cr>
" Disable Arrow Keys
"noremap <Up> <Nop>
"noremap <Down> <Nop>
"noremap <Left> <Nop>
"noremap <Right> <Nop>

"""""""""""""""""""""""""""Plugins""""""""""""""""""""""""
execute pathogen#infect()
syntax on
filetype off
filetype plugin indent on

"""""""""""""""""""""""""""Plugins setup""""""""""""""""""""""""""""""
" FZF Set
set rtp+=~/.fzf

" Prettier settings
let g:prettier#config#bracket_spacing = 'false'
let g:prettier#config#single_quote = 'true'

" Ale settings
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow

" use <tab> for trigger completion and navigate to the next complete item
" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~ '\s'
" endfunction
" 
" inoremap <silent><expr> <Tab>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<Tab>" :
"       \ coc#refresh()

"inoremap <expr> <Tab> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <Tab> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
