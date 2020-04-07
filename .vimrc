"""""""""""""""""""""""""""""Basic Setup""""""""""""""""""""""""""
if has('gui_running')
  set guifont=Iosevka\:h13
  set guioptions=
  set tw=120
endif

syntax on
set background=dark
let &t_SI.="\e[5 q" 
let &t_SR.="\e[4 q"
let &t_EI.="\e[1 q"
" let &t_EI = "\<Esc>]50;CursorShape=0\x7"
" let &t_SI = "\<Esc>]50;CursorShape=1\x7"
" let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let mapleader = " "
set belloff=all
set number
set nrformats=
set ruler
set incsearch
set shiftwidth=2
set smarttab
set autoindent
set softtabstop=2
set tabstop=2
set ttyfast
set textwidth=120
set smartindent
set wrap
set nospell
set lazyredraw
set backspace=indent,eol,start
set noswapfile
set nobackup
set nowritebackup
hi clear SpellBad
hi SpellBad cterm=bold,underline,italic
set nocursorline
au FileType * set fo-=c fo-=r fo-=o
"""""""""""""""""""""""""""Netrw config"""""""""""""""""""""""""""
let loaded_netrwPlugin = 1
" let g:netrw_banner = 0
" let g:netrw_liststyle = 3
" let g:netrw_browse_split = 0
" let g:netrw_altv = 1
" let g:netrw_winsize = 15
""""""""""""""""""""""""""""Key Maps""""""""""""""""""""""""""""""
nnoremap <CR> :noh<CR><CR>
nnoremap <S-Up> :m-2<CR>
nnoremap <S-Down> :m+<CR>
inoremap <S-Up> <Esc>:m-2<CR>
inoremap <S-Down> <Esc>:m+<CR>
vnoremap <Leader>s :sort<CR>
nnoremap <Right> :bn<CR>
nnoremap <Left> :bp<CR>
map <leader>q :bd<cr>
" inoremap <Up> <Nop>
" inoremap <Down> <Nop>
" inoremap <Left> <Nop>
" inoremap <Right> <Nop>
nnoremap <Up> <Nop>
nnoremap <Down> <Nop>

vnoremap <leader><Tab> :call gotonextindent(1)<cr>
vnoremap <leader><S-TAB> :call GoToNextIndent(-1)<CR>

noremap <Leader>y "*y
" noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

" inoremap (; (<CR>);<C-c>O
" inoremap (, (<CR>),<C-c>O
" inoremap {; {<CR>};<C-c>O
" inoremap {, {<CR>},<C-c>O
" inoremap [; [<CR>];<C-c>O
" inoremap [, [<CR>],<C-c>O
"""""""""""""""""""""""""""Plugins""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'gruvbox-community/gruvbox'
Plug 'Yggdroot/indentLine'
Plug 'rhysd/clever-f.vim'
Plug 'sheerun/vim-polyglot'
Plug 'itchyny/lightline.vim'
Plug 'taohexxx/lightline-buffer'
Plug 'cakebaker/scss-syntax.vim'
Plug 'tpope/vim-surround'
Plug 'gko/vim-coloresque'
Plug 'turbio/bracey.vim'
Plug 'kamykn/spelunker.vim'
Plug 'dense-analysis/ale'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'mcchrish/nnn.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'kristijanhusak/vim-js-file-import', {'do': 'npm install'}
" Plug 'jiangmiao/auto-pairs'
"Plug 'zxqfl/tabnine-vim'
" Plug 'mattn/emmet-vim'
" Plug 'prettier/vim-prettier', {'do': 'npm install'}
" if has('nvim')
"   Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" else
"   Plug 'Shougo/deoplete.nvim'
"   Plug 'roxma/nvim-yarp'
"   Plug 'roxma/vim-hug-neovim-rpc'
" endif
call plug#end()
""""""""""""""""""""""""""""Plugins setup""""""""""""""""""""""""""""""
" let g:deoplete#enable_at_startup = 1
" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
set tags=tags

" let g:AutoPairsShortcutToggle = ''
let g:gitgutter_override_sign_column_highlight = 0

" let g:user_emmet_mode='n'
" let g:user_emmet_leader_key='<leader>'
" let g:user_emmet_settings = {'javascript' : {'extends' : 'jsx'}, 'typescript': { 'extends' : 'jsx'}}

nnoremap <leader><leader> :Files<cr>
nnoremap <leader>r :Rg<cr>

let g:nnn#set_default_mappings = 0
nnoremap <silent> <leader>[ :NnnPicker<CR>
" let g:nnn#layout = { 'left': '~20%' } " or right, up, down
let g:nnn#layout = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Debug' } }

" Prettier settings
" let g:prettier#config#bracket_spacing = 'true'
" let g:prettier#config#single_quote = 'true'
" let g:prettier#config#semi = 'true'

set updatetime=100
set completeopt-=preview
set laststatus=2
set noshowmode
set signcolumn=yes

"""""""""""""""""""""""""""""""""""COC"""""""""""""""""""""""""""""""
let g:coc_global_extensions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-tsserver',
  \ 'coc-prettier',
  \ 'coc-json',
  \ 'coc-python',
	\ 'coc-html',
	\ 'coc-css',
	\ 'coc-emmet'
  \ ]

set signcolumn=yes
let g:python_host_prog  = "/usr/bin/python"
let g:python3_host_prog = "/usr/local/bin/python3"


" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

"" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <expr><C-f> coc#util#has_float() ? coc#util#float_scroll(1) : "\<C-f>"
nnoremap <expr><C-b> coc#util#has_float() ? coc#util#float_scroll(0) : "\<C-b>"

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)


" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)

nmap <leader>p :call CocAction('format')<cr>

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
""""""""""""""""""""LIGHTLINE""""""""""""""""""""
set showtabline=2
let g:lightline = {
    \ 'colorscheme': 'gruvbox',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \           [ 'readonly', 'filename' ]
    \   ]
    \ },
    \ 'tabline': {
    \   'left': [ [ 'bufferinfo' ],
    \             [ 'separator' ],
    \             [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
    \   'right': [ ['gitbranch'] ]
    \ },
    \ 'component_expand': {
    \   'buffercurrent': 'lightline#buffer#buffercurrent',
    \   'bufferbefore': 'lightline#buffer#bufferbefore',
    \   'bufferafter': 'lightline#buffer#bufferafter',
    \ },
    \ 'component_type': {
    \   'buffercurrent': 'tabsel',
    \   'bufferbefore': 'raw',
    \   'bufferafter': 'raw',
    \ },
    \ 'component_function': {
    \   'bufferinfo': 'lightline#buffer#bufferinfo',
    \   'gitbranch': 'fugitive#head',
    \ },
    \ 'component': {
    \   'separator': '',
  	\   'lineinfo': ' %3l:%-2v',
    \ },
    \ }

let g:lightline.separator = {
	  \   'left': '', 'right': ''
    \ }
let g:lightline.subseparator = {
	  \   'left': '', 'right': ''
    \ }

let g:lightline_buffer_ellipsis_icon = '..'
let g:lightline_buffer_expand_left_icon = '◀ '
let g:lightline_buffer_expand_right_icon = ' ▶'
let g:lightline_buffer_active_buffer_left_icon = ''
let g:lightline_buffer_active_buffer_right_icon = ''
let g:lightline_buffer_separator_icon = '  '

colorscheme gruvbox
