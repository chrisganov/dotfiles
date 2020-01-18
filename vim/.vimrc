"""""""""""""""""""""""""Basic Setup""""""""""""""""""""""""""
if has('gui_running')
    set guifont=Source\ Code\ Pro\:h16
    set guioptions=
    set tw=120
endif

let &t_EI = "\<Esc>]50;CursorShape=0\x7"
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_SR = "\<Esc>]50;CursorShape=2\x7"
let mapleader = " "
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
set smarttab
set autoindent
set tw=120
set smartindent
set cindent
set softtabstop=2
set tabstop=2
set textwidth=79
set ttyfast
set wrap
set nocursorline
set spell
set hlsearch
set incsearch
set redrawtime=10000
set lazyredraw
set backspace=indent,eol,start
set noswapfile
set nobackup
set nowritebackup
set hlsearch
filetype indent off
hi clear SpellBad
hi SpellBad cterm=bold,underline,italic

""""""""""""""""""""""""""""""FUNCTIONS""""""""""""""""""""""""""""""
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

" A function that jumps to the end of same indent 
function! GoToNextIndent(inc)
    " Get the cursor current position
    let currentPos = getpos('.')
    let currentLine = currentPos[1]
    let matchIndent = 0

    " Look for a line with the same indent level whithout going out of the buffer
    while !matchIndent && currentLine != line('$') + 1 && currentLine != -1
        let currentLine += a:inc
        let matchIndent = indent(currentLine) == indent('.')
    endwhile

    " If a line is found go to this line
    if (matchIndent)
        let currentPos[1] = currentLine
        call setpos('.', currentPos)
    endif
endfunction

"""""""""""""""""""""""""""netrw settings""""""""""""""""""""""""""""

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25

""""""""""""""""""""""""""""Key Maps""""""""""""""""""""""""""""""

nnoremap <S-Up> :m-2<CR>
nnoremap <S-Down> :m+<CR>
inoremap <S-Up> <Esc>:m-2<CR>
inoremap <S-Down> <Esc>:m+<CR>
vnoremap <Leader>s :sort<CR>
map <leader>, :bn<cr>
map <leader>. :bp<cr>
map <leader>d :bd<cr>
inoremap jj <Esc>
autocmd InsertEnter * :let @/=""
autocmd InsertLeave * :let @/=""
noremap <leader>q :bd<CR>

" Disable Arrow Keys in Normal Mode
noremap <Up> :call GoToNextIndent(-1)<cr>
noremap <Down> :call GoToNextIndent(1)<cr>
noremap <Left> <<
noremap <Right> >>
" Disable Arrow keys in Insert Mode
" inoremap <Up> <Nop>
" inoremap <Down> <Nop>
" inoremap <Left> <Nop>
" inoremap <Right> <Nop>


" On enter if it should indent a tag
inoremap <expr> <Enter> EnterOrIndentTag()

" Maps for got end of the indent function
vnoremap <leader><Tab> :call gotonextindent(1)<cr>
vnoremap <leader><S-TAB> :call GoToNextIndent(-1)<CR>

noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

"""""""""""""""""""""""""""Plugins""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')
Plug 'dense-analysis/ale'
Plug 'jiangmiao/auto-pairs'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'Yggdroot/indentLine'
Plug 'alvan/vim-closetag'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'prettier/vim-prettier'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'morhetz/gruvbox'
Plug 'sheerun/vim-polyglot'
Plug 'itchyny/lightline.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'ap/vim-css-color'
call plug#end()
syntax on
colorscheme gruvbox
set background=dark
filetype off
filetype plugin indent on

"""""""""""""""""""""""""""Plugins setup""""""""""""""""""""""""""""""
" Prettier settings
let g:prettier#config#bracket_spacing = 'false'
let g:prettier#config#single_quote = 'true'

" Ale setting
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
highlight ALEErrorSign ctermbg=NONE ctermfg=red
highlight ALEWarningSign ctermbg=NONE ctermfg=yellow
let b:ale_linters = ['eslint']

" Git Gutter
set updatetime=100


let g:ctrlp_working_path_mode = 0
let g:ctrlp_custom_ignore = 'docs\|dist\|build\|node_modules\|DS_Store\|git'

set completeopt-=preview

" always display status line
set laststatus=2

" hide mode
set noshowmode

" let g:lightline = {
"       \ 'colorscheme': 'jellybeans',
"       \ }

" STARTING FOR COC
" if hidden is not set, TextEdit might fail.
let g:coc_global_extensions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-tsserver',
  \ 'coc-eslint',
  \ 'coc-tslint-plugin',
  \ 'coc-prettier',
  \ 'coc-json',
  \ 'coc-rls',
  \ ]

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes


let g:python_host_prog  = "/usr/bin/python"
let g:python3_host_prog = "/usr/local/bin/python3"


" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"


" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"


function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"
" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
"" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <expr><C-f> coc#util#has_float() ? coc#util#float_scroll(1) : "\<C-f>"
nnoremap <expr><C-b> coc#util#has_float() ? coc#util#float_scroll(0) : "\<C-b>"


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

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <C-d> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <C-d> <Plug>(coc-range-select)
xmap <silent> <C-d> <Plug>(coc-range-select)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" auto fix keymap
nmap <F5> :call CocAction('format')<cr>

" coc-color-highlight
autocmd CursorHold * silent call CocActionAsync('highlight')

" coc-yank
nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>

" saerch for files is mapped as ctrl p
nnoremap <leader>b :CtrlPBuffer<CR>
noremap <leader><leader> :CtrlP<CR>
nnoremap <leader>t :CtrlPTag<cr>

set tags=tags

let g:lightline = {
  \   'active': {
  \     'left':[ [ 'mode', 'paste' ],
  \              [ 'gitbranch', 'readonly', 'filename', 'modified' ]
  \     ]
  \   },
	\   'component': {
	\     'lineinfo': ' %3l:%-2v',
	\   },
  \   'component_function': {
  \     'gitbranch': 'fugitive#head',
  \   }
  \ }
let g:lightline.separator = {
	\   'left': '', 'right': ''
  \}
let g:lightline.subseparator = {
	\   'left': '', 'right': ''
  \}

