set nocompatible
filetype off
let g:ale_disable_lsp = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vundle For Managing Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

    Plug 'gmarik/Vundle.vim'
    Plug 'lambdalisue/fern.vim'
    Plug 'lambdalisue/fern-renderer-nerdfont.vim'
    Plug 'lambdalisue/nerdfont.vim'
    Plug 'ryanoasis/vim-devicons'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-eunuch'
    Plug 'nelstrom/vim-visual-star-search'
    Plug 'unblevable/quick-scope'
    Plug 'cohama/lexima.vim'
    Plug 'itchyny/lightline.vim'
    Plug 'antoinemadec/FixCursorHold.nvim'
    Plug 'easymotion/vim-easymotion'
    Plug 'tpope/vim-commentary'
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'luochen1990/rainbow'
    Plug 'Yggdroot/indentLine'
    Plug 'vifm/vifm.vim'

    Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
    Plug 'dense-analysis/ale'
    Plug 'sbdchd/neoformat'
    Plug 'jackguo380/vim-lsp-cxx-highlight'
call plug#end()

filetype plugin indent on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General Settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = " "
set path+=**
set wildmenu
set incsearch
set hidden
set nobackup
set notimeout
set noswapfile
set number relativenumber
set clipboard=unnamedplus
set cursorline
set cursorcolumn
set termguicolors
set autoindent
set cmdheight=2
set updatetime=300
set signcolumn=no
set noshowmode
set mouse=nicr
set mouse=a

set foldmethod=syntax
set foldnestmax=10
set nofoldenable
set foldlevel=2

set expandtab
set smarttab
set shiftwidth=4
set tabstop=4

set undofile
set undodir=$HOME/.config/nvim/undo
set undolevels=10000
set undoreload=10000
syntax enable

lua require'colorizer'.setup()

let g:rehash256 = 1
let g:cursorhold_updatetime = 100

autocmd InsertEnter * norm zz

autocmd BufRead,BufNewFile xresources,xdefaults set filetype=xdefaults
autocmd BufWritePost xresources !xrdb %
autocmd BufWritePost DirMarks !shorts DirConf

ab #! #!/bin/sh

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => coc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
inoremap <silent><expr> <c-space> coc#refresh()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NeoFormat
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:neoformat_c_clangformat = {
    \ 'exe': 'clang-format',
    \ 'args': ['--style="{BasedOnStyle: mozilla, IndentWidth: 4}"']}
let g:neoformat_enabled_c = ['clangformat']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Ale
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_linters = {'c': ['clang']}
let g:ale_lint_delay = 100
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Easymotion
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map  <Leader><Leader>r <Plug>(easymotion-jumptoanywhere)
map  <Leader><Leader>h <Plug>(easymotion-linebackward)
map  <Leader><Leader>l <Plug>(easymotion-lineforward)
map  <Leader><Leader>H <Plug>(easymotion-lineanywhere)
map  <Leader><Leader>L <Plug>(easymotion-lineanywhere)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Rainbow
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:rainbow_active = 1
let g:rainbow_conf = {'guifgs': ['#51afef', '#c678dd', '#98be65'],}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VimWiki
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vimwiki_list = [{'path': '~/my-stuff/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Status Line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lightline = {'colorscheme': 'one'}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Fern
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>r :Fern . -drawer -toggle<CR>
let g:fern#default_hidden = "1"
let g:fern#renderer = "nerdfont"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Theming
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
highlight LineNr              guifg=#f0f0f0    guibg=none          gui=none
highlight CursorLineNr        guifg=#ecbe7b    guibg=#292d3e       gui=none
highlight VertSplit           guifg=#1c1f24    guibg=#5b6268       gui=none
highlight Title               guifg=#ecbe7b    guibg=none          gui=none
highlight Statement           guifg=#46d9ff    guibg=none          gui=none
highlight Directory           guifg=#51afef    guibg=none          gui=none
highlight NERDTreeClosable    guifg=#98be65
highlight NERDTreeOpenable    guifg=#5b6268
highlight Comment             guifg=#5b6268    guibg=none          gui=italic
highlight Constant            guifg=#a9a1e1    guibg=none          gui=none
highlight MatchParen          guifg=#000000    guibg=#51afef       gui=none
highlight Type                guifg=#ecbe7b    guibg=none          gui=none
highlight Special             guifg=#98be65    guibg=none          gui=none
highlight Identifier          guifg=none       guibg=none          gui=none
highlight PreProc             guifg=#46d9ff    guibg=none          gui=none
highlight String              guifg=#98be65    guibg=none          gui=none
highlight Number              guifg=#ecbe7b    guibg=none          gui=none
highlight Function            guifg=none       guibg=none          gui=none
highlight Pmenu               guifg=#b7b7b7    guibg=#000000       gui=none
highlight PmenuSel            guifg=#000000    guibg=#ecbe7b       gui=none
highlight PmenuSbar           guifg=none       guibg=#000000       gui=none
highlight PmenuThumb          guifg=none       guibg=#ecbe7b       gui=none
highlight CursorLine          guifg=none       guibg=#292d3e       gui=none
highlight CursorColumn        guifg=none       guibg=#292d3e       gui=none
highlight Folded              guifg=#308ac3    guibg=none          gui=none
highlight EndOfBuffer         guifg=#292d3e    guibg=none          gui=none
highlight QuickScopePrimary   guifg=#ff79c6    guibg=none          gui=underline
highlight QuickScopeSecondary guifg=#ffffff    guibg=none          gui=underline
highlight ALEErrorSign        guifg=#ff0000    guibg=none          gui=none
highlight ALEWarningSign      guifg=#fff000    guibg=none          gui=none
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
" C highlight
let g:lsp_cxx_hl_use_text_props = 1
highlight LspCxxHLSymFunction     guifg=#c678dd  guibg=none          gui=none
highlight LspCxxHLSymVariable     guifg=#a9a1e1  guibg=none          gui=none
highlight LspCxxHLSymNamespace    guifg=#51afef  guibg=none          gui=none
highlight LspCxxHLGroupNamespace  guifg=#51afef  guibg=none          gui=none
highlight LspCxxHLSymParameter    guifg=#da8548  guibg=none          gui=none
highlight LspCxxHLSymMacro        guifg=#ecbe7b  guibg=none          gui=none

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Splits and Tabbed Files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow splitright

nnoremap <Leader>h <C-w>h
nnoremap <Leader>j <C-w>j
nnoremap <Leader>k <C-w>k
nnoremap <Leader>l <C-w>l

noremap <silent> <Leader>wh :vertical resize +3<CR>
noremap <silent> <Leader>wl :vertical resize -3<CR>
noremap <silent> <Leader>wk :resize +3<CR>
noremap <silent> <Leader>wj :resize -3<CR>

noremap <leader>wc <C-w>c
noremap <leader>ws <C-w>v
noremap <leader>wv <C-w>s
noremap <leader>wr <C-w>R
map <Leader>vs :vs<Space>
map <Leader>vv :sp<Space>

map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K

map <Leader>tl :tabNext<CR>
map <Leader>th :tabprev<CR>
map <Leader>tk :+tabmove<CR>
map <Leader>tj :-tabmove<CR>
map <Leader>tn :tabnew<CR>
map <Leader>tc :tabclose<CR>

set fillchars+=vert:\ 

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>fs :w<CR>
map <Leader>fx :wq<CR>
map <Leader>fq :q!<CR>
map zx :wq<CR>
map zq :q!<CR>
map <Leader>. :edit<Space>
map <Leader>e :Chmod +x<Space>
map <Leader>E :!touch<Space>
map <Leader>m :Mkdir<Space>
map <Leader>M :!rm -rf<Space>
map <Leader>F :Neoformat<CR>
map <Leader>W GetTerm
nmap ms ysiw

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Other Stuff
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L
