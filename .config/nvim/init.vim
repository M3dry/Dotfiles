syntax enable

let mapleader = " "
set path+=**
set wildmenu
set incsearch
set hidden
set nobackup
set notimeout
set noswapfile
set nowrap
set number relativenumber
set clipboard=unnamedplus
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
set smartcase
set noerrorbells
set expandtab
set smarttab
set smartindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set undofile
set undodir=$HOME/.config/nvim/undo
set undolevels=10000000
set undoreload=10000000
set fillchars+=vert:\ 

call plug#begin("$HOME/.config/nvim/plugged")
    Plug 'itchyny/lightline.vim'
    Plug 'cohama/lexima.vim'
    Plug 'unblevable/quick-scope'
    Plug 'tpope/vim-surround'
    Plug 'easymotion/vim-easymotion'
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'vim-utils/vim-man'
    Plug 'preservim/nerdcommenter'
    Plug 'jremmen/vim-ripgrep'
    Plug 'kien/ctrlp.vim'
    Plug 'kien/ctrlp.vim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-lua/completion-nvim'
call plug#end()

let g:lightline = {'colorscheme': 'one'}
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

nmap sm ysiw

map  <Leader><Leader>r <Plug>(easymotion-jumptoanywhere)
map  <Leader><Leader>h <Plug>(easymotion-linebackward)
map  <Leader><Leader>l <Plug>(easymotion-lineforward)
map  <Leader><Leader>H <Plug>(easymotion-lineanywhere)
map  <Leader><Leader>L <Plug>(easymotion-lineanywhere)

lua require'colorizer'.setup()

let g:NERDCreateDefaultMappings = 1
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDCommentEmptyLines = 0
let g:NERDTrimTrailingWhitespace = 0
let g:NERDToggleCheckAllLines = 1

if executable('rg')
    let g:rg_derivative_root='true'
endif

let g:ctrlp_use_caching = 0

lua require'lspconfig'.clangd.setup{on_attach=require'completion'.on_attach}
autocmd BufEnter * lua require'completion'.on_attach()
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
imap <silent> <C-f> <Plug>(completion_trigger)
" augroup CompletionTriggerCharacter
    " autocmd!
    " autocmd BufEnter *.c,*.cpp let g:completion_trigger_character = ['.', '->']
" augroup end
let g:completion_enable_auto_popup = 0
let g:completion_trigger_on_delete = 0
let g:completion_matching_smart_case = 1
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
set completeopt=menuone,noinsert,noselect
set shortmess+=c

highlight LineNr              guifg=#eeffff    guibg=none          gui=none
highlight CursorLineNr        guifg=#ecbe7b    guibg=#292d3e       gui=none
highlight VertSplit           guifg=#1c1f24    guibg=#5b6268       gui=none
highlight Title               guifg=#ecbe7b    guibg=none          gui=none
highlight Statement           guifg=#46d9ff    guibg=none          gui=none
highlight Directory           guifg=#51afef    guibg=none          gui=none
highlight Comment             guifg=#5b6268    guibg=none          gui=italic
highlight Constant            guifg=#a9a1e1    guibg=none          gui=none
highlight MatchParen          guifg=#ff5370    guibg=#000000       gui=none
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

nnoremap <silent> <Leader>fs :w<CR>

nnoremap <Leader>wh <C-w>h
nnoremap <Leader>wj <C-w>j
nnoremap <Leader>wk <C-w>k
nnoremap <Leader>wl <C-w>l
noremap <silent> <Leader>wH :vertical resize +3<CR>
noremap <silent> <Leader>wJ :resize -3<CR>
noremap <silent> <Leader>wK :resize +3<CR>
noremap <silent> <Leader>wL :vertical resize -3<CR>
noremap <leader>wc <C-w>c
noremap <leader>wd <C-w>c
noremap <leader>wv <C-w>v
noremap <leader>ws <C-w>s
noremap <leader>wr <C-w>R
