set nocompatible              " be iMproved, required
filetype off                  " required
let mapleader = " "

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
    Plug 'bfrg/vim-cpp-modern'
    Plug 'itchyny/lightline.vim'
    Plug 'antoinemadec/FixCursorHold.nvim'
    Plug 'easymotion/vim-easymotion'
    Plug 'tpope/vim-commentary'
    Plug 'rrethy/vim-hexokinase'
    Plug 'luochen1990/rainbow'
    Plug 'Yggdroot/indentLine'
    Plug 'vifm/vifm.vim'

    Plug 'MarcWeber/vim-addon-mw-utils'
    Plug 'tomtom/tlib_vim'
    Plug 'garbas/vim-snipmate'
    Plug 'honza/vim-snippets'
call plug#end()

filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
" filetype plugin on

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal

" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General Settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set path+=**					" Searches current directory recursively.
set wildmenu					" Display all matches when tab complete.
set incsearch                   " Incremental search
set hidden                      " Needed to keep multiple buffers open
set nobackup                    " No auto backups
set noswapfile                  " No swap
set t_Co=256                    " Set if term supports 256 colors.
set number relativenumber       " Display line numbers
set clipboard=unnamedplus       " Copy/paste between vim and other programs.
set cursorline
set cursorcolumn
set termguicolors
set autoindent

set undofile
set undodir=$HOME/.config/nvim/undo
set undolevels=10000
set undoreload=10000

autocmd InsertEnter * norm zz

syntax enable
let g:rehash256 = 1
let g:Hexokinase_highlighters = [ 'backgroundfull' ]

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
let g:lightline = {
      \ 'colorscheme': 'one',
      \ }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set expandtab                   " Use spaces instead of tabs.
set smarttab                    " Be smart using tabs ;)
set shiftwidth=4                " One tab == four spaces.
set tabstop=4                   " One tab == four spaces.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Fern
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>b :Fern . -drawer -toggle<CR>
let g:fern#default_hidden = "1"
let g:fern#renderer = "nerdfont"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Theming
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
highlight LineNr              guifg=#b7b7b7    guibg=none          gui=none
highlight CursorLineNr        guifg=#ecbe7b       guibg=#292d3e       gui=none
highlight VertSplit           guifg=#1c1f24    guibg=#5b6268       gui=none
highlight Title               guifg=#ecbe7b    guibg=none          gui=none
highlight Statement           guifg=#46d9ff    guibg=none          gui=none
highlight Directory           guifg=#51afef    guibg=none          gui=none
highlight StatusLine          guifg=#202328    guibg=#5b6268       gui=none
highlight StatusLineNC        guifg=#202328    guibg=#5b6268       gui=none
highlight NERDTreeClosable    guifg=#98be65
highlight NERDTreeOpenable    guifg=#5b6268
highlight Comment             guifg=#5b6268    guibg=none          gui=none
highlight Constant            guifg=#a9a1e1    guibg=none          gui=none
highlight MatchParen          guifg=#000000    guibg=#51afef       gui=none
highlight Type                guifg=#ecbe7b    guibg=none          gui=none
highlight Special             guifg=#51afef    guibg=none          gui=none
highlight Identifier          guifg=none       guibg=none          gui=none
highlight PreProc             guifg=#46d9ff    guibg=none          gui=none
highlight String              guifg=#98be65    guibg=none          gui=none
highlight Number              guifg=#ecbe7b    guibg=none          gui=none
highlight Function            guifg=none       guibg=none          gui=none
highlight Pmenu               guifg=#308ac3    guibg=#111111       gui=none
highlight PmenuSel            guifg=#617fdf    guibg=#1e1e1e       gui=none
highlight PmenuSbar           guifg=#b7b7b7    guibg=#f8f87a       gui=none
highlight PmenuThumb          guifg=#b7b7b7    guibg=#51a0e4       gui=none
highlight CursorLine          guifg=none       guibg=#292d3e       gui=none
highlight CursorColumn        guifg=none       guibg=#2e302b       gui=none
highlight Folded              guifg=#308ac3    guibg=none          gui=none
highlight EndOfBuffer         guifg=#292d3e    guibg=none          gui=none
highlight QuickScopePrimary   guifg=#ff79c6    guibg=none          gui=underline
highlight QuickScopeSecondary guifg=#ffffff    guibg=none          gui=underline
highlight DiffAdd             guifg=#98be65    guibg=none          gui=none
highlight DiffChange          guifg=#46d9ff    guibg=none          gui=none
highlight DiffDelete          guifg=#ff6c6b    guibg=none          gui=none
highlight DiffText            guifg=#dfdfdf    guibg=none          gui=none

let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
let g:cpp_no_function_highlight = 1
let g:cpp_attributes_highlight = 1
let g:cpp_member_highlight = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VimWiki
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vimwiki_list = [{'path': '~/my-stuff/Documents/',}]
let g:vimwiki_folding = 'list'
nmap <Leader>cc <Plug>VimwikiToggleListItem

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Mouse Scrolling
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set mouse=nicr

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Splits and Tabbed Files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow splitright

" Remaps splits navigation to just Leader + hjkl
nnoremap <Leader>h <C-w>h
nnoremap <Leader>j <C-w>j
nnoremap <Leader>k <C-w>k
nnoremap <Leader>l <C-w>l

" Makes adjusing split sizes a bit more friendly
noremap <silent> <C-h> :vertical resize +3<CR>
noremap <silent> <C-l> :vertical resize -3<CR>
noremap <silent> <C-k> :resize +3<CR>
noremap <silent> <C-j> :resize -3<CR>

" Makes opening splits easier
noremap <leader>wc <C-w>c
noremap <leader>ws <C-w>v
noremap <leader>wv <C-w>s
noremap <leader>wr <C-w>R
map <Leader>vs :vs<Space>
map <Leader>vv :sp<Space>

" Change 2 split windows from vert to horiz or horiz to vert
map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K

" Tabs
map <Leader>tl :tabNext<CR>
map <Leader>th :tabprev<CR>
map <Leader>tL :+tabmove<CR>
map <Leader>tH :-tabmove<CR>
map <Leader>tn :tabnew<CR>
map <Leader>tc :tabclose<CR>

" Removes pipes | that act as seperators on splits
set fillchars+=vert:\

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Leader>fs :w<CR>
map zx :wq<CR>
map zq :q!<CR>
map <Leader>. :edit<Space>
map <Leader>e :Chmod +x<Space>
map <Leader>E :!touch<Space>
map <Leader>m :Mkdir<Space>
map <Leader>M :!rm -rf<Space>
noremap j gj
noremap k gk

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Other Stuff
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:python_highlight_all = 1

au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org            call org#SetOrgFileType()

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar


let g:cursorhold_updatetime = 100

let g:snipMate = get(g:, 'snipMate', {})
let g:snipMate = { 'snippet_version' : 1 }
let g:snipMate.scope_aliases = {}
let g:snipMate.scope_aliases['cpp'] = 'cpp'
autocmd BufRead,BufNewFile xresources,xdefaults set filetype=xdefaults
autocmd BufWritePost Xresources,Xdefaults,xresources,xdefaults !xrdb %
