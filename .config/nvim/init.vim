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
set nu
set relativenumber
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
set ignorecase
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
set scrolloff=7
set colorcolumn=80
set completeopt=menuone,noinsert,noselect
set shortmess+=c
set updatetime=50
set cmdheight=2

autocmd BufRead,BufNewFile xresources,xdefaults set filetype=xdefaults
autocmd BufWritePost xresources !xrdb %
autocmd BufWritePost DirMarks !shorts DirConf

call plug#begin("$HOME/.config/nvim/plugged")
    Plug 'hoob3rt/lualine.nvim'
    Plug 'windwp/nvim-autopairs'
    Plug 'nacro90/numb.nvim'
    Plug 'jghauser/mkdir.nvim'
    Plug 'unblevable/quick-scope'
    Plug 'tpope/vim-surround'
    Plug 'easymotion/vim-easymotion'
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'vim-utils/vim-man'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-eunuch'
    Plug 'jremmen/vim-ripgrep'
    Plug 'subnut/visualstar.vim'
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-speeddating'
    Plug 'glts/vim-magnum'
    Plug 'glts/vim-radical'
    Plug 'lfilho/cosco.vim'
    Plug 'jlanzarotta/bufexplorer'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'neovim/nvim-lspconfig'
    Plug 'folke/lsp-colors.nvim'
    Plug 'onsails/lspkind-nvim'
    Plug 'simrat39/symbols-outline.nvim'
    Plug 'nvim-lua/lsp-status.nvim'
    Plug 'ahmedkhalf/lsp-rooter.nvim'
    Plug 'nvim-lua/completion-nvim'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    Plug 'p00f/nvim-ts-rainbow'
    Plug 'famiu/bufdelete.nvim'
call plug#end()

lua <<EOF
require('lualine').setup {
    options = {
        theme = 'palenight',
        padding = 1,
        left_padding = 1,
        right_padding = 1
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'filename'},
        lualine_c = {
            {
                'diff',
                colored = true,
                color_added = "#c3e88d",
                color_modified = "#f78c6c",
                color_removed = "#ff5370",
                symbols = {added = ' ', modified = '柳', removed = ' '}
            },
            {
                'diagnostics',
                sources = {'nvim_lsp'},
                sections = {'error', 'warn', 'info', 'hint'},
                color_error = "#ff5370",
                color_warn  = "#f78c6c",
                color_info  = "#c3e88d",
                color_hint  = "#72a4ff",
                symbols = {
                    error = 'E:',
                    warn  = 'W:',
                    info  = 'I:',
                    hint  = 'H:'
                }
            }
        },
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    },
    inactive_sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'filename'},
        lualine_c = {
            {
                'diagnostics',
                sources = {'nvim_lsp'},
                sections = {'error', 'warn', 'info', 'hint'},
                color_error = "#ff5370",
                color_warn  = "#f78c6c",
                color_info  = "#c3e88d",
                color_hint  = "#72a4ff",
                symbols = {
                    error = 'E:',
                    warn  = 'W:',
                    info  = 'I:',
                    hint  = 'H:'
                }
            }
        },
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    }
}
EOF

lua <<EOF
local remap = vim.api.nvim_set_keymap
local npairs = require('nvim-autopairs')

-- skip it, if you use another global object
_G.MUtils= {}

vim.g.completion_confirm_key = ""

MUtils.completion_confirm=function()
  if vim.fn.pumvisible() ~= 0  then
    if vim.fn.complete_info()["selected"] ~= -1 then
      require'completion'.confirmCompletion()
      return npairs.esc("<c-y>")
    else
      vim.api.nvim_select_popupmenu_item(0 , false , false ,{})
      require'completion'.confirmCompletion()
      return npairs.esc("<c-n><c-y>")
    end
  else
    return npairs.autopairs_cr()
  end
end

remap('i' , '<CR>','v:lua.MUtils.completion_confirm()', {expr = true , noremap = true})

npairs.setup({
    disable_filetype = { "TelescopePrompt" },
    enable_check_bracket_line = false,
    check_ts = true,
})
EOF

lua <<EOF
require('numb').setup {
    show_numbers = true,
    show_cursorline = false
}
EOF

lua <<EOF
require('mkdir')
EOF

let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

nmap sm ysiw

map  <Leader><Leader>r <Plug>(easymotion-jumptoanywhere)
map  <Leader><Leader>h <Plug>(easymotion-linebackward)
map  <Leader><Leader>l <Plug>(easymotion-lineforward)
map  <Leader><Leader>H <Plug>(easymotion-lineanywhere)
map  <Leader><Leader>L <Plug>(easymotion-lineanywhere)

lua require'colorizer'.setup()

if executable('rg')
    let g:rg_derivative_root='true'
endif

lua <<EOF
local actions = require('telescope.actions')
require('telescope').setup {
    defaults = {
        mappings = {
            i = {
                ["<C-j>"] = actions.move_selection_next,
                ["<C-k>"] = actions.move_selection_previous,
            },
        },
        vimgrep_arguments = {
            'rg',
            '--color=never',
            '--no-heading',
            '--with-filename',
            '--line-number',
            '--column',
            '--smart-case'
        },
        prompt_position = "bottom",
        prompt_prefix = " ",
        selection_caret = " ",
        entry_prefix = " ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "descending",
        layout_strategy = "horizontal",
        layout_defaults = {
            horizontal = {
                mirror = false,
            },
            vertical = {
                mirror = false,
            },
        },
        file_sorter =  require'telescope.sorters'.get_fuzzy_file,
        file_ignore_patterns = {},
        generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
        shorten_path = true,
        winblend = 0,
        width = 0.75,
        preview_cutoff = 120,
        results_height = 1,
        results_width = 0.8,
        border = {},
        borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
        color_devicons = true,
        use_less = true,
        set_env = { ['COLORTERM'] = 'truecolor' },
        file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
        grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
        qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,
        buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker,
    }
}
EOF

nnoremap <C-s>      :Telescope current_buffer_fuzzy_find<CR>
nnoremap <Leader>.  :Telescope file_browser<CR>
nnoremap <Leader>lr :Telescope lsp_references<CR>
nnoremap <Leader>ld :Telescope lsp_definitions<CR>
nnoremap <Leader>lt :Telescope treesitter<CR>
nnoremap <Leader>ls :Telescope lsp_document_symbols<CR>
nnoremap <Leader>lS :Telescope lsp_dynamic_workspace_symbols<CR>
nnoremap <Leader>le :Telescope lsp_document_diagnostics<CR>
nnoremap <Leader>lE :Telescope lsp_workspace_diagnostics<CR>
nnoremap <Leader>lz :Telescope symbols<CR>

let g:cosco_ignore_comment_lines = 1

autocmd FileType c,cpp nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
autocmd FileType c,cpp imap <silent> <C-l> <c-o><Plug>(cosco-commaOrSemiColon)

lua require'lspconfig'.clangd.setup{}
autocmd BufEnter * lua require'completion'.on_attach()

lua <<EOF
require("lsp-colors").setup({
  Error = "#ff5370",
  Warning = "#f78c6c",
  Hint = "#72a4ff",
  Information = "#c3e88d",
})
EOF

lua <<EOF
require('lspkind').init({
    with_text = true,
    preset = 'codicons',
    symbol_map = {
      Text = '',
      Method = 'ƒ',
      Function = '',
      Constructor = '',
      Variable = '',
      Class = '',
      Interface = 'ﰮ',
      Module = '',
      Property = '',
      Unit = '',
      Value = '',
      Enum = '了',
      Keyword = '',
      Snippet = '﬌',
      Color = '',
      File = '',
      Folder = '',
      EnumMember = '',
      Constant = '',
      Struct = ''
    },
})
EOF

lua <<EOF
vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    auto_preview = false,
    position = 'right',
    show_numbers = false,
    show_relative_numbers = false,
    show_symbol_details = true,
    keymaps = {
        close = "<Esc>",
        close = "q",
        goto_location = "<Cr>",
        focus_location = "o",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a",
    },
    lsp_blacklist = {},
}
EOF

lua << EOF
require("lsp-rooter").setup()
EOF

inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
imap <silent> <C-f> <Plug>(completion_trigger)
let g:completion_enable_auto_popup = 1
let g:completion_timer_cycle = 400
let g:completion_trigger_keyword_length = 3
let g:completion_sorting = "length"
let g:completion_trigger_on_delete = 0
let g:completion_matching_smart_case = 1
let g:completion_matching_strategy_list = [ 'exact', 'substring', 'fuzzy' ]
let g:completion_enable_snippet = 'vim-vsnip'

imap <expr> <C-e>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'    : '<C-e>'
smap <expr> <C-e>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'    : '<C-e>'
imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)' : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)' : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'
let g:vsnip_filetypes = {}
let g:vsnip_snippet_dir = "$HOME/.config/nvim/snippets/"

lua <<EOF
require'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "gn",
        },
    },
    indent = {
        enable = false
    },
}

require'nvim-treesitter.configs'.setup {
    rainbow = {
        enable = true,
        extended_mode = false,
        max_file_lines = 10000,
        colors = {
            "#c792ea",
            "#89ddff",
            "#72a4ff",
            "#c3e88d",
            "#ffcb6b",
            "#f78c6c",
            "#ff5370",
        }
    }
}
EOF

highlight normal              guifg=#eeffff    guibg=none          gui=none
highlight LineNr              guifg=#eeffff    guibg=none          gui=none
highlight CursorLineNr        guifg=#ecbe7b    guibg=#292d3e       gui=none
highlight VertSplit           guifg=none       guibg=none          gui=none
highlight ColorColumn         guifg=none       guibg=#300000       gui=none
highlight Title               guifg=#ecbe7b    guibg=none          gui=none
highlight Directory           guifg=#51afef    guibg=none          gui=none
highlight Pmenu               guifg=#eeffff    guibg=#12111e       gui=none
highlight PmenuSel            guifg=#12111e    guibg=#c792ea       gui=none
highlight PmenuSbar           guifg=none       guibg=#12111e       gui=none
highlight PmenuThumb          guifg=none       guibg=#c792ea       gui=none
highlight Folded              guifg=#308ac3    guibg=none          gui=none
highlight EndOfBuffer         guifg=#292d3e    guibg=none          gui=none
highlight QuickScopePrimary   guifg=#ff79c6    guibg=none          gui=bold,italic
highlight QuickScopeSecondary guifg=#ffffff    guibg=none          gui=bold,italic
highlight MatchParen          guifg=#f78c6c    guibg=none          gui=none
highlight Comment             guifg=#3c435e    guibg=none          gui=italic
highlight Constant            guifg=#89ddff    guibg=none          gui=bold
highlight String              guifg=#c3e88d    guibg=none          gui=none
highlight Character           guifg=#f78c6c    guibg=none          gui=bold
highlight Number              guifg=#f78c6c    guibg=none          gui=bold
highlight Boolean             guifg=#89ddff    guibg=none          gui=bold,italic
highlight Float               guifg=#ffcb6b    guibg=none          gui=italic
highlight Identifier          guifg=#ffcb6b    guibg=none          gui=none
highlight Function            guifg=#82aaff    guibg=none          gui=italic
highlight Statement           guifg=#89ddff    guibg=none          gui=italic
highlight Conditional         guifg=#89ddff    guibg=none          gui=bold,italic
highlight Repeat              guifg=#89ddff    guibg=none          gui=bold,italic
highlight Label               guifg=#89ddff    guibg=none          gui=italic
highlight Operator            guifg=#72a4ff    guibg=none          gui=none
highlight Keyword             guifg=#89ddff    guibg=none          gui=bold,italic
highlight Exception           guifg=#89ddff    guibg=none          gui=bold,italic
highlight PreProc             guifg=#89ddff    guibg=none          gui=bold
highlight Include             guifg=#72a4ff    guibg=none          gui=bold,italic
highlight Define              guifg=#72a4ff    guibg=none          gui=bold,italic
highlight Macro               guifg=#72a4ff    guibg=none          gui=bold
highlight PreCondit           guifg=#72a4ff    guibg=none          gui=bold,italic
highlight Type                guifg=#c792ea    guibg=none          gui=none
highlight StorageClass        guifg=#89ddff    guibg=none          gui=italic
highlight Structure           guifg=#89ddff    guibg=none          gui=italic
highlight Typedef             guifg=#89ddff    guibg=none          gui=italic
highlight Special             guifg=#c3e88d    guibg=none          gui=none
highlight SpecialChar         guifg=#c3e88d    guibg=none          gui=italic
highlight Delimeter           guifg=#72a4ff    guibg=none          gui=none
" Lsp
highlight LspDiagnosticsDefaultError           guifg=#ff5370 guibg=none gui=none
highlight LspDiagnosticsDefaultWarning         guifg=#f78c6c guibg=none gui=none
highlight LspDiagnosticsDefaultHint            guifg=#72a4ff guibg=none gui=none
highlight LspDiagnosticsDefaultInformation     guifg=#c3e88d guibg=none gui=none
highlight LspDiagnosticsFloatingError          guifg=#ff5370 guibg=none gui=none
highlight LspDiagnosticsFloatingWarning        guifg=#f78c6c guibg=none gui=none
highlight LspDiagnosticsFloatingHint           guifg=#72a4ff guibg=none gui=none
highlight LspDiagnosticsFloatingInformation    guifg=#c3e88d guibg=none gui=none
highlight LspDiagnosticsSignError              guifg=#ff5370 guibg=none gui=none
highlight LspDiagnosticsSignWarning            guifg=#f78c6c guibg=none gui=none
highlight LspDiagnosticsSignHint               guifg=#72a4ff guibg=none gui=none
highlight LspDiagnosticsSignInformation        guifg=#c3e88d guibg=none gui=none
highlight LspDiagnosticsUnderlineError         guifg=#ff5370 guibg=none gui=strikethrough
highlight LspDiagnosticsUnderlineWarning       guifg=#f78c6c guibg=none gui=strikethrough
highlight LspDiagnosticsUnderlineHint          guifg=#72a4ff guibg=none gui=strikethrough
highlight LspDiagnosticsUnderlineInformation   guifg=#c3e88d guibg=none gui=strikethrough
highlight LspDiagnosticsVirtualTextError       guifg=#ff5370 guibg=none gui=none
highlight LspDiagnosticsVirtualTextWarning     guifg=#f78c6c guibg=none gui=none
highlight LspDiagnosticsVirtualTextHint        guifg=#72a4ff guibg=none gui=none
highlight LspDiagnosticsVirtualTextInformation guifg=#c3e88d guibg=none gui=none

" Tree sitter
highlight TSBoolean           guifg=#89ddff    guibg=none          gui=italic
highlight TSCharacter         guifg=#f78c6c    guibg=none          gui=none
highlight TSComment           guifg=#3c435e    guibg=none          gui=italic
highlight TSConditional       guifg=#89ddff    guibg=none          gui=italic
highlight TSConditional       guifg=#89ddff    guibg=none          gui=italic
highlight TSConstBuiltin      guifg=#89ddff    guibg=none          gui=bold,italic
highlight TSConstant          guifg=#89ddff    guibg=none          gui=bold
highlight TSConstructor       guifg=#89ddff    guibg=none          gui=bold,italic
highlight TSFunction          guifg=#82aaff    guibg=none          gui=none
highlight TSFuncMacro         guifg=#ffcb6b    guibg=none          gui=bold
highlight TSInclude           guifg=#72a4ff    guibg=none          gui=bold,italic
highlight TSNumber            guifg=#f78c6c    guibg=none          gui=bold
highlight TSOperator          guifg=#72a4ff    guibg=none          gui=none
highlight TSParameter         guifg=#ffcb6b    guibg=none          gui=italic
highlight TSProperty          guifg=#ff5370    guibg=none          gui=italic
highlight TSPunctBracket      guifg=#72a4ff    guibg=none          gui=none
highlight TSPunctDelimeter    guifg=#72a4ff    guibg=none          gui=none
highlight TSPunctDelimiter    guifg=#72a4ff    guibg=none          gui=none
highlight TSPunctSpecial      guifg=#72a4ff    guibg=none          gui=none
highlight TSString            guifg=#c3e88d    guibg=none          gui=none
highlight TSStringEscape      guifg=#72a4ff    guibg=none          gui=italic
highlight TSStringRegex       guifg=#ff5370    guibg=none          gui=italic
highlight TSType              guifg=#c792ea    guibg=none          gui=none
highlight TSTypeBuiltin       guifg=#c792ea    guibg=none          gui=italic
highlight TSURI               guifg=#ffcb6b    guibg=none          gui=underline
highlight TSVariable          guifg=#ffcb6b    guibg=none          gui=none
highlight TSVariableBuiltin   guifg=#89ddff    guibg=none          gui=bold
highlight TSNamespace         guifg=#c792ea    guibg=none          gui=italic

imap jk <Esc>
nnoremap zq :q<CR>
nnoremap zx :q!<CR>
nnoremap <silent> <Leader>fs :w<CR>
nnoremap <silent><Esc> :noh<CR>
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
noremap <leader>bk :lua require('bufdelete').bufdelete(0, true)<CR>
noremap <leader>to :SymbolOutline<CR>
