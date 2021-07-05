syntax enable

let mapleader = " "
set splitbelow
set splitright
set path+=**
set wildmenu
set wildignore=**/.git/*
set incsearch
set hidden
set nobackup
set notimeout
set noswapfile
set nu
set relativenumber
set clipboard=unnamedplus
set termguicolors
set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait600-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175
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
set fillchars+=vert:│
set scrolloff=7
set signcolumn=yes
set completeopt=menuone,noinsert,noselect
set shortmess+=c
set updatetime=50
set cmdheight=2
set list listchars=eol:↴,trail:·,tab:»\ ,nbsp:␣,extends:»

autocmd BufRead,BufNewFile xresources,xdefaults set filetype=xdefaults
autocmd BufWritePost xresources !xrdb %
autocmd BufWritePost DirMarks !shorts DirConf

let data_dir = stdpath('data') . '/site'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin("$HOME/.config/nvim/plugged")
    " Plug 'hoob3rt/lualine.nvim'
    Plug 'glepnir/galaxyline.nvim', {'branch': 'main'}
    Plug 'windwp/nvim-autopairs'
    Plug 'nacro90/numb.nvim'
    Plug 'jghauser/mkdir.nvim'
    Plug 'mbbill/undotree'
    Plug 'winston0410/range-highlight.nvim'
    Plug 'unblevable/quick-scope'
    Plug 'famiu/nvim-reload'
    Plug 'tpope/vim-surround'
    Plug 'easymotion/vim-easymotion'
    Plug 'mizlan/iswap.nvim'
    Plug 't9md/vim-choosewin'
    Plug 'tommcdo/vim-lion'
    Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
    Plug 'winston0410/commented.nvim'
    Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  }
    Plug 'tpope/vim-eunuch'
    Plug 'ThePrimeagen/vim-be-good'
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-fzy-native.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-dap.nvim'
    Plug 'oberblastmeister/neuron.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-speeddating'
    Plug 'glts/vim-magnum'
    Plug 'glts/vim-radical'
    Plug 'NFrid/due.nvim'
    Plug 'kevinhwang91/nvim-hlslens'
    Plug 'lfilho/cosco.vim'
    Plug 'szw/vim-maximizer'
    Plug 'lukas-reineke/indent-blankline.nvim', {'branch': 'lua'}
    Plug 'matbme/JABS.nvim'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'kkoomen/vim-doge', { 'do': { -> doge#install() } }
    Plug 'neovim/nvim-lspconfig'
    Plug 'tjdevries/nlua.nvim'
    Plug 'ray-x/lsp_signature.nvim'
    Plug 'folke/lsp-colors.nvim'
    Plug 'onsails/lspkind-nvim'
    Plug 'michaelb/sniprun', {'do': 'bash install.sh'}
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'simrat39/symbols-outline.nvim'
    Plug 'nvim-lua/lsp-status.nvim'
    Plug 'ahmedkhalf/lsp-rooter.nvim'
    Plug 'glepnir/lspsaga.nvim'
    Plug 'akinsho/nvim-toggleterm.lua'
    Plug 'hrsh7th/nvim-compe'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'hrsh7th/vim-vsnip-integ'
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    Plug 'nvim-treesitter/playground'
    Plug 'p00f/nvim-ts-rainbow'
    Plug 'mfussenegger/nvim-ts-hint-textobject'
    Plug 'lewis6991/spellsitter.nvim'
    Plug 'folke/trouble.nvim'
    Plug 'folke/todo-comments.nvim'
    Plug 'famiu/bufdelete.nvim'
    Plug 'mfussenegger/nvim-dap'
    Plug 'rcarriga/nvim-dap-ui'
    Plug 'tveskag/nvim-blame-line'
    Plug 'sindrets/diffview.nvim'
    Plug 'TimUntersberger/neogit'
call plug#end()

autocmd BufEnter github.com_*.txt set filetype=markdown

lua <<EOF
-- require('lualine').setup {
--     options = {
--         theme = 'palenight',
--         padding = 1,
--         left_padding = 1,
--         right_padding = 1,
--         section_separators = '',
--         component_separators = ''
--     },
--     sections = {
--         lualine_a = {
--             {
--                 'filename',
--                 color = {fg = "#12111e", bg = "#ff5370"}
--             },
--             {
--                 'location',
--                 color = {fg = "#12111e", bg = "#c792ea"}
--             }
--         },
--         lualine_b = {
--             {
--                 'encoding',
--                 color = {fg = "#12111e", bg = "#72a4ff"}
--             },
--             {
--                 'fileformat',
--                 color = {fg = "#12111e", bg = "#72a4ff"}
--             },
--         },
--         lualine_c = {},
--         lualine_x = {'filetype',
--             {
--                 'branch',
--                 color = {fg = "#c792ea", bg = "#292d3e"}
--             },
--         },
--         lualine_y = {
--             {
--                 'diff',
--                 colored = true,
--                 color_added = "#c3e88d",
--                 color_modified = "#f78c6c",
--                 color_removed = "#ff5370",
--                 symbols = {added = ' ', modified = '柳', removed = ' '}
--             },
--             {
--                 'diagnostics',
--                 sources = {'nvim_lsp'},
--                 sections = {'error', 'warn', 'info', 'hint'},
--                 color_error = "#ff5370",
--                 color_warn  = "#f78c6c",
--                 color_info  = "#c3e88d",
--                 color_hint  = "#72a4ff",
--                 symbols = {
--                     error = ' ',
--                     warn  = ' ',
--                     info  = ' ',
--                     hint  = ' '
--                 }
--             },
--         },
--         lualine_z = {
--             {
--                 'progress',
--                 color = { fg = "#eeffff", bg = "#3e4452"}
--             },
--         }
--     },
--     inactive_sections = {
--         lualine_a = {
--             {
--                 'filename',
--                 color = {fg = "#12111e", bg = "#ff5370"}
--             },
--             {
--                 'location',
--                 color = {fg = "#12111e", bg = "#c792ea"}
--             }
--         },
--         lualine_b = {
--             {
--                 'encoding',
--                 color = {fg = "#12111e", bg = "#72a4ff"}
--             },
--             {
--                 'fileformat',
--                 color = {fg = "#12111e", bg = "#72a4ff"}
--             },
--         },
--         lualine_c = {},
--         lualine_x = {
--             {
--                 'filetype',
--                 color = {fg = "#ff000", bg = "#292d3e"}
--             },
--             {
--                 'branch',
--                 color = {fg = "#c792ea", bg = "#292d3e"}
--             },
--         },
--         lualine_y = {
--             {
--                 'diff',
--                 colored = true,
--                 color_added = "#c3e88d",
--                 color_modified = "#f78c6c",
--                 color_removed = "#ff5370",
--                 symbols = {added = ' ', modified = '柳', removed = ' '}
--             },
--             {
--                 'diagnostics',
--                 sources = {'nvim_lsp'},
--                 sections = {'error', 'warn', 'info', 'hint'},
--                 color_error = "#ff5370",
--                 color_warn  = "#f78c6c",
--                 color_info  = "#c3e88d",
--                 color_hint  = "#72a4ff",
--                 symbols = {
--                     error = ' ',
--                     warn  = ' ',
--                     info  = ' ',
--                     hint  = ' '
--                 }
--             },
--         },
--         lualine_z = {
--             {
--                 'progress',
--                 color = { fg = "#eeffff", bg = "#3e4452"}
--             },
--         }
--     },
-- }

local gl = require('galaxyline')
local gls = gl.section
local condition = require('galaxyline.condition')

gls.left[1] = {
  RainbowRed = {
    provider = function() return ' ' end,
    highlight = {'NONE', "#ff5370"}
  },
}

gls.left[2] = {
  FileSize = {
    provider = 'FileSize',
    condition = condition.buffer_not_empty,
    separator = ' ',
    separator_highlight = {'#ff5370', "#c792ea"},
    highlight = {"#12111e", "#ff5370"}
  }
}

gls.left[3] = {
  FileName = {
    provider = 'FileName',
    condition = condition.buffer_not_empty,
    separator = ' ',
    separator_highlight = {'NONE', "#72a4ff"},
    highlight = {"#12111e", "#c792ea",'italic'}
  }
}

gls.left[4] = {
  LineInfo = {
    provider = function()
        return string.format("%d:%d %d", vim.fn.line('.'), vim.fn.col('.'), vim.fn.line('$'))
    end,
    separator = '',
    separator_highlight = {'NONE', "#292d3e"},
    highlight = {"#12111e", "#72a4ff"},
  },
}

gls.right[1] = {
  GitIcon = {
    provider = function() return '  ' end,
    condition = condition.check_git_workspace,
    highlight = {"#c792ea", "#292d3e"},
  }
}

gls.right[2] = {
  GitBranch = {
    provider = 'GitBranch',
    condition = condition.check_git_workspace,
    highlight = {"#c792ea", "#292d3e"},
  }
}

gls.right[3] = {
  DiffAdd = {
    provider = 'DiffAdd',
    condition = condition.hide_in_width,
    icon = '  ',
    highlight = {"#c3e88d", "#292d3e"},
  }
}
gls.right[4] = {
  DiffModified = {
    provider = 'DiffModified',
    condition = condition.hide_in_width,
    icon = ' 柳',
    highlight = {"#f78c6c", "#292d3e"},
  }
}
gls.right[5] = {
  DiffRemove = {
    provider = 'DiffRemove',
    condition = condition.hide_in_width,
    icon = '  ',
    highlight = {"#ff5370", "#292d3e"},
  }
}

gls.right[6] = {
  DiagnosticError = {
    provider = 'DiagnosticError',
    icon = ' ',
    separator = ' ',
    separator_highlight = {'NONE', "#292d3e"},
    highlight = {"#ff5370", "#292d3e"}
  }
}

gls.right[7] = {
  DiagnosticWarn = {
    provider = 'DiagnosticWarn',
    icon = ' ',
    highlight = {"#f78c6c", "#292d3e"},
  }
}

gls.right[8] = {
  DiagnosticHint = {
    provider = 'DiagnosticHint',
    icon = ' ',
    highlight = {"#72a4ff", "#292d3e"},
  }
}

gls.right[9] = {
  DiagnosticInfo = {
    provider = 'DiagnosticInfo',
    icon = ' ',
    highlight = {"#c3e88d", "#292d3e"},
  }
}

gls.right[10] = {
  ShowLspClient = {
    provider = 'GetLspClient',
    condition = function ()
      local tbl = {['dashboard'] = true,['']=true}
      if tbl[vim.bo.filetype] then
        return false
      end
      return true
    end,
    icon = '  ',
    separator = '',
    separator_highlight = {'NONE', "#292d3e"},
    highlight = {"#c3e88d", "#292d3e"}
  }
}

gls.right[11] = {
  PerCent = {
    provider = 'LinePercent',
    separator = ' ',
    separator_highlight = {'NONE', "#292d3e"},
    highlight = {"#eeffff", "#292d3e"},
  }
}

gls.right[12] = {
  RainbowBlue = {
    provider = function() return '' end,
    highlight = {'NONE', "#292d3e"}
  },
}
EOF

lua <<EOF
require("nvim-autopairs").setup({
    check_ts = true,
})

require("nvim-autopairs.completion.compe").setup({
  map_cr = true,
  map_complete = true
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

lua <<EOF
require("range-highlight").setup {
    highlight = "Visual",
	highlight_with_out_range = {
        d = true,
        delete = true,
        m = true,
        move = true,
        y = true,
        yank = true,
        c = true,
        change = true,
        j = true,
        join = true,
        ["<"] = true,
        [">"] = true,
        s = true,
        subsititue = true,
        sno = true,
        snomagic = true,
        sm = true,
        smagic = true,
        ret = true,
        retab = true,
        t = true,
        co = true,
        copy = true,
        ce = true,
        center = true,
        ri = true,
        right = true,
        le = true,
        left = true,
        sor = true,
        sort = true
	}
}
EOF

let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

map  <Leader><Leader>r <Plug>(easymotion-jumptoanywhere)
map  <Leader><Leader>h <Plug>(easymotion-linebackward)
map  <Leader><Leader>l <Plug>(easymotion-lineforward)
map  <Leader><Leader>H <Plug>(easymotion-lineanywhere)
map  <Leader><Leader>f <Plug>(easymotion-bd-f)
nmap <Leader><Leader>f <Plug>(easymotion-overwin-f)
nmap s <Plug>(easymotion-overwin-f2)
map  <Leader><Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader><Leader>L <Plug>(easymotion-overwin-line)
map  <Leader><Leader>w <Plug>(easymotion-bd-w)
nmap <Leader><Leader>w <Plug>(easymotion-overwin-w)

lua <<EOF
require('iswap').setup{
  keys = 'sdfghjkl',
  grey = 'disable',
  hl_snipe = 'CursorLineNr',
  hl_selection = 'Visual',
}
EOF

nnoremap <silent> <Leader>ly :ISwap<CR>

let g:choosewin_overlay_enable = 1
let g:choosewin_statusline_replace = 0
let g:choosewin_color_label = { 'gui': ['#12111e', '#72a4ff'] }
let g:choosewin_color_label_current = { 'gui': ['#12111e', '#ff5370'] }
let g:choosewin_color_overlay = { 'gui': ['#72a4ff', '#72a4ff'] }
let g:choosewin_color_overlay_current = { 'gui': ['#ff5370', '#ff5370'] }
let g:choosewin_color_other = { 'gui': ['#12111e', '#12111e'] }

nnoremap <silent> <Leader>ww :ChooseWin<CR>

let b:lion_squeeze_spaces = 1

let g:Hexokinase_highlighters = [ 'backgroundfull' ]
let g:Hexokinase_optInPatterns = [
\     'full_hex',
\     'triple_hex',
\     'rgb',
\     'rgba',
\     'hsl',
\     'hsla',
\     'colour_names'
\ ]

lua <<EOF
require('commented').setup({
    comment_padding = " ",
	keybindings = {n = "gc", v = "gc", nl = "gcc"},
	set_keybindings = true,
	ex_mode_cmd = "Comment"
})
EOF

let g:mkdp_auto_start = 0
let g:mkdp_auto_close = 1
let g:mkdp_refresh_slow = 0
let g:mkdp_command_for_global = 0
let g:mkdp_open_to_the_world = 0
let g:mkdp_open_ip = ''
let g:mkdp_browser = ''
let g:mkdp_echo_preview_url = 0
let g:mkdp_browserfunc = ''
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1,
    \ 'sequence_diagrams': {},
    \ 'flowchart_diagrams': {},
    \ 'content_editable': v:false,
    \ 'disable_filename': 0
    \ }
let g:mkdp_markdown_css = ''
let g:mkdp_highlight_css = ''
let g:mkdp_port = ''
let g:mkdp_page_title = '「${name}」'
let g:mkdp_filetypes = ['markdown']

nnoremap <Leader>mp <Plug>MarkdownPreview
nnoremap <Leader>ms <Plug>MarkdownPreviewStop
nnoremap <Leader>mt <Plug>MarkdownPreviewToggle

lua <<EOF
local actions = require('telescope.actions')
local trouble = require("trouble.providers.telescope")

require('telescope').load_extension('dap')
require('telescope').load_extension('fzy_native')

require('telescope').setup {
    defaults = {
        mappings = {
            i = {
                ["<C-j>"] = actions.move_selection_next,
                ["<C-k>"] = actions.move_selection_previous,
                ["<C-o>"] = trouble.open_with_trouble,
                ["<C-q>"] = actions.send_to_qflist
            },
            n = {
                ["<C-o>"] = trouble.open_with_trouble
            }
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
        sorting_strategy = "ascending",
        layout_strategy = "horizontal",
        layout_defaults = {
            horizontal = {
                mirror = false,
            },
            vertical = {
                mirror = false,
            },
        },
        file_ignore_patterns = {},
        file_sorter = require'telescope.sorters'.get_fzy_sorter,
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
    },
    extensions = {
        fzy_native = {
            override_generic_sorter = false,
            override_file_sorter = true,
        }
    }
}
EOF

nnoremap <silent> <C-s>      :Telescope current_buffer_fuzzy_find<CR>
nnoremap <silent> <Leader>.  :Telescope file_browser<CR>
nnoremap <silent> <Leader>lf :Telescope fd<CR>
nnoremap <silent> <Leader>ll :Telescope live_grep<CR>
nnoremap <silent> <Leader>lg :Telescope grep_string<CR>
nnoremap <silent> <Leader>lm :Telescope marks<CR>
nnoremap <silent> <Leader>lk :Telescope keymaps<CR>
nnoremap <silent> <Leader>lu :Telescope spell_suggest<CR>
nnoremap <silent> <Leader>lb :Telescope buffers<CR>
nnoremap <silent> <Leader>lz :Telescope symbols<CR>

" Project
nnoremap <silent> <Leader>pf  :Telescope git_files<CR>
nnoremap <silent> <Leader>ps  :Telescope git_status<CR>
nnoremap <silent> <Leader>pb  :lua require('m3dry.telescope').git_branches()<CR>
nnoremap <silent> <Leader>pc  :Telescope git_bcommits<CR>
nnoremap <silent> <Leader>pv  :Telescope git_commits<CR>

" Lsp
nnoremap <silent> <Leader>lr :Telescope lsp_references<CR>
nnoremap <silent> <Leader>ld :Telescope lsp_definitions<CR>
nnoremap <silent> <Leader>lt :Telescope treesitter<CR>
nnoremap <silent> <Leader>ls :Telescope lsp_document_symbols<CR>
nnoremap <silent> <Leader>lS :Telescope lsp_dynamic_workspace_symbols<CR>
nnoremap <silent> <Leader>le :Telescope lsp_document_diagnostics<CR>
nnoremap <silent> <Leader>lE :Telescope lsp_workspace_diagnostics<CR>
nnoremap <silent> <Leader>la :TSHighlightCapturesUnderCursor<CR>

" Dap
nnoremap <silent> <Leader>dtc :lua require'telescope'.extensions.dap.commands{}<CR>
nnoremap <silent> <Leader>dto :lua require'telescope'.extensions.dap.configurations{}<CR>
nnoremap <silent> <Leader>dtb :lua require'telescope'.extensions.dap.list_breakpoints{}<CR>
nnoremap <silent> <Leader>dtv :lua require'telescope'.extensions.dap.variables{}<CR>
nnoremap <silent> <Leader>dtf :lua require'telescope'.extensions.dap.frames{}<CR>

lua <<EOF
require'neuron'.setup {
    virtual_titles = true,
    mappings = false,
    run = nil,
    neuron_dir = "~/my-stuff/Neuron",
    leader = "<Leader>n"
}
EOF

nnoremap <silent> <Leader>nn :lua require'neuron/cmd'.new_edit(require'neuron'.config.neuron_dir)<CR>
nnoremap <silent> <Leader>nf :lua require'neuron/telescope'.find_zettels()<CR>
nnoremap <silent> <Leader>ni :lua require'neuron/telescope'.find_zettels {insert = true}<CR>
nnoremap <silent> <Leader>ns :lua require'neuron'.rib {address = "127.0.0.1:8200", verbose = true}<CR>

function NeuronBinds()
    nnoremap <buffer> <CR> :lua require'neuron'.enter_link()<CR>
    nnoremap <buffer> <Leader>nb :lua require'neuron/telescope'.find_backlinks()<CR>
    nnoremap <buffer> <Leader>nB :lua require'neuron/telescope'.find_backlinks {insert = true}<CR>
    nnoremap <buffer> <Leader>nt :lua require'neuron/telescope'.find_tags()<CR>
    nnoremap <buffer> gn] :lua require'neuron'.goto_next_extmark()<CR>
    nnoremap <buffer> gn[ :lua require'neuron'.goto_prev_extmark()<CR>
endfunction

autocmd BufRead "/home/m3/my-stuff/Neuron/*.md" :call NeuronBinds()<CR>

lua <<EOF
require('due_nvim').setup {
  prescript = 'due: ',
  prescript_hi = 'Comment',
  due_hi = 'Error',
  ft = '*',
  today = 'TODAY',
  today_hi = 'Character',
  overdue = 'OVERDUE',
  overdue_hi = 'Error',
  date_hi = 'Conceal',
  pattern_start = '<',
  pattern_end = '>'
}
EOF

lua <<EOF
require('hlslens').setup({
    nearest_float_when = 'always',
    override_lens = function(render, plist, nearest, idx, r_idx)
        local sfw = vim.v.searchforward == 1
        local indicator, text, chunks
        local abs_r_idx = math.abs(r_idx)
        if abs_r_idx > 1 then
            indicator = ('%d%s'):format(abs_r_idx, sfw ~= (r_idx > 1) and '' or '')
        elseif abs_r_idx == 1 then
            indicator = sfw ~= (r_idx == 1) and '' or ''
        else
            indicator = ''
        end

        local lnum, col = unpack(plist[idx])
        if nearest then
            local cnt = #plist
            if indicator ~= '' then
                text = ('[%s %d/%d]'):format(indicator, idx, cnt)
            else
                text = ('[%d/%d]'):format(idx, cnt)
            end
            chunks = {{' ', 'Ignore'}, {text, 'HlSearchLensNear'}}
        else
            text = ('[%s %d]'):format(indicator, idx)
            chunks = {{' ', 'Ignore'}, {text, 'HlSearchLens'}}
        end
        render.set_virt(0, lnum - 1, col - 1, chunks, nearest)
    end
})
EOF

noremap <silent> n <Cmd>execute('normal! ' . v:count1 . 'n')<CR>
            \<Cmd>lua require('hlslens').start()<CR>
noremap <silent> N <Cmd>execute('normal! ' . v:count1 . 'N')<CR>
            \<Cmd>lua require('hlslens').start()<CR>
noremap * *<Cmd>lua require('hlslens').start()<CR>
noremap # #<Cmd>lua require('hlslens').start()<CR>
noremap g* g*<Cmd>lua require('hlslens').start()<CR>
noremap g# g#<Cmd>lua require('hlslens').start()<CR>

let g:cosco_ignore_comment_lines = 1

autocmd FileType c,cpp nmap <silent> <Leader>; <Plug>(cosco-commaOrSemiColon)
autocmd FileType c,cpp imap <silent> <C-l> <c-o><Plug>(cosco-commaOrSemiColon)

let g:maximizer_set_default_mapping = 0
nnoremap <silent> <Leader>wm :MaximizerToggle<CR>
vnoremap <silent> <Leader>wm :MaximizerToggle<CR>

let g:indent_blankline_indent_level = 7
let g:indent_blankline_char = '│'
let g:indent_blankline_space_char = '·'
let g:indent_blankline_space_char_blankline = '·'
let g:indent_blankline_show_trailing_blankline_indent = v:false
let g:indent_blankline_bufname_exclude = ['README\..*', '.*\.md']

let g:doge_doc_standard_c = 'kernel_doc'

nnoremap <silent> <Leader>l; :DogeGenerate<CR>

lua <<EOF
local custom_nvim_lspconfig_attach = function(...) end

require'lspconfig'.clangd.setup {}
require('nlua.lsp.nvim').setup(require('lspconfig'), {
    on_attach = custom_nvim_lspconfig_attach
})
EOF

lua <<EOF
require'lsp_signature'.on_attach {
    bind = true,
    doc_lines = 10,
    floating_window = true,
    fix_pos = true,
    hint_enable = true,
    hint_prefix = "",
    hint_scheme = "Search",
    use_lspsaga = false,
    hi_parameter = "Search",
    max_height = 12,
    max_width = 120,
    handler_opts = {
      border = "single"
    },
    extra_trigger_chars = {"(", ","}
}
EOF

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
require'sniprun'.setup({
    display = {
      "Classic",
      "VirtualTextOk",
      -- "VirtualTextErr",
      -- "TempFloatingWindow",
      -- "LongTempFloatingWindow",
      -- "Terminal"
      },
    snipruncolors = {
      SniprunVirtualTextOk   =  {fg="#72a4ff"},
    },
    inline_messages = 0,
    borders = 'double'
})
EOF

nmap <silent> <Leader>fr :SnipRun<CR>
nmap <silent> <Leader>fc :SnipClose<CR>
nmap <silent> <Leader>fs :SnipReset<CR>
vmap <silent>         f  :SnipRun<CR>

let g:nvim_tree_side = 'left'
let g:nvim_tree_width = 25
let g:nvim_tree_ignore = [ '.git' ]
let g:nvim_tree_gitignore = 0
let g:nvim_tree_auto_open = 0
let g:nvim_tree_auto_close = 1
let g:nvim_tree_auto_ignore_ft = [ 'startify', 'dashboard' ]
let g:nvim_tree_quit_on_open = 0
let g:nvim_tree_follow = 1
let g:nvim_tree_indent_markers = 1
let g:nvim_tree_hide_dotfiles = 0
let g:nvim_tree_git_hl = 1
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_root_folder_modifier = ':~'
let g:nvim_tree_tab_open = 1
let g:nvim_tree_width_allow_resize  = 0
let g:nvim_tree_disable_netrw = 1
let g:nvim_tree_disable_default_keybindings = 1
let g:nvim_tree_hijack_netrw = 1
let g:nvim_tree_add_trailing = 1
let g:nvim_tree_group_empty = 1
let g:nvim_tree_lsp_diagnostics = 1
let g:nvim_tree_disable_window_picker = 1
let g:nvim_tree_hijack_cursor = 1
let g:nvim_tree_icon_padding = ' '
let g:nvim_tree_update_cwd = 1
let g:nvim_tree_window_picker_exclude = {
    \   'filetype': [
    \     'packer',
    \     'qf'
    \   ],
    \   'buftype': [
    \     'terminal'
    \   ]
    \ }
let g:nvim_tree_special_files = [ 'README.md', 'README.org', 'Makefile']
let g:nvim_tree_show_icons = {
    \ 'git': 1,
    \ 'folders': 1,
    \ 'files': 1,
    \ 'folder_arrows': 1,
    \ }
let g:nvim_tree_icons = {
    \ 'default': '',
    \ 'symlink': '',
    \ 'git': {
    \   'unstaged': "✗",
    \   'staged': "✓",
    \   'unmerged': "",
    \   'renamed': "➜",
    \   'untracked': "★",
    \   'deleted': "",
    \   'ignored': "◌"
    \   },
    \ 'folder': {
    \   'arrow_open': "ﬔ",
    \   'arrow_closed': "⬎",
    \   'default': "",
    \   'open': "",
    \   'empty': "",
    \   'empty_open': "",
    \   'symlink': "",
    \   'symlink_open': "",
    \   },
    \   'lsp': {
    \     'hint': "",
    \     'info': "",
    \     'warning': "",
    \     'error': "",
    \   }
    \ }

function TreeOpen()
    NvimTreeClose
    NvimTreeOpen
    wincmd p
endfunction

nnoremap <silent> <Leader>fo :call TreeOpen()<CR>
nnoremap <silent> <Leader>fc :NvimTreeClose<CR>
nnoremap <silent> <Leader>ff :NvimTreeFindFile<CR>

lua <<EOF
local tree_cb = require'nvim-tree.config'.nvim_tree_callback

vim.g.nvim_tree_bindings = {
  { key = {"<CR>", "o", "<2-LeftMouse>"},   cb = tree_cb("edit") },
  { key = {"<2-RightMouse>", "<C-}>", "l"}, cb = tree_cb("cd") },
  { key = "<C-v>",                          cb = tree_cb("vsplit") },
  { key = "<C-x>",                          cb = tree_cb("split") },
  { key = "<C-t>",                          cb = tree_cb("tabnew") },
  { key = "<",                              cb = tree_cb("prev_sibling") },
  { key = ">",                              cb = tree_cb("next_sibling") },
  { key = "P",                              cb = tree_cb("parent_node") },
  { key = "<BS>",                           cb = tree_cb("close_node") },
  { key = "<S-CR>",                         cb = tree_cb("close_node") },
  { key = "<Tab>",                          cb = tree_cb("preview") },
  { key = "K",                              cb = tree_cb("first_sibling") },
  { key = "J",                              cb = tree_cb("last_sibling") },
  { key = "I",                              cb = tree_cb("toggle_ignored") },
  { key = "H",                              cb = tree_cb("toggle_dotfiles") },
  { key = "R",                              cb = tree_cb("refresh") },
  { key = "a",                              cb = tree_cb("create") },
  { key = "d",                              cb = tree_cb("remove") },
  { key = "r",                              cb = tree_cb("rename") },
  { key = "<C-r",                           cb = tree_cb("full_rename") },
  { key = "x",                              cb = tree_cb("cut") },
  { key = "c",                              cb = tree_cb("copy") },
  { key = "p",                              cb = tree_cb("paste") },
  { key = "y",                              cb = tree_cb("copy_name") },
  { key = "Y",                              cb = tree_cb("copy_path") },
  { key = "gy",                             cb = tree_cb("copy_absolute_path") },
  { key = "[c",                             cb = tree_cb("prev_git_item") },
  { key = "}c",                             cb = tree_cb("next_git_item") },
  { key = {"-", "h"},                       cb = tree_cb("dir_up") },
  { key = "q",                              cb = tree_cb("close") },
  { key = "g?",                             cb = tree_cb("toggle_help") },
}
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

lua <<EOF
require("lsp-rooter").setup()
EOF

lua <<EOF
local saga = require 'lspsaga'

saga.init_lsp_saga {
    use_saga_diagnostic_sign = true,
    error_sign = '',
    warn_sign = '',
    hint_sign = '',
    infor_sign = '',
    dianostic_header_icon = ' ',
    code_action_icon = '',
    code_action_prompt = {
      enable = true,
      sign = true,
      sign_priority = 20,
      virtual_text = true,
    },
    finder_definition_icon = ' ',
    finder_reference_icon = ' ',
    max_preview_lines = 30,
    finder_action_keys = {
      open = '<CR>', vsplit = 's', split = 'i',
      quit = '<C-c>', scroll_down = '<C-f>', scroll_up = '<C-b>',
    },
    code_action_keys = {
      quit = 'q',exec = '<CR>'
    },
    rename_action_keys = {
      quit = '<C-c>', exec = '<CR>'
    },
    definition_preview_icon = ' ',
    border_style = "single",
    rename_prompt_prefix = '➤',
}
EOF

nnoremap <silent> <Leader>ln     :Lspsaga rename<CR>
nnoremap <silent> <Leader>lp     :Lspsaga preview_definition<CR>
nnoremap <silent> <Leader>lh     :Lspsaga hover_doc<CR>
nnoremap <silent> <Leader>lc     :Lspsaga code_action<CR>
nnoremap <silent> <Leader>lC     :Lspsaga range_code_action<CR>
nnoremap <silent> [e             :Lspsaga diagnostic_jump_next<CR>
nnoremap <silent> ]e             :Lspsaga diagnostic_jump_prev<CR>
nnoremap <silent> <Leader>lv     :Lspsaga show_line_diagnostics<CR>
nnoremap <silent> <Leader>lc     :Lspsaga show_cursor_diagnostics<CR>
nnoremap <silent> <Leader>li     :Lspsaga lsp_finder<CR>
nnoremap <silent> <Leader>lj     :Lspsaga signature_help<CR>
nnoremap <silent> <C-f>          :lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>
nnoremap <silent> <C-b>          :lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>

lua <<EOF
require("toggleterm").setup{
   
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
        end
    end,
    open_mapping = [[<c-n>]],
    hide_numbers = true,
    shade_filetypes = {},
    shade_terminals = true,
    shading_factor = '1',
    start_in_insert = true,
    insert_mappings = true,
    persist_size = true,
    direction = 'horizontal',
    close_on_exit = true,
    shell = vim.o.shell,
    float_opts = {
        border = 'single',
        highlights = {
            border = "Normal",
            background = "Normal",
        }
    }
}
EOF

lua <<EOF
require'compe'.setup {
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = 'always',
    throttle_time = 80,
    source_timeout = 200,
    resolve_timeout = 800,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = true,
    source = {
        nvim_lsp =
        {
            true,
            priority = 1000
        },
        vsnip = {
            true,
            kind = '﬌ Snippet',
            priority = 950
        },
        nvim_lua =
        {
            true,
            priority = 900
        },
        path =
        {
            true,
            kind = ' Path',
            priority = 700
        },
        buffer = {
            true,
            kind = '﬘ Buffer',
            priority = 600
        },
        calc =
        {
            true,
            kind = ' Math',
            priority = 500
        },
        spell =
        {
            true,
            kind = ' Spell',
            priority = 100
        }
    }
}
EOF

inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
cnoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
cnoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"

imap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)  ? '<Plug>(vsnip-jump-next)' : vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : vsnip#expandable() ? '<Plug>(vsnip-expand)' : '<Tab>'
let g:vsnip_filetypes = {}
let g:vsnip_snippet_dir = "$HOME/.config/nvim/snippets/"

autocmd FileType * call vsnip#get_complete_items(bufnr())

lua <<EOF
require'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,
    },
    indent = {
        enable = false
    },
    playground = {
        enable = true,
        disable = {},
        updatetime = 25,
        persist_queries = false,
        keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
        },
    },
    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = {"BufWrite", "CursorHold"},
    },
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
    },
    autopairs = {
        enable = true
    }
}

require('spellsitter').setup {
  hl = 'SpellBad',
  captures = {'comment'},
}
EOF

nnoremap <silent> <Leader><Leader>u :lua require('tsht').nodes()<CR>

lua << EOF
require("trouble").setup {
    position = "bottom",
    height = 10,
    width = 50,
    icons = true,
    mode = "lsp_document_diagnostics",
    fold_open = "",
    fold_closed = "",
    action_keys = {
        close = "q",
        cancel = "<esc>",
        refresh = "r",
        jump = {"<cr>", "<tab>"},
        open_split = { "<c-x>" },
        open_vsplit = { "<c-v>" },
        open_tab = { "<c-t>" },
        jump_close = {"o"},
        toggle_mode = "m",
        toggle_preview = "P",
        hover = "K",
        preview = "p",
        close_folds = {"zM", "zm"},
        open_folds = {"zR", "zr"},
        toggle_fold = {"zA", "za"},
        previous = "k",
        next = "j"
    },
    indent_lines = true,
    auto_open = false,
    auto_close = false,
    auto_preview = true,
    auto_fold = false,
    signs = {
        error = "",
        warning = "",
        hint = "",
        information = "",
        other = "﫠"
    },
    use_lsp_diagnostic_signs = false
}
EOF

lua << EOF
require("todo-comments").setup {
    signs = true,
    sign_priority = 8,
    keywords = {
        FIX  = { icon = " ", color = "#ff5370", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning" },
        WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
    },
    merge_keywords = true,
    highlight = {
      before = "",
      keyword = "bg",
      after = "bg",
      pattern = [[.*<(KEYWORDS)\s*:]],
      comments_only = true,
      max_line_len = 10000,
    },
    colors = {
        error = { "#ff5370" },
        warning = { "#f78c6c" },
        info = { "#c3e88d" },
        hint = { "#72a4ff" },
    },
    search = {
      command = "rg",
      args = {
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
      },
      pattern = [[\b(KEYWORDS):]],
    },
}
EOF

nnoremap <silent> gle :TroubleToggle lsp_document_diagnostics<CR>
nnoremap <silent> glw :TroubleToggle lsp_workspace_diagnostics<CR>
nnoremap <silent> glr :TroubleToggle lsp_references<CR>
nnoremap <silent> glc :TroubleClose<CR>

lua <<EOF
local dap = require('dap')

dap.adapters.c = {
    type = 'executable',
    attach = {
        pidProperty = "pid",
        pidSelect = "ask"
    },
    command = '/usr/bin/lldb-vscode',
    env = {
        LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES"
    },
    name = "lldb"
}

dap.configurations.c = {
  {
    type = "c",
    name = "Debug",
    request = "launch",
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
  },
}

env = function()
  local variables = {}
  for k, v in pairs(vim.fn.environ()) do
    table.insert(variables, string.format("%s=%s", k, v))
  end
  return variables
end,

require("dapui").setup({
  icons = {
    expanded = "⯆",
    collapsed = "⯈"
  },
  mappings = {
    -- Use a table to apply multiple mappings
    expand = {"<CR>", "<2-LeftMouse>"},
    open = "o",
    remove = "d",
    edit = "e",
  },
  sidebar = {
    open_on_start = true,
    elements = {
      -- You can change the order of elements in the sidebar
      "scopes",
      "breakpoints",
      "stacks",
      "watches"
    },
    width = 40,
    position = "left" -- Can be "left" or "right"
  },
  tray = {
    open_on_start = true,
    elements = {
      "repl"
    },
    height = 10,
    position = "bottom" -- Can be "bottom" or "top"
  },
  floating = {
    max_height = nil, -- These can be integers or a float between 0 and 1.
    max_width = nil   -- Floats will be treated as percentage of your screen.
  }
})
EOF

nnoremap <silent> <Leader>dc  :lua require'dap'.continue()<CR>
nnoremap <silent> <Leader>do  :lua require'dap'.step_over()<CR>
nnoremap <silent> <Leader>di  :lua require'dap'.step_into()<CR>
nnoremap <silent> <Leader>ds  :lua require'dap'.step_out()<CR>
nnoremap <silent> <Leader>dbt :lua require'dap'.toggle_breakpoint()<CR>
nnoremap <silent> <Leader>dbc :lua require'dap'.set_breakpoint(vim.fn.input("Breakpoint condition: "))<CR>
nnoremap <silent> <Leader>dbm :lua require'dap'.set_breakpoint(nil, nil, vim.fn.input("Log point message: "))<CR>
nnoremap <silent> <Leader>dro :lua require'dap'.repl.open()<CR>
nnoremap <silent> <Leader>drl :lua require'dap'.repl.run_last()<CR>

let g:blameLineUseVirtualText = 1
let g:blameLineVirtualTextHighlight = 'Comment'
let g:blameLineGitFormat = '%an:%as - %s %h'

lua <<EOF
local cb = require'diffview.config'.diffview_callback

require'diffview'.setup {
    diff_binaries = false,
    file_panel = {
        width = 35,
        use_icons = true
    },
    key_bindings = {
        disable_defaults = false,
        view = {
            ["<tab>"]     = cb("select_next_entry"),
            ["<s-tab>"]   = cb("select_prev_entry"),
            ["<leader>e"] = cb("focus_files"),
            ["<leader>b"] = cb("toggle_files"),
        },
        file_panel = {
            ["j"]             = cb("next_entry"),
            ["<down>"]        = cb("next_entry"),
            ["k"]             = cb("prev_entry"),
            ["<up>"]          = cb("prev_entry"),
            ["<cr>"]          = cb("select_entry"),
            ["o"]             = cb("select_entry"),
            ["<2-LeftMouse>"] = cb("select_entry"),
            ["-"]             = cb("toggle_stage_entry"),
            ["S"]             = cb("stage_all"),
            ["U"]             = cb("unstage_all"),
            ["R"]             = cb("refresh_files"),
            ["<tab>"]         = cb("select_next_entry"),
            ["<s-tab>"]       = cb("select_prev_entry"),
            ["<leader>e"]     = cb("focus_files"),
            ["<leader>b"]     = cb("toggle_files"),
        }
    }
}

local neogit = require('neogit')

neogit.setup {
  disable_signs = false,
  disable_context_highlighting = false,
  disable_commit_confirmation = false,
  signs = {
    -- { CLOSED, OPENED }
    section = { "⬎", "ﬔ" },
    item = { "⬎", "ﬔ" },
    hunk = { "", "" },
  },
  integrations = {
    diffview = true  
  },
  mappings = {
    status = {
      ["p"] = "PushPopup",
      ["P"] = "PullPopup",
    }
  }
}
EOF

nnoremap <silent> <Leader>gg :Neogit<CR>
nnoremap <silent> <Leader>gb :ToggleBlameLine<CR>
nnoremap <Leader>gd :DiffviewOpen

highlight normal              guifg=#eeffff    guibg=NONE          gui=NONE
highlight Visual              guifg=NONE       guibg=#4e5579       gui=NONE
highlight Search              guifg=NONE       guibg=#4e5579       gui=NONE
highlight LineNr              guifg=#eeffff    guibg=#111111       gui=NONE
highlight VertSplit           guifg=#72a4ff    guibg=NONE          gui=NONE
highlight CursorLineNr        guifg=#c792ea    guibg=#111111       gui=NONE
highlight SignColumn          guifg=#eeffff    guibg=NONE          gui=NONE
highlight ColorColumn         guifg=NONE       guibg=#300000       gui=NONE
highlight Title               guifg=#ecbe7b    guibg=NONE          gui=NONE
highlight diffAdded           guifg=#c3e88d    guibg=#353c34       gui=NONE
highlight diffRemoved         guifg=#ff5370    guibg=#634661       gui=NONE
highlight DiffAdd             guifg=#c3e88d    guibg=#353c34       gui=NONE
highlight DiffDelete          guifg=#ff5370    guibg=#634661       gui=NONE
highlight StatusLine          guifg=#eeffff    guibg=#292d3e       gui=NONE
highlight StatusLineNC        guifg=#eeffff    guibg=#292d3e       gui=NONE
highlight NonText             guifg=#3c435e    guibg=NONE          gui=NONE
highlight Error               guifg=#ff5370    guibg=NONE          gui=NONE
highlight ErrorMsg            guifg=#ff5370    guibg=NONE          gui=NONE
highlight WarningMsg          guifg=#f78c6c    guibg=NONE          gui=NONE
highlight Directory           guifg=#51afef    guibg=NONE          gui=NONE
highlight Pmenu               guifg=#eeffff    guibg=#12111e       gui=NONE
highlight PmenuSel            guifg=#12111e    guibg=#c792ea       gui=NONE
highlight PmenuSbar           guifg=NONE       guibg=#12111e       gui=NONE
highlight PmenuThumb          guifg=NONE       guibg=#c792ea       gui=NONE
highlight Folded              guifg=#308ac3    guibg=NONE          gui=NONE
highlight EndOfBuffer         guifg=#292d3e    guibg=NONE          gui=NONE
highlight MatchParen          guifg=#f78c6c    guibg=NONE          gui=NONE
highlight Comment             guifg=#3c435e    guibg=NONE          gui=italic
highlight Constant            guifg=#89ddff    guibg=NONE          gui=bold
highlight String              guifg=#c3e88d    guibg=NONE          gui=NONE
highlight Character           guifg=#f78c6c    guibg=NONE          gui=bold
highlight Number              guifg=#f78c6c    guibg=NONE          gui=bold
highlight Boolean             guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight Float               guifg=#ffcb6b    guibg=NONE          gui=italic
highlight Identifier          guifg=#ffcb6b    guibg=NONE          gui=NONE
highlight Function            guifg=#82aaff    guibg=NONE          gui=italic
highlight Statement           guifg=#89ddff    guibg=NONE          gui=italic
highlight Conditional         guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight Repeat              guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight Label               guifg=#89ddff    guibg=NONE          gui=italic
highlight Operator            guifg=#72a4ff    guibg=NONE          gui=NONE
highlight Keyword             guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight Exception           guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight PreProc             guifg=#89ddff    guibg=NONE          gui=bold
highlight Include             guifg=#72a4ff    guibg=NONE          gui=bold,italic
highlight Define              guifg=#72a4ff    guibg=NONE          gui=bold,italic
highlight Macro               guifg=#72a4ff    guibg=NONE          gui=bold
highlight PreCondit           guifg=#72a4ff    guibg=NONE          gui=bold,italic
highlight Type                guifg=#c792ea    guibg=NONE          gui=NONE
highlight StorageClass        guifg=#89ddff    guibg=NONE          gui=italic
highlight Structure           guifg=#89ddff    guibg=NONE          gui=italic
highlight Typedef             guifg=#89ddff    guibg=NONE          gui=italic
highlight Special             guifg=#c3e88d    guibg=NONE          gui=NONE
highlight SpecialChar         guifg=#c3e88d    guibg=NONE          gui=italic
highlight Delimeter           guifg=#72a4ff    guibg=NONE          gui=NONE

" Quick scope
highlight QuickScopePrimary   guifg=#ff79c6    guibg=NONE          gui=bold,italic
highlight QuickScopeSecondary guifg=#ffffff    guibg=NONE          gui=bold,italic

" Hlslens
highlight HlSearchFloat       guifg=#72a4ff    guibg=NONE          gui=NONE
highlight HlSearchNear        guifg=NONE       guibg=#4e5579       gui=NONE
highlight HlSearchLens        guifg=#ff5370    guibg=#12111e       gui=NONE

" Tree sitter
highlight TSBoolean           guifg=#89ddff    guibg=NONE          gui=italic
highlight TSCharacter         guifg=#f78c6c    guibg=NONE          gui=NONE
highlight TSComment           guifg=#3c435e    guibg=NONE          gui=italic
highlight TSConditional       guifg=#89ddff    guibg=NONE          gui=italic
highlight TSConstant          guifg=#89ddff    guibg=NONE          gui=bold
highlight TSConstBuiltin      guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight TSConstMacro        guifg=#72a4ff    guibg=NONE          gui=bold,italic
highlight TSConstructor       guifg=#89ddff    guibg=NONE          gui=bold,italic
highlight TSFunction          guifg=#82aaff    guibg=NONE          gui=NONE
highlight TSFuncMacro         guifg=#ffcb6b    guibg=NONE          gui=bold
highlight TSInclude           guifg=#72a4ff    guibg=NONE          gui=bold,italic
highlight TSNumber            guifg=#f78c6c    guibg=NONE          gui=bold
highlight TSOperator          guifg=#72a4ff    guibg=NONE          gui=NONE
highlight TSParameter         guifg=#ffcb6b    guibg=NONE          gui=italic
highlight TSProperty          guifg=#ff5370    guibg=NONE          gui=italic
highlight TSPunctBracket      guifg=#72a4ff    guibg=NONE          gui=NONE
highlight TSPunctDelimeter    guifg=#72a4ff    guibg=NONE          gui=NONE
highlight TSPunctDelimiter    guifg=#72a4ff    guibg=NONE          gui=NONE
highlight TSPunctSpecial      guifg=#72a4ff    guibg=NONE          gui=NONE
highlight TSString            guifg=#c3e88d    guibg=NONE          gui=NONE
highlight TSStringEscape      guifg=#72a4ff    guibg=NONE          gui=italic
highlight TSStringRegex       guifg=#ff5370    guibg=NONE          gui=italic
highlight TSType              guifg=#c792ea    guibg=NONE          gui=NONE
highlight TSTypeBuiltin       guifg=#c792ea    guibg=NONE          gui=italic
highlight TSURI               guifg=#ffcb6b    guibg=NONE          gui=underline
highlight TSVariable          guifg=#ffcb6b    guibg=NONE          gui=NONE
highlight TSVariableBuiltin   guifg=#89ddff    guibg=NONE          gui=bold
highlight TSNamespace         guifg=#c792ea    guibg=NONE          gui=italic
highlight TSKeyword           guifg=#89ddff    guibg=NONE          gui=italic

" Tabs
highlight TabLine             guifg=#eeffff    guibg=#111111       gui=NONE
highlight TabLineSel          guifg=#ff5370    guibg=#111111       gui=italic
highlight TabLineFill         guifg=#eeffff    guibg=#111111       gui=NONE

" Nvim Tree
highligh NvimTreeFolderName       guifg=#c792ea    guibg=NONE          gui=NONE
highligh NvimTreeFolderIcon       guifg=#c792ea    guibg=NONE          gui=NONE
highligh NvimTreeEmptyFolderName  guifg=#c792ea    guibg=NONE          gui=NONE
highligh NvimTreeOpenedFolderName guifg=#c792ea    guibg=NONE          gui=NONE
highligh NvimTreeNormal           guifg=#eeffff    guibg=NONE          gui=NONE
highligh NvimTreeSymlink          guifg=#72a4ff    guibg=NONE          gui=NONE
highligh NvimTreeRootFolder       guifg=#ffcb6b    guibg=NONE          gui=italic
highligh NvimTreeSpecialFile      guifg=#ff5370    guibg=NONE          gui=italic
highligh NvimTreeExecFile         guifg=#c3e88d    guibg=NONE          gui=NONE
highligh NvimTreeImageFile        guifg=#89ddff    guibg=NONE          gui=NONE

" Neogit
highligh NeogitDiffAdd              guifg=#9cb970    guibg=#232629          gui=NONE
highligh NeogitDiffAddHighlight     guifg=#c3e88d    guibg=#353c34          gui=NONE
highligh NeogitDiffDelete           guifg=#cc4259    guibg=#4f445f          gui=NONE
highligh NeogitDiffDeleteHighlight  guifg=#ff5370    guibg=#634661          gui=NONE
highligh NeogitDiffContextHighlight guifg=NONE       guibg=NONE             gui=NONE
highligh NeogitHunkHeader           guifg=#12111e    guibg=#44324a          gui=NONE
highligh NeogitHunkHeaderHighlight  guifg=#12111e    guibg=#bb80b3          gui=NONE
highligh NeogitstagedChanges        guifg=#c3e88d    guibg=NONE             gui=italic
highligh NeogitUnstagedChanges      guifg=#ff5370    guibg=NONE             gui=italic

" Indent
highlight IndentBlanklineChar               guifg=#292d3e    guibg=NONE          gui=NONE
highlight IndentBlanklineSpaceChar          guifg=#292d3e    guibg=NONE          gui=NONE
highlight IndentBlanklineSpaceCharBlankline guifg=#292d3e    guibg=NONE          gui=NONE

" Lsp
highlight LspDiagnosticsDefaultError           guifg=#ff5370 guibg=NONE gui=NONE
highlight LspDiagnosticsDefaultWarning         guifg=#f78c6c guibg=NONE gui=NONE
highlight LspDiagnosticsDefaultHint            guifg=#72a4ff guibg=NONE gui=NONE
highlight LspDiagnosticsDefaultInformation     guifg=#c3e88d guibg=NONE gui=NONE
highlight LspDiagnosticsFloatingError          guifg=#ff5370 guibg=NONE gui=NONE
highlight LspDiagnosticsFloatingWarning        guifg=#f78c6c guibg=NONE gui=NONE
highlight LspDiagnosticsFloatingHint           guifg=#72a4ff guibg=NONE gui=NONE
highlight LspDiagnosticsFloatingInformation    guifg=#c3e88d guibg=NONE gui=NONE
highlight LspDiagnosticsSignError              guifg=#ff5370 guibg=NONE gui=NONE
highlight LspDiagnosticsSignWarning            guifg=#f78c6c guibg=NONE gui=NONE
highlight LspDiagnosticsSignHint               guifg=#72a4ff guibg=NONE gui=NONE
highlight LspDiagnosticsSignInformation        guifg=#c3e88d guibg=NONE gui=NONE
highlight LspDiagnosticsUnderlineError         guifg=#ff5370 guibg=NONE gui=undercurl
highlight LspDiagnosticsUnderlineWarning       guifg=#f78c6c guibg=NONE gui=undercurl
highlight LspDiagnosticsUnderlineHint          guifg=#72a4ff guibg=NONE gui=undercurl
highlight LspDiagnosticsUnderlineInformation   guifg=#c3e88d guibg=NONE gui=undercurl
highlight LspDiagnosticsVirtualTextError       guifg=#ff5370 guibg=NONE gui=NONE
highlight LspDiagnosticsVirtualTextWarning     guifg=#f78c6c guibg=NONE gui=NONE
highlight LspDiagnosticsVirtualTextHint        guifg=#72a4ff guibg=NONE gui=NONE
highlight LspDiagnosticsVirtualTextInformation guifg=#c3e88d guibg=NONE gui=NONE

nmap ;s ysiw
nnoremap ]q :cnext<CR>zz
nnoremap [q :cprev<CR>zz
nnoremap zq :qa<CR>
nnoremap zx :qa!<CR>
nnoremap <silent> <Leader>, :JABSOpen<CR>
nnoremap <silent> <Leader>s :w<CR>
nnoremap <silent><Esc> :noh<CR>
nnoremap <Leader>wh <C-w>h
nnoremap <Leader>wj <C-w>j
nnoremap <Leader>wk <C-w>k
nnoremap <Leader>wl <C-w>l
nnoremap <Leader>w<C-o> <C-w><C-o>
nnoremap <Leader>wH <C-w>H
nnoremap <Leader>wJ <C-w>J
nnoremap <Leader>wK <C-w>K
nnoremap <Leader>wL <C-w>L
nnoremap <Leader>w<C-o> <C-w><C-o>
nnoremap <leader>wc <C-w>c
nnoremap <leader>wd <C-w>c
nnoremap <leader>wv <C-w>v
nnoremap <leader>ws <C-w>s
nnoremap <leader>wr <C-w>R
nnoremap <silent> ]w :vertical resize +5<CR>
nnoremap <silent> [w :vertical resize -5<CR>
nnoremap <silent> [W :resize -5<CR>
nnoremap <silent> ]W :resize +5<CR>
nnoremap <silent> <leader>wq :q!<CR>
nnoremap <silent> <leader>wi :so %<CR>
nnoremap <silent> <leader>bk :lua require('bufdelete').bufdelete(0, true)<CR>
nnoremap <silent> <leader>bd :bd<CR>
nnoremap <silent> <leader>to :SymbolOutline<CR>
nnoremap <silent> <leader>tut :UndotreeToggle<CR>
nnoremap <silent> <leader>tuc :UndotreeHide<CR>
nnoremap <silent> <leader>tuf :UndotreeFocus<CR>
nnoremap <silent> [t  :tabprevious<CR>
nnoremap <silent> ]t  :tabnext<CR>
nnoremap <silent> [T  :tabmove -1<CR>
nnoremap <silent> ]T  :tabmove +1<CR>
nnoremap <silent> gtn :tabnew<CR>
nnoremap <silent> gtc :tabclose<CR>
nnoremap <silent> gtf :tabfirst<CR>
nnoremap <silent> gtl :tablast<CR>
nnoremap <silent> gtu :tabrewind<CR>
