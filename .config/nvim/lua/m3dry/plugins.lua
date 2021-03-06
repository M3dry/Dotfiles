local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.api.nvim_command('packadd packer.nvim')
end

return require('packer').startup({function(use)
    use 'wbthomason/packer.nvim'
    use {
        'glepnir/galaxyline.nvim',
        config = function() require('m3dry.statusline') end,
        requires = {
            'kyazdani42/nvim-web-devicons',
        }
    }
    use {
        'windwp/nvim-autopairs',
        config = function()
            require("nvim-autopairs").setup({
                check_ts = true,
            })
            require("nvim-autopairs.completion.compe").setup({
                map_cr = true,
                map_complete = true
            })
        end
    }
    use {
        'Iron-E/nvim-cartographer',
        config = function() require('m3dry.keybinds') end
    }
    use {
        'Pocco81/AutoSave.nvim',
        event = 'InsertLeave',
        config = function()
            require("autosave").setup {
                enabled = true,
                execution_message = "AutoSave: " .. vim.fn.strftime("%H:%M:%S"),
                events = {"InsertLeave", "WinEnter"},
                conditions = {
                    exists = true,
                    filetype_is_not = {},
                    modifiable = true
                },
                write_all_buffers = false,
                on_off_commands = true,
                clean_command_line_interval = 2500
            }
        end
    }
    use {
        'nacro90/numb.nvim',
        config = function()
            require('numb').setup {
                show_numbers = true,
                show_cursorline = false
            }
        end
    }
    use {
        'jghauser/mkdir.nvim',
        config = function() require('mkdir') end
    }
    use {
        'mbbill/undotree',
        cmd = { 'UndotreeFocus', 'UndotreeHide', 'UndotreeShow', 'UndotreeToggle' }
    }
    use {
        'winston0410/range-highlight.nvim',
        config = function()
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
        end
    }
    use 'tpope/vim-surround'
    use {
        'ggandor/lightspeed.nvim',
        config = function ()
            require('lightspeed').setup {
                jump_to_first_match = true,
                jump_on_partial_input_safety_timeout = 400,
                highlight_unique_chars = false,
                grey_out_search_area = true,
                match_only_the_start_of_same_char_seqs = true,
                limit_ft_matches = 5,
                full_inclusive_prefix_key = '<c-x>',
                labels = nil,
                cycle_group_fwd_key = nil,
                cycle_group_bwd_key = nil,
            }
        end
    }
    use 'chaoren/vim-wordmotion'
    use {
        'mizlan/iswap.nvim',
        cmd = { 'ISwap' },
        config = function()
            require('iswap').setup{
                keys = 'sdfghjkl',
                grey = 'enable',
                hl_snipe = 'CursorLineNr',
                hl_selection = 'Visual',
            }
        end
    }
    use 'junegunn/vim-easy-align'
    use {
        'norcalli/nvim-colorizer.lua',
        config = function()
            require('colorizer').setup(
            {
                '*'
            },
            {
                mode = 'background',
                RGB = true,
                RRGGBB = true,
                names = true,
                RRGGBBAA = true,
                rgb_fn = true,
                hsl_fn = true,
                css = true,
                css_fn = true,
            })
        end
    }
    use {
        'winston0410/commented.nvim',
        config = function()
            require('commented').setup({
                comment_padding = " ",
            	keybindings = { n = "gc", v = "gc", nl = "gcc" },
            	set_keybindings = true,
            	ex_mode_cmd = "Comment"
            })
        end
    }
    use {
        'iamcco/markdown-preview.nvim',
        ft = { 'md' },
        run = 'cd app && yarn install'
    }
    use 'tpope/vim-eunuch'
    use {
        'ThePrimeagen/vim-be-good',
        cmd = { 'VimBeGood' }
    }
    use {
        'nvim-telescope/telescope.nvim',
        config = function() require('m3dry.telescope') end,
        requires = { 'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim', {
                'nvim-telescope/telescope-fzy-native.nvim',
                'nvim-telescope/telescope-symbols.nvim',
                'nvim-telescope/telescope-dap.nvim',
            }
        }
    }
    use {
        'ThePrimeagen/harpoon',
        config = function ()
            require("harpoon").setup {
                global_settings = {
                    save_on_toggle = false,
                    save_on_change = true,
                }
            }
        end
    }
    use 'tpope/vim-repeat'
    use 'monaqa/dial.nvim'
    use {
        'glts/vim-radical',
        requires = {
            'glts/vim-magnum'
        }
    }
    use {
        'NFrid/due.nvim',
        config = function()
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
        end
    }
    use {
        'kevinhwang91/nvim-hlslens',
        config = function()
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
        end
    }
    use {
        'lfilho/cosco.vim',
        cmd = { 'CommaOrSemiColon' }
    }
    use {
        'szw/vim-maximizer',
        cmd = { 'MaximizerToggle' }
    }
    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function()
            vim.g.indent_blankline_indent_level = 40
            vim.g.indent_blankline_char = '│'
            vim.g.indent_blankline_space_char = '·'
            vim.g.indent_blankline_space_char_blankline = ' '
            vim.g.indent_blankline_show_trailing_blankline_indent = false
            vim.g.indent_blankline_use_treesitter = true
            vim.g.indent_blankline_show_current_context = true
            vim.g.indent_blankline_context_patterns = {
                "class",
                "function",
                "method",
                "^if",
                "while",
                "for",
                "with",
                "func_literal",
                "block",
                "try",
                "except",
                "argument_list",
                "object",
                "dictionary",
                "variable",
                "field"
            }
            vim.g.indent_blankline_context_highlight_list = { 'IndentContext' }
            vim.g.indent_blankline_bufname_exclude = { 'README..*', '.*.md' }
        end
    }
    use {
        'matbme/JABS.nvim',
        cmd = { 'JABSOpen' }
    }
    use 'johann2357/nvim-smartbufs'
    use {
        'neovim/nvim-lspconfig',
        config = function() require('m3dry.lsp') end,
        requires = {
            'hrsh7th/nvim-compe',
            'folke/trouble.nvim',
            'ray-x/lsp_signature.nvim',
            'folke/lua-dev.nvim',
            'nvim-lua/lsp-status.nvim',
            'ahmedkhalf/lsp-rooter.nvim',
            'glepnir/lspsaga.nvim',
            {
                'simrat39/symbols-outline.nvim',
                cmd = { 'SymbolsOutlineOpen', 'SymbolsOutlineClose', 'SymbolsOutline' }
            },
            {
                'mfussenegger/nvim-dap',
                requires = 'rcarriga/nvim-dap-ui'
            },
            {
                'hrsh7th/vim-vsnip',
                requires = 'hrsh7th/vim-vsnip-integ',
            },
        }
    }
    use {
        'michaelb/sniprun',
        cmd = {
            'SnipRun', 'SnipClose', 'SnipReset',
            'SnipTerminate', 'SnipInfo',
            'SnipReplMemoryClean'
        },
        run = 'bash install.sh',
        config = function()
            require'sniprun'.setup {
                display = {
                    "Classic",
                    "VirtualTextOk",
                    -- "VirtualTextErr",
                    -- "TempFloatingWindow",
                    -- "LongTempFloatingWindow",
                    -- "Terminal"
                    },
                snipruncolors = {
                    SniprunVirtualTextOk = { fg="#72a4ff" },
                },
                inline_messages = 0,
                borders = 'double'
            }
        end
    }
    use {
        'gennaro-tedesco/nvim-jqx',
        config = function ()
            require('nvim-jqx.config').query_key = ';'
            require('nvim-jqx.config').sort = false
        end,
        cmd = { 'JqxList', 'JqxQuery' }
    }
    use {
        'wesQ3/vim-windowswap',
        config = function ()
            vim.g.windowswap_map_keys = 0
        end
    }
    use {
        'kyazdani42/nvim-tree.lua',
        cmd = {
            'NvimTreeToggle', 'NvimTreeClose', 'NvimTreeOpen',
            'NvimTreeFindFile', 'NvimTreeRefresh', 'NvimTreeResize',
            'NvimTreeClipboard' },
        config = function()
            vim.g.nvim_tree_side = 'left'
            vim.g.nvim_tree_width = 25
            vim.g.nvim_tree_ignore = { '.git' }
            vim.g.nvim_tree_gitignore = 0
            vim.g.nvim_tree_auto_open = 0
            vim.g.nvim_tree_auto_close = 1
            vim.g.nvim_tree_auto_ignore_ft = { 'startify', 'dashboard' }
            vim.g.nvim_tree_quit_on_open = 0
            vim.g.nvim_tree_follow = 1
            vim.g.nvim_tree_indent_markers = 1
            vim.g.nvim_tree_hide_dotfiles = 0
            vim.g.nvim_tree_git_hl = 1
            vim.g.nvim_tree_highlight_opened_files = 1
            vim.g.nvim_tree_root_folder_modifier = ':~'
            vim.g.nvim_tree_tab_open = 1
            vim.g.nvim_tree_width_allow_resize  = 0
            vim.g.nvim_tree_disable_netrw = 1
            vim.g.nvim_tree_disable_default_keybindings = 1
            vim.g.nvim_tree_hijack_netrw = 1
            vim.g.nvim_tree_add_trailing = 1
            vim.g.nvim_tree_group_empty = 1
            vim.g.nvim_tree_lsp_diagnostics = 1
            vim.g.nvim_tree_disable_window_picker = 1
            vim.g.nvim_tree_hijack_cursor = 1
            vim.g.nvim_tree_icon_padding = ' '
            vim.g.nvim_tree_update_cwd = 1
            vim.g.nvim_tree_special_files = { 'README.md', 'README.org', 'Makefile'}
            vim.g.nvim_tree_show_icons = {
                git = 1,
                folders = 1,
                files = 1,
                folder_arrows = 1,
            }
            vim.g.nvim_tree_icons = {
                default = '',
                symlink = '',
                git = {
                    unstaged = "✗",
                    staged = "✓",
                    unmerged = "",
                    renamed = "➜",
                    untracked = "★",
                    deleted = "",
                    ignored = "◌"
                },
                folder = {
                    arrow_open = "ﬔ",
                    arrow_closed = "⬎",
                    default = "",
                    open = "",
                    empty = "",
                    empty_open = "",
                    symlink = "",
                    symlink_open = "",
                },
                lsp = {
                    hint = "",
                    info = "",
                    warning = "",
                    error = "",
                }
            }
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
        end
    }
    use {
        'akinsho/nvim-toggleterm.lua',
        cmd = { 'TermExec' },
        keys = { { 'n', '<C-n>' }, { 'i', '<C-n>' } },
        config = function ()
            require("toggleterm").setup {
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
                direction = 'float',
                close_on_exit = true,
                shell = vim.o.shell,
                float_opts = {
                    border = 'curved',
                    winblend = 0,
                    width = 150,
                    height = 45,
                    highlights = {
                        border = "FloatBorder",
                        background = "NormalFloat",
                    }
                }
            }
        end
    }
    use {
        'kkoomen/vim-doge',
        run = ':call doge#install()',
        cmd = { 'DogeGenerate', 'DogeCreateDocStandard' },
        config = function()
            vim.g.doge_doc_standard_c = 'kernel_doc'
        end
    }
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function() require('m3dry.treesitter') end,
        requires = {
            {
                'nvim-treesitter/playground',
                'p00f/nvim-ts-rainbow',
                'mfussenegger/nvim-ts-hint-textobject',
                'nvim-treesitter/nvim-treesitter-refactor',
                'JoosepAlviste/nvim-ts-context-commentstring',
                'nvim-treesitter/nvim-treesitter-textobjects',
                'RRethy/nvim-treesitter-textsubjects',
            }
        }
    }
    use {
        'folke/todo-comments.nvim',
        config = function()
            require("todo-comments").setup {
                signs = true,
                sign_priority = 8,
                keywords = {
                    FIX  = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
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
        end
    }
    use {
        'mhinz/vim-sayonara',
        cmd = { 'Sayonara' }
    }
    use {
        'tversteeg/registers.nvim',
        keys = { { 'n', '"' }, { 'i', '<c-r>' } },
        cmd = { 'Registers' }
    }
    use 'romainl/vim-cool'
    use 'AndrewRadev/splitjoin.vim'
    use {
        'stevearc/qf_helper.nvim',
        cmd = {
            'QFToggle', 'QFOpen', 'QFNext', 'QFPrev',
            'LLToggle', 'LLOpen', 'LLNext', 'LLPrev'
        },
        config = function()
            require('qf_helper').setup {
                prefer_loclist = true,
                sort_lsp_diagnostics = true,
                quickfix = {
                    autoclose = true,
                    default_bindings = true,
                    default_options = true,
                    max_height = 10,
                    min_height = 1,
                    track_location = true,
                },
                loclist = {
                    autoclose = true,
                    default_bindings = true,
                    default_options = true,
                    max_height = 10,
                    min_height = 1,
                    track_location = true,
                }
            }
        end
    }
    use {
        'TimUntersberger/neogit',
        cmd = { 'Neogit' },
        config = function()
            require('neogit').setup {
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
        end
    }
    use {
        'tveskag/nvim-blame-line',
        cmd = { 'ToggleBlameLine', 'EnableBlameLine', 'DisableBlameLine', 'SingleBlameLine' },
        config = function ()
            vim.g.blameLineUseVirtualText = 1
            vim.g.blameLineVirtualTextHighlight = 'Comment'
            vim.g.blameLineGitFormat = '   %an • %as • %s %h'
        end
    }
    use {
        'sindrets/diffview.nvim',
        cmd = {
            'DiffviewOpen', 'DiffviewClose',
            'DiffviewRefresh', 'DiffviewFocusFiles',
            'DiffviewToggleFiles'
        },
        config = function()
            local cb = require'diffview.config'.diffview_callback

            require('diffview').setup {
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
                        ["<Leader>e"] = cb("focus_files"),
                        ["<Leader>b"] = cb("toggle_files"),
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
                        ["<Leader>e"]     = cb("focus_files"),
                        ["<Leader>b"]     = cb("toggle_files"),
                    }
                }
            }
        end
    }
    use {
        'lewis6991/gitsigns.nvim',
        config = function()
            require('gitsigns').setup {
                signs = {
                    add          = { hl = 'GitSignsAdd'   ,       text = '', numhl = 'GitSignsAddNr'   ,       linehl = 'GitSignsAddLn' },
                    change       = { hl = 'GitSignsChange',       text = '', numhl = 'GitSignsChangeNr',       linehl = 'GitSignsChangeLn' },
                    changedelete = { hl = 'GitSignsChangeDelete', text = '', numhl = 'GitSignsChangeDeleteNr', linehl = 'GitSignsChangeDeleteLn' },
                    delete       = { hl = 'GitSignsDelete',       text = '', numhl = 'GitSignsDeleteNr',       linehl = 'GitSignsDeleteLn' },
                    topdelete    = { hl = 'GitSignsTopDelete',    text = '', numhl = 'GitSignsTopDeleteNr',    linehl = 'GitSignsTopDeleteLn' },
                },
                numhl = true,
                linehl = false,
                keymaps = {
                },
                watch_index = {
                    interval = 1000,
                    follow_files = true
                },
                current_line_blame = false,
                current_line_blame_delay = 1000,
                current_line_blame_position = 'eol',
                sign_priority = 6,
                update_debounce = 100,
                status_formatter = nil,
                word_diff = false,
                use_decoration_api = true,
                use_internal_diff = true,
            }
        end
    }
    use {
        'RishabhRD/nvim-cheat.sh',
        cmd = { 'Cheat', 'CheatWithoutComments', 'CheatList', 'CheatListWithoutComments' },
        requires = {
            'RishabhRD/popfix'
        }
    }
end,
config = {
    display = {
        open_fn = function ()
            return require('packer.util').float({ border = 'single' })
        end
    },
    profile = {
        enable = true,
        threshold = 0.00001
    }
}})
