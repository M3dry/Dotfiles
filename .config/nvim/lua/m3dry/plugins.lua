return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use {
        'glepnir/galaxyline.nvim',
        branch = 'main',
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
    use {
        'unblevable/quick-scope',
        config = function()
            vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
        end
    }
    use {
        'famiu/nvim-reload',
    }
    use {
        'tpope/vim-surround',
    }
    use {
        'easymotion/vim-easymotion',
    }
    use {
        'mizlan/iswap.nvim',
        config = function()
            require('iswap').setup{
                keys = 'sdfghjkl',
                grey = 'enable',
                hl_snipe = 'CursorLineNr',
                hl_selection = 'Visual',
            }
        end
    }
    use {
        't9md/vim-choosewin',
        config = function()
            vim.cmd[[let g:choosewin_overlay_enable = 1
let g:choosewin_statusline_replace = 0
let g:choosewin_color_label = { 'gui': ['#12111e', '#72a4ff'] }
let g:choosewin_color_label_current = { 'gui': ['#12111e', '#ff5370'] }
let g:choosewin_color_overlay = { 'gui': ['#72a4ff', '#72a4ff'] }
let g:choosewin_color_overlay_current = { 'gui': ['#ff5370', '#ff5370'] }
let g:choosewin_color_other = { 'gui': ['#12111e', '#12111e'] }]]
        end
    }
    use {
        'tommcdo/vim-lion',
        config = function()
            vim.b.lion_squeeze_spaces = 1
        end
    }
    use {
        'rrethy/vim-hexokinase',
        run = 'make hexokinase',
        config = function()
            vim.g.Hexokinase_highlighters = { "backgroundfull"}
            vim.g.Hexokinase_optInPatterns = "full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names"
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
        run = 'cd app && yarn install'
    }
    use 'tpope/vim-eunuch'
    use 'ThePrimeagen/vim-be-good'
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
        'oberblastmeister/neuron.nvim',
        config = function()
            require'neuron'.setup {
                virtual_titles = true,
                mappings = false,
                run = nil,
                neuron_dir = "~/my-stuff/Neuron",
                leader = "<Leader>n"
            }
            vim.cmd(string.format("au BufRead %s/*.md lua require('m3dry.keybinds').neuronbinds()", require('neuron').config.neuron_dir))
        end
    }
    use 'tpope/vim-repeat'
    use 'tpope/vim-speeddating'
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
    use 'lfilho/cosco.vim'
    use 'szw/vim-maximizer'
    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function()
            vim.g.indent_blankline_indent_level = 7
            vim.g.indent_blankline_char = '│'
            vim.g.indent_blankline_space_char = '·'
            vim.g.indent_blankline_space_char_blankline = '·'
            vim.g.indent_blankline_show_trailing_blankline_indent = false
            vim.g.indent_blankline_use_treesitter = true
            vim.g.indent_blankline_show_current_context = true
            vim.g.indent_blankline_context_patterns = { '^if', '^for', '^do while', '^while' }
            vim.g.indent_blankline_context_highlight_list = { 'IndentContext' }
            vim.g.indent_blankline_bufname_exclude = { 'README..*', '.*.md' }
        end
    }
    use 'matbme/JABS.nvim'
    use 'johann2357/nvim-smartbufs'
    use {
        'neovim/nvim-lspconfig', 
        config = function()
            require'lspconfig'.clangd.setup {}
            require('nlua.lsp.nvim').setup(require('lspconfig'), {
                on_attach = function(...) end
            })
        end,
        requires = {
            {
                'hrsh7th/nvim-compe',
                config = function()
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
                end
            },
            {
                'folke/trouble.nvim',
                config = function()
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
                end
            },
            'tjdevries/nlua.nvim',
            {
                'ray-x/lsp_signature.nvim',
                config = function()
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
                end
            },
            {
                'onsails/lspkind-nvim',
                config = function()
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
                        }
                    })
                end
            },
            'nvim-lua/lsp-status.nvim',
            {
                'ahmedkhalf/lsp-rooter.nvim',
                config = function()
                    require("lsp-rooter").setup()
                end
            },
            {
                'glepnir/lspsaga.nvim',
                config = function()
                    require('lspsaga').init_lsp_saga {
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
                        rename_prompt_prefix = '',
                    }
                end
            },
            {
                'simrat39/symbols-outline.nvim',
                config = function()
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
                end
            },
            {
                'mfussenegger/nvim-dap',
                config = function()
                    require('dap').adapters.c = {
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
                    
                    require('dap').configurations.c = {
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
                    
                    require("dapui").setup{
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
                    }
                end,
                requires = 'rcarriga/nvim-dap-ui'
            },
            {
                'hrsh7th/vim-vsnip',
                requires = {
                    'hrsh7th/vim-vsnip-integ'
                },
                config = function()
                    vim.g.vsnip_snippet_dir = "$HOME/.config/nvim/snippets/"
                end
            },
            {
                'michaelb/sniprun',
                config = function()
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
                            SniprunVirtualTextOk = { fg="#72a4ff" },
                        },
                        inline_messages = 0,
                        borders = 'double'
                    })
                end,
                run = 'bash install.sh'
            },
        }
    }
    use {
        'kyazdani42/nvim-tree.lua',
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
        config = function()
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
        end
    }

    use {
        'kkoomen/vim-doge',
        run = ':call doge#install()',
        config = function()
            vim.g.doge_doc_standard_c = 'kernel_doc'
        end
    }
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function()
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
        end,
        requires = {
            {
                'nvim-treesitter/playground',
                'p00f/nvim-ts-rainbow',
                'mfussenegger/nvim-ts-hint-textobject',
                'lewis6991/spellsitter.nvim',
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
        end
    }
    use 'famiu/bufdelete.nvim'
    use {
        'TimUntersberger/neogit',
        config = function()
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

            vim.g.blameLineUseVirtualText = 1
            vim.g.blameLineVirtualTextHighlight = 'Comment'
            vim.g.blameLineGitFormat = '%an:%as - %s %h'
        end,
        requires = {
            'sindrets/diffview.nvim',
            'tveskag/nvim-blame-line'
        }
    }
end)
