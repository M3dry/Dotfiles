local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        "git",
        "clone",
        "--filter=blob:none",
        "--single-branch",
        "https://github.com/folke/lazy.nvim.git",
        lazypath,
    }
end
vim.opt.runtimepath:prepend(lazypath)

require("lazy").setup({
    --------
    -- UI --
    --------
    {
        "glepnir/galaxyline.nvim",
        lazy = false,
        config = function()
            require "m3dry.plugins.galaxyline"
        end,
    },
    "DaikyXendo/nvim-material-icon",
    {
        "nvim-tree/nvim-web-devicons",
        config = function()
            require("nvim-web-devicons").setup {
                override = require("nvim-material-icon").get_icons(),
            }
        end,
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        event = "VeryLazy",
        config = function()
            vim.opt.winbar = "%{%v:lua.require('m3dry.winbar').winbar()%}"
            require("indent_blankline").setup {
                indent_level = 40,
                char = "│",
                space_char_blankline = " ",
                show_trailing_blankline_indent = false,
                use_treesitter = true,
                show_current_context = true,
                context_highlight_list = { "IndentBlanklineContext" },
                bufname_exclude = { "README..*", ".*.md", ".*.norg" },
                context_patterns = {
                    "class",
                    "return",
                    "function",
                    "method",
                    "^if",
                    "^while",
                    "jsx_element",
                    "^for",
                    "^object",
                    "^table",
                    "block",
                    "arguments",
                    "if_statement",
                    "else_clause",
                    "jsx_element",
                    "jsx_self_closing_element",
                    "try_statement",
                    "catch_clause",
                    "import_statement",
                    "operation_type",
                },
            }
        end,
    },
    {
        "xiyaowong/virtcolumn.nvim",
        event = "VeryLazy",
        init = function()
            vim.api.nvim_set_option_value("colorcolumn", "120", {})
        end,
        config = function()
            vim.g.virtcolumn_char = "│"
        end,
    },
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        config = function()
            require("noice").setup {
                cmdline = {
                    enabled = false,
                },
                messages = {
                    enabled = false,
                },
                notify = {
                    enabled = false,
                },
                lsp = {
                    progress = {
                        enabled = false,
                    },
                    override = {
                        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                        ["vim.lsp.util.stylize_markdown"] = true,
                        ["cmp.entry.get_documentation"] = true,
                    },
                    hover = {
                        enabled = true,
                    },
                    signature = {
                        enabled = false,
                    },
                    message = {
                        enabled = true,
                    },
                    documentation = {
                        view = "hover",
                        opts = {
                            lang = "markdown",
                            replace = true,
                            render = "plain",
                            format = { "{message}" },
                            win_options = { concealcursor = "n", conceallevel = 3 },
                        },
                    },
                },
                markdown = {
                    hover = {
                        ["|(%S-)|"] = vim.cmd.help, -- vim help links
                        ["%[.-%]%((%S-)%)"] = require("noice.util").open, -- markdown links
                    },
                    highlights = {
                        ["|%S-|"] = "@text.reference",
                        ["@%S+"] = "@parameter",
                        ["^%s*(Parameters:)"] = "@text.title",
                        ["^%s*(Return:)"] = "@text.title",
                        ["^%s*(See also:)"] = "@text.title",
                        ["{%S-}"] = "@parameter",
                    },
                },
                health = {
                    checker = true, -- Disable if you don't want health checks to run
                },
                smart_move = {
                    enabled = true,
                    excluded_filetypes = { "cmp_menu", "cmp_docs", "notify" },
                },
                presets = {
                    bottom_search = false,
                    command_palette = false,
                    long_message_to_split = false,
                    inc_rename = false,
                    lsp_doc_border = false,
                },
                throttle = 1000 / 30,
            }
        end,
    },
    {
        "prichrd/netrw.nvim",
        event = "VeryLazy",
        config = function()
            require("netrw").setup {
                icons = {
                    symlink = " ",
                    directory = " ",
                    file = "",
                },
                use_devicons = true,
                mappings = {},
            }
        end,
    },

    ---------
    -- LSP --
    ---------
    {
        "neovim/nvim-lspconfig",
        event = "CursorMoved",
        dependencies = { "hrsh7th/cmp-nvim-lsp" },
        config = function()
            require "m3dry.plugins.null-ls"

            require "m3dry.plugins.lspconfig"
            vim.cmd.LspStart()
        end,
    },
    "jose-elias-alvarez/null-ls.nvim",
    "LostNeophyte/null-ls-embedded",
    {
        "lvimuser/lsp-inlayhints.nvim",
        config = function()
            require("lsp-inlayhints").setup {
                inlay_hints = {
                    parameter_hints = {
                        show = true,
                        prefix = " ƒ ",
                        separator = ", ",
                        remove_colon_start = false,
                        remove_colon_end = true,
                    },
                    type_hints = {
                        show = true,
                        prefix = " T ",
                        separator = ", ",
                        remove_colon_start = true,
                        remove_colon_end = false,
                    },
                },
                debug_mode = false,
            }
        end,
    },
    {
        "mrshmllow/document-color.nvim",
        config = function()
            require("document-color").setup {
                mode = "background",
            }
        end,
    },
    { "yioneko/nvim-type-fmt", event = "LspAttach" },
    "b0o/schemastore.nvim",
    "simrat39/rust-tools.nvim",
    "folke/neodev.nvim",
    { "p00f/clangd_extensions.nvim", ft = { "c", "cpp" } },
    "ray-x/go.nvim",
    {
        "amrbashir/nvim-docs-view",
        cmd = "DocsViewToggle",
        config = function()
            require("docs-view").setup {
                position = "bottom",
                height = 15,
            }
        end,
    },
    {
        "glepnir/lspsaga.nvim",
        cmd = "Lspsaga",
        config = function()
            require("lspsaga").init_lsp_saga {
                border_style = "",
                saga_winblend = 0,
                move_in_saga = { prev = "<C-k>", next = "<C-j>" },
                diagnostic_header = { " ", " ", " ", " " },
                show_diagnostic_source = true,
                code_action_icon = " ",
                code_action_num_shortcut = true,
                code_action_lightbulb = {
                    enable = true,
                    sign = true,
                    sign_priority = 20,
                    virtual_text = false,
                },
                code_action_keys = {
                    quit = { "q", "<C-c>", "<Esc>" },
                    exec = "<CR>",
                },
                rename_action_quit = "<C-c>",
                definition_preview_icon = " ",
                show_outline = {
                    win_position = "right",
                    left_with = "",
                    win_width = 30,
                    auto_enter = true,
                    auto_preview = true,
                    virt_text = "┃",
                    jump_key = "o",
                    auto_refresh = true,
                },
            }
        end,
    },
    {
        "smjonas/inc-rename.nvim",
        cmd = "IncRename",
        config = function()
            require("inc_rename").setup()
        end,
    },
    {
        "ahmedkhalf/project.nvim",
        lazy = false,
        config = function()
            require("project_nvim").setup {
                manual_mode = true,
                detection_methods = { "lsp", "pattern" },
                patterns = {
                    ".git",
                    "_darcs",
                    ".hg",
                    ".bzr",
                    ".svn",
                    "Makefile",
                    "Cargo.toml",
                    "go.mod",
                    "package.json",
                    "stylua.toml",
                },
                ignore_lsp = {},
                exclude_dirs = {},
                show_hidden = true,
                silent_chdir = true,
                datapath = vim.fn.stdpath "data",
            }
        end,
    },

    ----------
    -- REPL --
    ----------
    { "radenling/vim-dispatch-neovim", ft = "clojure" },
    { "tpope/vim-dispatch", ft = "clojure" },
    { "clojure-vim/vim-jack-in", ft = "clojure" },
    {
        "Olical/conjure",
        event = "CursorMoved",
        dependencies = { "m00qek/baleia.nvim" },
        config = function()
            vim.g["conjure#log#strip_ansi_escape_sequences_line_limit"] = 0
            vim.g["conjure#extract#tree_sitter#enabled"] = true
            vim.g["conjure#mapping#prefix"] = "<Leader>e"

            vim.api.nvim_create_autocmd("BufWinEnter", {
                pattern = "conjure-log*",
                callback = function(infs)
                    require("baleia").setup({ line_starts_at = 3 }).automatically(infs.buf)
                end,
            })
        end,
    },

    ---------
    -- DAP --
    ---------
    {
        "mfussenegger/nvim-dap",
        config = function()
            require "m3dry.plugins.dap"
            require("nvim-dap-virtual-text").setup {
                enabled = true,
                enabled_commands = true,
                highlight_changed_variables = true,
                highlight_new_as_changed = false,
                show_stop_reason = true,
                commented = false,
                only_first_definition = false,
                all_references = true,
                filter_references_pattern = "<module",
                virt_text_pos = "eol",
                all_frames = false,
                virt_lines = false,
                virt_text_win_col = nil,
            }
        end,
    },
    "rcarriga/nvim-dap-ui",
    "theHamsta/nvim-dap-virtual-text",
    "ofirgall/goto-breakpoints.nvim",

    ----------------
    -- COMPLETION --
    ----------------
    {
        "hrsh7th/nvim-cmp",
        event = { "InsertEnter", "CmdlineEnter *" },
        dependencies = {
            "saadparwaiz1/cmp_luasnip",
            "PaterJason/cmp-conjure",
            "uga-rosa/cmp-dynamic",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
            "hrsh7th/cmp-cmdline",
            "rcarriga/cmp-dap",
        },
        config = function()
            require "m3dry.plugins.cmp"
            require "m3dry.plugins.autopairs"
        end,
    },
    {
        "L3MON4D3/LuaSnip",
        event = "InsertEnter",
        config = function()
            local types = require "luasnip.util.types"
            require("luasnip").config.set_config {
                history = true,
                update_events = "TextChanged,TextChangedI,InsertLeave",
                region_check_events = "CursorMoved,CursorHold,InsertEnter",
                enable_autosnippets = true,
                ext_opts = {
                    [types.insertNode] = {
                        active = {
                            virt_text = { { "", "LuasnipInsert" } },
                        },
                        [types.choiceNode] = {
                            active = {
                                virt_text = { { "", "LuasnipChoice" } },
                            },
                        },
                    },
                },
                snip_env = {
                    s = require("luasnip.nodes.snippet").S,
                    sn = require("luasnip.nodes.snippet").SN,
                    t = require("luasnip.nodes.textNode").T,
                    f = require("luasnip.nodes.functionNode").F,
                    i = require("luasnip.nodes.insertNode").I,
                    c = require("luasnip.nodes.choiceNode").C,
                    d = require("luasnip.nodes.dynamicNode").D,
                    r = require("luasnip.nodes.restoreNode").R,
                    l = require("luasnip.extras").lambda,
                    rep = require("luasnip.extras").rep,
                    p = require("luasnip.extras").partial,
                    m = require("luasnip.extras").match,
                    n = require("luasnip.extras").nonempty,
                    dl = require("luasnip.extras").dynamic_lambda,
                    fmt = require("luasnip.extras.fmt").fmt,
                    fmta = require("luasnip.extras.fmt").fmta,
                    conds = require "luasnip.extras.expand_conditions",
                    types = require "luasnip.util.types",
                    events = require "luasnip.util.events",
                    parse = require("luasnip.util.parser").parse_snippet,
                    ai = require "luasnip.nodes.absolute_indexer",
                    postfix = require("luasnip.extras.postfix").postfix,
                },
            }

            require("luasnip.loaders.from_lua").load { paths = "~/.config/nvim/lua/m3dry/plugins/snippets/" }
        end,
    },

    --------------
    -- TERMINAL --
    --------------
    {
        "akinsho/nvim-toggleterm.lua",
        cmd = { "TermExec", "ToggleTermToggleAll" },
        keys = "<C-n>",
        config = function()
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
                shading_factor = "1",
                start_in_insert = true,
                insert_mappings = true,
                persist_size = true,
                direction = "horizontal",
                close_on_exit = true,
                shell = vim.o.shell,
                highlights = {
                    Normal = {
                        link = "Normal",
                    },
                    NormalFloat = {
                        guifg = "#eeffff",
                    },
                    FloatBorder = {
                        link = "FloatTermBorder",
                    },
                },
                float_opts = {
                    border = "curved",
                    winblend = 0,
                    width = 150,
                    height = 45,
                    highlights = {
                        border = "FloatTermBorder",
                        background = "FloatTermNormal",
                    },
                },
            }
        end,
    },

    -------------
    -- EDITING --
    -------------
    {
        "mrjones2014/legendary.nvim",
        event = "VeryLazy",
        config = function()
            require "m3dry.keybinds"
        end,
    },
    {
        "antoinemadec/FixCursorHold.nvim",
        event = "CursorHold",
        config = function()
            vim.g.cursorhold_updatetime = 100
        end,
    },
    {
        "mbbill/undotree",
        cmd = { "UndotreeFocus", "UndotreeHide", "UndotreeShow", "UndotreeToggle" },
    },
    {
        "ofirgall/open.nvim",
        keys = { "<Leader>o" },
        config = function()
            require("open").setup {}
        end,
    },
    "windwp/nvim-autopairs",
    {
        "eraserhd/parinfer-rust",
        event = { "CursorMoved", "InsertEnter" },
        build = "cargo build --release",
    },
    {
        "kylechui/nvim-surround",
        event = { "CursorMoved", "InsertEnter" },
        config = function()
            require("nvim-surround").setup()
        end,
    },
    {
        "RRethy/vim-illuminate",
        event = "CursorMoved",
        config = function()
            require("illuminate").configure()
        end,
    },
    {
        "ggandor/lightspeed.nvim",
        keys = {
            "s",
            "S",
            "gs",
            "gS",
            "f",
            "F",
            "t",
            "T",
        },
        config = function()
            require("lightspeed").setup {
                jump_to_unique_chars = { safety_timeout = 400 },
                substitute_chars = { ["\r"] = "﬋" },
                limit_ft_matches = 5,
            }
        end,
    },
    {
        "guns/vim-sexp",
        event = { "CursorMoved", "InsertEnter" },
        dependencies = { "tpope/vim-sexp-mappings-for-regular-people" },
        config = function()
            vim.g.sexp_mappings = {
                sexp_select_prev_element = "",
                sexp_select_next_element = "",
            }
        end,
    },
    { "tpope/vim-repeat", event = { "CursorMoved", "InsertEnter" } },
    { "monaqa/dial.nvim", event = { "CursorMoved", "InsertEnter" } },
    {
        "toppair/reach.nvim",
        config = function()
            require("reach").setup {
                notifications = true,
            }
        end,
    },
    { "mhinz/vim-sayonara", cmd = { "Sayonara" } },
    {
        "Pocco81/auto-save.nvim",
        event = { "InsertEnter", "TextChanged" },
        config = function()
            require("auto-save").setup {
                enabled = true,
                execution_message = {
                    message = function()
                        return ""
                    end,
                    dim = 0,
                    cleaning_interval = 1000,
                },
                trigger_events = { "InsertLeave", "TextChanged" },
                condition = function(buf)
                    if vim.fn.getbufvar(buf, "&modifiable") == 1 then
                        return true
                    end
                    return false
                end,
                write_all_buffers = false,
                debounce_delay = 1000,
            }
        end,
    },
    {
        "ja-ford/delaytrain.nvim",
        event = "CursorMoved",
        config = function()
            require("delaytrain").setup {
                delay_ms = 1000,
                grace_period = 2,
                keys = {
                    ["nv"] = { "h", "j", "k", "l" },
                    ["nvi"] = { "<Left>", "<Down>", "<Up>", "<Right>" },
                },
                ignore_filetypes = { "ccc-ui", "help" },
            }
        end,
    },
    {
        "Wansmer/treesj",
        cmd = { "TSJToggle", "TSJSplit", "TSJJoin" },
        config = function()
            local tsj = require "treesj"

            tsj.setup {
                -- Use default keymaps
                -- (<space>m - toggle, <space>j - join, <space>s - split)
                use_default_keymaps = false,
                check_syntax_error = true,
                max_join_length = 120,
                cursor_behavior = "hold",
                notify = true,
            }
        end,
    },

    ------------
    -- SPLITS --
    ------------
    { "szw/vim-maximizer", cmd = "MaximizerToggle" },
    {
        "s1n7ax/nvim-window-picker",
        keys = "<Leader>ww",
        config = function()
            require("window-picker").setup {
                autoselect_one = true,
                include_current = false,
                filter_rules = {
                    bo = {
                        filetype = { "neo-tree", "neo-tree-popup", "notify", "quickfix" },
                        buftype = { "terminal" },
                    },
                },
                fg_color = "#eeffff",
                other_win_hl_color = "#ff5370",
            }

            vim.keymap.set("n", "<leader>ww", function()
                vim.api.nvim_set_current_win(require("window-picker").pick_window() or vim.api.nvim_get_current_win())
            end, { desc = "Pick a window" })
        end,
    },
    {
        "mrjones2014/smart-splits.nvim",
        config = function()
            require("smart-splits").setup {
                ignored_filetypes = {
                    "nofile",
                    "quickfix",
                    "prompt",
                    "terminal",
                    "neo-tree",
                },
                ignored_buftypes = { "NvimTree" },
                move_cursor_same_row = false,
                resize_mode = {
                    quit_key = "<ESC>",
                    silent = false,
                },
            }
        end,
    },
    {
        "sindrets/winshift.nvim",
        cmd = "Winshift",
        config = function()
            require("winshift").setup {
                keymaps = {
                    disable_defaults = true,
                },
            }
        end,
    },
    {
        "stevearc/stickybuf.nvim",
        lazy = false,
        config = function()
            require("stickybuf").setup {
                buftype = {
                    [""] = false,
                    acwrite = false,
                    help = "buftype",
                    nofile = false,
                    nowrite = false,
                    quickfix = "buftype",
                    terminal = false,
                    prompt = "bufnr",
                },
                wintype = {
                    autocmd = false,
                    popup = "bufnr",
                    preview = false,
                    command = false,
                    [""] = false,
                    unknown = false,
                    floating = false,
                },
                filetype = {
                    aerial = "filetype",
                    nerdtree = "filetype",
                },
                bufname = {
                    ["Neogit.*Popup"] = "bufnr",
                },
                autocmds = {
                    defx = [[au FileType defx if &winfixwidth || &winfixheight | silent! PinFiletype | endif]],
                    neogit = [[au FileType NeogitStatus,NeogitLog,NeogitGitCommandHistory if winnr('$') > 1 | silent! PinFiletype | endif]],
                },
            }
        end,
    },

    -------------
    -- FS TREE --
    -------------
    {
        "nvim-neo-tree/neo-tree.nvim",
        cmd = "Neotree",
        branch = "v2.x",
        config = function()
            require "m3dry.plugins.neotree"
        end,
    },
    "mrbjarksen/neo-tree-diagnostics.nvim",

    ---------------
    -- TELESCOPE --
    ---------------
    {
        "nvim-telescope/telescope.nvim",
        cmd = "Telescope",
        dependencies = {
            "nvim-telescope/telescope-ui-select.nvim",
            "nvim-telescope/telescope-dap.nvim",
            "nvim-neorg/neorg-telescope",
            "nvim-telescope/telescope-symbols.nvim",
            "debugloop/telescope-undo.nvim",
        },
        config = function()
            require "m3dry.plugins.telescope"
        end,
    },
    {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
    },

    --------------
    -- QUICKFIX --
    --------------
    {
        "folke/trouble.nvim",
        cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
        config = function()
            require("trouble").setup {
                position = "bottom",
                height = 10,
                width = 50,
                icons = true,
                mode = "document_diagnostics",
                fold_open = "",
                fold_closed = "",
                action_keys = {
                    close = { "q", "<C-c>" },
                    cancel = "<esc>",
                    refresh = "r",
                    jump = { "<cr>", "<tab>" },
                    open_split = { "<c-x>" },
                    open_vsplit = { "<c-v>" },
                    open_tab = { "<c-t>" },
                    jump_close = { "o" },
                    toggle_mode = "m",
                    toggle_preview = "P",
                    hover = "K",
                    preview = "p",
                    close_folds = { "zM", "zm" },
                    open_folds = { "zR", "zr" },
                    toggle_fold = { "zA", "za" },
                    previous = "k",
                    next = "j",
                },
                indent_lines = true,
                auto_open = false,
                auto_close = false,
                auto_preview = true,
                auto_fold = false,
                signs = {
                    error = "",
                    warning = "",
                    hint = "",
                    information = "",
                    other = "",
                },
                use_lsp_diagnostic_signs = false,
            }
        end,
    },
    {
        "stevearc/qf_helper.nvim",
        cmd = {
            "QFToggle",
            "QFOpen",
            "QFNext",
            "QFPrev",
            "LLToggle",
            "LLOpen",
            "LLNext",
            "LLPrev",
        },
        config = function()
            require("qf_helper").setup {
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
                },
            }
        end,
    },

    -----------------
    -- NOTE TAKING --
    -----------------
    {
        "nvim-neorg/neorg",
        ft = "norg",
        build = ":Neorg sync-parsers",
        config = function()
            require "m3dry.plugins.neorg"
        end,
    },

    ------------
    -- COLORS --
    ------------
    {
        "NvChad/nvim-colorizer.lua",
        event = { "CursorMoved", "CursorHold" },
        config = function()
            require("colorizer").setup {
                filetypes = {
                    "*",
                    "!svelte",
                    "!tsx",
                    "!html",
                    "!css",
                },
                user_default_options = {
                    RGB = false,
                    RRGGBB = true,
                    names = false,
                    RRGGBBAA = false,
                    AARRGGBB = false,
                    rgb_fn = true,
                    hsl_fn = true,
                    css = true,
                    css_fn = true,
                    mode = "background",
                    virtualtext = "■",
                },
                buftypes = {},
            }

            vim.cmd.ColorizerAttachToBuffer()
        end,
    },
    {
        "folke/todo-comments.nvim",
        event = { "InsertEnter", "CursorMoved", "WinScrolled" },
        cmd = { "TodoQuickFix", "TodoLocList", "TodoTelescope", "TodoTrouble" },
        config = function()
            require("todo-comments").setup {
                signs = true,
                sign_priority = 8,
                keywords = {
                    FIX = {
                        icon = " ",
                        color = "error",
                        alt = { "FIXME", "BUG", "FIXIT", "ISSUE" },
                    },
                    TODO = {
                        icon = " ",
                        color = "info",
                    },
                    HACK = {
                        icon = " ",
                        color = "warning",
                    },
                    WARN = {
                        icon = " ",
                        color = "warning",
                        alt = { "WARNING", "XXX" },
                    },
                    PERF = { icon = " ", color = "performance", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
                    NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
                    TEST = { icon = " ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
                },
                gui_style = {
                    fg = "NONE",
                    bg = "BOLD",
                },
                merge_keywords = false,
                highlight = {
                    multiline = true,
                    multiline_pattern = "^.",
                    multiline_context = 10,
                    before = "",
                    keyword = "wide_bg",
                    after = "fg",
                    pattern = [[.*<(KEYWORDS)\s*:]],
                    comments_only = true,
                    max_line_len = 10000,
                    exclude = { "norg", "markdown" },
                },
                colors = {
                    error = { "DiagnosticError", "ErrorMsg", "#ff5370" },
                    warning = { "DiagnosticWarning", "WarningMsg", "#f78c6c" },
                    hint = { "DiagnosticHint", "#72a4ff" },
                    info = { "DiagnosticInfo", "#c3e88d" },
                    performance = { "Type", "#c792ea" },
                    test = { "Keyword", "#89ddff" },
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
        end,
    },

    ----------------
    -- TREESITTER --
    ----------------
    {
        "nvim-treesitter/nvim-treesitter",
        event = { "InsertEnter", "CursorMoved", "WinScrolled" },
        dependencies = { "mrjones2014/nvim-ts-rainbow", "yioneko/nvim-yati" },
        config = function()
            require "m3dry.plugins.treesitter"
        end,
    },
    {
        "JoosepAlviste/nvim-ts-context-commentstring",
        config = function()
            require("nvim-treesitter.configs").setup {
                context_commentstring = {
                    enable = true,
                },
            }
        end,
    },
    "nvim-treesitter/nvim-treesitter-context",
    {
        "nvim-treesitter/nvim-treesitter-textobjects",
        keys = {
            { "af", mode = "o" },
            { "if", mode = "o" },
            { "ip", mode = "o" },
            { "ap", mode = "o" },
            { "ic", mode = "o" },
            { "ac", mode = "o" },
            { "iC", mode = "o" },
            { "aC", mode = "o" },
            { "il", mode = "o" },
            { "al", mode = "o" },
            { "C", mode = "o" },
            { "as", mode = "o" },
            { "is", mode = "o" },
            "]m",
            "[m",
            "]f",
            "]p",
            "]c",
            "]o",
            "]l",
            "]s",
            "]F",
            "]P",
            "]C",
            "]O",
            "]L",
            "]S",
            "[f",
            "[p",
            "[c",
            "[o",
            "[l",
            "[s",
            "[F",
            "[P",
            "[C",
            "[O",
            "[L",
            "[S",
        },
        config = function()
            require("nvim-treesitter.configs").setup {
                textobjects = {
                    select = {
                        enable = true,
                        lookahead = true,
                        keymaps = {
                            ["af"] = "@function.outer",
                            ["if"] = "@function.inner",
                            ["ip"] = "@parameter.inner",
                            ["ap"] = "@parameter.outer",
                            ["ic"] = "@call.inner",
                            ["ac"] = "@call.outer",
                            ["iC"] = "@conditional.inner",
                            ["aC"] = "@conditional.outer",
                            ["il"] = "@loop.inner",
                            ["al"] = "@loop.outer",
                            ["C"] = "@comment.outer",
                            ["as"] = "@statement.outer",
                            ["is"] = "@statement.outer",
                        },
                    },
                    swap = {
                        enable = true,
                        swap_next = {
                            ["]m"] = "@parameter.inner",
                        },
                        swap_previous = {
                            ["[m"] = "@parameter.inner",
                        },
                    },
                    move = {
                        enable = true,
                        set_jumps = true,
                        goto_next_start = {
                            ["]f"] = "@function.outer",
                            ["]p"] = "@parameter.inner",
                            ["]c"] = "@call.outer",
                            ["]o"] = "@conditional.outer",
                            ["]l"] = "@loop.outer",
                            ["]s"] = "@comment.outer",
                        },
                        goto_next_end = {
                            ["]F"] = "@function.outer",
                            ["]P"] = "@parameter.inner",
                            ["]C"] = "@call.outer",
                            ["]O"] = "@conditional.outer",
                            ["]L"] = "@loop.outer",
                            ["]S"] = "@comment.outer",
                        },
                        goto_previous_start = {
                            ["[f"] = "@function.outer",
                            ["[p"] = "@parameter.inner",
                            ["[c"] = "@call.outer",
                            ["[o"] = "@conditional.outer",
                            ["[l"] = "@loop.outer",
                            ["[s"] = "@comment.outer",
                        },
                        goto_previous_end = {
                            ["[F"] = "@function.outer",
                            ["[P"] = "@parameter.inner",
                            ["[C"] = "@call.outer",
                            ["[O"] = "@conditional.outer",
                            ["[L"] = "@loop.outer",
                            ["[S"] = "@comment.outer",
                        },
                    },
                },
            }
        end,
    },
    {
        "nvim-treesitter/playground",
        cmd = { "TSHighlightCapturesUnderCursor", "TSPlaygroundToggle" },
        config = function()
            require("nvim-treesitter.configs").setup {
                playground = {
                    enable = true,
                    disable = {},
                    updatetime = 25,
                    persist_queries = false,
                    keybindings = {
                        toggle_query_editor = "o",
                        toggle_hl_groups = "i",
                        toggle_injected_languages = "t",
                        toggle_anonymous_nodes = "a",
                        toggle_language_display = "I",
                        focus_language = "f",
                        unfocus_language = "F",
                        update = "R",
                        goto_node = "<cr>",
                        show_help = "?",
                    },
                },
            }
        end,
    },

    -------------
    -- COMMENT --
    -------------
    {
        "numToStr/Comment.nvim",
        event = { "CursorMoved", "CursorHold" },
        config = function()
            require("Comment").setup {
                padding = true,
                sticky = true,
                ignore = "^$",
                toggler = {
                    line = "gcc",
                    block = "gbc",
                },
                opleader = {
                    line = "gc",
                    block = "gb",
                },
                mappings = {
                    basic = true,
                    extra = true,
                },
                pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
            }
        end,
    },

    ---------
    -- GIT --
    ---------
    {
        "lewis6991/gitsigns.nvim",
        event = "VeryLazy",
        config = function()
            require("gitsigns").setup {
                signs = {
                    add = {
                        hl = "GitSignsAdd",
                        text = "",
                        numhl = "GitSignsAddNr",
                        linehl = "GitSignsAddLn",
                    },
                    change = {
                        hl = "GitSignsChange",
                        text = "",
                        numhl = "GitSignsChangeNr",
                        linehl = "GitSignsChangeLn",
                    },
                    changedelete = {
                        hl = "GitSignsChangeDelete",
                        text = "",
                        numhl = "GitSignsChangeDeleteNr",
                        linehl = "GitSignsChangeDeleteLn",
                    },
                    delete = {
                        hl = "GitSignsDelete",
                        text = "",
                        numhl = "GitSignsDeleteNr",
                        linehl = "GitSignsDeleteLn",
                    },
                    topdelete = {
                        hl = "GitSignsTopDelete",
                        text = "",
                        numhl = "GitSignsTopDeleteNr",
                        linehl = "GitSignsTopDeleteLn",
                    },
                    untracked = {
                        hl = "GitSignsUntracked",
                        text = "ﱡ",
                        numhl = "GitSignsUntrackedNr",
                        linehl = "GitSignsUntrackedLn",
                    },
                },
                signcolumn = true,
                numhl = false,
                linehl = false,
                word_diff = false,
                keymaps = nil,
                watch_gitdir = {
                    interval = 1000,
                    follow_files = true,
                },
                attach_to_untracked = true,
                current_line_blame = false,
                current_line_blame_opts = {
                    virt_text = true,
                    virt_text_pos = "eol",
                    delay = 500,
                },
                current_line_blame_formatter_opts = {
                    relative_time = false,
                },
                sign_priority = 1,
                update_debounce = 100,
                status_formatter = nil,
                max_file_length = 40000,
                preview_config = {
                    border = "rounded",
                    style = "minimal",
                    relative = "cursor",
                    row = 0,
                    col = 1,
                },
                yadm = {
                    enable = false,
                },
            }
        end,
    },

    ----------
    -- MISC --
    ----------
    {
        "elkowar/yuck.vim",
        lazy = false,
    },
    {
        "jghauser/mkdir.nvim",
        event = { "CursorMoved", "InsertEnter" },
        config = function()
            require "mkdir"
        end,
    },
    {
        "winston0410/range-highlight.nvim",
        event = "CmdlineEnter *",
        config = function()
            require("range-highlight").setup {}
        end,
    },
    {
        "nacro90/numb.nvim",
        event = "CmdlineEnter *",
        config = function()
            require("numb").setup()
        end,
    },
    "winston0410/cmd-parser.nvim",
    {
        "AckslD/messages.nvim",
        cmd = "Messages",
        config = function()
            require("messages").setup()
        end,
    },
    {
        "folke/zen-mode.nvim",
        cmd = "ZenMode",
        config = function()
            require("zen-mode").setup {}
        end,
    },

    ---------------------
    -- OWNING THE LIBS --
    ---------------------
    "nvim-lua/popup.nvim",
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
}, {
    defaults = { lazy = true },
    install = {
        missing = true,
        colorscheme = {},
    },
    lockfile = HOME .. "/.config/flake/dots/nvim/lazy-lock.json",
    performance = {
        rtp = {
            disabled_plugins = {
                "gzip",
                "zip",
                "zipPlugin",
                "tar",
                "tarPlugin",
                "getscript",
                "getscriptPlugin",
                "vimball",
                "vimballPlugin",
                "2html_plugin",
                "logipat",
                "rrhelper",
                "spellfile_plugin",
                "matchit",
                "tutor_mode_plugin",
                "remote_plugins",
                "shada_plugin",
                "filetype",
                "spellfile",
                "tohtml",
            },
        },
    },
})
