require('lspconfig').clangd.setup {
    cmd = {
        'clangd', '--background-index',
        '--clang-tidy', '--completion-style=bundled', '--header-insertion=iwyu',
        '--suggest-missing-includes', '--cross-file-rename'
    }
}

require('lspconfig').sumneko_lua.setup {
    cmd = { "lua-language-server" },
    settings = {
        Lua = {
            runtime = {
                version = 'LuaJIT',
                path = vim.split(package.path, ';'),
            },
            diagnostics = {
                globals = { 'vim' },
            },
            workspace = {
                library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
                }
            },
            telemetry = {
                enable = false,
            }
        }
    }
}

require('compe').setup {
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
    documentation = { border = 'single' },
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

require("trouble").setup {
    position = "bottom",
    height = 10,
    width = 50,
    icons = true,
    mode = "lsp_document_diagnostics",
    fold_open = "⬎",
    fold_closed = "ﬔ",
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

require('lspsaga').init_lsp_saga {
    use_saga_diagnostic_sign = true,
    error_sign = '',
    warn_sign = '',
    hint_sign = '',
    infor_sign = '',
    dianostic_header_icon = ' ',
    code_action_icon = ' ',
    code_action_prompt = {
        enable = true,
        sign = true,
        sign_priority = 20,
        virtual_text = true,
    },
    finder_definition_icon = ' ',
    finder_reference_icon = ' ',
    max_preview_lines = 30,
    finder_action_keys = {
        open = '<CR>', vsplit = 'v', split = 's',
        quit = '<C-c>', scroll_down = '<C-f>', scroll_up = '<C-b>',
    },
    code_action_keys = {
        quit = '<C-c>', exec = '<CR>'
    },
    rename_action_keys = {
        quit = '<C-c>', exec = '<CR>'
    },
    definition_preview_icon = ' ',
    border_style = "round",
    rename_prompt_prefix = '>',
}

require'lsp_signature'.on_attach {
    bind = true,
    doc_lines = 15,
    floating_window = true,
    fix_pos = true,
    hint_enable = true,
    hint_prefix = "",
    hint_scheme = "LspSignatureHint",
    use_lspsaga = false,
    hi_parameter = "LspSignatureParameter",
    max_height = 12,
    max_width = 120,
    handler_opts = {
      border = "single"
    },
    extra_trigger_chars = {"(", ","}
}

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

require("lsp-rooter").setup()

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
        goto_location = "<Cr>",
        focus_location = "o",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a",
    },
    lsp_blacklist = {},
}

require('m3dry.dap')

vim.g.vsnip_snippet_dir = "$HOME/.config/nvim/snippets/"

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
