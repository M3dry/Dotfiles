local lspc = require "lspconfig"
local lsp = require "m3dry.plugins.lsputils"

vim.fn.sign_define(
    "DiagnosticSignError",
    { text = " ", texthl = "DiagnosticSignError", linehl = "DiagnosticLineError", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignWarn",
    { text = " ", texthl = "DiagnosticSignWarn", linehl = "DiagnosticLineWarn", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignWarning",
    { text = " ", texthl = "DiagnosticSignWarning", linehl = "DiagnosticLineWarning", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignInformation",
    { text = " ", texthl = "DiagnosticSignInformation", linehl = "DiagnosticLineInformation", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignInfo",
    { text = " ", texthl = "DiagnosticSignInfo", linehl = "DiagnosticLineInfo", numhl = "" }
)
vim.fn.sign_define(
    "DiagnosticSignHint",
    { text = " ", texthl = "DiagnosticSignHint", linehl = "DiagnosticLineHint", numhl = "" }
)

lspc.hls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.zls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.clojure_lsp.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.tsserver.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
    settings = {
        typescript = {
            inlayHints = {
                includeInlayParameterNameHints = "all",
                includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                includeInlayFunctionParameterTypeHints = true,
                includeInlayVariableTypeHints = true,
                includeInlayPropertyDeclarationTypeHints = true,
                includeInlayFunctionLikeReturnTypeHints = true,
                includeInlayEnumMemberValueHints = true,
            },
        },
        javascript = {
            inlayHints = {
                includeInlayParameterNameHints = "all",
                includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                includeInlayFunctionParameterTypeHints = true,
                includeInlayVariableTypeHints = true,
                includeInlayPropertyDeclarationTypeHints = true,
                includeInlayFunctionLikeReturnTypeHints = true,
                includeInlayEnumMemberValueHints = true,
            },
        },
    },
}


lspc.bashls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.svelte.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.html.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.cssls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.taplo.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.pylsp.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.gopls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
    settings = {
        gopls = {
            analyses = {
                nilness = true,
                unusedparams = true,
                unusedwrite = true,
                useany = true,
            },
            experimentalPostfixCompletions = true,
            gofumpt = true,
            staticcheck = true,
            usePlaceholders = true,
            hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                compositeLiteralTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
            },
        },
    },
}

require("go").setup {
    lsp_gofump = true,
    lsp_inlay_hints = {
        enable = false,
    },
}

require("neodev").setup {
    library = {
        enabled = true,
        runtime = true,
        types = true,
        plugins = true,
    },
    setup_jsonls = false,
}

lspc.lua_ls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
    single_file_support = true,
    settings = {
        Lua = {
            workspace = {
                checkThirdParty = false,
            },
            completion = {
                workspaceWord = false,
            },
        },
    },
}

require("rust-tools").setup {
    tools = {
        autoSetHints = false,
        inlay_hints = {
            auto = false,
        },
    },
    server = {
        standalone = false,
        on_attach = lsp.on_attach,
        capabilities = lsp.capabilities,
        root_dir = lspc.util.root_pattern "Cargo.toml",
    },
}

lsp.capabilities.offsetEncoding = { "utf-16" }

require("clangd_extensions").setup {
    server = {
        on_attach = lsp.on_attach,
        capabilities = lsp.capabilities,
    },
    extensions = {
        autoSetHints = false,
        ast = {
            role_icons = {
                type = " ",
                declaration = " ",
                expression = " ",
                specifier = " ",
                statement = "",
                ["template argument"] = "T",
            },

            kind_icons = {
                Compound = "",
                Recovery = "",
                TranslationUnit = "",
                PackExpansion = "",
                TemplateTypeParm = "T",
                TemplateTemplateParm = "T",
                TemplateParamObject = "T",
            },

            highlights = {
                detail = "ClangdAST",
            },
        },
        memory_usage = {
            border = "none",
        },
        symbol_info = {
            border = "none",
        },
    },
}
