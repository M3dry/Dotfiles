local lspc = require "lspconfig"
local lsp = require "m3dry.lsputils"

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

lspc.pylsp.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.bashls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.ocamllsp.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.hls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
    filetypes = { "haskell", "lhaskell", "cabal" },
}

lspc.zls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.clojure_lsp.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.elixirls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
    cmd = { "elixir-ls" },
}

lspc.elmls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.rust_analyzer.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.clangd.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.glsl_analyzer.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.neocmake.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

lspc.nixd.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
}

vim.fn.sign_define("DiagnosticSignError", { text = " " })
vim.fn.sign_define("DiagnosticSignWarn", { text = " " })
vim.fn.sign_define("DiagnosticSignWarning", { text = " " })
vim.fn.sign_define("DiagnosticSignInformation", { text = " " })
vim.fn.sign_define("DiagnosticSignInfo", { text = " " })
vim.fn.sign_define("DiagnosticSignHint", { text = " " })
