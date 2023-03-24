local M = {}

local capabilities = require("cmp_nvim_lsp").default_capabilities()
capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
}

M.capabilities = capabilities

M.on_attach = function(client, bufnr)
    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        underline = true,
        virtual_text = {
            prefix = "",
            source = "if_many",
            spacing = 2,
        },
        float = {
            source = "if_many",
        },
        signs = true,
        update_in_insert = false,
    })

    if client.server_capabilities.colorProvider then
        require("document-color").buf_attach(bufnr)
    end

    if client.server_capabilities.documentSymbolProvider then
        require("nvim-navic").attach(client, bufnr)
    end

    require("lsp-inlayhints").on_attach(client, bufnr)
end

return M
