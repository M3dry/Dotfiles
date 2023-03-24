local lsp = require "m3dry.plugins.lsputils"

require("lspconfig").jsonls.setup {
    on_attach = lsp.on_attach,
    capabilities = lsp.capabilities,
    settings = {
        json = {
            schemas = require("schemastore").json.schemas(),
            validate = { enable = true },
        },
    },
}
