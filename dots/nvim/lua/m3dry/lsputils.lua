local M = {}

M.capabilities = require("blink.cmp").get_lsp_capabilities {}

M.on_attach = function(client, bufnr)
    require("workspace-diagnostics").populate_workspace_diagnostics(client, bufnr)
    vim.lsp.inlay_hint.enable()
end

return M
