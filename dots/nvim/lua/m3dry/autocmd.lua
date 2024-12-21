local M = {}

function M.init()
    local au = vim.api.nvim_create_autocmd
    local group = vim.api.nvim_create_augroup("M3DRY", { clear = true })

    au({ "BufRead", "BufNewFile" }, {
        group = group,
        pattern = { "xresources", "xdefaults" },
        command = "set filetype=xdefaults",
    })

    au("BufWritePost", {
        group = group,
        pattern = "xresources",
        command = "!xrdb <afile>",
    })

    au({ "BufEnter", "CursorHold", "InsertLeave" }, {
        group = group,
        callback = function()
            vim.lsp.codelens.refresh { bufnr = 0 }
        end,
    })
end

return M
