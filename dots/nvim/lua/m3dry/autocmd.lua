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

    au("BufNewFile", {
        group = group,
        callback = function()
            require("m3dry.templates").insert_template()
        end,
    })
end

return M
