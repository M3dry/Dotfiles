local M = {}

M.replace = function ()
    local column = vim.api.nvim_win_get_cursor(0)[2]

    if vim.api.nvim_get_current_line():sub(column + 1, column + 2) == "->" then
        vim.api.nvim_input("xxa.<esc>")
    elseif vim.api.nvim_get_current_line():sub(column, column + 1) == "->" then
        vim.api.nvim_input("hxxa.<esc>")
    elseif vim.api.nvim_get_current_line():sub(column + 1, column + 1) == "." then
        vim.api.nvim_input("xa-><esc>")
    end
end

return M
