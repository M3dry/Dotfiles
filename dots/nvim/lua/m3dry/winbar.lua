local M = {}

function M.winbar()
    if vim.bo.buftype == "terminal" then
        return "%#WinbarPath#%=Terminal%="
    elseif vim.bo.ft == "neo-tree" then
        return "%#WinbarPath#%=Neotree%="
    end
    local fullpath = vim.fn.expand "%:p"
    local icon = ""
    local diagnostic = require("galaxyline.provider_diagnostic")
    local err = diagnostic.get_diagnostic_error()
    local warn = diagnostic.get_diagnostic_warn()
    local hint = diagnostic.get_diagnostic_hint()
    local info = diagnostic.get_diagnostic_info()
    diagnostic = ""

    if err ~= nil and err ~= '' then
        diagnostic = "%#WinbarDiagnosticError# " .. err
    end
    if warn ~= nil and warn ~= '' then
        diagnostic = diagnostic .. "%#WinbarDiagnosticWarn# " .. warn
    end
    if hint ~= nil and hint ~= '' then
        diagnostic = diagnostic .. "%#WinbarDiagnosticHint# " .. hint
    end
    if info ~= nil and info ~= '' then
        diagnostic = diagnostic .. "%#WinbarDiagnosticInfo# " .. info
    end


    if vim.fn.empty(vim.fn.expand "%:t") == 1 then
        return "  No Name "
    end

    if fullpath:sub(1, fullpath.len(HOME)) == HOME then
        fullpath = "~" .. fullpath:sub(fullpath.len(HOME) + 1, fullpath.len(fullpath))
    end

    if not vim.bo.modifiable then
        icon = "%#WinbarIcon#" .. " "
    end

    if vim.bo.readonly then
        icon = icon .. "%#WinbarIcon#" .. "  "
    end

    if vim.bo.modified then
        icon = icon .. "%#WinbarIcon#" .. " "
    end

    if vim.bo.filetype == "help" then
        return "Vim help"
    end

    return diagnostic .. icon ..  "%= %#WinbarPath#" .. fullpath .. " "
end

return M
