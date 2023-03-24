local gl = require "galaxyline"
local gls = gl.section
local condition = require "galaxyline.condition"

gls.left[1] = {
    RainbowRed = {
        provider = function()
            return " "
        end,
        highlight = { "none", "#ff5370" },
    },
}

gls.left[2] = {
    FileSize = {
        provider = "FileSize",
        condition = condition.buffer_not_empty,
        highlight = { "#000000", "#ff5370" },
    },
}

gls.left[3] = {
    LLineInfo = {
        provider = function()
            return string.format("  %d:%d", vim.fn.line ".", vim.fn.col ".")
        end,
        highlight = { "#000000", "#72a4ff" },
    },
}

gls.right[1] = {
    RLineInfo = {
        provider = function()
            return string.format("%d:%d  %d ", vim.fn.line "$", vim.fn.col "$" - 1, vim.fn.wordcount().words)
        end,
        condition = condition.hide_in_width,
        highlight = { "#000000", "#72a4ff" },
    },
}

gls.right[2] = {
    GitIcon = {
        provider = function()
            if condition.check_git_workspace() then
                return "  "
            end
        end,
        separator = "",
        separator_highlight = { "#c792ea", "#72a4ff" },
        highlight = { "#000000", "#c792ea" },
    },
}

gls.right[3] = {
    GitBranch = {
        provider = "GitBranch",
        condition = condition.check_git_workspace,
        highlight = { "#000000", "#c792ea" },
    },
}

gls.right[4] = {
    Space = {
        provider = function()
            return " "
        end,
        condition = condition.check_git_workspace and condition.hide_in_width,
        highlight = { "#000000", "#c792ea" },
    },
}

gls.right[5] = {
    DiffAdd = {
        provider = "DiffAdd",
        condition = condition.check_git_workspace and condition.hide_in_width,
        icon = "  ",
        highlight = { "#000000", "#c792ea" },
    },
}
gls.right[6] = {
    DiffModified = {
        provider = "DiffModified",
        condition = condition.check_git_workspace and condition.hide_in_width,
        icon = "  ",
        highlight = { "#000000", "#c792ea" },
    },
}
gls.right[7] = {
    DiffRemove = {
        provider = "DiffRemove",
        condition = condition.check_git_workspace and condition.hide_in_width,
        icon = "  ",
        highlight = { "#000000", "#c792ea" },
    },
}

gls.right[8] = {
    ShowLspClient = {
        provider = "GetLspClient",
        condition = function()
            local tbl = { ["dashboard"] = true, [""] = true }
            if tbl[vim.bo.filetype] then
                return false
            end
            return true
        end,
        icon = " ",
        separator = " ",
        separator_highlight = { "#ff5370", "#c792ea" },
        highlight = { "#000000", "#ff5370" },
    },
}

gls.right[9] = {
    RainbowBlue = {
        provider = function()
            return " "
        end,
        highlight = { "none", "#ff5370" },
    },
}
