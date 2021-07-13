local gl = require('galaxyline')
local gls = gl.section
local condition = require('galaxyline.condition')

gls.left[1] = {
    RainbowRed = {
        provider = function() return ' ' end,
        highlight = {'none', "#ff5370"}
    },
}

gls.left[2] = {
    FileSize = {
        provider = 'FileSize',
        condition = condition.buffer_not_empty,
        separator = ' ',
        separator_highlight = {'#ff5370', "#c792ea"},
        highlight = {"#12111e", "#ff5370"}
    }
}

gls.left[3] = {
    FileName = {
        provider = 'FileName',
        condition = condition.buffer_not_empty,
        separator = ' ',
        separator_highlight = {'none', "#72a4ff"},
        highlight = {"#12111e", "#c792ea",'italic'}
    }
}

gls.left[4] = {
    LineInfo = {
        provider = function()
            return string.format("%d:%d %d", vim.fn.line('.'), vim.fn.col('.'), vim.fn.line('$'))
        end,
        separator = '',
        separator_highlight = {'none', "#292d3e"},
        highlight = {"#12111e", "#72a4ff"},
    }
}

gls.right[1] = {
    GitIcon = {
        provider = function() return '  ' end,
        condition = condition.check_git_workspace,
        highlight = {"#c792ea", "#292d3e"},
    }
}

gls.right[2] = {
    GitBranch = {
        provider = 'GitBranch',
        condition = condition.check_git_workspace,
        highlight = {"#c792ea", "#292d3e"},
    }
}

gls.right[3] = {
    DiffAdd = {
        provider = 'DiffAdd',
        condition = condition.hide_in_width,
        icon = '  ',
        highlight = {"#c3e88d", "#292d3e"},
    }
}
gls.right[4] = {
    DiffModified = {
        provider = 'DiffModified',
        condition = condition.hide_in_width,
        icon = ' 柳',
        highlight = {"#f78c6c", "#292d3e"},
    }
}
gls.right[5] = {
    DiffRemove = {
        provider = 'DiffRemove',
        condition = condition.hide_in_width,
        icon = '  ',
        highlight = {"#ff5370", "#292d3e"},
    }
}

gls.right[6] = {
    DiagnosticError = {
        provider = 'DiagnosticError',
        icon = ' ',
        separator = ' ',
        separator_highlight = {'none', "#292d3e"},
        highlight = {"#ff5370", "#292d3e"}
    }
}

gls.right[7] = {
    DiagnosticWarn = {
        provider = 'DiagnosticWarn',
        icon = ' ',
        highlight = {"#f78c6c", "#292d3e"},
    }
}

gls.right[8] = {
    DiagnosticHint = {
        provider = 'DiagnosticHint',
        icon = ' ',
        highlight = {"#72a4ff", "#292d3e"},
    }
}

gls.right[9] = {
    DiagnosticInfo = {
        provider = 'DiagnosticInfo',
        icon = ' ',
        highlight = {"#c3e88d", "#292d3e"},
    }
}

gls.right[10] = {
    ShowLspClient = {
        provider = 'GetLspClient',
        condition = function ()
            local tbl = {['dashboard'] = true,['']=true}
            if tbl[vim.bo.filetype] then
                return false
            end
            return true
        end,
        icon = '  ',
        separator = '',
        separator_highlight = {'none', "#292d3e"},
        highlight = {"#c3e88d", "#292d3e"}
    }
}

gls.right[11] = {
    PerCent = {
        provider = 'LinePercent',
        separator = ' ',
        separator_highlight = {'none', "#292d3e"},
        highlight = {"#eeffff", "#292d3e"},
    }
}

gls.right[12] = {
    RainbowBlue = {
        provider = function() return '' end,
        highlight = {'none', "#292d3e"}
    }
}
