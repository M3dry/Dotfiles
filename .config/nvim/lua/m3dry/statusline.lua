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
        highlight = {"#000000", "#ff5370"}
    }
}

gls.left[3] = {
    FileName = {
        provider = function()
            local fullpath = vim.fn.expand('%:p')
            local icon = ''

            if vim.fn.empty(vim.fn.expand('%:t')) == 1 then
                return '-x---x-'
            end

            if fullpath:sub(1, fullpath.len(os.getenv("HOME"))) == os.getenv("HOME") then
                fullpath = '~' .. fullpath:sub(fullpath.len(os.getenv("HOME")) + 1, fullpath.len(fullpath))
            end

            if not vim.bo.modifiable then
                icon = ' '
            end

            if vim.bo.readonly then
                icon = icon .. ' '
            end

            if vim.bo.modified then
                icon = icon .. ' '
            end

            if vim.bo.filetype == 'help' then
                return 'Help '
            end
            return icon .. fullpath .. ' '
        end,
        separator = ' ',
        separator_highlight = {'none', "#72a4ff"},
        highlight = {"#000000", "#c792ea",'italic'}
    }
}

gls.left[4] = {
    LineInfo = {
        provider = function()
            return string.format("%d:%d %d ", vim.fn.line('.'), vim.fn.col('.'), vim.fn.line('$'))
        end,
        highlight = {"#000000", "#72a4ff"},
    }
}

gls.right[1] = {
    GitIcon = {
        provider = function() return '  ' end,
        condition = condition.check_git_workspace,
        highlight = {"#000000", "#72a4ff"},
    }
}

gls.right[2] = {
    GitBranch = {
        provider = 'GitBranch',
        condition = condition.check_git_workspace,
        highlight = {"#000000", "#72a4ff"},
    }
}
gls.right[3] = {
    DiffAdd = {
        provider = 'DiffAdd',
        condition = condition.check_git_workspace,
        icon = '   ',
        highlight = {"#000000", "#72a4ff"},
    }
}
gls.right[4] = {
    DiffModified = {
        provider = 'DiffModified',
        condition = condition.check_git_workspace,
        icon = ' 柳',
        highlight = {'#000000', '#72a4ff'},
    }
}
gls.right[5] = {
    DiffRemove = {
        provider = 'DiffRemove',
        condition = condition.check_git_workspace,
        icon = '  ',
        highlight = {'#000000', '#72a4ff'},
    }
}

gls.right[6] = {
    DiagnosticError = {
        provider = 'DiagnosticError',
        icon = ' ',
        separator = ' ',
        separator_highlight = { 'none', '#c792ea' },
        highlight = {'#000000', '#c792ea'}
    }
}

gls.right[7] = {
    DiagnosticWarn = {
        provider = 'DiagnosticWarn',
        icon = ' ',
        highlight = {"#000000", "#c792ea"},
    }
}

gls.right[8] = {
    DiagnosticHint = {
        provider = 'DiagnosticHint',
        icon = ' ',
        highlight = {"#000000", "#c792ea"},
    }
}

gls.right[9] = {
    DiagnosticInfo = {
        provider = 'DiagnosticInfo',
        icon = ' ',
        separator = ' ',
        separator_highlight = { 'none', '#ff5370' },
        highlight = {'#000000', '#c792ea'},
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
        icon = ' ',
        separator = '',
        separator_highlight = { 'none', '#ff5370' },
        highlight = { '#000000', '#ff5370' }
    }
}

gls.right[12] = {
    RainbowBlue = {
        provider = function() return ' ' end,
        highlight = { 'none', '#ff5370' }
    }
}
