HOME = os.getenv "HOME"

vim.filetype.add({
    extension = {
        jingoo = 'html',
        lt = 'lt',
        todo = 'todo'
    },
    pattern = {
        ['.*'] = {
            priority = -math.huge,
            function (_, bufnr)
                local content = vim.filetype.getlines(bufnr, 1)
                if vim.filetype.matchregex(content, [[^#!.*bb]]) then
                    return 'clojure'
                end
            end
        }
    }
})

require "m3dry.options"
require "m3dry.plugins"
require "m3dry.colors"
require "m3dry.tabline"
require("m3dry.autocmd").init()
