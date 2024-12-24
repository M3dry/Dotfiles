local ls = require "luasnip"
local s = ls.snippet
local i = ls.insert_node
local c = ls.choice_node
local t = ls.text_node
local fmt = require("luasnip.extras.fmt").fmt

return function()
    local path = vim.fn.expand "%:p"
    local conf = vim.fs.normalize "~/.config/nvim"

    if path:sub(1, #conf) == conf then
        return s(
            "",
            c(1, {
                fmt(
                    [[
                        local M = {{}}

                        {}

                        return M
                    ]],
                    {
                        i(1),
                    }
                ),
                t "",
            })
        )
    else
        return s("", {})
    end
end
