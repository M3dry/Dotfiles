local ls = require "luasnip"
local s = ls.snippet
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

return function()
    return s(
        "",
        fmt(
            [[
                #pragma once
                
                {}
            ]],
            {
                i(0),
            }
        )
    )
end
