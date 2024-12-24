local ls = require "luasnip"
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

return function()
    local path = vim.fn.expand "%"
    local src_dir = path:match "([^/]+)/"
    local init = 1
    if src_dir then
        init = #src_dir + 2
    end

    local module_path = path:sub(init, #path)

    if module_path == "Main.hs" then
        return s(
            "",
            fmt(
                [[
                    module Main (main) where
                    
                    main :: IO ()
                    main = {}
                ]],
                {
                    i(0),
                }
            )
        )
    else
        local module = module_path:sub(1, #module_path - #vim.fn.expand "%:e" - 1):gsub("/", ".")

        return s(
            "",
            fmt(
                [[
                    module {} () where
                    
                    {}
                ]],
                {
                    t(module),
                    i(0),
                }
            )
        )
    end
end
