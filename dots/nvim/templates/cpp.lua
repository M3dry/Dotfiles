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

    if path:sub(init, #path) == "main.cpp" then
        return s(
            "",
            fmt(
                [[
                    int main(int argc, char** argv) {{
                        {}

                        return 0;
                    }}
                ]],
                {
                    i(0),
                }
            )
        )
    else
        return s(
            "",
            fmt(
                [[
                    #include "{}"
                    
                    {}
                ]],
                {
                    t(vim.fn.expand("%:t:r") .. ".hpp"),
                    i(0),
                }
            )
        )
    end
end
