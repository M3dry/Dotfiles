local M = {}

M.config = {
    templates = {
        c = function()
            if vim.fn.expand "%:t" == "main.c" then
                return '#include "' .. vim.fn.expand "%:t:r" .. '.h"',
                    "",
                    "int",
                    "main(int argc, char **argv)",
                    "{",
                    "    $$",
                    "",
                    "    return 0;",
                    "}"
            else
                return '#include "' .. vim.fn.expand "%:t:r" .. '.h"', "", "$$"
            end
        end,
        h = function()
            return "#ifndef " .. vim.fn.expand("%:t"):upper():gsub(" ", "_"):gsub("%.", "_"),
                "#define " .. vim.fn.expand("%:t"):upper():gsub(" ", "_"):gsub("%.", "_"),
                "",
                "$$",
                "",
                "#endif // " .. vim.fn.expand("%:t"):upper():gsub(" ", "_"):gsub("%.", "_")
        end,
        lua = function()
            local fullpath = vim.fn.expand "%:p"

            if
                fullpath:sub(1, fullpath.len(HOME .. "/.config/nvim"))
                == HOME .. "/.config/nvim"
            then
                return "local M = {}", "", "$$", "", "return M"
            else
                return "$$"
            end
        end,
        go = function()
            local f = io.open(vim.fn.expand "%:p:h" .. "/main.go", "r")
            local package

            if f ~= nil then
                package = "main"
            else
                package = vim.fn.expand("%:p:h"):gsub(".*/", "")
            end

            if vim.fn.expand "%:t" == "main.go" then
                return "package main", "", "func main() {", "	$$", "}"
            elseif vim.fn.expand("%:t"):find "_test" then
                return "package " .. package, "", 'import "testing" ', "", "func Test$$(t *testing.T) {", "	", "}"
            else
                return "package " .. package
            end
        end,
        rs = function()
            if vim.fn.expand "%:t" ==  "main.rs" then
                return "fn main() {", "    $$", "}"
            end
        end
    },
}

function M.insert_template()
    local template = M.config.templates[vim.fn.expand "%:e"]
    local printrow = -1

    if template == nil then
        return
    end

    for _, t in ipairs { template() } do
        local s, e = t:find "%$%$"

        printrow = printrow + 1

        if s ~= nil or e ~= nil then
            vim.api.nvim_buf_set_lines(0, printrow, printrow, false, { t:sub(0, e - 2) .. t:sub(e + 1, #t) })
            vim.api.nvim_del_current_line()
            if (s - 2) > 0 then
                vim.api.nvim_win_set_cursor(0, { printrow + 1, s - 2 })
            else
                vim.api.nvim_win_set_cursor(0, { printrow + 1, s })
            end
            vim.api.nvim_input "a"
        else
            vim.api.nvim_buf_set_lines(0, printrow, printrow, false, { t })
        end
    end
end

return M
