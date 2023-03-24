local M = {}

M.config = {
    ["c"] = { ";", "," },
    ["go"] = { "," },
    ["lua"] = { "," },
    ["json"] = { "," },
    ["javascript"] = { ";", "," },
    ["typescript"] = { ";", "," },
    ["svelte"] = { ";", "," },
    ["rust"] = { ";", "," },
}

local function insertCharTable(charTable)
    local _, row, col, _ = unpack(vim.fn.getpos ".")
    local line = unpack(vim.api.nvim_buf_get_lines(0, row - 1, row, true))
    local printed = false

    for index, value in ipairs(charTable) do
        if line:sub(#line - #charTable[index] + 1, #line) == value then
            if index == #charTable then
                vim.api.nvim_buf_set_text(0, row - 1, #line - #charTable[index], row - 1, #line, { "" })
            else
                vim.api.nvim_buf_set_text(
                    0,
                    row - 1,
                    #line - #charTable[index],
                    row - 1,
                    #line,
                    { charTable[index + 1] }
                )
            end
            printed = true
        end
    end

    if not printed then
        vim.api.nvim_buf_set_text(0, row - 1, #line, row - 1, #line, { charTable[1] })
    end
end

function M.insert()
    local char = M.config[vim.bo.ft]

    if type(char) == "table" then
        insertCharTable(char)
    end
end

return M
