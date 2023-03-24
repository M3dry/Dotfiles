local M = {}

M.options = {
    cycles = {
        ["c"] = {
            { "#ifdef", "#ifndef" },
            { "&", "*" },
            { ".", "->" },
        },
        ["rust"] = {
            { "i8", "i16", "i32", "i64", "i128", "isize" },
            { "u8", "u16", "u32", "u64", "u128", "usize" },
            { "&", "&mut ", "*" },
            { "iter", "iter_mut", "into_iter" },
        },
    },
}

local function cycle(direction)
    local _, row, col, _ = unpack(vim.fn.getpos ".")
    local line = vim.fn.getline(row)
    local cycles = { curstr = {}, next = {}, pos = { start = {}, finish = {} } }
    local i = 1
    local longest

    for _, v in ipairs(M.options.cycles[vim.bo.ft]) do
        for n, z in ipairs(v) do
            local start, finish
            local escstr = z

            if z:find "%." then
                local escstart, _ = escstr:find "%."

                escstr = escstr:sub(1, escstart - 1) .. "%" .. escstr:sub(escstart, #escstr)
            end

            if col - #z + 1 <= 1 then
                start, finish = line:sub(1, col + #z - 1):find(escstr)
                if finish and start <= 0 then
                    start = 1
                end
            else
                start, finish = line:sub(col - #z + 1, col + #z - 1):find(escstr)
                if start and finish then
                    finish = col + start - 1
                    start = col - #z + start
                end
            end

            if start and finish then
                if direction == "n" then
                    cycles.next[i] = v[n + 1] or v[1]
                elseif direction == "p" then
                    cycles.next[i] = v[n - 1] or v[#v]
                end

                cycles.curstr[i] = z
                cycles.pos.start[i] = start - 1
                cycles.pos.finish[i] = finish
                i = i + 1
            end
        end
    end

    if i > 1 then
        longest = 1
    else
        return false
    end

    for n = 1, i - 1 do
        if #cycles.curstr[longest] < #cycles.curstr[n] then
            longest = n
        end
    end

    vim.api.nvim_buf_set_text(
        0,
        row - 1,
        cycles.pos.start[longest],
        row - 1,
        cycles.pos.finish[longest],
        { cycles.next[longest] }
    )
end

function M.cycle_next()
    cycle "n"
end

function M.cycle_prev()
    cycle "p"
end

return M
