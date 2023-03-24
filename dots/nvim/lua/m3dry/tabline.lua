local M = {}
local fn = vim.fn

M.options = {
    show_index = true,
    separator = ":",
    show_bufnum = true,
    show_modify = false,
    save_icon = " ",
    no_name = " No Name",
}

local function tabline(options)
    local s = ""

    for index = 1, fn.tabpagenr "$" do
        local winnr = fn.tabpagewinnr(index)
        local buflist = fn.tabpagebuflist(index)
        local bufnr = buflist[winnr]
        local bufname = fn.bufname(bufnr)
        local bufmodified = fn.getbufvar(bufnr, "&mod")

        if index == fn.tabpagenr() then
            s = s .. "%" .. index .. "T" .. "%#TabLineSelSep#" .. "▌"
            if options.show_index then
                s = s .. "%#TabLineSelIndex#" .. index
            end
            if options.show_bufnum then
                s = s .. "%#TabLineSelNumSep#" .. options.separator .. "%#TabLineSelBufNum#" .. bufnr
            end
            s = s .. "%#TabLineSel#"
        else
            s = s .. "%" .. index .. "T" .. "%#TabLineSep#" .. "▌"
            if options.show_index then
                s = s .. "%#TabLineIndex#" .. index
            end
            if options.show_bufnum then
                s = s .. "%#TabLineNumSep#" .. options.separator .. "%#TabLineBufNum#" .. bufnr
            end
            s = s .. "%#TabLine#"
        end

        if bufname == "" then
            s = s .. options.no_name .. " "
        else
            s = s .. " " .. fn.fnamemodify(bufname, ":t") .. " "
        end

        if options.show_modify and bufmodified == 1 then
            if index == fn.tabpagenr() then
                s = s .. "%#TabLineSelMod#" .. options.save_icon
            else
                s = s .. "%#TabLineMod#" .. options.save_icon
            end
        end
    end

    return s .. "%#TabLineFill#"
end

function _G.nvim_tabline()
    return tabline(M.options)
end

vim.o.showtabline = 1
vim.o.tabline = "%!v:lua.nvim_tabline()"
vim.g.loaded_nvim_tabline = 1

return M
