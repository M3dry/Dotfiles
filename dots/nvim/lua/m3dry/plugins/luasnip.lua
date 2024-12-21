local ls = require "luasnip"
local types = require "luasnip.util.types"

vim.keymap.set({ "i", "s" }, "<C-s>", function()
    if ls.choice_active() then
        require "luasnip.extras.select_choice"()
    end
end, { noremap = true, silent = true })
vim.keymap.set({ "i", "s" }, "<C-n>", function()
    if ls.choice_active() then
        ls.change_choice(1)
    end
end, { noremap = true, silent = true })
vim.keymap.set({ "i", "s" }, "<C-p>", function()
    if ls.choice_active(-1) then
        ls.change_choice(-1)
    end
end, { noremap = true, silent = true })
vim.keymap.set({ "i" }, "<Tab>", function()
    if ls.expandable() then
        return "<Plug>luasnip-expand-snippet"
    else
        return "<Tab>"
    end
end, { expr = true })

ls.setup {
    update_events = { "TextChanged", "TextChangedI", "InsertLeave" },
    delete_check_events = { "CursorMoved", "CursorHold" },
    ext_opts = {
        [types.insertNode] = {
            active = {
                virt_text = { { "", "LuasnipInsert" } },
            },
            [types.choiceNode] = {
                active = {
                    virt_text = { { "", "LuasnipChoice" } },
                },
            },
        },
    },
}

require("luasnip.loaders.from_lua").load {
    paths = { "~/.config/nvim/snippets/" },
}
