local function map(m, k, v)
    vim.keymap.set(m, k, v, { noremap = true, silent = true })
end

map({ "n" }, "<Leader>qw", "<Cmd>wqa<CR>")
map({ "n" }, "<Leader>qq", "<Cmd>qa!<CR>")

map({ "n" }, "<Esc>", "<Cmd>nohlsearch<CR>")

map({ "n" }, "<Leader>Q", "<Cmd>copen<CR>")
map({ "n" }, "]q", "<Cmd>cnext<CR>")
map({ "n" }, "[q", "<Cmd>cnext<CR>")
map({ "n" }, "<Leader>qc", "<Cmd>cclose<CR>")

map({ "n" }, "<Leader>L", "<Cmd>lopen<CR>")
map({ "n" }, "]l", "<Cmd>lnext<CR>")
map({ "n" }, "[l", "<Cmd>lprev<CR>")
map({ "n" }, "<Leader>lc", "<Cmd>lclose<CR>")

map({ "n" }, "<C-d>", "<C-d>zz")
map({ "n" }, "<C-u>", "<C-u>zz")
map({ "n" }, "<C-f>", "<C-f>zz")
map({ "n" }, "<C-b>", "<C-b>zz")
map({ "n" }, "H", "^")
map({ "n" }, "L", "g_")
map({ "n" }, "J", "mzJ`z")
map({ "n" }, "<Leader><Leader>", "<C-^>")

map({ "v" }, "J", ":m '>+1<CR>gv")
map({ "v" }, "K", ":m '<-2<CR>gv")
map({ "v" }, "<", "<gv")
map({ "v" }, ">", ">gv")

map({ "i" }, ".", ".<C-g>u")
map({ "i" }, ",", ",<C-g>u")
map({ "i" }, "!", "!<C-g>u")
map({ "i" }, "?", "?<C-g>u")
map({ "i" }, ";", ";<C-g>u")
map({ "i" }, ":", ":<C-g>u")
map({ "i" }, "=", "=<C-g>u")

map({ "i" }, "<C-m>", function()
    local row, col = unpack(vim.api.nvim_win_get_cursor(0))
    local after = vim.api.nvim_get_current_line():sub(col + 1, -1)
    local closer_col = #after + 1
    local closer_i = nil
    for i, closer in ipairs { ")", "]", "}", ">", "'", '"', "`", "," } do
        local cur_index, _ = after:find(closer)
        if cur_index and (cur_index < closer_col) then
            closer_col = cur_index
            closer_i = i
        end
    end
    if closer_i then
        vim.api.nvim_win_set_cursor(0, { row, col + closer_col })
    else
        vim.api.nvim_win_set_cursor(0, { row, col + 1 })
    end
end)

map({ "n" }, "<Leader>,", "<Cmd>Telescope buffers<CR>")
map({ "n" }, "<Leader>js", "<Cmd>Telescope current_buffer_fuzzy_find<CR>")
map({ "n" }, "<Leader>jf", "<Cmd>Telescope find_files<CR>")
map({ "n" }, "<Leader>jg", "<Cmd>Telescope live_grep<CR>")
map({ "n" }, "<Leader>jq", "<Cmd>Telescope quickfix<CR>")
map({ "n" }, "<Leader>jl", "<Cmd>Telescope loclist<CR>")
map({ "n" }, "<Leader>jh", "<Cmd>Telescope help_tags<CR>")
map({ "n" }, "<Leader>jt", "<Cmd>TodoTelescope<CR>")

map({ "n" }, ",d", vim.lsp.buf.definition)
map({ "n" }, ",D", vim.lsp.buf.declaration)
map({ "n" }, ",o", vim.lsp.buf.signature_help)
map({ "i" }, "<C-i>", vim.lsp.buf.signature_help)
map({ "n" }, ",q", function()
    vim.diagnostic.setqflist { open = true }
end)
map({ "n" }, ",l", function()
    vim.diagnostic.setloclist { open = true }
end)
map({ "n" }, "]e", function()
    vim.diagnostic.goto_next { float = { header = "" } }
end)
map({ "n" }, "[e", function()
    vim.diagnostic.goto_prev { float = { header = "" } }
end)
map({ "n" }, ",v", function()
    vim.diagnostic.open_float { header = "" }
end)
map({ "n" }, ",h", vim.lsp.buf.hover)
map({ "n", "v" }, ",c", vim.lsp.buf.code_action)
map({ "n", "v" }, ",C", vim.lsp.codelens.run)
map({ "n", "x" }, ",f", ":Format<CR>")
map({ "n" }, ",t", vim.lsp.buf.type_definition)
map({ "n" }, ",n", vim.lsp.buf.rename)
map({ "n" }, ",r", vim.lsp.buf.references)
map({ "n" }, ",R", "<Cmd>Telescope lsp_references<CR>")
map({ "n" }, ",e", "<Cmd>Telescope diagnostics bufnr=0<CR>")
map({ "n" }, ",E", "<Cmd>Telescope diagnostics<CR>")
map({ "n" }, ",a", "<Cmd>Inspect<CR>")

map({ "n" }, "<Leader>G", "<Cmd>Neogit<CR>")
map({ "n" }, "<Leader>gd", "<Cmd>DiffViewOpen<CR>")

map({ "n" }, "]t", "<Cmd>tabnext<CR>")
map({ "n" }, "[t", "<Cmd>tabprev<CR>")
map({ "n" }, "]T", "<Cmd>tabmove +1<CR>")
map({ "n" }, "[T", "<Cmd>tabmove -1<CR>")
map({ "n" }, "gtn", "<Cmd>tabnew<CR>")
map({ "n" }, "gtc", "<Cmd>tabclose<CR>")
map({ "n" }, "gt[", "<Cmd>tabfirst<CR>")
map({ "n" }, "gt]", "<Cmd>tablast<CR>")

-- map({"n"}, "<C-w>r", nil)
