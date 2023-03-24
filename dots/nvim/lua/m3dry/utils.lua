local M = {}

function M.kindComparator(entry1, entry2)
    local types = require "cmp.types"
    local kind1 = entry1:get_kind()
    local kind2 = entry2:get_kind()

    kind1 = kind1 == types.lsp.CompletionItemKind.Text and 1000 or kind1
    kind2 = kind2 == types.lsp.CompletionItemKind.Text and 1000 or kind2

    if kind1 ~= kind2 then
        if kind1 == types.lsp.CompletionItemKind.Snippet then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Snippet then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Module then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Module then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Keyword then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Keyword then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Function then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Function then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Method then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Method then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Enum then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Enum then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.EnumMember then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.EnumMember then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Struct then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Struct then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Field then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Field then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Constant then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Constant then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.TypeParameter then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.TypeParameter then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Variable then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Variable then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Value then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Value then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Reference then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Reference then
            return false
        end
        if kind1 == types.lsp.CompletionItemKind.Interface then
            return true
        end
        if kind2 == types.lsp.CompletionItemKind.Interface then
            return false
        end

        local diff = kind1 - kind2
        if diff < 0 then
            return true
        elseif diff > 0 then
            return false
        end
    end
end

function M.nameComparator(entry1, entry2)
    local name1 = entry1.source.name
    local name2 = entry2.source.name

    if name1 ~= name2 then
        if name1 == "dynamic" then
            return true
        end
        if name2 == "dynamic" then
            return false
        end
        if name1 == "neorg" then
            return true
        end
        if name2 == "neorg" then
            return false
        end
        if name1 == "conjure" then
            return true
        end
        if name2 == "conjure" then
            return false
        end
        if name1 == "nvim_lsp" then
            return true
        end
        if name2 == "nvim_lsp" then
            return false
        end
        if name1 == "luasnip" then
            return true
        end
        if name2 == "luasnip" then
            return false
        end
        if name1 == "path" then
            return true
        end
        if name2 == "path" then
            return false
        end
        if name1 == "npm" then
            return true
        end
        if name2 == "npm" then
            return false
        end
        if name1 == "buffer" then
            return true
        end
        if name2 == "buffer" then
            return false
        end
    end
end

function M.ts_context(capture)
    local highlighter = require "vim.treesitter.highlighter"
    local ts_utils = require "nvim-treesitter.ts_utils"
    local buf = vim.api.nvim_get_current_buf()

    local row, col = unpack(vim.api.nvim_win_get_cursor(0))
    row = row - 1
    if vim.api.nvim_get_mode().mode == "i" then
        col = col - 1
    end

    local self = highlighter.active[buf]
    if not self then
        return false
    end

    local node_types = {}

    self.tree:for_each_tree(function(tstree, tree)
        if not tstree then
            return
        end

        local root = tstree:root()
        local root_start_row, _, root_end_row, _ = root:range()
        if root_start_row > row or root_end_row < row then
            return
        end

        local query = self:get_query(tree:lang())
        if not query:query() then
            return
        end

        local iter = query:query():iter_captures(root, self.bufnr, row, row + 1)
        for _, node, _ in iter do
            if ts_utils.is_in_node_range(node, row, col) then
                table.insert(node_types, node:type())
            end
        end
    end, true)

    print(vim.inspect(node_types))
    return vim.tbl_contains(node_types, capture)
end

function M.dprint(val)
    print(vim.inspect(val))
end

return M
