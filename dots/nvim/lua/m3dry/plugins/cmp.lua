local cmp = require "cmp"
local luasnip = require "luasnip"
local compare = require "cmp.config.compare"
local icons = {
    Text = "  ",
    Method = "  ",
    Function = " ƒ ",
    Constructor = "  ",
    Field = "  ",
    Variable = "  ",
    Class = "  ",
    Interface = "  ",
    Module = "  ",
    Property = "  ",
    Unit = "  ",
    Value = "  ",
    Enum = "  ",
    Keyword = "  ",
    Snippet = "  ",
    Color = "  ",
    File = "  ",
    Reference = "  ",
    Folder = "  ",
    EnumMember = "  ",
    Constant = "  ",
    Struct = "  ",
    Event = "  ",
    Operator = "  ",
    TypeParameter = " T ",
}

cmp.setup {
    enabled = function()
        local context = require "cmp.config.context"

        if vim.api.nvim_get_mode().mode == "c" then
            return true
        elseif vim.bo.buftype == "prompt" then
            return false
        else
            return not context.in_treesitter_capture "comment" and not context.in_syntax_group "Comment"
        end
    end,
    preselect = cmp.PreselectMode.Item,
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    completion = {
        keyword_length = 1,
        completeopt = "menu,menuone,noinsert",
        col_offset = -3,
        side_padding = -2,
    },
    formatting = {
        fields = { "kind", "abbr", "menu" },
        format = function(entry, item)
            item.menu = "(" .. item.kind .. "/" .. entry.source.name .. ")"
            item.kind = icons[item.kind]

            return item
        end,
    },
    matching = {
        disallow_fuzzy_matching = false,
        disallow_partial_matching = false,
        disallow_prefix_unmatching = false,
    },
    sorting = {
        comparators = {
            require("m3dry.utils").nameComparator,
            require("m3dry.utils").kindComparator,
            compare.offset,
            compare.exact,
            compare.score,
            compare.sort_text,
            compare.order,
        },
    },
    sources = cmp.config.sources({
        { name = "neorg" },
        {
            name = "nvim_lsp",
            entry_filter = function(entry, _)
                return require("cmp.types").lsp.CompletionItemKind[entry:get_kind()] ~= "Text"
            end,
        },
        { name = "luasnip" },
        { name = "path" },
    }, {
        { name = "conjure" },
        {
            name = "buffer",
            option = {
                get_bufnrs = function()
                    local bufs = {}
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                        bufs[vim.api.nvim_win_get_buf(win)] = true
                    end
                    return vim.tbl_keys(bufs)
                end,
            },
        },
    }),
    view = {
        entries = { name = "custom" },
    },
    window = {
        documentation = {
            border = { " ", "", "", "", "", "", " ", " " },
            winhighlight = "Normal:CmpDocumentation,FloatBorder:CmpDocumentationBorder,CursorLine:CmpSelect,Search:None",
        },
        completion = {
            border = { "", "", "", "", "", "", "", "" },
            winhighlight = "Normal:CmpNormal,FloatBorder:CmpBorder,CursorLine:CmpSelect,Search:None",
            col_offset = -3,
            side_padding = 0,
            scrollbar = false,
        },
    },
    experimental = {
        ghost_text = true,
    },
    mapping = {
        ["<Tab>"] = cmp.mapping(function(fallback)
            if luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
        ["<C-j>"] = cmp.mapping {
            i = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Replace },
            c = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Replace },
        },
        ["<C-k>"] = cmp.mapping {
            i = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Replace },
            c = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Replace },
        },
        ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
        ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
        ["<C-Space>"] = cmp.mapping(cmp.mapping.complete { reason = cmp.ContextReason.Manual }, { "i", "c" }),
        ["<C-e>"] = cmp.mapping {
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
        },
        ["<CR>"] = cmp.mapping.confirm { select = true, behavior = cmp.ConfirmBehavior.Insert },
    },
}

cmp.setup.cmdline("/", {
    mapping = cmp.mapping.preset.cmdline(),
    formatting = {
        fields = { "abbr" },
    },
    sources = {
        { name = "buffer" },
    },
})

cmp.setup.cmdline(":", {
    mapping = {
        ["<CR>"] = cmp.mapping.confirm { select = true, behavior = cmp.ConfirmBehavior.Insert },
        ["<Tab>"] = {
            c = function(_)
                if cmp.visible() then
                    local entry = cmp.get_selected_entry()
                    if not entry then
                        cmp.select_next_item()
                        cmp.confirm()
                    else
                        cmp.confirm()
                    end
                else
                    cmp.complete { reason = cmp.ContextReason.Manual }
                end
            end,
        },
        ["<S-Tab>"] = {
            c = function(_)
                if cmp.visible() then
                    local entry = cmp.get_selected_entry()
                    if not entry then
                        cmp.select_next_item()
                        cmp.confirm()
                    else
                        cmp.confirm()
                    end
                else
                    cmp.complete { reason = cmp.ContextReason.Manual }
                end
            end,
        },
        ["<C-e>"] = {
            c = cmp.mapping.close(),
        },
    },
    formatting = {
        fields = { "kind", "abbr" },
    },
    sources = cmp.config.sources(
        { { name = "path" }, },
        { { name = "cmdline" }, }
    ),
})

cmp.setup.filetype({ "dap-repl", "dapui_watches" }, {
    sources = {
        { name = "dap" },
    },
})
