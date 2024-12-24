require("blink-cmp").setup {
    keymap = {
        preset = "none",
        ["<C-k>"] = { "select_prev", "fallback" },
        ["<C-j>"] = { "select_next", "fallback" },
        ["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-b>"] = { "scroll_documentation_up", "fallback" },
        ["<C-f>"] = { "scroll_documentation_down", "fallback" },
        ["<C-l>"] = { "snippet_forward", "fallback" },
        ["<C-h>"] = { "snippet_backward", "fallback" },
        ["<C-y>"] = { "select_and_accept", "fallback" },
        ["<C-e>"] = { "hide", "fallback" },
    },
    snippets = {
        expand = function(snippet)
            require("luasnip").lsp_expand(snippet)
        end,
        active = function(filter)
            if filter and filter.direction then
                return require("luasnip").jumpable(filter.direction)
            end
            return require("luasnip").in_snippet()
        end,
        jump = function(direction)
            require("luasnip").jump(direction)
        end,
    },
    sources = {
        default = { "lsp", "path", "luasnip", "buffer" },
    },
    completion = {
        documentation = {
            auto_show = true,
            auto_show_delay_ms = 0,
        },
        ghost_text = {
            enabled = true,
        }
    },
    signature = {
        enabled = true,
    },
}
