require("nvim-treesitter.configs").setup {
    ensure_installed = {
        "nix",
        "c",
        "cpp",
        "rust",
        "haskell",
        "clojure",
        "scheme",
        "fennel",
        "lua",
        "luadoc",
        "toml",
        "json",
        "yaml",
        "bash",
        "python",
        "markdown",
        "markdown_inline",
        "latex",
        "html",
        "query",
        "vim",
        "regex",
    },
    highlight = {
        enable = true,
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ip"] = "@parameter.inner",
                ["ap"] = "@parameter.outer",
                ["ic"] = "@conditional.inner",
                ["ac"] = "@conditional.outer",
                ["il"] = "@loop.inner",
                ["al"] = "@loop.outer",
                ["iC"] = "@call.inner",
                ["aC"] = "@call.outer",
                ["C"] = "@comment.outer",
                ["as"] = "@statement.outer",
                ["is"] = "@statement.outer",
            },
        },
        swap = {
            enable = true,
            swap_next = {
                ["]m"] = "@parameter.inner",
            },
            swap_previous = {
                ["[m"] = "@parameter.inner",
            },
        },
        move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
                ["]f"] = "@function.outer",
                ["]p"] = "@parameter.inner",
                ["]c"] = "@conditional.outer",
                ["]o"] = "@call.outer",
                ["]r"] = "@loop.outer",
                ["]s"] = "@comment.outer",
            },
            goto_next_end = {
                ["]F"] = "@function.outer",
                ["]P"] = "@parameter.inner",
                ["]C"] = "@conditional.outer",
                ["]O"] = "@call.outer",
                ["]R"] = "@loop.outer",
                ["]S"] = "@comment.outer",
            },
            goto_previous_start = {
                ["[f"] = "@function.outer",
                ["[p"] = "@parameter.inner",
                ["[c"] = "@conditional.outer",
                ["[o"] = "@call.outer",
                ["[r"] = "@loop.outer",
                ["[s"] = "@comment.outer",
            },
            goto_previous_end = {
                ["[F"] = "@function.outer",
                ["[P"] = "@parameter.inner",
                ["[C"] = "@conditional.outer",
                ["[O"] = "@call.outer",
                ["[R"] = "@loop.outer",
                ["[S"] = "@comment.outer",
            },
        },
    },
}
