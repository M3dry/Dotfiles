require('nvim-treesitter.configs').setup {
    highlight = {
        enable = true,
        custom_captures = {
            ["variable.array"] = "TSVariableArray",
            ["variable.pointer"] = "TSVariablePointer",
            ["function.call"] = "TSFunctionCall",
            ["keyword.constant"] = "TSKeywordConstant",
            ["parameter.type"] = "TSParameterType",
            ["string.lib"] = "TSStringLib",
            ["property.declaration"] = "TSPropertyDeclaration",
            ["enum"] = "TSEnum",
        }
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "gnn",
            node_incremental = "gri",
            scope_incremental = "grc",
            node_decremental = "grd"
        }
    },
    indent = {
        enable = true
    },
    playground = {
        enable = true,
        disable = {},
        updatetime = 25,
        persist_queries = false,
        keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
        }
    },
    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = {"BufWrite", "CursorHold"}
    },
    rainbow = {
        enable = true,
        extended_mode = false,
        max_file_lines = 20000,
        colors = {
            "#c792ea",
            "#72a4ff",
            "#89ddff",
            "#c3e88d",
            "#ffcb6b",
            "#f78c6c",
            "#ff5370",
        }
    },
    autopairs = {
        enable = true
    },
    refactor = {
        navigation = {
            enable = true,
            keymaps = {
                goto_next_usage = "]u",
                goto_previous_usage = "[u",
            },
        }
    },
    context_commentstring = {
        enable = true,
        config = {
          c = '/*%s*/',
          cpp = '/*%s*/',
          lua = '--%s'
        }
    },
    textsubjects = {
        enable = true,
        keymaps = {
            ['.'] = 'textsubjects-smart',
            [';'] = 'textsubjects-container-outer',
        }
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
                ["ic"] = "@call.inner",
                ["ac"] = "@call.outer",
                ["it"] = "@conditional.inner",
                ["at"] = "@conditional.outer",
                ["il"] = "@loop.inner",
                ["al"] = "@loop.outer",
                ["oc"] = "@comment.outer",
                ["as"] = "@statement.outer",
                ["is"] = "@statement.outer",
            }
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
                ["]c"] = "@call.outer",
                ["]o"] = "@conditional.outer",
                ["]l"] = "@loop.outer",
                ["]s"] = "@comment.outer",
            },
            goto_next_end = {
                ["]F"] = "@function.outer",
                ["]P"] = "@parameter.inner",
                ["]C"] = "@call.outer",
                ["]O"] = "@conditional.outer",
                ["]L"] = "@loop.outer",
                ["]S"] = "@comment.outer",
            },
            goto_previous_start = {
                ["[f"] = "@function.outer",
                ["[p"] = "@parameter.inner",
                ["[c"] = "@call.outer",
                ["[o"] = "@conditional.outer",
                ["[l"] = "@loop.outer",
                ["[s"] = "@comment.outer",
            },
            goto_previous_end = {
                ["[F"] = "@function.outer",
                ["[P"] = "@parameter.inner",
                ["[C"] = "@call.outer",
                ["[O"] = "@conditional.outer",
                ["[L"] = "@loop.outer",
                ["[S"] = "@comment.outer",
            },
        }
    },
    matchup = {
        enable = true,
    }
}

require('m3dry.treesitter.queries')
