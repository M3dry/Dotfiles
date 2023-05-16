vim.g["neo_tree_remove_legacy_commands"] = true

require("neo-tree").setup {
    close_if_last_window = false,
    popup_border_style = "rounded",
    enable_git_status = true,
    enable_diagnostics = true,
    sort_case_insensitive = false,
    sources = {
        "filesystem",
        "buffers",
        "git_status",
        "diagnostics",
    },
    default_component_configs = {
        container = {
            enable_character_fade = true,
        },
        indent = {
            indent_size = 2,
            padding = 1,
            with_markers = true,
            indent_marker = "│",
            last_indent_marker = "└",
            highlight = "NeoTreeIndentMarker",
            with_expanders = nil,
            expander_collapsed = "",
            expander_expanded = "",
            expander_highlight = "NeoTreeExpander",
        },
        icon = {
            folder_closed = "",
            folder_open = "",
            folder_empty = "",
            default = "",
            highlight = "NeoTreeFileIcon",
        },
        modified = {
            symbol = "",
            highlight = "NeoTreeModified",
        },
        name = {
            trailing_slash = false,
            use_git_status_colors = true,
            highlight = "NeoTreeFileName",
        },
        git_status = {
            symbols = {
                added = " ",
                modified = " ",
                deleted = " ",
                renamed = "",
                untracked = "",
                ignored = "",
                unstaged = "",
                staged = "",
                conflict = "",
            },
        },
    },
    window = {
        position = "left",
        width = 45,
        mapping_options = {
            noremap = true,
            nowait = true,
        },
        mappings = {
            ["<Tab>"] = function(state)
                local node = state.tree:get_node()
                if require("neo-tree.utils").is_expandable(node) then
                    state.commands["toggle_node"](state)
                else
                    state.commands["open"](state)
                    vim.cmd "Neotree reveal"
                end
            end,
            ["<CR>"] = "open",
            ["S"] = "split_with_window_picker",
            ["s"] = "vsplit_with_window_picker",
            ["w"] = "open_with_window_picker",
            ["C"] = "close_node",
            ["z"] = "close_all_nodes",
            ["Z"] = "expand_all_nodes",
            ["a"] = {
                "add",
                config = {
                    show_path = "absolute", -- "none", "relative", "absolute"
                },
            },
            ["A"] = {
                "add_directory",
                config = {
                    show_path = "absolute", -- "none", "relative", "absolute"
                },
            },
            ["d"] = "delete",
            ["r"] = "rename",
            ["y"] = "copy_to_clipboard",
            ["x"] = "cut_to_clipboard",
            ["p"] = "paste_from_clipboard",
            ["c"] = {
                "copy",
                config = {
                    show_path = "absolute",
                },
            },
            ["m"] = {
                "move",
                config = {
                    show_path = "absolute",
                },
            },
            ["q"] = "close_window",
            ["R"] = "refresh",
            ["?"] = "show_help",
            ["<"] = "prev_source",
            [">"] = "next_source",
        },
    },
    nesting_rules = {},
    filesystem = {
        components = {
        --     harpoon_index = function(config, node, _)
        --         local Marked = require "harpoon.mark"
        --         local path = node:get_id()
        --         local succuss, index = pcall(Marked.get_index_of, path)

        --         if succuss and index and index > 0 then
        --             return {
        --                 text = string.format(" ﯠ %d ", index), -- <-- Add your favorite harpoon like arrow here
        --                 highlight = config.highlight or "NeoTreeDirectoryIcon",
        --             }
        --         else
        --             return {
        --                 text = " ﯡ ",
        --                 highlight = config.highlight or "NeoTreeDirectoryIcon",
        --             }
        --         end
        --     end,
        },
        renderers = {
            file = {
                { "icon" },
                { "name", use_git_status_colors = true },
                -- { "harpoon_index" },
                { "diagnostics" },
                { "git_status", highlight = "NeoTreeDimText" },
            },
        },
        filtered_items = {
            visible = true,
            hide_dotfiles = false,
            hide_gitignored = false,
            hide_by_name = {
                "node_modules",
                ".git",
            },
        },
        follow_current_file = true,
        group_empty_dirs = true,
        hijack_netrw_behavior = "open_default",
        use_libuv_file_watcher = true,
        window = {
            mappings = {
                ["<Bs>"] = "navigate_up",
                ["."] = "set_root",
                ["H"] = "toggle_hidden",
                ["/"] = "fuzzy_finder",
                ["D"] = "fuzzy_finder_directory",
                ["f"] = "filter_on_submit",
                ["<c-x>"] = "clear_filter",
                ["[g"] = "prev_git_modified",
                ["]g"] = "next_git_modified",
            },
        },
    },
    buffers = {
        follow_current_file = true,
        group_empty_dirs = true,
        show_unloaded = true,
        window = {
            mappings = {
                ["bd"] = "buffer_delete",
                ["<Bs>"] = "navigate_up",
                ["."] = "set_root",
            },
        },
    },
    git_status = {
        window = {
            mappings = {
                ["A"] = "git_add_all",
                ["gu"] = "git_unstage_file",
                ["ga"] = "git_add_file",
                ["gr"] = "git_revert_file",
                ["gc"] = "git_commit",
                ["gp"] = "git_push",
                ["gg"] = "git_commit_and_push",
            },
        },
    },
    diagnostics = {
        bind_to_cwd = true,
        diag_sort_function = "severity",
        follow_behavior = {
            always_focus_file = false,
            expand_followed = true,
            collapse_others = true,
        },
        follow_current_file = true,
        group_dirs_and_files = true,
        group_empty_dirs = true,
        show_unloaded = true,
    },
}
