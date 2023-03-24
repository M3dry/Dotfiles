local actions = require "telescope.actions"
local trouble = require "trouble.providers.telescope"
local telescope = require "telescope"

telescope.setup {
    defaults = {
        mappings = {
            i = {
                ["<C-j>"] = actions.move_selection_worse,
                ["<C-k>"] = actions.move_selection_better,
                ["<C-n>"] = actions.move_selection_worse + actions.toggle_selection,
                ["<C-p>"] = actions.move_selection_better + actions.toggle_selection,
                ["<C-o>"] = trouble.open_with_trouble,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-S-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-S-L>"] = actions.smart_add_to_loclist,
                ["<C-r>"] = actions.drop_all,
                ["<C-a>"] = actions.select_all,
                ["<C-t>"] = require("telescope.actions.layout").toggle_preview,
            },
            n = {
                ["<C-o>"] = trouble.open_with_trouble,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-S-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-S-L>"] = actions.smart_add_to_loclist,
                ["<C-r>"] = actions.drop_all,
                ["<C-a>"] = actions.select_all,
                ["<C-c>"] = actions.close,
            },
        },
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
        },
        prompt_prefix = "   ",
        selection_caret = "  ",
        entry_prefix = "  ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "ascending",
        layout_strategy = "horizontal",
        scroll_strategy = "cycle",
        layout_config = {
            horizontal = {
                prompt_position = "top",
                preview_width = 0.55,
                results_width = 0.8,
            },
            vertical = {
                mirror = false,
            },
            width = 0.87,
            height = 0.80,
            preview_cutoff = 120,
        },
        file_ignore_patterns = {},
        winblend = 0,
        border = {},
        borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
        color_devicons = true,
        set_env = { ["COLORTERM"] = "truecolor" },
        file_sorter = require("telescope.sorters").get_fzf_sorter,
        generic_sorter = require("telescope.sorters").get_fzf_sorter,
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
        buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
    },
    pickers = {
        git_branches = {
            attach_mappings = function(_, map)
                map("i", "<C-d>", actions.git_delete_branch)
                map("n", "<C-d>", actions.git_delete_branch)
                return true
            end,
        },
        current_buffer_fuzzy_find = {
            previewer = false,
        },
        buffers = {
            attach_mappings = function(_, map)
                map("i", "<C-d>", actions.delete_buffer)
                map("n", "<C-d>", actions.delete_buffer)
                return true
            end,
            previewer = false,
        },
    },
    extensions = {
        fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
        },
    },
}

telescope.load_extension "ui-select"
telescope.load_extension "dap"
telescope.load_extension "fzf"
telescope.load_extension "projects"
telescope.load_extension "undo"
