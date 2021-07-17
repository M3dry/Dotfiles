local cb = require'diffview.config'.diffview_callback

require('diffview').setup {
    diff_binaries = false,
    file_panel = {
        width = 35,
        use_icons = true
    },
    key_bindings = {
        disable_defaults = false,
        view = {
            ["<tab>"]     = cb("select_next_entry"),
            ["<s-tab>"]   = cb("select_prev_entry"),
            ["<Leader>e"] = cb("focus_files"),
            ["<Leader>b"] = cb("toggle_files"),
        },
        file_panel = {
            ["j"]             = cb("next_entry"),
            ["<down>"]        = cb("next_entry"),
            ["k"]             = cb("prev_entry"),
            ["<up>"]          = cb("prev_entry"),
            ["<cr>"]          = cb("select_entry"),
            ["o"]             = cb("select_entry"),
            ["<2-LeftMouse>"] = cb("select_entry"),
            ["-"]             = cb("toggle_stage_entry"),
            ["S"]             = cb("stage_all"),
            ["U"]             = cb("unstage_all"),
            ["R"]             = cb("refresh_files"),
            ["<tab>"]         = cb("select_next_entry"),
            ["<s-tab>"]       = cb("select_prev_entry"),
            ["<Leader>e"]     = cb("focus_files"),
            ["<Leader>b"]     = cb("toggle_files"),
        }
    }
}

require('neogit').setup {
    disable_signs = false,
    disable_context_highlighting = false,
    disable_commit_confirmation = false,
    signs = {
        -- { CLOSED, OPENED }
        section = { "⬎", "ﬔ" },
        item = { "⬎", "ﬔ" },
        hunk = { "", "" },
    },
    integrations = {
        diffview = true
    },
    mappings = {
        status = {
            ["p"] = "PushPopup",
            ["P"] = "PullPopup",
        }
    }
}

require('gitsigns').setup {
  signs = {
        add          = { hl = 'GitSignsAdd'   , text = '',  numhl = 'GitSignsAddNr'   , linehl = 'GitSignsAddLn' },
        change       = { hl = 'GitSignsChange', text = '',  numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
        delete       = { hl = 'GitSignsDelete', text = '',  numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
        topdelete    = { hl = 'GitSignsDelete', text = '‾', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
        changedelete = { hl = 'GitSignsChange', text = '~', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
  },
  numhl = true,
  linehl = false,
  keymaps = {
        noremap = true,
        buffer = true,
  },
  watch_index = {
    interval = 1000,
    follow_files = true
  },
  current_line_blame = false,
  current_line_blame_delay = 1000,
  current_line_blame_position = 'eol',
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil,
  word_diff = false,
  use_decoration_api = true,
  use_internal_diff = true,
}

vim.g.blameLineUseVirtualText = 1
vim.g.blameLineVirtualTextHighlight = 'Comment'
vim.g.blameLineGitFormat = '   %an • %as • %s %h'
