local toolbox = require "legendary.toolbox"

require("legendary").setup {
    default_opts = {
        keymaps = {
            noremap = true,
            silent = true,
        },
        commands = {},
        autocmds = {},
    },
    keymaps = {
        {
            itemgroup = "Save & Exit",
            icon = "",
            keymaps = {
                { "<Leader>qw", "<Cmd>wqa<CR>", description = "Save and exit all" },
                { "<Leader>qq", "<Cmd>qa!<CR>", description = "Exit focefully all" },
            },
        },
        {
            itemgroup = "QuickFix",
            icon = "龍",
            keymaps = {
                { "]q", "<Cmd>QFNext<CR>zz", description = "Next entry in quickfix" },
                { "[q", "<Cmd>QFPrev<CR>zz", description = "Previous entry in quickfix" },
            },
        },
        {
            itemgroup = "LocList",
            icon = "",
            keymaps = {
                { "]q", "<Cmd>LLNext<CR>zz", description = "Next entry in locllist" },
                { "[q", "<Cmd>LLPrev<CR>zz", description = "Previous entry in locllist" },
            },
        },
        {
            itemgroup = "Buffers",
            icon = "﬘",
            keymaps = {
                { "<Leader>bc", "<Cmd>Sayonara<CR>", description = "Close buffer and window" },
                { "<Leader>bk", "<Cmd>Sayonara!<CR>", description = "Close buffer and keep window" },
                {
                    "<Leader>,",
                    toolbox.lazy_required_fn(
                        "reach",
                        "buffers",
                        { modified_icon = " ", auto_handles = { "a", "s", "d", "f", "j", "k", "l", ";" } }
                    ),
                    description = "Switch buffers",
                },
            },
        },
        {
            itemgroup = "Tabs",
            icon = "ﱚ",
            keymaps = {
                { "]t", "<Cmd>tabnext<CR>", description = "Switch to the next tab" },
                { "[t", "<Cmd>tabprevious<CR>", description = "Switch to the Previous tab" },
                { "]T", "<Cmd>tabmove +1<CR>", description = "Move the current tab by one" },
                { "[T", "<Cmd>tabmove -1<CR>", description = "Move the current tab back by one " },
                { "gtn", "<Cmd>tabnew<CR>", description = "Create a new tab" },
                { "gtc", "<Cmd>tabclose<CR>", description = "Close the current tab" },
                { "gtf", "<Cmd>tabfirst<CR>", description = "Go to the first tab" },
                { "gtl", "<Cmd>tablast<CR>", description = "Go to the last tab" },
            },
        },
        {
            itemgroup = "Splits",
            icon = "",
            keymaps = {
                {
                    "<Leader>w",
                    "<C-w>",
                    description = "All vim window maps",
                    opts = {
                        noremap = false,
                    },
                },
                { "<Leader>wd", "<C-w>c", description = "Close current window" },
                { "<Leader>wm", "<Cmd>MaximizerToggle<CR>", description = "Maximize current window" },
                { "]w", "<C-w><", description = "Switch to the next tab" },
                { "[w", "<C-W>>", description = "Switch to the Previous tab" },
                { "]W", "<C-w>+", description = "Move the current tab by one" },
                { "[W", "<C-w>-", description = "Move the current tab back by one " },
            },
        },
        {
            itemgroup = "Neotree",
            icon = "",
            keymaps = {
                {
                    "\\",
                    "<Cmd>Neotree left reveal toggle<CR>",
                    description = "Open neo-tree on the left",
                },
                {
                    "<C-l>",
                    "<Cmd>Neotree diagnostics bottom reveal toggle<CR>",
                    description = "Open diagnostics neo-tree on the bottom",
                },
                {
                    "<C-\\>",
                    "<Cmd>Neotree buffers bottom reveal toggle<CR>",
                    description = "Open buffer neo-tree on the bottom",
                },
            },
        },
        {
            itemgroup = "Telescope",
            icon = " ",
            keymaps = {
                {
                    "<Leader>js",
                    toolbox.lazy_required_fn("telescope.builtin", "current_buffer_fuzzy_find"),
                    description = "Search current buffer",
                },
                {
                    "<Leader>jm",
                    toolbox.lazy_required_fn("telescope.builtin", "marks"),
                    description = "Search through marks",
                },
                {
                    "<Leader>jf",
                    toolbox.lazy_required_fn("telescope.builtin", "find_files"),
                    description = "Find files",
                },
                {
                    "<Leader>jb",
                    toolbox.lazy_required_fn("telescope.builtin", "buffers"),
                    description = "Jump to buffer",
                },
                {
                    "<Leader>je",
                    toolbox.lazy_required_fn("telescope.builtin", "symbols"),
                    description = "Search through symbols",
                },
                {
                    "<Leader>jq",
                    toolbox.lazy_required_fn("telescope.builtin", "quickfix"),
                    description = "Search through quickfix",
                },
                {
                    "<Leader>jl",
                    toolbox.lazy_required_fn("telescope.builtin", "loclist"),
                    description = "Search through loclist",
                },
                {
                    "<Leader>jh",
                    toolbox.lazy_required_fn("telescope.builtin", "help_tags"),
                    description = "Search  through help",
                },
                {
                    "<Leader>jc",
                    toolbox.lazy_required_fn("telescope.builtin", "highlights"),
                    description = "Search through highlights",
                },
                {
                    "<Leader>jp",
                    toolbox.lazy_required_fn("telescope", "extensions.projects.projects"),
                    description = "Change project",
                },
                {
                    "<Leader>jg",
                    toolbox.lazy_required_fn("telescope.builtin", "live_grep"),
                    description = "Search through project",
                },
                {
                    "<Leader>ks",
                    toolbox.lazy_required_fn("telescope.builtin", "git_status"),
                    description = "Search through git status",
                },
                {
                    "<Leader>kS",
                    toolbox.lazy_required_fn("telescope.builtin", "git_stash"),
                    description = "Show git stash",
                },
                {
                    "<Leader>kf",
                    toolbox.lazy_required_fn("telescope.builtin", "git_files"),
                    description = "Show git files",
                },
                {
                    "<Leader>kb",
                    toolbox.lazy_required_fn("telescope.builtin", "git_branches"),
                    description = "Show git branches",
                },
                {
                    "<Leader>kc",
                    toolbox.lazy_required_fn("telescope.builtin", "git_commits"),
                    description = "Show git commints",
                },
                {
                    "<Leader>kC",
                    toolbox.lazy_required_fn("telescope.builtin", "git_bcommits"),
                    description = "Show git commits for current buffer",
                },
            },
        },
        {
            itemgroup = "Trouble",
            icon = " ",
            keymaps = {
                {
                    ",e",
                    "<Cmd>TroubleToggle document_diagnostics<CR>",
                    description = "Toggle trouble document diagnostics",
                },
                {
                    ",E",
                    "<Cmd>TroubleToggle workspace_diagnostics<CR>",
                    description = "Toggle trouble workspace diagnostics",
                },
                {
                    ",t",
                    "<Cmd>TroubleToggle lsp_type_definitions<CR>",
                    description = "Toggle trouble lsp type definition",
                },
                { ",gf", "<Cmd>TroubleRefresh<CR>", description = "Refresh trouble" },
                { ",gq", "<Cmd>TroubleToggle quickfix<CR>", description = "Trouble toggle quickfix" },
                { ",gl", "<Cmd>TroubleToggle loclist<CR>", description = "Trouble toggle loclist" },
                { ",gc", "<Cmd>TroubleClose<CR>", description = "Close trouble" },
            },
        },
        {
            itemgroup = "UndoTree",
            icon = " ",
            keymaps = {
                { "<Leader>tut", "<Cmd>UndotreeToggle<CR>", description = "Toggle UndoTree" },
                { "<Leader>tuc", "<Cmd>UndotreeHide<CR>", description = "Hide UndoTree" },
                { "<Leader>tuf", "<Cmd>UndotreeFocus<CR>", description = "Focus UndoTree" },
            },
        },
        {
            itemgroup = "Semismart",
            icon = ";",
            keymaps = {
                {
                    "<C-;>",
                    toolbox.lazy_required_fn("m3dry.semismart", "insert"),
                    description = "Puts a char at the end of line",
                    mode = { "n", "i" },
                },
            },
        },
        {
            itemgroup = "Dap",
            icon = " ",
            keymaps = {
                { "<Leader>dc", toolbox.lazy_required_fn("dap", "continue"), description = "Continue" },
                { "<Leader>dso", toolbox.lazy_required_fn("dap", "step_over"), description = "Step over" },
                { "<Leader>dsi", toolbox.lazy_required_fn("dap", "step_into"), description = "Step into" },
                { "<Leader>dsO", toolbox.lazy_required_fn("dap", "step_out"), description = "Step out" },
                {
                    "<Leader>db",
                    toolbox.lazy_required_fn("dap", "toggle_breakpoint"),
                    description = "Toggle a breakpoint",
                },
                {
                    "<Leader>di",
                    function()
                        require("dap").set_breakpoint(vim.fn.input "Condition: ")
                    end,
                    description = "Set breakpoint with a condition",
                },
                {
                    "<Leader>dl",
                    function()
                        require("dap").set_breakpoint(nil, nil, vim.fn.input "Log message: ")
                    end,
                    description = "Set breakpoint with a log message",
                },
                { "<Leader>dut", toolbox.lazy_required_fn("dapui", "toggle"), description = "Toggle dapui" },
                {
                    "<Leader>duf",
                    toolbox.lazy_required_fn("dapui", "float_element"),
                    description = "Dapui float element",
                },
                {
                    "<Leader>dtc",
                    toolbox.lazy_required_fn("telescope", "extensions.dap.commands"),
                    description = "Telescope dap commands",
                },
                {
                    "<Leader>dtC",
                    toolbox.lazy_required_fn("telescope", "extensions.dap.configurations"),
                    description = "Telescope dap config",
                },
                {
                    "<Leader>dtb",
                    toolbox.lazy_required_fn("telescope", "extensions.dap.list_breakpoints"),
                    description = "Telescope dap breakpoints",
                },
                {
                    "<Leader>dtv",
                    toolbox.lazy_required_fn("telescope", "extensions.dap.variables"),
                    description = "Telescope dap variables",
                },
                {
                    "<Leader>dtf",
                    toolbox.lazy_required_fn("telescope", "extensions.dap.frames"),
                    description = "Telescope dap frames",
                },
                {
                    "]b",
                    toolbox.lazy_required_fn("goto-breakpoints", "next"),
                    description = "Go to the next breakpoint",
                },
                {
                    "[b",
                    toolbox.lazy_required_fn("goto-breakpoints", "prev"),
                    description = "Go to the previous breakpoint",
                },
            },
        },
        {
            itemgroup = "Lsp",
            icon = " ",
            keymaps = {
                { ",d", vim.lsp.buf.definition, description = "Go to definition" },
                { ",D", vim.lsp.buf.declaration, description = "Go to declaration" },
                { ",o", vim.lsp.buf.signature_help, description = "Show signature help" },
                {
                    "<C-k>",
                    vim.lsp.buf.signature_help,
                    description = "Show signature help",
                    mode = "i",
                },
                {
                    ",h",
                    vim.lsp.buf.hover,
                    description = "Show info about undercursor in a floating window",
                },
                { ",H", "<Cmd>DocsViewToggle<CR>", description = "Show info about undercursor in a split" },
                {
                    ",q",
                    toolbox.lazy(vim.diagnostic.setqflist, { open = false }),
                    description = "Put diagnostics into quickfix",
                },
                {
                    ",l",
                    toolbox.lazy(vim.diagnostic.setloclist, { open = false }),
                    description = "Put diagnostics into loclist",
                },
                {
                    "]e",
                    toolbox.lazy(vim.diagnostic.goto_next, { float = { header = "" } }),
                    description = "Go to next diagnostic",
                },
                {
                    "[e",
                    toolbox.lazy(vim.diagnostic.goto_prev, { float = { header = "" } }),
                    description = "Go to previous diagnostic",
                },
                {
                    ",v",
                    toolbox.lazy(vim.diagnostic.open_float, { header = "" }),
                    description = "Open diagnostic floating window",
                },
                {
                    ",c",
                    vim.lsp.buf.code_action,
                    description = "Code action",
                    mode = { "n", "v" },
                },
                { ",f", vim.lsp.buf.format, description = "Format code" },
                { ",F", vim.lsp.buf.range_formatting, description = "Range format code" },
                {
                    ",.",
                    toolbox.lazy_required_fn("null-ls-embedded", "buf_format"),
                    description = "Format all embedded languages in buffer",
                },
                {
                    ",,",
                    toolbox.lazy_required_fn("null-ls-embedded", "format_current"),
                    description = "Format embedded language under cursor",
                },
                {
                    ",gt",
                    vim.lsp.buf.type_definition,
                    description = "Go to type definition",
                },
                {
                    ",a",
                    "<Cmd>TSHighlightCapturesUnderCursor<CR>",
                    description = "Show capture under cursor in a floating window",
                },
                {
                    ",n",
                    ":IncRename ",
                    description = "Rename all references of symbol under cursor",
                    opts = { silent = false },
                },
                {
                    ",p",
                    "<Cmd>Lspsaga preview_definition<CR>",
                    description = "Preview definition",
                },
                {
                    ",r",
                    toolbox.lazy_required_fn("telescope.builtin", "lsp_references"),
                    description = "See all references",
                },
                {
                    ",T",
                    toolbox.lazy_required_fn("telescope.builtin", "treesitter"),
                    description = "Treesitter symbols",
                },
                {
                    ",s",
                    toolbox.lazy_required_fn("telescope.builtin", "lsp_document_symbols"),
                    description = "Document symbols",
                },
                {
                    ",S",
                    toolbox.lazy_required_fn("telescope.builtin", "lsp_dynamic_workspace_symbols"),
                    description = "Dynamic workspace symbols",
                },
                { ",gr", toolbox.lazy_required_fn("telescope.builtin", "lsp_references"), description = "References" },
                {
                    ",ge",
                    toolbox.lazy_required_fn("telescope.builtin", "diagnostics", { bufnr = 0 }),
                    description = "Buffer diagnostics",
                },
                { ",gE", toolbox.lazy_required_fn("telescope.builtin", "diagnostics"), description = "Diagnostics" },
            },
        },
        {
            itemgroup = "Rust tools",
            icon = "",
            keymaps = {
                { "<Leader>rr", "<Cmd>RustRunnables<CR>", description = "Runnables" },
                { "<Leader>rb", "<Cmd>RustDebuggables<CR>", description = "Debuggables" },
                { "<Leader>rc", "<Cmd>RustOpenCargo<CR>", description = "Open cargo" },
                { "<Leader>ro", "<Cmd>RustOpenExternalDocs<CR>", description = "External docs" },
            },
        },
        {
            itemgroup = "Clangd extensions",
            icon = " ",
            keymaps = {
                { "<Leader>cc", "<Cmd>ClangdSwitchSourceHeader<CR>", description = "Switch source header" },
                { "<Leader>ca", "<Cmd>ClangdAST<CR>", description = "AST" },
                { "<Leader>cm", "<Cmd>ClangdMemoryUsage<CR>", description = "Memory usage" },
                { "<Leader>cs", "<Cmd>ClangdSymbolInfo<CR>", description = "Symbol info" },
                { "<Leader>ct", "<Cmd>ClangdTypeHierarchy<CR>", description = "Type hierarch" },
            },
        },
        {
            itemgroup = "Luasnip",
            icon = " ",
            keymaps = {
                {
                    "<C-l>",
                    function()
                        if require("luasnip").choice_active() then
                            require("luasnip").change_choice(1)
                        end
                    end,
                    description = "Next choice",
                    mode = "i",
                    opts = { noremap = false },
                },
                {
                    "<C-h>",
                    function()
                        if require("luasnip").choice_active(-1) then
                            require("luasnip").change_choice(-1)
                        end
                    end,
                    description = "Previous choice",
                    mode = "i",
                    opts = { noremap = false },
                },
                {
                    "<C-s>",
                    require "luasnip.extras.select_choice",
                    description = "Select choice",
                    mode = "i",
                },
            },
        },
        {
            itemgroup = "Git",
            icon = " ",
            keymaps = {
                { "<Leader>Gg", "<Cmd>Neogit<CR>", description = "Open Neogit" },
                { "<Leader>Gd", ":DiffviewOpen ", description = "Open DiffviewOpen", opts = { silent = false } },
            },
        },
        {
            itemgroup = "Gitsigns",
            icon = " ",
            keymaps = {
                {
                    "<Leader>Gs",
                    {
                        n = { toolbox.lazy_required_fn("gitsigns", "stage_hunk") },
                        v = {
                            toolbox.lazy_required_fn("gitsigns", "stage_hunk", { vim.fn.line ".", vim.fn.line "v" }),
                        },
                    },
                    description = "Stage hunk",
                    mode = "v",
                },
                {
                    "<Leader>Gu",
                    "<Cmd>lua require('gitsigns').undo_stage_hunk()<CR>",
                    description = "Undo stage hunk",
                },
                {
                    "<Leader>Gr",
                    {
                        n = { toolbox.lazy_required_fn("gitsigns", "reset_hunk") },
                        v = { "<Cmd>lua require('gitsigns').reset_hunk({ vim.fn.line('.'), vim.fn.line('v') })<CR>" },
                    },
                    description = "Reset hunk",
                },
                { "<Leader>GR", toolbox.lazy_required_fn("gitsigns", "reset_buffer"), description = "Reset buffer" },
                { "<Leader>Gp", toolbox.lazy_required_fn("gitsigns", "preview_hunk"), description = "Preview hunk" },
                { "<Leader>Gl", toolbox.lazy_required_fn("gitsigns", "toggle_linehl"), description = "Toggle linehl" },
                { "<Leader>Gb", toolbox.lazy_required_fn("gitsigns", "blame_line"), description = "Blame line" },
                {
                    "]g",
                    "&diff ? ']g' : '<Cmd>lua require(\"gitsigns.actions\").next_hunk()<CR>'",
                    description = "Go to next hunk",
                    opts = { expr = true },
                },
                {
                    "[g",
                    "&diff ? '[g' : '<Cmd>lua require(\"gitsigns.actions\").prev_hunk()<CR>'",
                    description = "Go to previous hunk",
                    opts = { expr = true },
                },
                {
                    "ih",
                    {
                        o = { toolbox.lazy_required_fn("gitsigns.actions", "select_hunk") },
                        x = { toolbox.lazy_required_fn("gitsigns.actions", "select_hunk") },
                    },
                },
            },
        },
        {
            itemgroup = "Cycle",
            icon = "ﯩ ",
            keymaps = {
                { "]v", toolbox.lazy_required_fn("m3dry.cycle", "cycle_next"), description = "Next char" },
                { "[v", toolbox.lazy_required_fn("m3dry.cycle", "cycle_prev"), description = "Previous char" },
            },
        },
        {
            itemgroup = "Dial",
            icon = "∆",
            keymaps = {
                { "<C-a>", "<Plug>(dial-increment)", hide = true, mode = "v", opts = { noremap = false } },
                { "<C-x>", "<Plug>(dial-decrement)", hide = true, mode = "v", opts = { noremap = false } },
                { "g<C-a>", "<Plug>(dial-increment-additional)", hide = true, mode = "v", opts = { noremap = false } },
                { "g<C-x>", "<Plug>(dial-decrement-additional)", hide = true, mode = "v", opts = { noremap = false } },
            },
        },
        {
            itemgroup = "Todo comments",
            icon = " ",
            keymaps = {
                {
                    "<Leader>ttt",
                    "<Cmd>TodoTrouble<CR>",
                    description = "Open trouble with todo comments",
                },
                {
                    "<Leader>tte",
                    "<Cmd>TodoTelescope<CR>",
                    description = "Open Telescope with todo comments",
                },
                {
                    "<Leader>ttq",
                    "<Cmd>TodoQuickFix<CR>",
                    description = "Open QuickFix with todo comments",
                },
                {
                    "<Leader>ttl",
                    "<Cmd>TodoLocList<CR>",
                    description = "Open LocList with todo comments",
                },
                {
                    "]c",
                    toolbox.lazy_required_fn("todo-comments", "jump_next"),
                    description = "Go to the next todo",
                },
                {
                    "[c",
                    toolbox.lazy_required_fn("todo-comments", "jump_prev"),
                    description = "Go to the previous todo",
                },
            },
        },
        {
            itemgroup = "Treehopper",
            icon = " ",
            keymaps = {
                {
                    "m",
                    toolbox.lazy_required_fn("tsht", "nodes"),
                    description = "Select up to node",
                    modes = { "o", "x" },
                },
            },
        },
        {
            itemgroup = "Treesj",
            icon = "פּ ",
            keymaps = {
                { "<Leader>J", "<Cmd>TSJToggle<CR>", description = "Toggle treesj" },
            },
        },
        {
            itemgroup = "Query secretary",
            icon = " ",
            keymaps = {
                {
                    "<Leader>qs",
                    toolbox.lazy_required_fn("query-secretary", "query_window_initiate"),
                    description = "Open query secretary",
                },
            },
        },
        {
            itemgroup = "Ccc",
            icon = " ",
            keymaps = {
                { "<Leader>C", "<Cmd>CccPick<CR>", description = "Ccc pick" },
            },
        },
        {
            itemgroup = "Nabla",
            icon = " ",
            keymaps = {
                { "<Leader>np", toolbox.lazy_required_fn("nabla", "popup"), description = "Open nabla popup" },
            },
        },
        {
            itemgroup = "AsciiTree",
            icon = " ",
            keymaps = {
                { "<Leader>at", "<Cmd>AsciiTree<CR>", description = "Make asciitree" },
                { "<Leader>au", "<Cmd>AsciiTreeUndo", description = "Undo asciitree" },
            },
        },
        {
            "<Leader>o",
            toolbox.lazy_required_fn("open", "open_cword"),
            description = "Open link under cursor",
        },
        {
            "<Esc>",
            "<Cmd>noh<CR>",
            description = "Hide search highlight",
        },
        {
            "<Leader>.",
            ":e ",
            description = "Open a file",
            opts = { silent = false },
        },
        { "<C-d>", "<C-d>zz", description = "<C-d> + center" },
        { "<C-u>", "<C-u>zz", description = "<C-u> + center" },
        { "<C-f>", "<C-f>zz", description = "<C-f> + center" },
        { "<C-b>", "<C-b>zz", description = "<C-b> + center" },
        { "H", "^", description = "Go to the start of line" },
        { "L", "g_", description = "Go to the end of line" },
        { "J", "mzJ`z", description = "Join line" },
        { "<Leader><Leader>", "<C-^>", description = "Switch file" },
        {
            "J",
            ":m '>+1<CR>gv",
            description = "Move selected lines up by one",
            mode = "v",
        },
        {
            "K",
            ":m '<-2<CR>gv",
            description = "Move selected lines down by one",
            mode = "v",
        },
        {
            "<",
            "<gv",
            description = "Dedent selected lines and restore selection",
            mode = "v",
        },
        {
            ">",
            ">gv",
            description = "Indent selected lines and restore selection",
            mode = "v",
        },
        {
            ".",
            ".<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            ",",
            ",<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            "!",
            "!<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            "?",
            "?<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            ";",
            ";<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            ":",
            ":<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            "=",
            "=<C-g>u",
            mode = "i",
            hide = true,
        },
        {
            "<C-p>",
            function()
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
            end,
            description = "Move out of pair",
            mode = "i",
        },
    },
}
