require("todo-comments").setup {
    signs = true,
    sign_priority = 8,
    keywords = {
        FIX = {
            icon = " ",
            color = "error",
            alt = { "FIXME", "BUG", "FIXIT", "ISSUE" },
        },
        TODO = {
            icon = " ",
            color = "info",
        },
        HACK = {
            icon = " ",
            color = "warning",
        },
        WARN = {
            icon = " ",
            color = "warning",
            alt = { "WARNING", "XXX" },
        },
        PERF = { icon = " ", color = "performance", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
        TEST = { icon = " ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
    },
    gui_style = {
        fg = "NONE",
        bg = "BOLD",
    },
    merge_keywords = false,
    highlight = {
        multiline = true,
        multiline_pattern = "^.",
        multiline_context = 10,
        before = "",
        keyword = "wide_bg",
        after = "fg",
        pattern = [[.*<(KEYWORDS)\s*:]],
        comments_only = true,
        max_line_len = 10000,
        exclude = { "norg", "markdown" },
    },
    colors = {
        error = { "DiagnosticError", "ErrorMsg", "#ff5370" },
        warning = { "DiagnosticWarning", "WarningMsg", "#f78c6c" },
        hint = { "DiagnosticHint", "#72a4ff" },
        info = { "DiagnosticInfo", "#c3e88d" },
        performance = { "Type", "#c792ea" },
        test = { "Keyword", "#89ddff" },
    },
    search = {
        command = "rg",
        args = {
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
        },
        pattern = [[\b(KEYWORDS):]],
    },
}
