if require("nvim-treesitter.parsers").has_parser("c") then
    local highlights = ''
    local path = '.config/nvim/queries/c'

    for _, line in ipairs(vim.fn.readfile(string.format("%s/%s/highlights.scm", os.getenv("HOME"), path))) do
        highlights = highlights .. '\n' .. line
    end

    require("vim.treesitter.query").set_query("c", "highlights", highlights)

    local textsubjects = ''

    for _, line in ipairs(vim.fn.readfile(string.format("%s/%s/textsubjects-smart.scm", os.getenv("HOME"), path))) do
        textsubjects = textsubjects .. '\n' .. line
    end

    require("vim.treesitter.query").set_query("c", "textsubjects-smart", textsubjects)

    for _, line in ipairs(vim.fn.readfile(string.format("%s/%s/textsubjects-big.scm", os.getenv("HOME"), path))) do
        textsubjects = textsubjects .. '\n' .. line
    end

    require("vim.treesitter.query").set_query("c", "textsubjects-big", textsubjects)

    for _, line in ipairs(vim.fn.readfile(string.format("%s/%s/textsubjects-container-outer.scm", os.getenv("HOME"), path))) do
        textsubjects = textsubjects .. '\n' .. line
    end

    require("vim.treesitter.query").set_query("c", "textsubjects-container-outer", textsubjects)
end
