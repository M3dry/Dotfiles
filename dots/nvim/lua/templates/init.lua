local M = {}

---@alias TemplatesConfig { path: string }

---@type TemplatesConfig
M.config = {
    path = "",
}

---@type table<string, function(): table>
local handlers = {}

---@return string[]
local function available_filetypes()
    if M.config.path == "" then
        return {}
    end

    local fts = {}
    for name, type in vim.fs.dir(M.config.path) do
        if type == "file" and string.match(name, ".lua$") then
            table.insert(fts, string.sub(name, 1, #name - 4))
        end
    end

    return fts
end

local function load_filetypes()
    local fts = available_filetypes()

    for _, ft in pairs(fts) do
        local f, err = loadfile(vim.fs.normalize(vim.fs.joinpath(M.config.path, ft) .. ".lua"))
        if err then
            print(err)
        end

        ---@diagnostic disable-next-line: need-check-nil
        handlers[ft] = f()
    end
end

---Doesn't check if file is empty, does nothing if handler doesn't exist
local function load_template()
    local ft = vim.fn.expand "%:e"

    if handlers[ft] == nil then
        return
    end

    require("luasnip").snip_expand(handlers[ft]())
end

local function setup_autocmd()
    vim.api.nvim_create_autocmd("BufNewFile", {
        group = vim.api.nvim_create_augroup("TEMPLATES", { clear = true }),
        pattern = "*",
        callback = function()
            load_template()
        end,
    })
end

---@param opts TemplatesConfig
local function setup(opts)
    -- TODO: merge default `M.config` values with `opts` values
    M.config = opts

    load_filetypes()
    setup_autocmd()
end

M.setup = setup

return M
