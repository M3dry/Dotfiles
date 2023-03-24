local null_ls = require "null-ls"

null_ls.setup {
    sources = {
        -- Code actions
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.code_actions.shellcheck,
        null_ls.builtins.code_actions.statix,
        -- Diagnostics
        null_ls.builtins.diagnostics.statix,
        -- Formatting
        null_ls.builtins.formatting.prettier.with {
            extra_filetypes = { "svelte" },
        },
        null_ls.builtins.formatting.alejandra,
        null_ls.builtins.formatting.fourmolu,
        null_ls.builtins.formatting.rustfmt.with {
            extra_args = function(params)
                local Path = require "plenary.path"
                local cargo_toml = Path:new(params.root .. "/" .. "Cargo.toml")

                if cargo_toml:exists() and cargo_toml:is_file() then
                    for _, line in ipairs(cargo_toml:readlines()) do
                        local edition = line:match [[^edition%s*=%s*%"(%d+)%"]]
                        if edition then
                            return { "--edition=" .. edition }
                        end
                    end
                end
                return { "--edition=2021" }
            end,
        },
        null_ls.builtins.formatting.cljstyle,
        null_ls.builtins.formatting.taplo,
        null_ls.builtins.formatting.stylua,
    },
}

require("null-ls-embedded").config = {
    ignore_langs = {
        ["*"] = { "comment" },
        markdown = { "inline_markdown" },
        norg = { "norg_meta" },
    },
}
