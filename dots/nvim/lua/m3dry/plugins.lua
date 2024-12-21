local function bootstrap_pckr()
    local pckr_path = vim.fn.stdpath "data" .. "/pckr/pckr.nvim"

    if not (vim.uv or vim.loop).fs_stat(pckr_path) then
        vim.fn.system {
            "git",
            "clone",
            "--filter=blob:none",
            "https://github.com/lewis6991/pckr.nvim",
            pckr_path,
        }
    end

    vim.opt.rtp:prepend(pckr_path)
end

bootstrap_pckr()

local event = require "pckr.loader.event"
local cmd = require "pckr.loader.cmd"

require("pckr").add {
    {
        "cpea2506/one_monokai.nvim",
        config = function()
            require("one_monokai").setup {
                transparent = false,
                colors = { dark_bg = "#0f111b" },
                themes = function(colors)
                    local same_fg_bg = function(color)
                        return { fg = color, bg = color }
                    end

                    return {
                        Normal = { bg = colors.dark_bg },
                        DiagnosticUnderlineError = { sp = colors.red, undercurl = true },
                        DiagnosticUnderlineHint = { sp = colors.light_gray, undercurl = true },
                        DiagnosticUnderlineInfo = { sp = colors.green, undercurl = true },
                        DiagnosticUnderlineWarn = { sp = colors.yellow, undercurl = true },

                        TelescopeResultsBorder = same_fg_bg(colors.dark_bg),
                        TelescopeResultsTitle = { fg = colors.dark_bg },

                        TelescopePreviewBorder = same_fg_bg(colors.bg),
                        TelescopePreviewNormal = same_fg_bg(colors.bg),
                        TelescopePreviewTitle = same_fg_bg(colors.bg),

                        TelescopePromptBorder = same_fg_bg(colors.green),
                        TelescopePromptNormal = { fg = colors.dark_bg, bg = colors.green },
                        TelescopePromptTitle = { fg = colors.dark_bg },
                        TelescopePromptPrefix = { fg = colors.dark_bg },
                        TelescopePromptCounter = { fg = colors.dark_bg },

                        ["@variable"] = { fg = colors.purple },

                        TabLine = { fg = colors.fg, bg = colors.bg },
                        TabLineSel = { fg = colors.dark_bg, bg = colors.fg },
                        TabLineFill = same_fg_bg(colors.dark_bg),
                    }
                end,
                italics = true,
            }
        end,
    },

    {
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("ibl").setup {
                indent = { char = "│" },
                scope = {
                    enabled = false,
                },
            }
        end,
    },

    {
        "jghauser/mkdir.nvim",
        config = function()
            require "mkdir"
        end,
    },

    "mbbill/undotree",

    {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require "m3dry.plugins.treesitter"
        end,
    },

    { "nvim-treesitter/nvim-treesitter-textobjects", requires = "nvim-treesitter/nvim-treesitter" },

    {
        "folke/todo-comments.nvim",
        cond = { cmd "TodoTelescope", event "CursorMoved" },
        config = function()
            require "m3dry.plugins.todo-comments"
        end,
    },

    {
        "neovim/nvim-lspconfig",
        requires = { "saghen/blink.cmp", "kosayanda/nvim-lightbulb" },
        config = function()
            require "m3dry.plugins.lsp"
        end,
    },

    { "rachartier/tiny-inline-diagnostic.nvim",
        config = function()
            vim.diagnostic.config({ virtual_text = false })
            require("tiny-inline-diagnostic").setup {
                preset = "powerline",
                hi = {
                    background = "#0f111b",
                }
            }
        end
    },

    {
        "stevearc/conform.nvim",
        config = function()
            require("conform").setup {
                formatters_by_ft = {
                    lua = { "stylua" },
                    haskell = { "fourmolu" },
                    rust = { "rustfmt" },
                    nix = { "alejandra" },
                },
            }

            vim.api.nvim_create_user_command("Format", function(args)
                local range = nil
                if args.count ~= -1 then
                    local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
                    range = {
                        start = { args.line1, 0 },
                        ["end"] = { args.line2, end_line:len() },
                    }
                end
                require("conform").format { async = true, lsp_format = "fallback", range = range }
            end, { range = true })
        end,
    },

    {
        "jmbuhr/otter.nvim",
        requries = "nvim-treesitter/nvim-treesitter,",
        config = function()
            require("otter").setup {}
        end,
    },

    {
        "folke/lazydev.nvim",
        config = function()
            require("lazydev").setup {
                path = "${3rd}/luv/library",
                words = { "vim%.uv" },
            }
        end,
    },

    {
        "saghen/blink.cmp",
        cond = event "InsertEnter",
        requires = "L3MON4D3/LuaSnip",
        run = "nix run .#build-plugin",
        config = function()
            require "m3dry.plugins.blink"
        end,
    },

    {
        "kosayoda/nvim-lightbulb",
        cond = event "InsertEnter",
        config = function()
            require("nvim-lightbulb").setup {
                autocmd = {
                    enabled = true,
                },
                hide_in_unfocused_buffer = false,
                code_lenses = true,
                sign = {
                    enabled = true,
                    text = "",
                    lens_text = "",
                },
            }
        end,
    },

    {
        "L3MON4D3/LuaSnip",
        tag = "v2.*",
        run = "make install_jsregexp",
        cond = event "InsertEnter",
        config = function()
            require "m3dry.plugins.luasnip"
        end,
    },

    {
        "utilyre/sentiment.nvim",
        config = function()
            require("sentiment").setup {}
        end,
    },

    {
        "kylechui/nvim-surround",
        config = function()
            require("nvim-surround").setup {}
        end,
    },

    {
        "windwp/nvim-autopairs",
        config = function()
            require("nvim-autopairs").setup {
                enable_check_bracketline = false,
                fastwrap = {
                    map = "<C-v>",
                },
            }
        end,
    },

    { "eraserhd/parinfer-rust", build = "cargo build --release" },

    {
        "nvim-telescope/telescope.nvim",
        requires = {
            "nvim-lua/plenary.nvim",
            "nvim-tree/nvim-web-devicons",
            { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
            "nvim-telescope/telescope-ui-select.nvim",
        },
        config = function()
            require "m3dry.plugins.telescope"
        end,
    },

    {
        "ggandor/lightspeed.nvim",
        config = function()
            require("lightspeed").setup {}
        end,
    },

    {
        "NeogitOrg/neogit",
        cond = { cmd "Neogit", cmd "NeogitCommit", cmd "NeogitLogCurrent", cmd "NeogitResetState" },
        requires = {
            "nvim-lua/plenary.nvim",
            "sindrets/diffview.nvim",
            "nvim-telescope/telescope.nvim",
        },
        config = function()
            require("neogit").setup {
                integrations = {
                    telescope = true,
                    diffview = true,
                },
            }
        end,
    },
}
