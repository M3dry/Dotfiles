require("neorg").setup {
    load = {
        ["core.defaults"] = {},
        ["core.norg.dirman"] = {
            config = {
                autochdir = true,
                default_workspace = "notes",
                workspaces = {
                    notes = "~/my-stuff/Neorg/Notes/",
                    default = "~/my-stuff/Neorg/"
                },
            },
        },
        ["core.norg.journal"] = {
            config = {
                workspace = "default",
                journal_folder = "Journal",
            },
        },
        ["core.norg.completion"] = {
            config = {
                engine = "nvim-cmp",
            },
        },
        ["core.integrations.nvim-cmp"] = {},
        ["core.export"] = {},
        ["core.export.markdown"] = {
            config = {
                extensions = "all",
            },
        },
        ["core.norg.qol.toc"] = {
            config = {
                toc_split_placement = "right",
            },
        },
        ["core.norg.esupports.metagen"] = {
            config = {
                type = "auto",
            },
        },
        ["core.presenter"] = {
            config = {
                zen_mode = "zen-mode",
            },
        },
        ["core.norg.concealer"] = {
            config = {
                icons = {
                    todo = {
                        enabled = true,
                        done = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_done) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        pending = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_pending) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        undone = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_undone) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        uncertain = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_uncertain) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        on_hold = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_on_hold) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        cancelled = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_cancelled) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        recurring = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_recurring) @icon",
                            extract = function()
                                return 1
                            end,
                        },

                        urgent = {
                            enabled = true,
                            icon = "",
                            query = "(todo_item_urgent) @icon",
                            extract = function()
                                return 1
                            end,
                        },
                    },
                    list = {
                        enabled = true,
                        level_1 = {
                            enabled = true,
                            icon = "•",
                            query = "(unordered_list1_prefix) @icon",
                        },

                        level_2 = {
                            enabled = true,
                            icon = " ➤",
                            query = "(unordered_list2_prefix) @icon",
                        },

                        level_3 = {
                            enabled = true,
                            icon = "  •",
                            query = "(unordered_list3_prefix) @icon",
                        },

                        level_4 = {
                            enabled = true,
                            icon = "   ➤",
                            query = "(unordered_list4_prefix) @icon",
                        },

                        level_5 = {
                            enabled = true,
                            icon = "    •",
                            query = "(unordered_list5_prefix) @icon",
                        },

                        level_6 = {
                            enabled = true,
                            icon = "     ➤",
                            query = "(unordered_list6_prefix) @icon",
                        },
                    },
                    heading = {
                        enabled = true,

                        level_1 = {
                            enabled = true,
                            icon = "⬢",
                            highlight = "@neorg.headings.1.prefix",
                            query = "[ (heading1_prefix) (link_target_heading1) @no-conceal ] @icon",
                        },

                        level_2 = {
                            enabled = true,
                            icon = " ⬡",
                            highlight = "@neorg.headings.2.prefix",
                            query = "[ (heading2_prefix) (link_target_heading2) @no-conceal ] @icon",
                        },

                        level_3 = {
                            enabled = true,
                            icon = "  ◆",
                            highlight = "@neorg.headings.3.prefix",
                            query = "[ (heading3_prefix) (link_target_heading3) @no-conceal ] @icon",
                        },

                        level_4 = {
                            enabled = true,
                            icon = "   ◈",
                            highlight = "@neorg.headings.4.prefix",
                            query = "[ (heading4_prefix) (link_target_heading4) @no-conceal ] @icon",
                        },

                        level_5 = {
                            enabled = true,
                            icon = "    ◇",
                            highlight = "@neorg.headings.5.prefix",
                            query = "[ (heading5_prefix) (link_target_heading5) @no-conceal ] @icon",
                        },

                        level_6 = {
                            enabled = true,
                            highlight = "@neorg.headings.6.prefix",
                            query = "[ (heading6_prefix) (link_target_heading6) @no-conceal ] @icon",
                            render = function(self, text)
                                local icons = { "●", "◉", "○", "✹", "✿", "✤", "✜" }
                                local text_len = text:len()
                                return {
                                    {
                                        string.rep(" ", text_len - 2)
                                            .. (icons[text_len - string.len "******"] or icons[#icons])
                                            or icons[#icons],
                                        self.highlight,
                                    },
                                }
                            end,
                        },
                    },
                },
                dim_code_blocks = {
                    conceal = false,
                },
            },
        },
        ["core.keybinds"] = {
            config = {
                default_keybinds = false,
                hook = function(keybinds)
                    local leader = "<Leader>n"

                    keybinds.map_event_to_mode("norg", {
                        n = {
                            -- Tasks state
                            -- undone
                            { leader .. "tu", "core.norg.qol.todo_items.todo.task_undone" },
                            -- done
                            { leader .. "td", "core.norg.qol.todo_items.todo.task_done" },
                            -- pending
                            { leader .. "tp", "core.norg.qol.todo_items.todo.task_pending" },
                            -- hold
                            { leader .. "th", "core.norg.qol.todo_items.todo.task_on_hold" },
                            -- cancelled
                            { leader .. "tc", "core.norg.qol.todo_items.todo.task_cancelled" },
                            -- recurring
                            { leader .. "tr", "core.norg.qol.todo_items.todo.task_recurring" },
                            -- important
                            { leader .. "ti", "core.norg.qol.todo_items.todo.task_important" },
                            -- Task view
                            { leader .. "tv", "core.gtd.base.views" },
                            -- Task edit
                            { leader .. "te", "core.gtd.base.edit" },
                            -- Capture
                            { leader .. "c", "core.gtd.base.capture" },
                            -- New note
                            { leader .. "n", "core.norg.dirman.new.note" },
                            -- Hop to the destination of the link under the cursor
                            { "<CR>", "core.norg.esupports.hop.hop-link" },
                        },
                    }, {
                        silent = true,
                        noremap = true,
                    })

                    -- Map the below keys only when traverse-heading mode is active
                    keybinds.map_event_to_mode("traverse-heading", {
                        n = {
                            { "j", "core.integrations.treesitter.next.heading" },
                            { "k", "core.integrations.treesitter.previous.heading" },
                        },
                    }, {
                        silent = true,
                        noremap = true,
                    })

                    keybinds.map_event_to_mode("toc-split", {
                        n = {
                            { "<CR>", "core.norg.qol.toc.hop-toc-link" },
                            { "q", "core.norg.qol.toc.close" },
                            { "<Esc>", "core.norg.qol.toc.close" },
                        },
                    }, {
                        silent = true,
                        noremap = true,
                        nowait = true,
                    })

                    -- Map the below keys on gtd displays
                    keybinds.map_event_to_mode("gtd-displays", {
                        n = {
                            { "<CR>", "core.gtd.ui.goto_task" },
                            { "q", "core.gtd.ui.close" },
                            { "<Esc>", "core.gtd.ui.close" },
                            { "e", "core.gtd.ui.edit_task" },
                            { "<Tab>", "core.gtd.ui.details" },
                        },
                    }, {
                        silent = true,
                        noremap = true,
                        nowait = true,
                    })

                    -- Map the below keys on presenter mode
                    keybinds.map_event_to_mode("presenter", {
                        n = {
                            { "<CR>", "core.presenter.next_page" },
                            { "<C-l>", "core.presenter.next_page" },
                            { "<C-h>", "core.presenter.previous_page" },
                            { "q", "core.presenter.close" },
                            { "<Esc>", "core.presenter.close" },
                        },
                    }, {
                        silent = true,
                        noremap = true,
                        nowait = true,
                    })

                    -- Apply the below keys to all modes
                    keybinds.map_to_mode("all", {
                        n = {
                            { leader .. "mn", ":Neorg mode norg<CR>" },
                            { leader .. "mh", ":Neorg mode traverse-heading<CR>" },
                            { leader .. "T", ":Neorg toc split<CR>" },
                        },
                    }, {
                        silent = true,
                        noremap = true,
                    })
                end,
            },
        },
    },
}
