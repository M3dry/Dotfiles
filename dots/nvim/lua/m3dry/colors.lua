local hi = {}

hi["Normal"] = { fg = "#eeffff", bg = "#0f111b" }
hi["WinSeparator"] = { bg = "none" }
hi["Visual"] = { bg = "#4e5579" }
hi["Search"] = { bg = "#4e5579" }
hi["LineNr"] = { fg = "#c0d4ff", bg = "#292d3e", bold = true }
hi["LineNrAbove"] = { fg = "#c792ea", bg = "#292d3e" }
hi["LineNrBelow"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["Todo"] = { fg = "#12111e", bg = "#c3e88d" }
hi["CursorLineNr"] = { fg = "#c792ea", bg = "#111111" }
hi["SignColumn"] = { fg = "#eeffff", bg = "#292d3e" }
hi["ColorColumn"] = { fg = "#292d3e" }
hi["VirtColumn"] = { fg = "#292d3e" }
hi["Title"] = { fg = "#c792ea" }
hi["diffAdded"] = { fg = "#c3e88d", bg = "#353c34" }
hi["diffRemoved"] = { fg = "#ff5370", bg = "#634661" }
hi["DiffAdd"] = { fg = "#c3e88d", bg = "#353c34" }
hi["DiffDelete"] = { fg = "#ff5370", bg = "#634661" }
hi["NonText"] = { fg = "#4e5579" }
hi["Error"] = { fg = "#ff5370" }
hi["ErrorMsg"] = { fg = "#ff5370" }
hi["WarningMsg"] = { fg = "#f78c6c" }
hi["Directory"] = { fg = "#72a4ff" }
hi["Pmenu"] = { fg = "#eeffff", bg = "#292d3e" }
hi["PmenuSel"] = { fg = "#12111e", bg = "#72a4ff" }
hi["PmenuSbar"] = {}
hi["PmenuThumb"] = { bg = "#c792ea" }
hi["Folded"] = { fg = "#72a4ff" }
hi["EndOfBuffer"] = { fg = "#292d3e" }
hi["MatchParen"] = { fg = "#ff5370", bold = true }
hi["MatchArea"] = { bg = "#292d3e" }
hi["Comment"] = { fg = "#a8b5f7", italic = true }
hi["Constant"] = { fg = "#89ddff", bold = true }
hi["String"] = { fg = "#c3e88d" }
hi["Character"] = { fg = "#f78c6c", bold = true }
hi["Number"] = { fg = "#f78c6c", bold = true }
hi["Boolean"] = { fg = "#89ddff", bold = true, italic = true }
hi["Float"] = { fg = "#ffcb6b", italic = true }
hi["FloatBorder"] = { fg = "#292d3e", bg = "#292d3e" }
hi["NormalFloat"] = { fg = "#eeffff", bg = "#292d3e" }
hi["Identifier"] = { fg = "#ffcb6b" }
hi["Function"] = { fg = "#72a4ff", italic = true }
hi["Statement"] = { fg = "#89ddff", italic = true }
hi["Conditional"] = { fg = "#89ddff", bold = true, italic = true }
hi["Repeat"] = { fg = "#89ddff", bold = true, italic = true }
hi["Label"] = { fg = "#89ddff", italic = true }
hi["Operator"] = { fg = "#72a4ff" }
hi["Keyword"] = { fg = "#89ddff", bold = true, italic = true }
hi["Exception"] = { fg = "#89ddff", bold = true, italic = true }
hi["PreProc"] = { fg = "#89ddff", bold = true }
hi["Include"] = { fg = "#72a4ff", bold = true, italic = true }
hi["Define"] = { fg = "#72a4ff", bold = true, italic = true }
hi["Macro"] = { fg = "#72a4ff", bold = true }
hi["PreCondit"] = { fg = "#72a4ff", bold = true, italic = true }
hi["Type"] = { fg = "#c792ea" }
hi["StorageClass"] = { fg = "#89ddff", italic = true }
hi["Structure"] = { fg = "#89ddff", italic = true }
hi["Typedef"] = { fg = "#89ddff", italic = true }
hi["Special"] = { fg = "#c3e88d" }
hi["SpecialChar"] = { fg = "#c3e88d", italic = true }
hi["Delimeter"] = { fg = "#72a4ff" }

-- Telescope
hi["TelescopeResultsBorder"] = { fg = "#12111e", bg = "#12111e" }
hi["TelescopePreviewBorder"] = { fg = "#12111e", bg = "#12111e" }
hi["TelescopePromptBorder"] = { fg = "#292d3e", bg = "#292d3e" }
hi["TelescopePreviewNormal"] = { fg = "#eeffff", bg = "#12111e" }
hi["TelescopeResultsNormal"] = { fg = "#eeffff", bg = "#12111e" }
hi["TelescopePromptNormal"] = { fg = "#eeffff", bg = "#292d3e" }
hi["TelescopePreviewTitle"] = { fg = "#12111e", bg = "#c3e88d" }
hi["TelescopeResultsTitle"] = { fg = "#12111e", bg = "#c3e88d" }
hi["TelescopePromptTitle"] = { fg = "#292d3e", bg = "#ff5370" }
hi["TelescopePromptPrefix"] = { fg = "#ff5370", bg = "#292d3e" }

-- Lightspeed
hi["LightspeedGreyWash"] = {}

-- Hlslens
hi["HlSearchFloat"] = { fg = "#72a4ff" }
hi["HlSearchNear"] = { bg = "#4e5579" }
hi["HlSearchLens"] = { fg = "#ff5370" }
hi["HlSearchLensNear"] = { fg = "#72a4ff" }

-- Autopairs
hi["FastWrap"] = { fg = "#ff5370", bg = "#12111e" }

-- FloatTerm
hi["FloatTermNormal"] = { fg = "#eeffff" }
hi["FloatTermBorder"] = { fg = "#ff5370" }

-- Lsp-inlayhints
hi["LspInlayHint"] = { fg = "#3c435e" }

-- Treesitter
hi["@boolean"] = { fg = "#89ddff", italic = true }
hi["@repeat"] = { fg = "#89ddff", italic = true }
hi["@comment"] = { fg = "#a8b5f7", italic = true }
hi["@conditional"] = { fg = "#89ddff", italic = true }
hi["@constant"] = { fg = "#89ddff", bold = true }
hi["@constant.builtin"] = { fg = "#89ddff", bold = true, italic = true }
hi["@constant.macro"] = { fg = "#89ddff", italic = true }
hi["@constructor"] = { fg = "#89ddff", bold = true, italic = true }
hi["@define"] = { fg = "#72a4ff", italic = true }
hi["@function"] = { fg = "#72a4ff", italic = true }
hi["@function.call"] = { fg = "#c792ea", italic = true }
hi["@function.builtin"] = { fg = "#72a4ff", italic = true }
hi["@function.macro"] = { fg = "#89ddff", italic = true }
hi["@method"] = { fg = "#89ddff" }
hi["@method.call"] = { fg = "#89ddff", italic = true }
hi["@repeat"] = { fg = "#89ddff", italic = true }
hi["@label"] = { fg = "#ff5370", italic = true }
hi["@include"] = { fg = "#72a4ff", italic = true }
hi["@number"] = { fg = "#f78c6c" }
hi["@float"] = { fg = "#f78c6c", italic = true }
hi["@operator"] = { fg = "#72a4ff" }
hi["@parameter"] = { fg = "#ffcb6b" }
hi["@parameter.reference"] = { fg = "#ffcb6b", italic = true }
hi["@property"] = { fg = "#ff5370", italic = true }
hi["@symbol"] = { fg = "#ff5370" }
hi["@punctuation.bracket"] = { fg = "#72a4ff" }
hi["@punctuation.delimiter"] = { fg = "#72a4ff" }
hi["@punctuation.special"] = { fg = "#ff5370" }
hi["@string"] = { fg = "#c3e88d" }
hi["@string.escape"] = { fg = "#72a4ff" }
hi["@string.regex"] = { fg = "#ff5370", italic = true }
hi["@string.special"] = { fg = "#89ddff" }
hi["@character"] = { fg = "#f78c6c" }
hi["@character.special"] = { fg = "#ff5370" }
hi["@type"] = { fg = "#c792ea" }
hi["@type.builtin"] = { fg = "#c792ea", italic = true }
hi["@type.definition"] = { fg = "#c792ea", italic = true }
hi["@field"] = { fg = "#ff5370" }
hi["@variable"] = { fg = "#ffcb6b" }
hi["@variable.builtin"] = { fg = "#89ddff", bold = true }
hi["@namespace"] = { fg = "#c792ea", italic = true }
hi["@keyword"] = { fg = "#89ddff", italic = true }
hi["@keyword.operator"] = { fg = "#89ddff", italic = true }
hi["@keyword.function"] = { fg = "#89ddff", italic = true }
hi["@keyword.return"] = { fg = "#ff5370", italic = true }
hi["@text.uri"] = { fg = "#89ddff", bold = true, italic = true }

-- Treesitter Neorg
hi["@neorg.tags.ranged_verbatim.begin"] = { fg = "#c792ea" }
hi["@neorg.tags.ranged_verbatim.end"] = { fg = "#c792ea" }
hi["@neorg.tags.ranged_verbatim.code_block"] = { bg = "#292d3e" }
hi["@neorg.tags.carryover.begin"] = { fg = "#c792ea" }
hi["@neorg.tags.carryover.name.word"] = { fg = "#89ddff" }
hi["@neorg.tags.carryover.parameters.word"] = { fg = "#c792ea" }
hi["@neorg.modifiers.escape"] = { fg = "#72a4ff" }
hi["@neorg.markup.superscript"] = { fg = "#c792ea", italic = true }
hi["@neorg.markup.subscript"] = { fg = "#72a4ff", italic = true }
hi["@neorg.markup.verbatim"] = { fg = "#89ddff", italic = true }
hi["@neorg.markup.variable"] = { fg = "#ffcb6b", italic = true }
hi["@neorg.markup.spoiler"] = { fg = "#89ddff" }
hi["@neorg.markup.inline_comment"] = { fg = "#a8b5f7" }
hi["@neorg.markup.inline_math"] = { fg = "#89ddff" }
hi["@neorg.headings.1.prefix"] = { fg = "#82aaff" }
hi["@neorg.headings.2.prefix"] = { fg = "#c792ea" }
hi["@neorg.headings.3.prefix"] = { fg = "#bb80b3" }
hi["@neorg.headings.4.prefix"] = { fg = "#a1bfff" }
hi["@neorg.headings.5.prefix"] = { fg = "#d5adef" }
hi["@neorg.headings.6.prefix"] = { fg = "#c0d4ff" }
hi["@neorg.headings.1.title"] = { fg = "#82aaff" }
hi["@neorg.headings.2.title"] = { fg = "#c792ea" }
hi["@neorg.headings.3.title"] = { fg = "#bb80b3" }
hi["@neorg.headings.4.title"] = { fg = "#a1bfff" }
hi["@neorg.headings.5.title"] = { fg = "#d5adef" }
hi["@neorg.headings.6.title"] = { fg = "#c0d4ff" }
hi["@neorg.lists.unordered.1.prefix"] = { fg = "#82aaff" }
hi["@neorg.lists.unordered.2.prefix"] = { fg = "#c792ea" }
hi["@neorg.lists.unordered.3.prefix"] = { fg = "#bb80b3" }
hi["@neorg.lists.unordered.4.prefix"] = { fg = "#a1bfff" }
hi["@neorg.lists.unordered.5.prefix"] = { fg = "#d5adef" }
hi["@neorg.lists.unordered.6.prefix"] = { fg = "#c0d4ff" }
hi["@neorg.lists.ordered.1.prefix"] = { fg = "#82aaff" }
hi["@neorg.lists.ordered.2.prefix"] = { fg = "#c792ea" }
hi["@neorg.lists.ordered.3.prefix"] = { fg = "#bb80b3" }
hi["@neorg.lists.ordered.4.prefix"] = { fg = "#a1bfff" }
hi["@neorg.lists.ordered.5.prefix"] = { fg = "#d5adef" }
hi["@neorg.lists.ordered.6.prefix"] = { fg = "#c0d4ff" }
hi["@neorg.links.unordered.1.prefix"] = { fg = "#82aaff" }
hi["@neorg.links.unordered.2.prefix"] = { fg = "#c792ea" }
hi["@neorg.links.unordered.3.prefix"] = { fg = "#bb80b3" }
hi["@neorg.links.unordered.4.prefix"] = { fg = "#a1bfff" }
hi["@neorg.links.unordered.5.prefix"] = { fg = "#d5adef" }
hi["@neorg.links.unordered.6.prefix"] = { fg = "#c0d4ff" }
hi["@neorg.links.ordered.1.prefix"] = { fg = "#82aaff" }
hi["@neorg.links.ordered.2.prefix"] = { fg = "#c792ea" }
hi["@neorg.links.ordered.3.prefix"] = { fg = "#bb80b3" }
hi["@neorg.links.ordered.4.prefix"] = { fg = "#a1bfff" }
hi["@neorg.links.ordered.5.prefix"] = { fg = "#d5adef" }
hi["@neorg.links.ordered.6.prefix"] = { fg = "#c0d4ff" }
hi["@neorg.quotes.1.prefix"] = { fg = "#82aaff" }
hi["@neorg.quotes.2.prefix"] = { fg = "#c792ea" }
hi["@neorg.quotes.3.prefix"] = { fg = "#bb80b3" }
hi["@neorg.quotes.4.prefix"] = { fg = "#a1bfff" }
hi["@neorg.quotes.5.prefix"] = { fg = "#d5adef" }
hi["@neorg.quotes.6.prefix"] = { fg = "#c0d4ff" }

-- Neotree
hi["NeoTreeCursorLine"] = { bg = "#2f3245" }
hi["NeoTreeBufferNumber"] = { fg = "#72a4ff" }
hi["NeoTreeDirectoryName"] = { fg = "#c792ea" }
hi["NeoTreeDirectoryIcon"] = { fg = "#c792ea" }
hi["NeoTreeDotfile"] = { fg = "#3c435e" }
hi["NeoTreeEndOfBuffer"] = { fg = "#292d3e" }
hi["NeoTreeExpander"] = { fg = "#ff0000" }
hi["NeoTreeFileName"] = { fg = "#eeffff" }
hi["NeoTreeFileNameOpened"] = { fg = "#ff5370" }
hi["NeoTreeFileIcon"] = { fg = "#ff5370" }
hi["NeoTreeGitAdded"] = { fg = "#c3e88d" }
hi["NeoTreeGitConflict"] = { fg = "#f78c6c" }
hi["NeoTreeGitDeleted"] = { fg = "#ff5370" }
hi["NeoTreeGitIgnored"] = { fg = "#3c435e" }
hi["NeoTreeGitModified"] = { fg = "#f78c6c" }
hi["NeoTreeGitRenamed"] = { fg = "#f78c6c" }
hi["NeoTreeGitStaged"] = { fg = "#c3e88d" }
hi["NeoTreeGitUnstaged"] = { fg = "#ff5370" }
hi["NeoTreeGitUntracked"] = { fg = "#eeffff" }
hi["NeoTreeDiagTotalCount"] = { fg = "#c792ea", bg = "#292d3e" }
hi["NeoTreeDiagPosition"] = { fg = "#ff5370", bg = "#292d3e" }
hi["NeoTreeDiagSource"] = { fg = "#a8b5f7", bg = "#292d3e" }
hi["NeoTreeDiagCode"] = { fg = "#a8b5f7", bg = "#292d3e" }
hi["NeoTreeNormal"] = { fg = "#eeffff", bg = "#292d3e" }
hi["NeoTreeNormalNC"] = { fg = "#eeffff", bg = "#292d3e" }
hi["NeoTreeRootName"] = { fg = "#ff5370" }

-- Quickfix
hi["qfLineNr"] = { fg = "#3c435e" }

-- Winbar
hi["WinbarNC"] = { fg = "#3c435e", bg = "#292d3e" }
hi["Winbar"] = { fg = "#eeffff", bg = "#292d3e" }
hi["WinbarNavic"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["WinbarIcon"] = { fg = "#ff5370", bg = "#292d3e" }
hi["WinbarHarpoon"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["WinbarPath"] = { fg = "#c792ea", bg = "#292d3e" }
hi["WinbarDiagnosticError"] = { fg = "#ff5370", bg = "#292d3e" }
hi["WinbarDiagnosticWarn"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["WinbarDiagnosticHint"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["WinbarDiagnosticInfo"] = { fg = "#c3e88d", bg = "#292d3e" }

-- Tabs
hi["TabLine"] = { fg = "#3c435e", bg = "#292d3e" }
hi["TabLineSel"] = { fg = "#c792ea", bg = "#292d3e", italic = true }
hi["TabLineIndex"] = { fg = "#3c435e", bg = "#292d3e" }
hi["TabLineSelIndex"] = { fg = "#c792ea", bg = "#292d3e", italic = true }
hi["TabLineNumSep"] = { fg = "#3c435e", bg = "#292d3e" }
hi["TabLineSelNumSep"] = { fg = "#c792ea", bg = "#292d3e", italic = true }
hi["TabLineBufNum"] = { fg = "#3c435e", bg = "#292d3e" }
hi["TabLineSelBufNum"] = { fg = "#c792ea", bg = "#292d3e", italic = true }
hi["TabLineSep"] = { fg = "#3c435e", bg = "#292d3e" }
hi["TabLineSelSep"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["TabLineMod"] = { fg = "#ff5370", bg = "#292d3e" }
hi["TabLineSelMod"] = { fg = "#ff5370", bg = "#292d3e" }
hi["TabLineFill"] = { bg = "#292d3e" }

-- Nvim Tree
hi["NvimTreeFolderName"] = { fg = "#c792ea" }
hi["NvimTreeFolderIcon"] = { fg = "#c792ea" }
hi["NvimTreeEmptyFolderName"] = { fg = "#c792ea" }
hi["NvimTreeOpenedFolderName"] = { fg = "#c792ea" }
hi["NvimTreeNormal"] = { fg = "#eeffff" }
hi["NvimTreeSymlink"] = { fg = "#72a4ff" }
hi["NvimTreeRootFolder"] = { fg = "#ffcb6b", italic = true }
hi["NvimTreeSpecialFile"] = { fg = "#ff5370", italic = true }
hi["NvimTreeExecFile"] = { fg = "#c3e88d" }
hi["NvimTreeImageFile"] = { fg = "#89ddff" }

-- Neogit
hi["NeogitDiffContextHighlight"] = {}
hi["NeogitDiffAdd"] = { fg = "#9cb970", bg = "#232629" }
hi["NeogitDiffAddHighlight"] = { fg = "#c3e88d", bg = "#353c34" }
hi["NeogitDiffDelete"] = { fg = "#cc4259", bg = "#4f445f" }
hi["NeogitDiffDeleteHighlight"] = { fg = "#ff5370", bg = "#634661" }
hi["NeogitHunkHeader"] = { fg = "#12111e", bg = "#44324a" }
hi["NeogitHunkHeaderHighlight"] = { fg = "#12111e", bg = "#bb80b3" }
hi["NeogitstagedChanges"] = { fg = "#c3e88d", italic = true }
hi["NeogitUnstagedChanges"] = { fg = "#ff5370", italic = true }

-- GitSigns
hi["GitSignsAdd"] = { fg = "#c3e88d", bg = "#292d3e" }
hi["GitSignsAddNr"] = { fg = "#c3e88d", bg = "#292d3e" }
hi["GitSignsAddLn"] = { bg = "#353c34" }
hi["GitSignsChange"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["GitSignsChangeNr"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["GitSignsChangeLn"] = { bg = "#292d3e" }
hi["GitSignsChangeDelete"] = { fg = "#ff5370", bg = "#292d3e" }
hi["GitSignsChangeDeleteNr"] = { fg = "#ff5370", bg = "#292d3e" }
hi["GitSignsChangeDeleteLn"] = { bg = "#292d3e" }
hi["GitSignsDelete"] = { fg = "#ff5370", bg = "#292d3e" }
hi["GitSignsDeleteNr"] = { fg = "#ff5370", bg = "#292d3e" }
hi["GitSignsDeleteLn"] = { bg = "#634661" }
hi["GitSignsTopDelete"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["GitSignsTopDeleteNr"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["GitSignsTopDeleteLn"] = { bg = "#634661" }
hi["GitSignsUntracked"] = { fg = "#89ddff", bg = "#292d3e" }
hi["GitSignsUntrackedNr"] = { fg = "#89ddff", bg = "#292d3e" }
hi["GitSignsUntrackedLn"] = { bg = "#292d3e" }

-- Indent
hi["IndentBlanklineChar"] = { fg = "#292d3e" }
hi["IndentBlanklineSpaceChar"] = { fg = "#292d3e" }
hi["IndentBlanklineSpaceCharBlankline"] = { fg = "#292d3e" }
hi["IndentBlanklineContext"] = { fg = "#f78c6c" }

-- Illuminate
hi["IlluminatedWordText"] = { bg = "#292d3e" }
hi["IlluminatedWordRead"] = { bg = "#292d3e" }
hi["IlluminatedWordWrite"] = { bg = "#2e456b" }

-- Trouble
hi["TroubleCount"] = { fg = "#c792ea" }
hi["TroubleFoldIcon"] = { fg = "#ff5370" }
hi["TroubleIndent"] = { fg = "#ff5370" }
hi["TroubleLocation"] = { fg = "#3c435e" }

-- Cmp
hi["CmpNormal"] = { fg = "#eeffff", bg = "#12111e" }
hi["CmpBorder"] = { fg = "#12111e", bg = "#12111e" }
hi["CmpSelect"] = { bg = "#292d3e" }
hi["CmpDocumentation"] = { fg = "#eeffff", bg = "#292d3e" }
hi["CmpDocumentationBorder"] = {}
hi["CmpItemMenu"] = { fg = "#3c435e" }
hi["CmpItemAbbrDefault"] = { fg = "#eeffff" }
hi["CmpItemAbbrMatch"] = { fg = "#ff5370" }
hi["CmpItemAbbrMatchFuzzy"] = { fg = "#72a4ff" }
hi["CmpItemKindDefault"] = { fg = "#12111e", bg = "#eeffff" }
hi["CmpItemMenuDefault"] = { fg = "#3c435e" }
hi["CmpItemKindSnippet"] = { fg = "#12111e", bg = "#ff5370" }
hi["CmpItemKindModule"] = { fg = "#12111e", bg = "#f78c6c" }
hi["CmpItemKindKeyword"] = { fg = "#12111e", bg = "#72a4ff" }
hi["CmpItemKindFunction"] = { fg = "#12111e", bg = "#c792ea" }
hi["CmpItemKindMethod"] = { fg = "#12111e", bg = "#c792ea" }
hi["CmpItemKindEnum"] = { fg = "#12111e", bg = "#f78c6c" }
hi["CmpItemKindEnumMember"] = { fg = "#12111e", bg = "#72a4ff" }
hi["CmpItemKindStruct"] = { fg = "#12111e", bg = "#f78c6c" }
hi["CmpItemKindField"] = { fg = "#12111e", bg = "#ff5370" }
hi["CmpItemKindConstant"] = { fg = "#12111e", bg = "#89ddff" }
hi["CmpItemKindTypeParameter"] = { fg = "#12111e", bg = "#89ddff" }
hi["CmpItemKindVariable"] = { fg = "#12111e", bg = "#ffcb6b" }
hi["CmpItemKindValue"] = { fg = "#12111e", bg = "#ffcb6b" }
hi["CmpItemKindReference"] = { fg = "#12111e", bg = "#ffcb6b" }
hi["CmpItemKindInterface"] = { fg = "#12111e", bg = "#89ddff" }
hi["CmpItemKindClass"] = { fg = "#12111e", bg = "#c792ea" }
hi["CmpItemKindColor"] = { fg = "#12111e", bg = "#eeffff" }
hi["CmpItemKindFolder"] = { fg = "#12111e", bg = "#c792ea" }
hi["CmpItemKindFile"] = { fg = "#12111e", bg = "#72a4ff" }
hi["CmpItemKindText"] = { fg = "#12111e", bg = "#c3e88d" }

-- Luasnip
hi["LuasnipChoice"] = { fg = "#ff5370" }
hi["LuasnipInsert"] = { fg = "#c3e88d" }

-- ISwap
hi["ISwapSnipe"] = { fg = "#ff5370", bg = "#292d3e" }
hi["ISwapSelect"] = { bg = "#292d3e" }
hi["ISwapFlash"] = { fg = "#12111e", bg = "#c3e88d" }

-- Dap
hi["DapBreakpoint"] = { fg = "#ff5370", bg = "#292d3e" }
hi["DapBreakpointCondition"] = { fg = "#ff5370", bg = "#292d3e" }
hi["DapLogPoint"] = { fg = "#ff5370", bg = "#292d3e" }
hi["DapStopped"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["DapBreakpointRejected"] = { fg = "#f78c6c", bg = "#292d3e" }

-- Lspsaga
hi["LspSagaCodeActionBorder"] = { fg = "#292d3e", bg = "#292d3e" }
hi["LspSagaCodeActionContent"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["LspSagaCodeActionTitle"] = { fg = "#ff5370", bg = "#292d3e" }
hi["LspSagaCodeActionTruncateLine"] = { fg = "#292d3e", bg = "#292d3e" }

-- Glance
hi["GlanceIndent"] = { fg = "#f78c6c" }
hi["GlanceFoldIcon"] = { fg = "#c792ea" }
hi["GlanceListCount"] = { fg = "#f78c6c", italic = true }
hi["GlanceListCursorLine"] = { bg = "#292d3e" }
hi["GlanceListFileName"] = { fg = "#89ddff", italic = true }
hi["GlanceListFilePath"] = { fg = "#72a4ff", italic = true }
hi["GlanceListMatch"] = { fg = "#ff5370", bg = "#353c34" }
hi["GlancePreviewMatch"] = { bg = "#353c34" }
hi["GlanceWinBarTitle"] = { fg = "#c792ea", bg = "#292d3e" }
hi["GlanceWinBarFilename"] = { fg = "#89ddff", bg = "#292d3e", italic = true }
hi["GlanceWinBarFilepath"] = { fg = "#72a4ff", bg = "#292d3e", italic = true }

-- Lsp
hi["LspFloatWinBorder"] = { fg = "#292d3e", bg = "#292d3e" }
hi["LspFloatWinNormal"] = { fg = "#eeffff", bg = "#292d3e" }

hi["DiagnosticSignError"] = { fg = "#ff5370", bg = "#292d3e" }
hi["DiagnosticSignWarn"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["DiagnosticSignWarning"] = { fg = "#f78c6c", bg = "#292d3e" }
hi["DiagnosticSignHint"] = { fg = "#72a4ff", bg = "#292d3e" }
hi["DiagnosticSignInfo"] = { fg = "#c3e88d", bg = "#292d3e" }

hi["DiagnosticLineError"] = { bg = "#634661" }
hi["DiagnosticLineWarn"] = { bg = "#6b483a" }
hi["DiagnosticLineWarning"] = { bg = "#6b483a" }
hi["DiagnosticLineHint"] = { bg = "#2e456b" }
hi["DiagnosticLineInfo"] = { bg = "#353c34" }

hi["DiagnosticError"] = { fg = "#ff5370" }
hi["DiagnosticWarn"] = { fg = "#f78c6c" }
hi["DiagnosticWarning"] = { fg = "#f78c6c" }
hi["DiagnosticHint"] = { fg = "#72a4ff" }
hi["DiagnosticInfo"] = { fg = "#c3e88d" }

hi["DiagnosticUnderlineError"] = {}
hi["DiagnosticUnderlineWarn"] = {}
hi["DiagnosticUnderlineWarning"] = {}
hi["DiagnosticUnderlineHint"] = {}
hi["DiagnosticUnderlineInfo"] = {}

for group, colors in pairs(hi) do
    vim.api.nvim_set_hl(0, group, {
        fg = colors.fg,
        bg = colors.bg,
        sp = colors.sp,
        blend = colors.blend or 0,
        bold = colors.bold,
        italic = colors.italic,
        standout = colors.standout,
        underline = colors.underline,
        undercurl = colors.undercurl,
        underdouble = colors.underdouble,
        underdotted = colors.underdotted,
        underdashed = colors.underdashed,
        strikethrough = colors.strikethrough,
        reverse = colors.reverse,
    })
end
