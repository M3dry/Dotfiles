local hi = {}

hi['normal'] =                               { fg = "#eeffff" }
hi['Visual'] =                               { bg = "#4e5579" }
hi['Search'] =                               { bg = "#4e5579" }
hi['LineNr'] =                               { fg = "#eeffff", bg = "#111111" }
hi['VertSplit'] =                            { fg = "#72a4ff" }
hi['CursorLineNr'] =                         { fg = "#c792ea", bg = "#111111" }
hi['SignColumn'] =                           { fg = "#eeffff" }
hi['ColorColumn'] =                          { bg = "#300000" }
hi['Title'] =                                { fg = "#c792ea" }
hi['diffAdded'] =                            { fg = "#c3e88d", bg = "#353c34" }
hi['diffRemoved'] =                          { fg = "#ff5370", bg = "#634661" }
hi['DiffAdd'] =                              { fg = "#c3e88d", bg = "#353c34" }
hi['DiffDelete'] =                           { fg = "#ff5370", bg = "#634661" }
hi['StatusLine'] =                           { fg = "#eeffff", bg = "#292d3e" }
hi['StatusLineNC'] =                         { fg = "#eeffff", bg = "#292d3e" }
hi['NonText'] =                              { fg = "#3c435e" }
hi['Error'] =                                { fg = "#ff5370" }
hi['ErrorMsg'] =                             { fg = "#ff5370" }
hi['WarningMsg'] =                           { fg = "#f78c6c" }
hi['Directory'] =                            { fg = "#72a4ff" }
hi['Pmenu'] =                                { fg = "#eeffff", bg = "#12111e" }
hi['PmenuSel'] =                             { fg = "#12111e", bg = "#c792ea" }
hi['PmenuSbar'] =                            { bg = "#12111e" }
hi['PmenuThumb'] =                           { bg = "#c792ea" }
hi['Folded'] =                               { fg = "#72a4ff" }
hi['EndOfBuffer'] =                          { fg = "#292d3e" }
hi['MatchParen'] =                           { fg = "#f78c6c" }
hi['Comment'] =                              { fg = "#3c435e", style = "italic" }
hi['Constant'] =                             { fg = "#89ddff", style = "bold" }
hi['String'] =                               { fg = "#c3e88d" }
hi['Character'] =                            { fg = "#f78c6c", style = "bold" }
hi['Number'] =                               { fg = "#f78c6c", style = "bold" }
hi['Boolean'] =                              { fg = "#89ddff", style = "bold,italic" }
hi['Float'] =                                { fg = "#ffcb6b", style = "italic" }
hi['Identifier'] =                           { fg = "#ffcb6b" }
hi['Function'] =                             { fg = "#82aaff", style = "italic" }
hi['Statement'] =                            { fg = "#89ddff", style = "italic" }
hi['Conditional'] =                          { fg = "#89ddff", style = "bold,italic" }
hi['Repeat'] =                               { fg = "#89ddff", style = "bold,italic" }
hi['Label'] =                                { fg = "#89ddff", style = "italic" }
hi['Operator'] =                             { fg = "#72a4ff" }
hi['Keyword'] =                              { fg = "#89ddff", style = "bold,italic" }
hi['Exception'] =                            { fg = "#89ddff", style = "bold,italic" }
hi['PreProc'] =                              { fg = "#89ddff", style = "bold" }
hi['Include'] =                              { fg = "#72a4ff", style = "bold,italic" }
hi['Define'] =                               { fg = "#72a4ff", style = "bold,italic" }
hi['Macro'] =                                { fg = "#72a4ff", style = "bold" }
hi['PreCondit'] =                            { fg = "#72a4ff", style = "bold,italic" }
hi['Type'] =                                 { fg = "#c792ea" }
hi['StorageClass'] =                         { fg = "#89ddff", style = "italic" }
hi['Structure'] =                            { fg = "#89ddff", style = "italic" }
hi['Typedef'] =                              { fg = "#89ddff", style = "italic" }
hi['Special'] =                              { fg = "#c3e88d" }
hi['SpecialChar'] =                          { fg = "#c3e88d", style = "italic" }
hi['Delimeter'] =                            { fg = "#72a4ff" }
hi['IndentContext'] =                        { fg = "#c792ea" }

-- " Quick scope
hi['QuickScopePrimary'] =                    { fg = "#ff79c6", style = "bold,italic" }
hi['QuickScopeSecondary'] =                  { fg = "#ffffff", style = "bold,italic" }

-- " Hlslens
hi['HlSearchFloat'] =                        { fg = "#72a4ff" }
hi['HlSearchNear'] =                         { bg = "#4e5579" }
hi['HlSearchLens'] =                         { fg = "#ff5370", bg = "#12111e" }

-- " Tree sitter
hi['TSBoolean'] =                            { fg = "#89ddff", style = "italic" }
hi['TSCharacter'] =                          { fg = "#f78c6c" }
hi['TSComment'] =                            { fg = "#3c435e", style = "italic" }
hi['TSConditional'] =                        { fg = "#89ddff", style = "italic" }
hi['TSConstant'] =                           { fg = "#89ddff", style = "bold" }
hi['TSConstBuiltin'] =                       { fg = "#89ddff", style = "bold,italic" }
hi['TSConstMacro'] =                         { fg = "#72a4ff", style = "bold,italic" }
hi['TSConstructor'] =                        { fg = "#89ddff", style = "bold,italic" }
hi['TSFunction'] =                           { fg = "#82aaff" }
hi['TSFuncMacro'] =                          { fg = "#ffcb6b", style = "bold" }
hi['TSInclude'] =                            { fg = "#72a4ff", style = "bold,italic" }
hi['TSNumber'] =                             { fg = "#f78c6c", style = "bold" }
hi['TSOperator'] =                           { fg = "#72a4ff" }
hi['TSParameter'] =                          { fg = "#ffcb6b", style = "italic" }
hi['TSProperty'] =                           { fg = "#ff5370", style = "italic" }
hi['TSPunctBracket'] =                       { fg = "#72a4ff" }
hi['TSPunctDelimeter'] =                     { fg = "#72a4ff" }
hi['TSPunctDelimiter'] =                     { fg = "#72a4ff" }
hi['TSPunctSpecial'] =                       { fg = "#72a4ff" }
hi['TSString'] =                             { fg = "#c3e88d" }
hi['TSStringEscape'] =                       { fg = "#72a4ff", style = "italic" }
hi['TSStringRegex'] =                        { fg = "#ff5370", style = "italic" }
hi['TSType'] =                               { fg = "#c792ea" }
hi['TSTypeBuiltin'] =                        { fg = "#c792ea", style = "italic" }
hi['TSURI'] =                                { fg = "#ffcb6b", style = "underline" }
hi['TSVariable'] =                           { fg = "#ffcb6b" }
hi['TSVariableBuiltin'] =                    { fg = "#89ddff", style = "bold" }
hi['TSNamespace'] =                          { fg = "#c792ea", style = "italic" }
hi['TSKeyword'] =                            { fg = "#89ddff", style = "italic" }

-- " Tabs
hi['TabLine'] =                              { fg = "#eeffff", bg = "#111111" }
hi['TabLineSel'] =                           { fg = "#ffcb6b", bg = "#111111", style = "italic" }
hi['TabLineIndex'] =                         { fg = "#eeffff", bg = "#111111" }
hi['TabLineSelIndex'] =                      { fg = "#ffcb6b", bg = "#111111" }
hi['TabLineNumSep'] =                        { fg = "#eeffff", bg = "#111111" }
hi['TabLineSelNumSep'] =                     { fg = "#ffcb6b", bg = "#111111" }
hi['TabLineBufNum'] =                        { fg = "#eeffff", bg = "#111111" }
hi['TabLineSelBufNum'] =                     { fg = "#ffcb6b", bg = "#111111" }
hi['TabLineSep'] =                           { fg = "#ff5370", bg = "#111111" }
hi['TabLineMod'] =                           { fg = "#ff5370", bg = "#111111" }
hi['TabLineSelMod'] =                        { fg = "#ff5370", bg = "#111111" }
hi['TabLineFill'] =                          { fg = "#eeffff", bg = "#111111" }

-- " Nvim Tree
hi['NvimTreeFolderName'] =                   { fg = "#c792ea" }
hi['NvimTreeFolderIcon'] =                   { fg = "#c792ea" }
hi['NvimTreeEmptyFolderName'] =              { fg = "#c792ea" }
hi['NvimTreeOpenedFolderName'] =             { fg = "#c792ea" }
hi['NvimTreeNormal'] =                       { fg = "#eeffff" }
hi['NvimTreeSymlink'] =                      { fg = "#72a4ff" }
hi['NvimTreeRootFolder'] =                   { fg = "#ffcb6b", style = "italic" }
hi['NvimTreeSpecialFile'] =                  { fg = "#ff5370", style = "italic" }
hi['NvimTreeExecFile'] =                     { fg = "#c3e88d" }
hi['NvimTreeImageFile'] =                    { fg = "#89ddff" }

-- " Neogit
hi['NeogitDiffContextHighlight'] =           {}
hi['NeogitDiffAdd'] =                        { fg = "#9cb970", bg = "#232629" }
hi['NeogitDiffAddHighlight'] =               { fg = "#c3e88d", bg = "#353c34" }
hi['NeogitDiffDelete'] =                     { fg = "#cc4259", bg = "#4f445f" }
hi['NeogitDiffDeleteHighlight'] =            { fg = "#ff5370", bg = "#634661" }
hi['NeogitHunkHeader'] =                     { fg = "#12111e", bg = "#44324a" }
hi['NeogitHunkHeaderHighlight'] =            { fg = "#12111e", bg = "#bb80b3" }
hi['NeogitstagedChanges'] =                  { fg = "#c3e88d", style = "italic" }
hi['NeogitUnstagedChanges'] =                { fg = "#ff5370", style = "italic" }

-- " Indent
hi['IndentBlanklineChar'] =                  { fg = "#292d3e" }
hi['IndentBlanklineSpaceChar'] =             { fg = "#292d3e" }
hi['IndentBlanklineSpaceCharBlankline'] =    { fg = "#292d3e" }

-- " Lsp
hi['LspDiagnosticsDefaultError'] =           { fg = "#ff5370" }
hi['LspDiagnosticsDefaultWarning'] =         { fg = "#f78c6c" }
hi['LspDiagnosticsDefaultHint'] =            { fg = "#72a4ff" }
hi['LspDiagnosticsDefaultInformation'] =     { fg = "#c3e88d" }
hi['LspDiagnosticsFloatingError'] =          { fg = "#ff5370" }
hi['LspDiagnosticsFloatingWarning'] =        { fg = "#f78c6c" }
hi['LspDiagnosticsFloatingHint'] =           { fg = "#72a4ff" }
hi['LspDiagnosticsFloatingInformation'] =    { fg = "#c3e88d" }
hi['LspDiagnosticsSignError'] =              { fg = "#ff5370" }
hi['LspDiagnosticsSignWarning'] =            { fg = "#f78c6c" }
hi['LspDiagnosticsSignHint'] =               { fg = "#72a4ff" }
hi['LspDiagnosticsSignInformation'] =        { fg = "#c3e88d" }
hi['LspDiagnosticsUnderlineError'] =         { fg = "#ff5370", style = "undercurl" }
hi['LspDiagnosticsUnderlineWarning'] =       { fg = "#f78c6c", style = "undercurl" }
hi['LspDiagnosticsUnderlineHint'] =          { fg = "#72a4ff", style = "undercurl" }
hi['LspDiagnosticsUnderlineInformation'] =   { fg = "#c3e88d", style = "undercurl" }
hi['LspDiagnosticsVirtualTextError'] =       { fg = "#ff5370" }
hi['LspDiagnosticsVirtualTextWarning'] =     { fg = "#f78c6c" }
hi['LspDiagnosticsVirtualTextHint'] =        { fg = "#72a4ff" }
hi['LspDiagnosticsVirtualTextInformation'] = { fg = "#c3e88d" }

for group, colors in pairs(hi) do
	local fg = colors.fg or 'none'
	local bg = colors.bg or 'none'
	local guisp = colors.guisp or 'none'
	local style = colors.style or 'none'

    vim.cmd(string.format('hi %s guifg=%s guibg=%s guisp=%s gui=%s',
        group, fg, bg, guisp, style))
end
