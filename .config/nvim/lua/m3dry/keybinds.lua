local M = {}
local map       = require('cartographer')
local nmap      = map.n
local vmap      = map.v
local xmap      = map.x
local imap      = map.i
local cmap      = map.c
local nnoremap  = nmap.nore
local nnoresmap = nnoremap.silent
local vnoremap  = vmap.nore
local vnoresmap = vnoremap.silent
local xnoremap  = xmap.nore
local xnoresmap = xnoremap.silent
local inoremap  = imap.nore
local inoresmap = inoremap.silent
local cnoremap  = cmap.nore
local cnoresmap = cnoremap.silent

-- Save & exit
    nnoresmap['zq']                = '<Cmd>qa<CR>'
    nnoresmap['zx']                = '<Cmd>qa!<CR>'
    nnoresmap['<Leader>s']         = '<Cmd>w<CR>'

-- Surround
    nmap[';s']                     = 'ysiw'

-- Easy align
    xmap['<Leader>a']              = '<Plug>(EasyAlign)'
    nmap['<Leader>a']              = '<Plug>(EasyAlign)'

-- Quickfix
    function M.quickfix ()
        local bind = nnoresmap.buffer

        vim.cmd[[QFToggle]]
        vim.o.number = false

        bind['j']                  = 'j<CR><C-w>p'
        bind['k']                  = 'k<CR><C-w>p'
        bind['q']                  = '<Cmd>QFToggle<CR>'
    end

    function M.loclist ()
        local bind = nnoresmap.buffer

        vim.cmd[[LLToggle]]
        vim.o.number = false

        bind['j']                  = 'j<CR><C-w>p'
        bind['k']                  = 'k<CR><C-w>p'
        bind['q']                  = '<Cmd>LLToggle<CR>'
    end

    nnoresmap[']q']                 = '<Cmd>QNext<CR>zz'
    nnoresmap['[q']                 = '<Cmd>QPrev<CR>zz'
    nnoresmap['<Leader>mq']         = '<Cmd>lua require(\'m3dry.keybinds\').quickfix()<CR>'
    nnoresmap[']l']                 = '<Cmd>LLNext<CR>zz'
    nnoresmap['[l']                 = '<Cmd>LLPrev<CR>zz'
    nnoresmap['<Leader>ml']         = '<Cmd>lua require(\'m3dry.keybinds\').loclist()<CR>'

-- Buffers
    nnoresmap['<Leader>bk']        = '<Cmd>Sayonara!<CR>'
    nnoresmap['<Leader>bd']        = '<Cmd>Sayonara<CR>'
    nnoresmap['<Leader>,']         = '<Cmd>JABSOpen<CR>'

-- Tabs
    nnoresmap['[t']                = '<Cmd>tabprevious<CR>'
    nnoresmap[']t']                = '<Cmd>tabnext<CR>'
    nnoresmap['[T']                = '<Cmd>tabmove -1<CR>'
    nnoresmap[']T']                = '<Cmd>tabmove +1<CR>'
    nnoresmap['gt']                = ''
    nnoresmap['gtk']               = '<Cmd>tabprevious<CR>'
    nnoresmap['gtj']               = '<Cmd>tabnext<CR>'
    nnoresmap['gtn']               = '<Cmd>tabnew<CR>'
    nnoresmap['gtc']               = '<Cmd>tabclose<CR>'
    nnoresmap['gtf']               = '<Cmd>tabfirst<CR>'
    nnoresmap['gtl']               = '<Cmd>tablast<CR>'
    nnoresmap['gtu']               = '<Cmd>tabrewind<CR>'

-- Splits
    nnoresmap['<Leader>wh']        = '<C-w>h'
    nnoresmap['<Leader>wj']        = '<C-w>j'
    nnoresmap['<Leader>wk']        = '<C-w>k'
    nnoresmap['<Leader>wl']        = '<C-w>l'
    nnoresmap['<Leader>ww']        = '<Cmd>call WindowSwap#EasyWindowSwap()<CR>'
    nnoresmap['<Leader>wy']        = '<Cmd>call WindowSwap#MarkWindowSwap()<CR>'
    nnoresmap['<Leader>wp']        = '<Cmd>call WindowSwap#DoWindowSwap()<CR>'
    nnoresmap['<C-l>']             = '<Cmd>vertical resize +2<CR>'
    nnoresmap['<C-h>']             = '<Cmd>vertical resize -2<CR>'
    nnoresmap['<C-k>']             = '<Cmd>resize +2<CR>'
    nnoresmap['<C-j>']             = '<Cmd>resize -2<CR>'
    nnoresmap['<Leader>wH']        = '<C-w>H'
    nnoresmap['<Leader>wJ']        = '<C-w>J'
    nnoresmap['<Leader>wK']        = '<C-w>K'
    nnoresmap['<Leader>wL']        = '<C-w>L'
    nnoresmap['<Leader>w<C-o>']    = '<C-w><C-o>'
    nnoresmap['<Leader>wc']        = '<C-w>c'
    nnoresmap['<Leader>wd']        = '<C-w>c'
    nnoresmap['<Leader>wv']        = '<C-w>v'
    nnoresmap['<Leader>ws']        = '<C-w>s'
    nnoresmap['<Leader>wr']        = '<C-w>R'
    nnoresmap['<Leader>wq']        = '<Cmd>q!<CR>'
    nnoresmap['<Leader>wi']        = '<Cmd>so %<CR>'
    nnoresmap['<Leader>wm']        = '<Cmd>MaximizerToggle<CR>'

-- ISwap
    nnoresmap['<Leader>ly']        = '<Cmd>ISwap<CR>'

-- Markdown
    nnoremap['<Leader>mp']         = '<Plug>MarkDownPreview'
    nnoremap['<Leader>ms']         = '<Plug>MarkDownPreviewStop'
    nnoremap['<Leader>mt']         = '<Plug>MarkDownPreviewToggle'

-- Telescope
    nnoresmap['<C-s>']             = '<Cmd>Telescope current_buffer_fuzzy_find<CR>'
    nnoresmap['<Leader>tk']        = '<Cmd>Telescope keymaps<CR>'
    nnoresmap['<Leader>tm']        = '<Cmd>Telescope marks<CR>'
    nnoresmap['<Leader>tu']        = '<Cmd>Telescope spell_suggest<CR>'
    nnoresmap['<Leader>tf']        = '<Cmd>Telescope fd<CR>'
    nnoresmap['<Leader>tb']        = '<Cmd>Telescope buffers<CR>'
    nnoresmap['<Leader>tz']        = '<Cmd>Telescope symbols<CR>'
    nnoresmap['<Leader>tq']        = '<Cmd>Telescope quickfix<CR>'
    nnoresmap['<Leader>tl']        = '<Cmd>Telescope loclist<CR>'
    nnoresmap['<Leader>th']        = '<Cmd>Telescope highlights<CR>'
    -- Project
    nnoresmap['<Leader>mf']        = '<Cmd>Telescope git_files<CR>'
    nnoresmap['<Leader>ms']        = '<Cmd>Telescope git_status<CR>'
    nnoresmap['<Leader>mb']        = '<Cmd>lua require(\'m3dry.telescope\').git_branches()<CR>'
    nnoresmap['<Leader>mc']        = '<Cmd>Telescope git_bcommits<CR>'
    nnoresmap['<Leader>mv']        = '<Cmd>Telescope git_commits<CR>'
    nnoresmap['<Leader>mG']        = '<Cmd>Telescope live_grep<CR>'
    nnoresmap['<Leader>mgg']       = '<Cmd>lua require(\'telescope.builtin\').grep_string { search = vim.fn.input("Search: ") }<CR>'
    nnoresmap['<Leader>mgw']       = '<Cmd>lua require(\'telescope.builtin\').grep_string { search = vim.fn.expand("<cword>") }<CR>'
    nnoresmap['<Leader>mgl']       = '<Cmd>lua require(\'telescope.builtin\').grep_string { search = vim.api.nvim_get_current_line() }<CR>'
    -- Lsp
    nnoresmap['<Leader>lr']        = '<Cmd>Telescope lsp_references<CR>'
    nnoresmap['<Leader>lT']        = '<Cmd>Telescope treesitter<CR>'
    nnoresmap['<Leader>lCc']       = '<Cmd>Telescope lsp_code_actions<CR>'
    nnoresmap['<Leader>lCr']       = '<Cmd>Telescope lsp_range_code_actions<CR>'
    nnoresmap['<Leader>lsd']       = '<Cmd>Telescope lsp_document_symbols<CR>'
    nnoresmap['<Leader>lsw']       = '<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>'
    nnoresmap['<Leader>led']       = '<Cmd>Telescope lsp_document_diagnostics<CR>'
    nnoresmap['<Leader>lew']       = '<Cmd>Telescope lsp_workspace_diagnostics<CR>'
    -- Dap
    nnoresmap['<Leader>dtc']       = '<Cmd>lua require(\'telescope\').extensions.dap.commands{}<CR>'
    nnoresmap['<Leader>dto']       = '<Cmd>lua require(\'telescope\').extensions.dap.configurations{}<CR>'
    nnoresmap['<Leader>dtb']       = '<Cmd>lua require(\'telescope\').extensions.dap.list_breakpoints{}<CR>'
    nnoresmap['<Leader>dtv']       = '<Cmd>lua require(\'telescope\').extensions.dap.variables{}<CR>'
    nnoresmap['<Leader>dtf']       = '<Cmd>lua require(\'telescope\').extensions.dap.frames{}<CR>'

-- Harpoon
    nnoresmap['<Leader>ha']        = '<Cmd>lua require(\'harpoon.mark\').add_file()<CR>'
    nnoresmap['<Leader>hc']        = '<Cmd>lua require(\'harpoon.mark\').clear_all()<CR>'
    nnoresmap['<Leader>hq']        = '<Cmd>lua require(\'harpoon.mark\').to_quickfix_list()<CR>'
    nnoresmap['<Leader>hd']        = '<Cmd>lua require(\'harpoon.mark\').rm_file(require(\'harpoon.mark\').get_current_index())<CR>'
    nnoresmap['<Leader>ht']        = '<Cmd>lua require(\'harpoon.ui\').toggle_quick_menu()<CR>'
    nnoresmap[']h']                = '<Cmd>lua require(\'harpoon.ui\').nav_next()<CR>'
    nnoresmap['[h']                = '<Cmd>lua require(\'harpoon.ui\').nav_prev()<CR>'
    nnoresmap['<Leader>hw']        = '<Cmd>lua require(\'harpoon.ui\').nav_file(3)<CR>'
    nnoresmap['<Leader>he']        = '<Cmd>lua require(\'harpoon.ui\').nav_file(2)<CR>'
    nnoresmap['<Leader>hr']        = '<Cmd>lua require(\'harpoon.ui\').nav_file(1)<CR>'
    nnoresmap['<Leader>hu']        = '<Cmd>lua require(\'harpoon.ui\').nav_file(4)<CR>'
    nnoresmap['<Leader>hi']        = '<Cmd>lua require(\'harpoon.ui\').nav_file(5)<CR>'
    nnoresmap['<Leader>ho']        = '<Cmd>lua require(\'harpoon.ui\').nav_file(6)<CR>'

-- Treesitter
    nnoresmap['<Leader>la']            = '<Cmd>TSHighlightCapturesUnderCursor<CR>'

-- Symbol Outline
    nnoresmap['<Leader>to']        = '<Cmd>SymbolsOutline<CR>'

-- Undo Tree
    nnoresmap['<Leader>tut']       = '<Cmd>UndotreeToggle<CR>'
    nnoresmap['<Leader>tuc']       = '<Cmd>UndotreeHide<CR>'
    nnoresmap['<Leader>tuf']       = '<Cmd>UndotreeFocus<CR>'

-- Hlslens
    nnoresmap['<Esc>']             = '<Cmd>noh<CR>'
    nnoresmap['n']                 = '<Cmd>execute(\'normal! \' . v:count1 . \'n\')<CR><Cmd>lua require(\'hlslens\').start()<CR>'
    nnoresmap['N']                 = '<Cmd>execute(\'normal! \' . v:count1 . \'N\')<CR><Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['*']                  = '*<Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['#']                  = '#<Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['g*']                 = 'g*<Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['g#']                 = 'g#<Cmd>lua require(\'hlslens\').start()<CR>'

-- Cosco
    function M.cosco()
        nmap['<Leader>;']          = '<Cmd>CommaOrSemiColon<CR>'
        imap['<C-l>']              = '<C-o><Cmd>CommaOrSemiColon<CR>'
    end

-- Smart buffers
    nnoresmap['<Leader>qq']        = '<Cmd>lua require(\'nvim-smartbufs\').close_current_buffer()<CR>'
    nnoresmap['<Leader>qa']        = '<Cmd>lua require(\'nvim-smartbufs\').close_all()<CR>'
    nnoresmap['[b']                = '<Cmd>lua require(\'nvim-smartbufs\').goto_next_buffer()<CR>'
    nnoresmap[']b']                = '<Cmd>lua require(\'nvim-smartbufs\').goto_prev_buffer()<CR>'

    for i = 1, 9 do
        nnoresmap[string.format("<Leader>%d", i)]  = string.format("<Cmd>lua require(\'nvim-smartbufs\').goto_buffer(%d)<CR>", i)
        nnoresmap[string.format("<Leader>q%d", i)] = string.format("<Cmd>lua require(\'nvim-smartbufs\').close_buffer(%d)<CR>", i)
    end

-- Doge
    nnoresmap['<Leader>l;']        = '<Cmd>DogeGenerate<CR>'

-- Sniprun
    nnoresmap['<Leader>fr']        = '<Cmd>SnipRun<CR>'
    nnoresmap['<Leader>fc']        = '<Cmd>SnipClose<CR>'
    nnoresmap['<Leader>fs']        = '<Cmd>SnipReset<CR>'
    vnoresmap['f']                 = '<Cmd>SnipRun<CR>'

-- NvimTree
    function M.TreeOpen()
        vim.cmd([[NvimTreeClose
NvimTreeOpen
wincmd p]])
    end

    nnoresmap['<Leader>fo']        = '<Cmd>lua require(\'m3dry.keybinds\').TreeOpen()<CR>'
    nnoresmap['<Leader>fc']        = '<Cmd>NvimTreeClose<CR>'
    nnoresmap['<Leader>ff']        = '<Cmd>NvimTreeFindFile<CR>'
    nnoremap['<Leader>.']          = ':e '

-- Lsp
    nnoresmap['<Leader>ld']        = '<Cmd>lua vim.lsp.buf.definition()<CR>'
    nnoresmap['<Leader>lD']        = '<Cmd>lua vim.lsp.buf.declaration()<CR>'
    nnoresmap['<Leader>ll']        = '<Cmd>lua vim.lsp.diagnostic.set_loclist({open_loclist = false})<CR>'
    nnoresmap['<Leader>lt']        = '<Cmd>lua vim.lsp.buf.type_definition()<CR>'
    nnoresmap['<Leader>ln']        = '<Cmd>lua vim.lsp.buf.rename()<CR>'
    -- LspSaga
    nnoresmap['<Leader>lp']        = '<Cmd>Lspsaga preview_definition<CR>'
    nnoresmap['<Leader>lh']        = '<Cmd>Lspsaga hover_doc<CR>'
    nnoresmap['<Leader>lc']        = '<Cmd>Lspsaga code_action<CR>'
    vnoresmap['<Leader>lc']        = '<Cmd>Lspsaga range_code_action<CR>'
    nnoresmap['[e']                = '<Cmd>Lspsaga diagnostic_jump_next<CR>'
    nnoresmap[']e']                = '<Cmd>Lspsaga diagnostic_jump_prev<CR>'
    nnoresmap['<Leader>lv']        = '<Cmd>Lspsaga show_cursor_diagnostics<CR>'
    nnoresmap['<Leader>lV']        = '<Cmd>Lspsaga show_line_diagnostics<CR>'
    nnoresmap['<Leader>li']        = '<Cmd>Lspsaga lsp_finder<CR>'
    nnoresmap['<Leader>lj']        = '<Cmd>Lspsaga signature_help<CR>'

-- Completion
    inoremap.c.expr['<C-j>']       = 'pumvisible() ? "\\<C-n>" : "\\<C-j>"'
    inoremap.c.expr['<C-k>']       = 'pumvisible() ? "\\<C-p>" : "\\<C-k>"'

-- Vsnip
    imap.s.expr['<Tab>']           = 'vsnip#jumpable(1)  ? \'<Plug>(vsnip-jump-next)\' : vsnip#expandable() ? \'<Plug>(vsnip-expand)\' : \'<Tab>\''
    imap.s.expr['<S-Tab>']         = 'vsnip#jumpable(-1) ? \'<Plug>(vsnip-jump-prev)\' : vsnip#expandable() ? \'<Plug>(vsnip-expand)\' : \'<S-Tab>\''

-- Treesitter
    nnoresmap['<Leader><Leader>u'] = '<Cmd>lua require(\'tsht\').nodes()<CR>'

-- Trouble
    nnoresmap['gle']               = '<Cmd>:TroubleToggle lsp_document_diagnostics<CR>'
    nnoresmap['glE']               = '<Cmd>:TroubleToggle lsp_workspace_diagnostics<CR>'
    nnoresmap['glw']               = '<Cmd>:TroubleToggle lsp_workspace_diagnostics<CR>'
    nnoresmap['glr']               = '<Cmd>:TroubleToggle lsp_references<CR>'
    nnoresmap['glc']               = '<Cmd>:TroubleClose<CR>'

-- Dap
    nnoresmap['<Leader>dc']        = '<Cmd>lua require(\'dap\').continue()<CR>'
    nnoresmap['<Leader>do']        = '<Cmd>lua require(\'dap\').step_over()<CR>'
    nnoresmap['<Leader>di']        = '<Cmd>lua require(\'dap\').step_into()<CR>'
    nnoresmap['<Leader>ds']        = '<Cmd>lua require(\'dap\').step_out()<CR>'
    nnoresmap['<Leader>dbt']       = '<Cmd>lua require(\'dap\').toggle_breakpoint()<CR>'
    nnoresmap['<Leader>dbc']       = '<Cmd>lua require(\'dap\').set_breakpoint(vim.fn.input("Breakpoint condition: "))<CR>'
    nnoresmap['<Leader>dbm']       = '<Cmd>lua require(\'dap\').set_breakpoint(nil, nil, vim.fn.input("Log point message: "))<CR>'
    nnoresmap['<Leader>dro']       = '<Cmd>lua require(\'dap\').repl.open()<CR>'
    nnoresmap['<Leader>drl']       = '<Cmd>lua require(\'dap\').repl.run_last()<CR>'

-- Git
    nnoresmap['<Leader>gg']        = '<Cmd>Neogit<CR>'
    nnoresmap['<Leader>gt']        = '<Cmd>ToggleBlameLine<CR>'
    nnoremap['<Leader>gd']         = ':DiffviewOpen '
    -- GitSigns
    nnoresmap['<Leader>gs']        = '<Cmd>lua require"gitsigns".stage_hunk()<CR>'
    vnoresmap['<Leader>gs']        = '<Cmd>lua require"gitsigns".stage_hunk({ vim.fn.line("."), vim.fn.line("v") })<CR>'
    nnoresmap['<Leader>gu']        = '<Cmd>lua require"gitsigns".undo_stage_hunk()<CR>'
    nnoresmap['<Leader>gr']        = '<Cmd>lua require"gitsigns".reset_hunk()<CR>'
    vnoresmap['<Leader>gr']        = '<Cmd>lua require"gitsigns".reset_hunk({ vim.fn.line("."), vim.fn.line("v") })<CR>'
    nnoresmap['<Leader>gR']        = '<Cmd>lua require"gitsigns".reset_buffer()<CR>'
    nnoresmap['<Leader>gp']        = '<Cmd>lua require"gitsigns".preview_hunk()<CR>'
    nnoresmap['<Leader>gl']        = '<Cmd>lua require"gitsigns".toggle_linehl()<CR>'
    nnoresmap['<Leader>gb']        = '<Cmd>lua require"gitsigns".blame_line()<CR>'
    nnoresmap.expr[']g']           = '&diff ? \']g\' : \'<cmd>lua require("gitsigns.actions").next_hunk()<CR>\''
    nnoresmap.expr['[g']           = '&diff ? \'[g\' : \'<cmd>lua require("gitsigns.actions").prev_hunk()<CR>\''

-- Parrot
    nnoresmap['<Leader>ti']        = '<Cmd>lua require(\'m3dry.parrot\').replace()<CR>'

-- Dial
    nmap['<C-a>']                  = '<Plug>(dial-increment)'
    nmap['<C-x>']                  = '<Plug>(dial-decrement)'
    vmap['<C-a>']                  = '<Plug>(dial-increment)'
    vmap['<C-x>']                  = '<Plug>(dial-decrement)'
    vmap['g<C-a>']                 = '<Plug>(dial-increment-additional)'
    vmap['g<C-x>']                 = '<Plug>(dial-decrement-additional)'

return M
