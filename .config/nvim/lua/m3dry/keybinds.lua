local M = {}
local map       = require('cartographer')
local nmap      = map.n
local vmap      = map.v
local imap      = map.i
local cmap      = map.c
local nnoremap  = map.n.nore
local nnoresmap = nnoremap.silent
local vnoremap  = map.v.nore
local vnoresmap = vnoremap.silent
local inoremap  = map.i.nore
local inoresmap = inoremap.silent
local cnoremap  = map.c.nore
local cnoresmap = cnoremap.silent

nmap[';s']                         = 'ysiw'
nnoremap['[q']                     = '<Cmd>cnext<CR>zz'
nnoremap[']q']                     = '<Cmd>cprev<CR>zz'
nnoresmap['zq']                    = '<Cmd>qa<CR>'
nnoresmap['zx']                    = '<Cmd>qa!<CR>'
nnoresmap['<Leader>,']             = '<Cmd>JABSOpen<CR>'
nnoresmap['<Leader>s']             = '<Cmd>w<CR>'
nnoresmap['<Esc>']                 = '<Cmd>noh<CR>'
nnoresmap['<Leader>wh']            = '<C-w>h'
nnoresmap['<Leader>wj']            = '<C-w>j'
nnoresmap['<Leader>wk']            = '<C-w>k'
nnoresmap['<Leader>wl']            = '<C-w>l'
nnoresmap['<Leader>wH']            = '<C-w>H'
nnoresmap['<Leader>wJ']            = '<C-w>J'
nnoresmap['<Leader>wK']            = '<C-w>K'
nnoresmap['<Leader>wL']            = '<C-w>L'
nnoresmap['<Leader>w<C-o>']        = '<C-w><C-o>'
nnoresmap['<Leader>wc']            = '<C-w>c'
nnoresmap['<Leader>wd']            = '<C-w>c'
nnoresmap['<Leader>wv']            = '<C-w>v'
nnoresmap['<Leader>ws']            = '<C-w>s'
nnoresmap['<Leader>wr']            = '<C-w>R'
nnoresmap['<Leader>wq']            = '<Cmd>q!<CR>'
nnoresmap['<Leader>wi']            = '<Cmd>so %<CR>'
nnoresmap['<Leader>wm']            = '<Cmd>MaximizerToggle<CR>'
nnoresmap['<Leader>ww']            = '<Cmd>ChooseWin<CR>'
nnoresmap[']w']                    = '<Cmd>vertical resize +5<CR>'
nnoresmap['[w']                    = '<Cmd>vertical resize -5<CR>'
nnoresmap[']W']                    = '<Cmd>resize +5<CR>'
nnoresmap['[W']                    = '<Cmd>resize -5<CR>'
nnoresmap['<Leader>bk']            = '<Cmd>lua require(\'bufdelete\').bufdelete(0, true)<CR>'
nnoresmap['<Leader>bd']            = '<Cmd>bd<CR>'
nnoresmap['<Leader>to']            = '<Cmd>SymbolsOutline<CR>'
nnoresmap['<Leader>tut']           = '<Cmd>UndotreeToggle<CR>'
nnoresmap['<Leader>tuc']           = '<Cmd>UndotreeHide<CR>'
nnoresmap['<Leader>tuf']           = '<Cmd>UndotreeFocus<CR>'
nnoresmap['[t']                    = '<Cmd>tabprevious<CR>'
nnoresmap[']t']                    = '<Cmd>tabnext<CR>'
nnoresmap['[T']                    = '<Cmd>tabmove -1<CR>'
nnoresmap[']T']                    = '<Cmd>tabmove +1<CR>'
nnoresmap['gtk']                   = '<Cmd>tabprevious<CR>'
nnoresmap['gtj']                   = '<Cmd>tabnext<CR>'
nnoresmap['gtn']                   = '<Cmd>tabnew<CR>'
nnoresmap['gtc']                   = '<Cmd>tabclose<CR>'
nnoresmap['gtf']                   = '<Cmd>tabfirst<CR>'
nnoresmap['gtl']                   = '<Cmd>tablast<CR>'
nnoresmap['gtu']                   = '<Cmd>tabrewind<CR>'

-- Easymotion
    map['<Leader><Leader>r']       = '<Plug>(easymotion-jumptoanywhere)'
    map['<Leader><Leader>h']       = '<Plug>(easymotion-linebackward)'
    map['<Leader><Leader>l']       = '<Plug>(easymotion-lineforward)'
    map['<Leader><Leader>a']       = '<Plug>(easymotion-lineanywhere)'
    map['<Leader><Leader>A']       = '<Plug>(easymotion-bd-jk)'
    map['<Leader><Leader>f']       = '<Plug>(easymotion-bd-f)'
    map['<Leader><Leader>w']       = '<Plug>(easymotion-bd-w)'
    nmap['<Leader><Leader>w']      = '<Plug>(easymotion-overwin-w)'
    nmap['<Leader><Leader>L']      = '<Plug>(easymotion-overwin-f)'
    nmap['s']                      = '<Plug>(easymotion-overwin-f2)'

nnoresmap['<Leader>ly']            = '<Cmd>ISwap<CR>'

-- Markdown
    nnoremap['<Leader>mp']         = '<Plug>MarkDownPreview'
    nnoremap['<Leader>ms']         = '<Plug>MarkDownPreviewStop'
    nnoremap['<Leader>mt']         = '<Plug>MarkDownPreviewToggle'

-- Telescope
    nnoresmap['<C-s>']             = '<Cmd>Telescope current_buffer_fuzzy_find<CR>'
    nnoresmap['<Leader>.']         = '<Cmd>Telescope file_browser<CR>'
    nnoresmap['<Leader>lf']        = '<Cmd>Telescope fd<CR>'
    nnoresmap['<Leader>ll']        = '<Cmd>Telescope live_grep<CR>'
    nnoresmap['<Leader>lg']        = '<Cmd>Telescope grep_string<CR>'
    nnoresmap['<Leader>lm']        = '<Cmd>Telescope marks<CR>'
    nnoresmap['<Leader>lk']        = '<Cmd>Telescope keymaps<CR>'
    nnoresmap['<Leader>lu']        = '<Cmd>Telescope spell_suggest<CR>'
    nnoresmap['<Leader>lb']        = '<Cmd>Telescope buffers<CR>'
    nnoresmap['<Leader>lz']        = '<Cmd>Telescope symbols<CR>'
    -- Git
    nnoresmap['<Leader>pf']        = '<Cmd>Telescope git_files<CR>'
    nnoresmap['<Leader>ps']        = '<Cmd>Telescope git_status<CR>'
    nnoresmap['<Leader>pb']        = '<Cmd>lua require(\'m3dry.telescope\').git_branches()<CR>'
    nnoresmap['<Leader>pc']        = '<Cmd>Telescope git_bcommits<CR>'
    nnoresmap['<Leader>pv']        = '<Cmd>Telescope git_commits<CR>'
    -- Lsp
    nnoresmap['<Leader>lr']        = '<Cmd>Telescope lsp_references<CR>'
    nnoresmap['<Leader>ld']        = '<Cmd>Telescope lsp_definitions<CR>'
    nnoresmap['<Leader>lt']        = '<Cmd>Telescope treesitter<CR>'
    nnoresmap['<Leader>ls']        = '<Cmd>Telescope lsp_document_symbols<CR>'
    nnoresmap['<Leader>lS']        = '<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>'
    nnoresmap['<Leader>le']        = '<Cmd>Telescope lsp_document_diagnostics<CR>'
    nnoresmap['<Leader>lE']        = '<Cmd>Telescope lsp_workspace_diagnostics<CR>'
    -- Dap
    nnoresmap['<Leader>dtc']       = '<Cmd>lua require(\'telescope\').extensions.dap.commands{}<CR>'
    nnoresmap['<Leader>dto']       = '<Cmd>lua require(\'telescope\').extensions.dap.configurations{}<CR>'
    nnoresmap['<Leader>dtb']       = '<Cmd>lua require(\'telescope\').extensions.dap.list_breakpoints{}<CR>'
    nnoresmap['<Leader>dtv']       = '<Cmd>lua require(\'telescope\').extensions.dap.variables{}<CR>'
    nnoresmap['<Leader>dtf']       = '<Cmd>lua require(\'telescope\').extensions.dap.frames{}<CR>'

nnoresmap['<Leader>la']            = '<Cmd>TSHighlightCapturesUnderCursor<CR>'

-- Neuron
    nnoresmap['<Leader>nn']        = '<Cmd>lua require(\'neuron.cmd\').new_edit(require(\'neuron\').config.neuron_dir)<CR>'
    nnoresmap['<Leader>nf']        = '<Cmd>lua require(\'neuron.telescope\').find_zettels()<CR>'
    nnoresmap['<Leader>ni']        = '<Cmd>lua require(\'neuron.telescope\').find_zettels{insert = true}<CR>'
    nnoresmap['<Leader>ns']        = '<Cmd>lua require(\'neuron\').rib{address = "127.0.0.1:8200", verbose = true}<CR>'
    
    function M.neuronbinds()
        local bind = nnoresmap.buffer

        bind['<CR>']               = '<Cmd>lua require(\'neuron\').enter_link()<CR>'
        bind['<Leader>nb']         = '<Cmd>lua require(\'neuron.telescope\').find_backlinks()<CR>'
        bind['<Leader>nB']         = '<Cmd>lua require(\'neuron.telescope\').find_backlinks{insert = true}<CR>'
        bind['<Leader>nt']         = '<Cmd>lua require(\'neuron.telescope\').find_tags()<CR>'
        bind['gn]']                = '<Cmd>lua require(\'neuron\').goto_next_extmark()<CR>'
        bind['gn[']                = '<Cmd>lua require(\'neuron\').goto_prev_extmark()<CR>'
    end
    
    vim.cmd(string.format("au BufRead %s/*.md lua require('m3dry.keybinds').neuronbinds()", require('neuron').config.neuron_dir))

-- Hlslens
    nnoresmap['n']                 = '<Cmd>execute(\'normal! \' . v:count1 . \'n\')<CR><Cmd>lua require(\'hlslens\').start()<CR>'
    nnoresmap['N']                 = '<Cmd>execute(\'normal! \' . v:count1 . \'N\')<CR><Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['*']                  = '*<Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['#']                  = '#<Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['g*']                 = 'g*<Cmd>lua require(\'hlslens\').start()<CR>'
    nnoremap['g#']                 = 'g#<Cmd>lua require(\'hlslens\').start()<CR>'

-- Cosco
    function M.cosco()
        nmap['<Leader>;']         = '<Plug>(cosco-commaOrSemiColon)'
        imap['<C-l>']             = '<C-o><Plug>(cosco-commaOrSemiColon)'
    end

    vim.cmd("au FileType c,cpp lua require('m3dry.keybinds').cosco()")

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
        vim.cmd("NvimTreeClose")
        vim.cmd("NvimTreeOpen")
        vim.cmd("wincmd p")
    end

    nnoresmap['<Leader>fo']        = '<Cmd>lua require(\'m3dry.keybinds\').TreeOpen()<CR>'
    nnoresmap['<Leader>fc']        = '<Cmd>NvimTreeClose<CR>'
    nnoresmap['<Leader>ff']        = '<Cmd>NvimTreeFindFile<CR>'

-- LspSaga
    nnoresmap['<Leader>ln']        = '<Cmd>Lspsaga rename<CR>'
    nnoresmap['<Leader>lp']        = '<Cmd>Lspsaga preview_definition<CR>'
    nnoresmap['<Leader>lh']        = '<Cmd>Lspsaga hover_doc<CR>'
    nnoresmap['<Leader>lc']        = '<Cmd>Lspsaga code_action<CR>'
    nnoresmap['<Leader>lC']        = '<Cmd>Lspsaga range_code_action<CR>'
    nnoresmap['[e']                = '<Cmd>Lspsaga diagnostic_jump_next<CR>'
    nnoresmap[']e']                = '<Cmd>Lspsaga diagnostic_jump_prev<CR>'
    nnoresmap['<Leader>lv']        = '<Cmd>Lspsaga show_cursor_diagnostics<CR>'
    nnoresmap['<Leader>lV']        = '<Cmd>Lspsaga show_line_diagnostics<CR>'
    nnoresmap['<Leader>li']        = '<Cmd>Lspsaga lsp_finder<CR>'
    nnoresmap['<Leader>lj']        = '<Cmd>Lspsaga signature_help<CR>'
    nnoresmap['<C-k>']             = '<Cmd>lua require(\'lspsaga.action\').smart_scroll_with_saga(1)<CR>'
    nnoresmap['<C-j>']             = '<Cmd>lua require(\'lspsaga.action\').smart_scroll_with_saga(-1)<CR>'

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
    nnoresmap['<Leader>gb']        = '<Cmd>ToggleBlameLine<CR>'
    nnoremap['<Leader>gd']         = ':DiffviewOpen '

return M
