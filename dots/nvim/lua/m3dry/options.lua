local opt = vim.opt
local g = vim.g

opt.splitbelow = true
opt.splitright = true
opt.path:append "**"
opt.wildmenu = false
opt.wildignore = "**/.git/*"
opt.incsearch = true
opt.hidden = true
opt.backup = false
opt.timeout = false
opt.swapfile = false
opt.number = true
opt.relativenumber = true
opt.clipboard = "unnamedplus"
opt.termguicolors = true
opt.guicursor =
    "n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait600-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175"
opt.autoindent = true
opt.cmdheight = 1
opt.showmode = false
opt.mouse = "nicra"
opt.fillchars = [[vert: ,vertleft: ,vertright: ,horiz: ,horizup: ,horizdown: ,verthoriz: ]]
opt.smartcase = true
opt.ignorecase = true
opt.errorbells = false
opt.expandtab = true
opt.smarttab = true
opt.smartindent = true
opt.tabstop = 4
opt.softtabstop = opt.tabstop:get()
opt.shiftwidth = opt.tabstop:get()
opt.breakindent = true
opt.breakindentopt = "shift:1,sbr"
opt.showbreak = "↳"
opt.cpoptions:append "n"
opt.undofile = true
opt.undodir = "/home/m3/.cache/nvim/undo"
opt.undolevels = 10000000
opt.undoreload = 10000000
opt.laststatus = 3
opt.scrolloff = 7
opt.signcolumn = "auto:1"
opt.completeopt = "menu,menuone,noselect"
opt.shortmess:append "c"
opt.updatetime = 50
opt.list = true
opt.listchars = [[eol:﬋,lead:·,trail:·,tab:  ,nbsp:␣,extends:»]]
opt.pumblend = 10
opt.winblend = opt.pumblend:get()
opt.joinspaces = false
opt.winminwidth = 0
opt.winminheight = 0
opt.conceallevel = 2
opt.foldlevel = 15
g.mapleader = " "
g.c_syntax_for_h = true
