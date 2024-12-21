local opt = vim.opt
local g = vim.g

g.mapleader = " "

opt.splitbelow = true
opt.splitright = true

opt.wildmenu = false
opt.incsearch = true
opt.smartcase = true
opt.ignorecase = true

opt.undofile = true
opt.undodir = "/home/m3/.cache/nvim/undo"
opt.undolevels = 10000000
opt.undoreload = 10000000
opt.hidden = true
opt.backup = false
opt.swapfile = false
opt.updatetime = 50

opt.number = true
opt.relativenumber = true

opt.clipboard = "unnamedplus"

opt.termguicolors = true
opt.guicursor =
    "n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait600-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175"
opt.mouse = "nicra"
opt.fillchars = [[vert: ,vertleft: ,vertright: ,horiz: ,horizup: ,horizdown: ,verthoriz: ]]
opt.showbreak = "↳"
opt.cpoptions:append "n"
opt.list = true
opt.listchars = [[eol:﬋,lead:·,trail:·,tab:  ,nbsp:␣,extends:»]]
opt.signcolumn = "auto:1"
opt.cmdheight = 1

opt.laststatus = 2

opt.scrolloff = 7

opt.autoindent = true
opt.expandtab = true
opt.smarttab = true
opt.smartindent = true
opt.tabstop = 4
opt.softtabstop = opt.tabstop:get()
opt.shiftwidth = opt.tabstop:get()
opt.breakindent = true
opt.breakindentopt = "shift:1,sbr"
opt.joinspaces = false

opt.completeopt = "menu,menuone,noselect"
opt.shortmess:append "c"

g.c_syntax_for_h = true
