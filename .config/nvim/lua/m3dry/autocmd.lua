vim.cmd([[aug M3DRY
au!
au BufRead,BufNewFile xresources,xdefaults set filetype=xdefaults
au BufWritePost xresources !xrdb %
au BufWritePost DirMarks !shorts DirConf
au FileType c,cpp,lua lua require('m3dry.keybinds').cosco()
au FileType * call vsnip#get_complete_items(bufnr())
au TextYankPost * lua vim.highlight.on_yank { higroup="Yank", timeout=150, on_visual=true }
au BufWritePost plugins.lua source <afile> | PackerCompile
aug END]])
