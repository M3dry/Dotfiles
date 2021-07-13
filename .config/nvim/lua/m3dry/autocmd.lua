vim.cmd([[au BufRead,BufNewFile xresources,xdefaults set filetype=xdefaults
au BufWritePost xresources !xrdb %
au BufWritePost DirMarks !shorts DirConf
au FileType c,cpp lua require('m3dry.keybinds').cosco()
au FileType * call vsnip#get_complete_items(bufnr())]])
