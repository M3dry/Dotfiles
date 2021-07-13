require('m3dry.options')
require('m3dry.plugins')
require('m3dry.autocmd')
require('m3dry.colors')
require('m3dry.tabline').setup {
    show_index = true,
    separator = ':',
    show_bufnum = true,
    show_modify = true,
    save_icon = ' ',
    no_name = '[--x--]'
}
