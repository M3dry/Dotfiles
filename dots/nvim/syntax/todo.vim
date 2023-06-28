syn match todoHeader /# .*\n/
syn match todoBullet /^- /
syn match todoParen /\[\([a-zA-Z0-9_\-^ ]*\)\]/ contains=todoTodoState
syn match todoTodoState /[a-zA-Z0-9_\-^ ]/ contained

hi def link todoHeader Title
hi def link todoBullet Keyword
hi def link todoTodoState Character
hi todoParen guifg=#72a4ff
