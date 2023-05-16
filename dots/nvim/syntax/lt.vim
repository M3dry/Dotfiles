syn keyword listKeyword defun for lambda match if else elif let mut do and or band bor xor false true use struct enum as crate pub
syn keyword listType i8 i16 i32 i64 u8 u16 u32 u64 string char bool
syn match listOperator /[\(\.\.=\?\)=\(/=\)-+/%]/
syn match listNumber '\d\+'
syn match listNumber '[-+]\d\+'
syn match listParen /[()<>\[\]]/
syn match listVariable /[a-z_][a-zA-Z0-9_]*/ nextgroup=listArrow skipWhite
syn match listMacroUse /[a-z_][a-zA-Z0-9_]*!/
syn match listType /[A-Z][a-zA-Z0-9_]*/
syn match listArrow /\(->\)/
syn match listArrow /\(<-\)/
syn match listString /".*"/
syn match listChar /'.'/

let b:current_syntax = "cel"

hi def link listKeyword Keyword
hi def link listType Type
hi def link listMacroUse Macro
hi def link listOperator Operator
hi def link listNumber Number
hi def link listArrow Keyword
hi def link listString String
hi def link listChar Character
hi def link listVariable Identifier
hi listParen guifg=#72a4ff
