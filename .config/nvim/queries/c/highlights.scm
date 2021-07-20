[
  "const"
  "default"
  "enum"
  "extern"
  "inline"
  "sizeof"
  "static"
  "struct"
  "typedef"
  "union"
  "volatile"
  "goto"
  "register"
  "unsigned"
] @keyword

"return" @keyword.return

[
  "while"
  "for"
  "do"
  "continue"
  "break"
] @repeat

[
 "if"
 "else"
 "case"
 "switch"
] @conditional

"#define" @constant.macro

[
  "#if"
  "#ifdef"
  "#ifndef"
  "#else"
  "#elif"
  "#endif"
  (preproc_directive)
] @keyword.constant

"#include" @include

[
  "=" "-" "*" "/"
  "+" "%" "~" "|"
  "&" "^" "<<" ">>"
  "->" "<" "<=" ">="
  ">" "==" "!=" "!"
  "&&" "||" "-=" "+="
  "*=" "/=" "%=" "|="
  "&=" "^=" ">>=" "<<="
  "--" "++"
] @operator

[
 (true)
 (false)
] @boolean

[ "." ";" ":" "," ] @punctuation.delimiter

"..." @punctuation.special

(conditional_expression [ "?" ":" ] @conditional)


[ "(" ")" "[" "]" "{" "}"] @punctuation.bracket

(string_literal) @string
(system_lib_string) @string.lib
(escape_sequence) @string.escape

(null) @constant.builtin
(number_literal) @number
(char_literal) @character

(identifier) @variable

(array_declarator
  declarator: (identifier) @variable.array)

(array_declarator
  declarator: (pointer_declarator) @variable.array)

(pointer_declarator
  declarator: (identifier) @variable.pointer)

(pointer_declarator
  declarator: (pointer_declarator) @variable.pointer)

(call_expression
 function: (identifier) @function.call)

(function_declarator
 declarator: [(identifier) @function
              (parenthesized_declarator
               (pointer_declarator (field_identifier) @function))])

(preproc_function_def
  name: (identifier) @function.macro)


[
 (preproc_arg)
 (preproc_defined)
]  @function.macro


(((field_expression
     (field_identifier) @property)) @_parent
 (#not-has-parent? @_parent template_method function_declarator call_expression))

(((field_identifier) @property)
 (#not-has-ancestor? @property field_declaration)
 (#not-has-ancestor? @property function_declarator))

(field_declaration
  (_) @property.declaration)

(enumerator name: (identifier) @enum)

(statement_identifier) @label

[
(type_identifier)
(primitive_type)
(sized_type_specifier)
(type_descriptor)
 ] @type

(declaration (type_qualifier) @type)
(cast_expression type: (type_descriptor) @type)
(sizeof_expression value: (parenthesized_expression (identifier) @type))

[(primitive_type)
 (sized_type_specifier)] @type.builtin

((identifier) @constant
 (.match? @constant "^[A-Z_][A-Z_\\d]*$"))

;; Preproc def / undef
(preproc_def
  name: (_) @constant)
(preproc_call
  directive: (preproc_directive) @_u
  argument: (_) @constant
  (#eq? @_u "#undef"))


(comment) @comment

;; Parameters
(parameter_declaration
  declarator: (identifier) @parameter)

(parameter_declaration
  declarator: (pointer_declarator) @parameter)

(parameter_declaration
  type: (type_identifier) @parameter.type)

(preproc_params (_) @parameter)

(ERROR) @error
