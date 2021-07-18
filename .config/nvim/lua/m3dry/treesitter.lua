require('nvim-treesitter.configs').setup {
    highlight = {
        enable = true,
        custom_captures = {
            ["variable.array"] = "TSVariableArray",
            ["variable.pointer"] = "TSVariablePointer",
            ["function.call"] = "TSFunctionCall",
            ["keyword.constant"] = "TSKeywordConstant",
        },
    },
    indent = {
        enable = false
    },
    playground = {
        enable = true,
        disable = {},
        updatetime = 25,
        persist_queries = false,
        keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
        },
    },
    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = {"BufWrite", "CursorHold"},
    },
    rainbow = {
        enable = true,
        extended_mode = false,
        max_file_lines = 20000,
        colors = {
            "#c792ea",
            "#72a4ff",
            "#89ddff",
            "#c3e88d",
            "#ffcb6b",
            "#f78c6c",
            "#ff5370",
        }
    },
    autopairs = {
        enable = true
    },
    refactor = {
        navigation = {
            enable = true,
            keymaps = {
                goto_next_usage = "]u",
                goto_previous_usage = "[u",
            },
        }
    },
    context_commentstring = {
        enable = true,
        config = {
          c = '/*%s*/',
          cpp = '/*%s*/',
          lua = '--%s'
        }
    },
    textsubjects = {
        enable = true,
        keymaps = {
            ['.'] = 'textsubjects-smart',
            [';'] = 'textsubjects-container-outer',
        }
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ip"] = "@parameter.inner",
                ["ap"] = "@parameter.outer",
                ["ic"] = "@call.inner",
                ["ac"] = "@call.outer",
                ["it"] = "@conditional.inner",
                ["at"] = "@conditional.outer",
                ["il"] = "@loop.inner",
                ["al"] = "@loop.outer",
                ["oc"] = "@comment.outer",
                ["as"] = "@statement.outer",
                ["is"] = "@statement.outer",
            }
        },
        swap = {
            enable = true,
            swap_next = {
                ["]m"] = "@parameter.inner",
            },
            swap_previous = {
                ["[m"] = "@parameter.inner",
            },
        },
        move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
                ["]f"] = "@function.outer",
                ["]p"] = "@parameter.inner",
                ["]c"] = "@call.outer",
                ["]o"] = "@conditional.outer",
                ["]l"] = "@loop.outer",
                ["]s"] = "@comment.outer",
                ["<C-j>"] = "@statement.outer",
            },
            goto_next_end = {
                ["]F"] = "@function.outer",
                ["]P"] = "@parameter.inner",
                ["]C"] = "@call.outer",
                ["]O"] = "@conditional.outer",
                ["]L"] = "@loop.outer",
                ["]S"] = "@comment.outer",
                ["<C-l>"] = "@statement.outer",
            },
            goto_previous_start = {
                ["[f"] = "@function.outer",
                ["[p"] = "@parameter.inner",
                ["[c"] = "@call.outer",
                ["[o"] = "@conditional.outer",
                ["[l"] = "@loop.outer",
                ["[s"] = "@comment.outer",
                ["<C-k>"] = "@statement.outer",
            },
            goto_previous_end = {
                ["[F"] = "@function.outer",
                ["[P"] = "@parameter.inner",
                ["[C"] = "@call.outer",
                ["[O"] = "@conditional.outer",
                ["[L"] = "@loop.outer",
                ["[S"] = "@comment.outer",
                ["<C-h>"] = "@statement.outer",
            },
        }
    }
}

if require("nvim-treesitter.parsers").has_parser("c") then
    local highlight_query =[[
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
    (system_lib_string) @string
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
    
    ; (call_expression
    ;   function: (identifier) @function)
    ; (call_expression
    ;   function: (field_expression
    ;     field: (field_identifier) @function))
    ; (function_declarator
    ;   declarator: (identifier) @function)
    (((field_expression
         (field_identifier) @property)) @_parent
     (#not-has-parent? @_parent template_method function_declarator call_expression))
    
    (((field_identifier) @property)
     (#has-ancestor? @property field_declaration)
     (#not-has-ancestor? @property function_declarator))
    
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
    
    (preproc_params
      (identifier)) @parameter
    
    (ERROR) @error
    ]]
    require("vim.treesitter.query").set_query("c", "highlights", highlight_query)
end
