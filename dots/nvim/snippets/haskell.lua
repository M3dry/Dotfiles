local ls = require "luasnip"
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local c = ls.choice_node
local d = ls.dynamic_node
local fn = ls.function_node
local r = ls.restore_node
local fmt = require("luasnip.extras.fmt").fmt
local k = require("luasnip.nodes.key_indexer").new_key

local function indent_str()
    local indent = vim.bo.shiftwidth
    if indent == 0 then
        indent = vim.bo.tabstop
    end
    return string.rep(" ", indent)
end

---@param mk_node function
---@param extra_indent number
local function _indent_newline(mk_node, extra_indent, _, parent)
    extra_indent = extra_indent == nil and 0 or extra_indent
    local pos = parent:get_buf_position()
    local indent_count = pos[2]
    return mk_node(string.rep(" ", indent_count) .. (string.rep(indent_str(), extra_indent)))
end

local function indent_newline_text(idx, txt, extra_indent)
    local function mk_node(istr)
        return isn(nil, { t { "", istr .. txt } }, "")
    end
    return d(idx, function(...)
        return _indent_newline(mk_node, extra_indent, ...)
    end)
end

local function top_level(line_to_cursor, trigger, _)
    return #line_to_cursor - #trigger == 0
end

local function let(idx)
    return d(idx, function()
        return sn(1, {
            c(1, {
                t "in ",
                sn(nil, {
                    t "  ",
                    i(1, "x"),
                    t " = ",
                    i(2, "undefined"),
                    indent_newline_text(3, ""),
                    let(4),
                }),
            }),
        })
    end)
end

local function get_indent()
    local pos = vim.api.nvim_win_get_cursor(0)
    return string.rep(" ", pos[2])
end

local function case(idx, indent)
    local sec, ms = vim.uv.gettimeofday()
    local id = sec * 1000 + ms

    return d(idx, function()
        if not indent then
            indent = get_indent()
        end

        return sn(1, {
            t { "", indent },
            i(1, "_"),
            t " -> ",
            c(2, {
                r(nil, id, i(1, "undefined")),
                sn(nil, {
                    r(1, id),
                    case(2, indent),
                }),
            }),
        })
    end)
end

local function adt(idx, indent)
    local sec, ms = vim.uv.gettimeofday()
    local id = sec * 1000 + ms

    return d(idx, function()
        if not indent then
            indent = get_indent()
            indent = indent:sub(1, #indent - 2)
        end

        return sn(1, {
            c(1, {
                r(nil, id, i(1)),
                sn(nil, {
                    r(1, id),
                    t { "", indent .. "| " },
                    adt(2, indent),
                }),
            }),
        })
    end)
end

local function rec(idx, prefix)
    local sec, ms = vim.uv.gettimeofday()
    local id = sec * 1000 + ms

    return d(idx, function()
        return sn(1, {
            t { "", prefix },
            c(1, {
                r(nil, id, {
                    i(1),
                    t " :: ",
                    i(2),
                }),
                sn(nil, {
                    r(1, id),
                    rec(2, "    , "),
                }),
            }),
        })
    end)
end

local function condition_fn(idx)
    local sec, ms = vim.uv.gettimeofday()
    local id = sec * 1000 + ms

    return d(idx, function()
        return sn(1, {
            t { "", "    | " },
            r(1, "cond" .. tostring(id), i(1, "undefined")),
            t " = ",
            c(2, {
                r(nil, id, i(1, "undefined")),
                sn(nil, {
                    r(1, id),
                    condition_fn(2),
                }),
                sn(nil, {
                    r(1, id),
                    t { "", "    | otherwise = " },
                    i(2, "undefined"),
                }),
            }),
        })
    end)
end

local function fun(_, parent, _, user_data)
    if not user_data then
        user_data = {
            name = parent.captures[1],
            argc = 0,
        }
    end

    local idx = 2
    local choices = {}
    local end_snip = {
        r(1, "type" .. user_data.argc, i(1)),
        t { "", user_data.name },
    }
    if user_data.argc > 0 then
        for _ = 1, user_data.argc do
            table.insert(end_snip, t " ")
            table.insert(end_snip, r(idx, "arg" .. tostring(idx), i(1, "_")))
            idx = idx + 1
        end
    end
    table.insert(
        end_snip,
        c(idx, {
            sn(nil, {
                t " = ",
                i(1, "undefined"),
            }),
            condition_fn(nil),
        })
    )
    table.insert(choices, sn(nil, end_snip))

    table.insert(
        choices,
        sn(nil, {
            r(1, "type" .. user_data.argc),
            t " -> ",
            d(2, function()
                return fun(nil, parent, nil, {
                    name = user_data.name,
                    argc = user_data.argc + 1,
                })
            end),
        })
    )

    return sn(1, {
        c(1, choices),
    })
end

local function list_compr(idx)
    return d(idx, function()
        return sn(1, {
            c(1, {
                t "]",
                sn(nil, {
                    t ", ",
                    i(2),
                    t " <- ",
                    i(1),
                    list_compr(3),
                }),
                sn(nil, {
                    t " | ",
                    i(2),
                    t " <- ",
                    i(1),
                    list_compr(3),
                }),
                sn(nil, {
                    t ", let ",
                    i(1),
                    t " = ",
                    i(2),
                    list_compr(3),
                }),
                sn(nil, {
                    t ", ",
                    i(1),
                    list_compr(2),
                })
            })
        })
    end)
end

return {
    s("if", {
        c(1, {
            r(nil, "if"),
            sn(nil, {
                t "(",
                r(1, "if"),
                t ")",
            }),
        }),
    }, {
        stored = {
            ["if"] = {
                t "if ",
                i(1, "undefined"),
                t " then ",
                i(2, "undefined"),
                t " else ",
                i(3, "undefined"),
            },
        },
    }),

    s("ifm", {
        t "if ",
        i(1, "undefined"),
        indent_newline_text(2, "then ", 1),
        i(3, "undefined"),
        indent_newline_text(4, "else ", 1),
        i(5, "undefined"),
    }),

    s("let", {
        t "let ",
        i(1, "x"),
        t " = ",
        i(2, "undefined"),
        indent_newline_text(3, "  "),
        let(4),
    }),

    s("case", {
        t "case ",
        i(1),
        t " of",
        case(2),
    }),

    s("\\", {
        c(1, {
            sn(nil, {
                t "(",
                r(1, "lambda"),
                t ")",
            }),
            r(nil, "lambda"),
        }),
    }, {
        stored = {
            ["lambda"] = {
                t "\\",
                i(1),
                t " -> ",
                i(2, "undefined"),
            },
        },
    }),

    s("adt", {
        t "data ",
        i(1, "", { key = "name" }),
        t " = ",
        c(2, {
            sn(nil, {
                fn(function(node_text, _)
                    return node_text[1][1] .. " "
                end, k "name"),
                i(1),
            }),
            adt(nil),
        }),
        t { "", "    deriving (Show)" },
    }, {
        condition = top_level,
    }),

    s("new", {
        t "newtype ",
        i(1),
        t " = ",
        fn(function(node_text, _)
            return node_text[1][1] .. " "
        end, { 1 }),
        i(0),
        t { "", "    deriving (Show)" },
    }, {
        condition = top_level,
    }),

    s("rec", {
        t "data ",
        i(1),
        t " = ",
        fn(function(node_text, _)
            return node_text[1][1] .. " "
        end, { 1 }),
        rec(2, "    { "),
        t { "", "    }", "    deriving (Show)" },
    }, {
        condition = top_level,
    }),

    s({
        trig = "(%a[%a%d_']*)",
        regTrig = true,
        hidden = true,
    }, {
        fn(function(_, parent)
            return parent.captures[1]
        end),
        t " :: ",
        d(1, fun),
    }, {
        condition = top_level,
    }),

    s(
        "l|",
        fmt(
            [[
                [ {} | {} <- {}{}
            ]],
            {
                i(0, "undefined"),
                i(2),
                i(1),
                list_compr(3),
            }
        )
    ),
}, {}
