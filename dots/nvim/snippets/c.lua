local ls = require "luasnip"
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local extras = require "luasnip.extras"
local rep = extras.rep
local fmt = require("luasnip.extras.fmt").fmt
local postfix = require("luasnip.extras.postfix").postfix

local function ef(idx)
    return d(idx, function()
        return sn(
            1,
            fmt(
                [[
                    {}
                ]],
                {
                    c(1, {
                        i(1),
                        sn(
                            1,
                            fmt(
                                [[
                                     else if ({}) {{
                                        {}
                                    }}{}
                                ]],
                                {
                                    i(1),
                                    i(2, "// TODO: else if"),
                                    ef(3),
                                }
                            )
                        ),
                        sn(
                            1,
                            fmt(
                                [[
                                     else {{
                                        {}
                                    }}
                                ]],
                                i(1, "-- TODO: else")
                            )
                        ),
                    }),
                }
            )
        )
    end)
end

local function switch_branch(idx)
    return d(idx, function()
        return sn(
            1,
            fmt(
                [[
                    {}
                ]],
                {
                    c(1, {
                        sn(
                            1,
                            fmt(
                                [[
                                    case {}:
                                            {}
                                            break;{}
                                ]],
                                {
                                    i(1),
                                    i(2, "// TODO: case"),
                                    c(3, {
                                        t "",
                                        sn(1, {
                                            t { "", "    " },
                                            switch_branch(1),
                                        }),
                                    }),
                                }
                            )
                        ),
                        sn(
                            1,
                            fmt(
                                [[
                                    default:
                                        {}
                                ]],
                                i(1)
                            )
                        ),
                        t "",
                    }),
                }
            )
        )
    end)
end

return {
    s(
        {
            trig = "for([%a%d_]+)",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                for (int {} = 0; {} {} {}; {}{}) {{
                    {}
                }}
            ]],
            {
                d(1, function(_, snip)
                    return sn(1, i(1, snip.captures[1]))
                end),
                rep(1),
                c(2, {
                    t "<",
                    t ">",
                    t "<=",
                    t ">=",
                }),
                c(3, {
                    i(1, "10"),
                    sn(1, fmt("strlen({})", { i(1, "str") })),
                }),
                rep(1),
                c(4, {
                    t "++",
                    t "--",
                }),
                i(0, "// TODO: for loop"),
            }
        )
    ),

    s(
        "if",
        fmt(
            [[
                if ({}) {{
                    {}
                }}{}
            ]],
            {
                i(1),
                i(2, "// TODO: if"),
                ef(3),
            }
        )
    ),

    s(
        "switch",
        fmt(
            [[
                switch ({}) {{
                    {}
                }}
            ]],
            {
                i(1),
                switch_branch(2),
            }
        )
    ),

    postfix(
        {
            trig = "fn([%a%d_]+)",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                {} {}({}) {{
                    {}
                }}
            ]],
            {
                f(function(_, parent)
                    local postfix = parent.snippet.env.POSTFIX_MATCH
                    if postfix == "" then
                        return "void"
                    end
                    return parent.snippet.env.POSTFIX_MATCH
                end),
                d(1, function(_, snip)
                    return sn(1, i(1, snip.captures[1]))
                end),
                i(2),
                i(0, "// TODO: function"),
            }
        )
    ),
}, {}
