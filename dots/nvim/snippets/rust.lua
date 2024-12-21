local ls = require "luasnip"
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local fmt = require("luasnip.extras.fmt").fmt
local postfix = require("luasnip.extras.postfix").postfix

local function pub()
    return c(1, {
        t "",
        t "pub(crate) ",
        t "pub ",
    })
end

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
                                     else if {} {{
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

local function match_branch(idx)
    return d(idx, function()
        return sn(
            1,
            fmt(
                [[
                    {} => {},{}
                ]],
                {
                    c(1, {
                        i(1),
                        t "_",
                    }),
                    c(2, {
                        i(1),
                        sn(
                            1,
                            fmt(
                                [[
{{
        {}
    }}
                                ]],
                                i(1),
                                {
                                    dedent = false,
                                }
                            )
                        ),
                    }),
                    c(3, {
                        t "",
                        sn(1, { t { "", "    " }, match_branch(1) }),
                    }),
                }
            )
        )
    end)
end

local function struct_field(idx)
    return d(idx, function()
        return sn(
            1,
            fmt(
                [[
                    {}{}: {},{}
                ]],
                {
                    pub(),
                    i(2),
                    i(3),
                    c(4, {
                        t "",
                        sn(1, { t { "", "    " }, struct_field(1) }),
                    }),
                }
            )
        )
    end)
end

local function label()
    return f(function(_, snip)
        local match = snip.snippet.env.POSTFIX_MATCH
        if match ~= "" then
            return "'" .. snip.snippet.env.POSTFIX_MATCH .. ": "
        end
        return ""
    end)
end

return {
    s(
        {
            trig = "test([%d%a_]*)",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                #[test]
                fn {}() {{
                    {}
                }}
            ]],
            {
                d(1, function(_, snip)
                    if snip.captures[1] ~= "" then
                        return sn(1, t(snip.captures[1]))
                    end
                    return sn(1, i(1))
                end),
                i(0),
            }
        )
    ),

    s(
        "if",
        fmt(
            [[
                if {} {{
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
        "match",
        fmt(
            [[
                match {} {{
                    {}
                }}
            ]],
            {
                i(1),
                match_branch(2),
            }
        )
    ),

    s(
        "struct",
        fmt(
            [[
                {}struct {} {{
                    {}
                }}
            ]],
            {
                pub(),
                i(2),
                struct_field(3),
            }
        )
    ),

    s(
        "trait",
        fmt(
            [[
                trait {} {{
                    {}
                }}
            ]],
            {
                i(1),
                i(0),
            }
        )
    ),

    s(
        "impl",
        fmt(
            [[
                impl {}{} {{
                    {}
                }}
            ]],
            {
                i(1),
                c(2, {
                    t "",
                    sn(1, {
                        t " for ",
                        i(1),
                    }),
                }),
                i(0),
            }
        )
    ),

    postfix(
        "while",
        fmt(
            [[
                {}while {} {{
                    {}
                }}
            ]],
            {
                label(),
                i(1),
                i(0),
            }
        )
    ),

    postfix(
        "loop",
        fmt(
            [[
                {}loop {{
                    {}
                }}
            ]],
            {
                label(),
                i(0),
            }
        )
    ),

    postfix(
        {
            trig = "for([%a%d,_]+);?([%a%d_]*)",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                {}for {} in {} {{
                    {}
                }}
            ]],
            {
                label(),
                f(function(_, snip)
                    if snip.captures[1]:find "," then
                        return "(" .. snip.captures[1]:gsub(",", ", ") .. ")"
                    end
                    return snip.captures[1]
                end),
                d(1, function(_, snip)
                    local captured = snip.captures[2]

                    if captured ~= "" then
                        if snip.captures[1]:find "," and not snip.captures[1]:find "%.iter%(%)%.enumerate%(%)" then
                            return sn(1, t(captured .. ".iter().enumerate()"))
                        end
                        return sn(1, t(captured))
                    else
                        return sn(
                            1,
                            c(1, {
                                d(1, function()
                                    if snip.captures[1]:find "," then
                                        return sn(1, { i(1, "arr"), t ".iter().enumerate()" })
                                    else
                                        return sn(1, { t "0..", i(1, "arr"), t ".len()" })
                                    end
                                end),
                                i(1, "arr"),
                            })
                        )
                    end
                end),
                i(0, "// TODO: for loop"),
            }
        )
    ),

    postfix(
        {
            trig = "fn([%a%d_]*)",
            match_pattern = "[%w%_%-%,%<%>]+$",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                {}fn {}({}) {}{{
                    {}
                }}
            ]],
            {
                pub(),
                d(2, function(_, snip)
                    if snip.captures[1] ~= "" then
                        return sn(1, t(snip.captures[1]))
                    end
                    return sn(1, i(1))
                end),
                i(3),
                f(function(_, parent)
                    local ret = parent.snippet.env.POSTFIX_MATCH
                    if ret == "" then
                        return ""
                    else
                        if ret:find "," then
                            return "-> (" .. ret:gsub(",", ", ") .. ") "
                        else
                            return "-> " .. ret .. " "
                        end
                    end
                end),
                i(0, "// TODO: function"),
            }
        )
    ),

    s(
        "modtest",
        fmt(
            [[
                #[cfg(test)]
                mod tests {{
                {}
                }}
            ]],
            {
                c(1, {
                    sn(
                        1,
                        fmt(
                            [[
    use super::*;

    {}
                            ]],
                            i(1),
                            {
                                dedent = false,
                            }
                        )
                    ),
                    i(1),
                }),
            }
        )
    ),
}, {}
