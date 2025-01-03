local ls = require "luasnip"
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local fmt = require("luasnip.extras.fmt").fmt

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
                        t "end",
                        sn(
                            1,
                            fmt(
                                [[
                                    elseif {} then
                                        {}
                                    {}
                                ]],
                                {
                                    i(1),
                                    i(2, "-- TODO: else if"),
                                    ef(3),
                                }
                            )
                        ),
                        sn(
                            1,
                            fmt(
                                [[
                                    else
                                        {}
                                    end
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

return {
    s(
        "req",
        fmt(
            [[
                local {} = require "{}"
            ]],
            {
                f(function(name)
                    local parts = vim.split(name[1][1], ".", true)
                    return parts[#parts] or ""
                end, { 1 }),
                i(1),
            }
        )
    ),

    s(
        {
            trig = "for([%a%d,_]+);?([%a%d_()]*)",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                for {}{} do
                    {}
                end
            ]],
            {
                f(function(_, snip)
                    return snip.captures[1]:gsub(",", ", ")
                end),
                d(1, function(_, snip)
                    local captured = snip.captures[2]

                    if captured ~= "" then
                        if snip.captures[1]:find "," then
                            if captured:find "ipairs" then
                                return sn(1, t(" in " .. captured))
                            else
                                return sn(1, t(" in ipairs(" .. captured .. ")"))
                            end
                        else
                            return sn(1, {
                                t " in ",
                                c(1, {
                                    t("pairs(" .. captured .. ")"),
                                    t(captured),
                                }),
                            })
                        end
                    else
                        return sn(
                            1,
                            c(1, {
                                d(1, function()
                                    if snip.captures[1]:find "," then
                                        return sn(1, {
                                            t " in ipairs(",
                                            i(1),
                                            t ")",
                                        })
                                    end
                                    return sn(1, {
                                        t " in pairs(",
                                        i(1),
                                        t ")",
                                    })
                                end),
                                d(2, function()
                                    if not snip.captures[1]:find "," then
                                        return sn(1, {
                                            t " = ",
                                            i(1, "1"),
                                            t ",",
                                            i(2, "10"),
                                        })
                                    else
                                        return sn(1, {
                                            t " in ",
                                            i(1),
                                        })
                                    end
                                end),
                            })
                        )
                    end
                end),
                i(0),
            }
        )
    ),

    s(
        {
            trig = "([local]*)fn([%a%d_.]*)",
            regTrig = true,
            hidden = true,
        },
        fmt(
            [[
                {}function{}({})
                    {}
                end
            ]],
            {
                f(function(_, snip)
                    local captured = snip.captures[1]

                    if captured == "" then
                        return ""
                    else
                        return "local "
                    end
                end),
                f(function(_, snip)
                    local captured = snip.captures[2]

                    if captured ~= "" then
                        return " " .. captured
                    end

                    return ""
                end),
                i(1),
                i(0, "-- TODO: function"),
            }
        )
    ),

    s(
        "if",
        fmt(
            [[
                if {} then
                    {}
                {}
            ]],
            {
                i(1),
                i(2, "-- TODO: if"),
                ef(3),
            }
        )
    ),
}, {}
