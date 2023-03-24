return {
    s(
        "todo",
        fmt(
            [[
                {}: {}
            ]],
            {
                c(1, {
                    t "TODO",
                    t "FIXME",
                    t "BUG",
                    t "NOTE",
                    t "INFO",
                    t "TEST",
                    t "TODO(m3dry)",
                    t "FIXME(m3dry)",
                    t "BUG(m3dry)",
                    t "NOTE(m3dry)",
                    t "INFO(m3dry)",
                    t "TEST(m3dry)",
                }),
                i(0),
            }
        ),
        {
            condition = function()
                local context = require("m3dry.utils").ts_context

                return context "string" or context "comment"
            end
        }
    ),
}, {}
