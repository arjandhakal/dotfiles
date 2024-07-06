local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.formatting.prettier,
        null_ls.builtins.diagnostics.eslint.with({
            diagnostics_format = "[eslint] #{m}\n(#{c})",
            -- enabling only if the root has .eslintrc fule
            condition = function(utils)
                return utils.root_has_file(".eslintrc.js") or utils.root_has_file(".eslintrc.cjs") or
                    utils.root_has_file(".eslintrc.json")
            end,
        }),
        null_ls.builtins.completion.spell,
    },
})
