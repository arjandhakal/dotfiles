local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>.', builtin.find_files, {})
vim.keymap.set('n', '<C-p>', builtin.git_files, {})
vim.keymap.set('n', '<leader>/', function()
    builtin.grep_string({ search = vim.fn.input("Grep > ") });
end)
vim.keymap.set('n', '<leader>cD', builtin.lsp_references, {})
