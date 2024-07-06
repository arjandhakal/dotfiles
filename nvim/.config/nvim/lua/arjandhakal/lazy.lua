-- Defining the path to clone and install lazy, if not already
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end

-- same as writing vim.opt.rtp.prepend(vim.opt.rtp, lazypath)
vim.opt.rtp:prepend("lazypath")
