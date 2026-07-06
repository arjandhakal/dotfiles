## Dotfiles

Personal configuration files managed with [GNU Stow](https://www.gnu.org/software/stow/).

### Packages

- **emacs** — Literate Emacs config (`config.org` tangled via org-babel). Includes Evil mode, LSP, Clojure/ClojureDart, org-roam, and more.
- **eca** — ECA config with short GPT-5.5 reasoning variants and model aliases.
- **nvim** — Lua-based Neovim config with lazy.nvim, Telescope, Conjure, and LSP.

### Setup

```sh
cd ~/.dotfiles
stow eca     # symlinks .config/eca/config.json
stow emacs   # symlinks .emacs.d/{init.el,config.org,cornell-note.el}
stow nvim    # symlinks .config/nvim/
```
