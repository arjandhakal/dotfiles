## Dotfiles

Personal configuration files managed with [GNU Stow](https://www.gnu.org/software/stow/).

### Packages

- **emacs** — Literate Emacs config (`config.org` tangled via org-babel). Includes Evil mode, LSP, OCaml, Clojure/ClojureDart, org-roam, and more.
- **eca** — ECA config with short GPT-5.5 reasoning variants and model aliases.
- **nvim** — Lua-based Neovim config with lazy.nvim, Telescope, Conjure, and LSP.

### Setup

```sh
cd ~/.dotfiles
stow eca     # symlinks .config/eca/config.json
stow emacs   # symlinks .emacs.d config and lisp/ clients
stow nvim    # symlinks .config/nvim/
```

### Pi session integration

The Emacs package includes a vendored `lisp/sync-emacs.el` from
[`clanker_jiken/sync-emacs`](https://github.com/arjandhakal/clanker_jiken/tree/main/sync-emacs).
Install the Pi package globally with `pi install /path/to/clanker-jiken/sync-emacs`,
then run `/reload` in existing Pi terminals. In Emacs, use `M-x connect-pi`
or `C-c P c`; `C-c P b` sends a buffer, `C-c P r` sends a region, and
`C-c P a` pins a source buffer to a connection.

When updating the extension, refresh the vendored client too:

```sh
cp /path/to/clanker-jiken/sync-emacs/emacs/sync-emacs.el ~/.dotfiles/emacs/.emacs.d/lisp/
```
