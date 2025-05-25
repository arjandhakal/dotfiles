(load "./package.el")
(eval-when-compile
  (require 'use-package))

(global-set-key [remap list-buffers] 'ibuffer)
;;(load-theme 'dracula)

;; removing unneeded windows (tool bar and menu bar)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)


;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 0)

;; Downloading another keyboard mode because I don't want
;; pinky finger
(unless (package-installed-p 'meow)
  (package-install 'meow))


;; Clojure Mode
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

;; Cider
(unless (package-installed-p 'cider)
  (package-install 'cider))

;; Rainbow delimeters to make it fun to code in clojure / racket
(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)


;; Adding hook to run rainbow delimiters mode when clojure
(if (package-installed-p 'rainbow-delimiters)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; Paredit
(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(if (package-installed-p 'paredit)
    (add-hook 'clojure-mode-hook #'paredit-mode))

;; Racket Mode
(unless (package-installed-p 'racket-mode)
  (package-install 'racket-mode))

;; Themes
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;;  Setting up any ts file to use typescript mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)


;; The `vertico' package applies a vertical layout to the minibuffer.
;; It also pops up the minibuffer eagerly so we can see the available
;; options without further interactions.  This package is very fast
;; and "just works", though it also is highly customisable in case we
;; need to modify its behaviour.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))


;; The `marginalia' package provides helpful annotations next to
;; completion candidates in the minibuffer.  The information on
;; display depends on the type of content.  If it is about files, it
;; shows file permissions and the last modified date.  If it is a
;; buffer, it shows the buffer's size, major mode, and the like.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))


;; The `consult' package provides lots of commands that are enhanced
;; variants of basic, built-in functionality.  One of the headline
;; features of `consult' is its preview facility, where it shows in
;; another Emacs window the context of what is currently matched in
;; the minibuffer.  Here I define key bindings for some commands you
;; may find useful.  The mnemonic for their prefix is "alternative
;; search" (as opposed to the basic C-s or C-r keys).
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))


;; The `orderless' package lets the minibuffer use an out-of-order
;; pattern matching algorithm.  It matches space-separated words or
;; regular expressions in any order.  In its simplest form, something
;; like "ins pac" matches `package-menu-mark-install' as well as
;; `package-install'.  This is a powerful tool because we no longer
;; need to remember exactly how something is named.
;;
;; Note that Emacs has lots of "completion styles" (pattern matching
;; algorithms), but let us keep things simple.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:7cc77fd0-8f98-4fc0-80be-48a758fcb6e2
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))


;; The `embark' package lets you target the thing or context at point
;; and select an action to perform on it.  Use the `embark-act'
;; command while over something to find relevant commands.
;;
;; When inside the minibuffer, `embark' can collect/export the
;; contents to a fully fledged Emacs buffer.  The `embark-collect'
;; command retains the original behaviour of the minibuffer, meaning
;; that if you navigate over the candidate at hit RET, it will do what
;; the minibuffer would have done.  In contrast, the `embark-export'
;; command reads the metadata to figure out what category this is and
;; places them in a buffer whose major mode is specialised for that
;; type of content.  For example, when we are completing against
;; files, the export will take us to a `dired-mode' buffer; when we
;; preview the results of a grep, the export will put us in a
;; `grep-mode' buffer.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:61863da4-8739-42ae-a30f-6e9d686e1995
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :ensure t)

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at
;; the top.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
(savehist-mode 1)

;; The built-in `recentf-mode' keeps track of recently visited files.
;; You can then access those through the `consult-buffer' interface or
;; with `recentf-open'/`recentf-open-files'.
(recentf-mode 1)


;; Using magit because it is very easy to use git client
(use-package magit
  :ensure t)


;; Set a directory for backup and autosave files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

;; Create the directories if they don't exist
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/autosaves" t)


;; Set a directory for lock files
(setq lock-file-name-transforms '((".*" "~/.emacs.d/lockfiles/\\1" t)))

;; Create the lockfiles directory if it doesn't exist
(make-directory "~/.emacs.d/lockfiles" t)
