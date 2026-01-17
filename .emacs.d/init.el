;; Increase GC threshold to 100MB during startup
(setq gc-cons-threshold (* 100 1024 1024))

;; Reset it to a reasonable default (2MB) after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Ensure environment variables are loaded from shell on macOS
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(global-set-key [remap list-buffers] 'ibuffer)
;; Some themes
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))
(require 'doom-themes)
(load-theme 'doom-shades-of-purple t) ; Use 'doom-one' for a dark theme similar to Doom Emacs

;; removing unneeded windows (tool bar and menu bar)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq frame-title-format "Arjan's Emacs - %b")

;; This variable must be set BEFORE evil is loaded.
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ;; Disabling some evil overrides with emacs default
  ;; A very good list is available here
  ;; https://github.com/noctuid/evil-guide/blob/master/README.org#use-some-emacs-keybindings
  (define-key evil-insert-state-map (kbd "C-v") nil)       ; Restores scroll-up-command
  (define-key evil-insert-state-map (kbd "C-k") nil)       ; Restores kill-line
  (define-key evil-insert-state-map (kbd "C-o") nil)       ; Restores open-line
  (define-key evil-insert-state-map (kbd "C-r") nil)       ; Restores isearch-backward
  (define-key evil-insert-state-map (kbd "C-y") nil)       ; Restores yank
  (define-key evil-insert-state-map (kbd "C-e") nil)       ; Restores move-end-of-line
  (define-key evil-insert-state-map (kbd "C-n") nil)       ; Restores next-line
  (define-key evil-insert-state-map (kbd "C-p") nil)       ; Restores previous-line
  (define-key evil-insert-state-map (kbd "C-x C-n") nil)   ; Restores set-goal-column
  (define-key evil-insert-state-map (kbd "C-x C-p") nil)   ; Restores mark-page
  (define-key evil-insert-state-map (kbd "C-t") nil)       ; Restores transpose-chars
  (define-key evil-insert-state-map (kbd "C-d") nil)       ; Restores delete-char
  (define-key evil-insert-state-map (kbd "C-a") nil)       ; Restores move-beginning-of-line
  (define-key evil-insert-state-map (kbd "C-w") nil))       ; Restores kill-region

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
;; Clojure Mode
(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
;; Cider
(unless (package-installed-p 'cider)
  (package-install 'cider))

;; Rainbow delimeters to make it fun to code in clojure / racket
(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

;; Racket Mode
(unless (package-installed-p 'racket-mode)
  (package-install 'racket-mode))


(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 1)


(use-package paredit :ensure t)

;; Define a hook for all Lisp-like languages
(defun arjan/lisp-setup ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1))

(dolist (mode '(clojure-mode-hook
                racket-mode-hook
                scheme-mode-hook
                lisp-data-mode-hook))
  (add-hook mode 'arjan/lisp-setup))


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
  :bind (:map global-map
         ;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line))
  :init
  ;; Define a custom keymap for SPC leader key
  (defvar my-spc-map (make-sparse-keymap)
    "Custom keymap for SPC leader key in Evil normal and visual states.")
  ;; Set SPC as leader key in Evil normal and visual states
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC") my-spc-map)
    (define-key evil-visual-state-map (kbd "SPC") my-spc-map))
  ;; Define bindings in the SPC keymap
  (define-key my-spc-map (kbd "SPC") #'consult-find)
  (define-key my-spc-map (kbd ".") #'consult-buffer)
  (define-key my-spc-map (kbd "f r") #'consult-recent-file))

(setq consult-find-args "find . -not ( -path '*/dist/*' -o -path '*/node_modules/*' )")


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


;; Keep .emacs.d clean
(use-package no-littering
  :ensure t)


;; LSPss
;; Install and configure YASnippet for LSP snippet support
(use-package yasnippet
  :config
  (yas-global-mode 1)) ;; Enable YASnippet globally

;; Install and configure Company for autocompletion
(use-package company
  :hook (prog-mode . company-mode) ;; Enable in programming modes
  :config
  (setq company-idle-delay 0.2) ;; Faster completion
  (setq company-minimum-prefix-length 1))

;; Install and configure Flycheck for LSP diagnostics
(use-package flycheck
  :hook (prog-mode . flycheck-mode) ;; Enable in programming modes
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))


(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((clojure-mode       . lsp)
         (clojurec-mode      . lsp)
	 (java-ts-mode      . lsp-deferred)
	 ((tsx-ts-mode
	   typescript-ts-mode
	   js-ts-mode) . lsp-deferred)
         (lsp-mode           . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename)
              ("M-<return>" . lsp-execute-code-action))
  :config
  (setq lsp-diagnostics-provider :flycheck)
  ;; Disable visual features
  (setq lsp-headerline-breadcrumb-enable nil   ;; No breadcrumbs
	lsp-ui-sideline-enable           nil   ;; No sideline
        lsp-lens-enable                  nil   ;; No lenses

        ;; Disable all mode line features, since I use a custom mode line
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable  nil

        ;; Limit raising of the echo area to show docs
        lsp-signature-doc-lines 3)
;; Ensure typescript-language-server is used for TypeScript and TSX
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("typescript-language-server" "--stdio"))
    :major-modes '(typescript-ts-mode tsx-ts-mode)
    :server-id 'ts-ls)))

;; Formatting Clojure files on save
;; (defun my-clojure-lsp-format-hook ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer nil 'local))

;; (add-hook 'clojure-mode-hook #'my-clojure-lsp-format-hook)
;; (add-hook 'clojurescript-mode-hook #'my-clojure-lsp-format-hook)
;; (add-hook 'clojurec-mode-hook #'my-clojure-lsp-format-hook)

;; JAVA LSP
(use-package lsp-java
  :ensure t
  :defer t
  :hook (java-ts-mode . lsp-deferred)
  :config
  ;; Try to find 'java' in the path, or fallback to a standard location
  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx1G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"))

  ;; Set a custom directory for the server installation
  (setq lsp-java-server-install-dir (concat user-emacs-directory ".lsp-java-server/"))
  )



;; Enabling movement by Shift Keys
(windmove-default-keybindings)


;; Automatically hide the detailed listing when visiting a Dired
;; buffer.  This can always be toggled on/off by calling the
;; `dired-hide-details-mode' interactively with M-x or its keybindings
;; (the left parenthesis by default).
(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;; Do not outright delete files.  Move them to the system trash
;; instead.  The `trashed' package can act on them in a Dired-like
;; fashion.  I use it and can recommend it to either restore (R) or
;; permanently delete (D) the files.
(setq delete-by-moving-to-trash t)


;; Icons for all types of files
;; Note: Run M-x all-the-icons-install-fonts to install necessary fonts for icons.
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))



;; Tree sitter for major libraries
(use-package treesit
      :ensure nil
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
	     ("\\.go\\'" . go-ts-mode)
	     ("\\.java\\'" . java-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		   (java "https://github.com/tree-sitter/tree-sitter-java")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
		 (java-mode . java-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))



;; Install and configure nov package
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))


;; Make sure to have poppler installed
;; so that pdf-tools can properly render it
;; Configure pdf-tools
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; Enable pdf-tools installation (compiles native components)
  (pdf-tools-install)
  ;; Optimize for comfortable reading
  (setq pdf-view-display-size 'fit-page)  ; Fit page to window width
  (setq pdf-view-continuous t)           ; Smooth scrolling
  (setq pdf-view-midnight-colors '("#ffffff" . "#1e1e1e")) ; Dark mode for night reading
  ;; Keybindings for convenience
  (bind-key "C-+" 'pdf-view-enlarge pdf-view-mode-map)
  (bind-key "C--" 'pdf-view-shrink pdf-view-mode-map)
  (bind-key "j" 'pdf-view-next-line-or-next-page pdf-view-mode-map)
  (bind-key "k" 'pdf-view-previous-line-or-previous-page pdf-view-mode-map)
  (bind-key "g" 'pdf-view-goto-page pdf-view-mode-map)
  :hook
  (pdf-view-mode . (lambda ()
                     ;; Auto-revert for live updates (e.g., if PDF changes)
                     (auto-revert-mode)
                     ;; Optional: Enable midnight mode (dark theme) by default
                     (pdf-view-midnight-minor-mode))))


;; Mac only
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;;; Org Journal settings
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")

;;; Org Agenda and general Org settings
(with-eval-after-load 'org
  (setq org-agenda-files '("~/arjan-files/Documents/things-to-do.org"
                           "~/arjan-files/Documents/work-things-to-do.org"
                           "~/arjan-files/Documents/what-i-am-doing.org")
        org-directory "~/arjan-files/Documents/"))

;;; Org Roam settings
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/arjan-files/Documents/roam-notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           ;; CHANGED: Removed "%<%Y%m%d%H%M%S>-" from the filename
           :target (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; Using Org Journal
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/arjan-files/Documents/personal/journal"
        org-journal-date-format "%A, %d %B %Y"))



(use-package org-roam-ui
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;; Terraform Mode
(use-package terraform-mode
  ;; if using straight
  ;; :straight t

  ;; if using package.el
  ;; :ensure t
  :custom (terraform-indent-level 4)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

;; Configure Org Babel for restclient
(use-package ob-restclient
  :ensure t)

(org-babel-do-load-languages
    'org-babel-load-languages
    '((restclient . t)
      (python . t)
      (scheme . t)))

;; Ensure Org-mode is loaded and configured for Clojure
(use-package org
  :ensure t
  :config
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider))



;; Hooking up Emacs with a local
;; running instance of LLM
;; using gptel
(use-package gptel
  :ensure t)
(setq
 gptel-model 'gemma3:12b-it-qat
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(gemma3:12b-it-qat)))

;; General Packages
(use-package project)


;; Using ORG Mode (mermaid)
;; Link: https://emacstil.com/til/2021/09/19/org-mermaid/
;; brew install mermaid-cli (mac)
;; (setq ob-mermaid-cli-path "/usr/local/bin/mmdc")
;; Usage
;; In any org file, you can use `mermaid` source type, and specify the output file’s name:
;;
; #+begin_src mermaid :file test.png
;; sequenceDiagram
;;  A-->B: Works!
;; #+end_src

(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path (executable-find "mmdc")))


;; This block ensures org-modern is installed and applies all your desired
;; settings specifically and only when Org mode is active.

;; 1. Configure and load the org-modern package
(use-package org-modern
  :ensure t ; Automatically installs the package if it's not already there
  :hook
  (org-mode . org-modern-mode)          ; Enable modern look in Org files
  (org-agenda-finalize . org-modern-agenda)) ; Enable modern look for the agenda

;; 2. Create a function for your custom Org settings
(defun my-custom-org-mode-settings ()
  "Apply my preferred settings for Org mode."
  ;; Enable visual indentation for headlines and content
  (org-indent-mode)

  ;; --- FONT SETTINGS FOR ORG-MODE ---
  ;; brew install --cask font-iosevka
  ;; Remap the default monospace font to Iosevka for this buffer
  (face-remap-add-relative 'default '(:family "Iosevka" :height 1.0))

  ;; Remap the variable-pitch font (for proportional text) to Iosevka Aile
  (face-remap-add-relative 'variable-pitch '(:family "Iosevka Aile" :height 1.0))

  ;; Set the specific font for org-modern's symbols
  ;(set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  ;; Displaying image
  (setq org-startup-with-inline-images t)


  ;; The `setq` form configures multiple variables at once.
  (setq
   ;; Editing settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Styling settings
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"))

;; 3. Add your custom settings to org-mode-hook
;; This is the crucial step that makes your settings apply *only* to Org files.
(add-hook 'org-mode-hook #'my-custom-org-mode-settings)

;; Adding breadcrumb 
(use-package breadcrumb
  :ensure t
  :hook
  ;; Use the buffer-local mode provided in the source, 
  ;; rather than the global mode.
  ((prog-mode . breadcrumb-local-mode)
   (markdown-mode . breadcrumb-local-mode))
  
  :config
  ;; The source code defines specific variables for project vs imenu separators.
  ;; We set both to a cleaner symbol.
  (setq breadcrumb-project-crumb-separator " › "
        breadcrumb-imenu-crumb-separator " › ")

  ;; Don't let the project path eat up the whole header line.
  ;; (Keeps 50% of space available for the function/imenu path)
  (setq breadcrumb-project-max-length 0.5)
  
  ;; optional: adjust how quickly it updates (default is usually fine)
  ;; (setq breadcrumb-idle-delay 0.5) 
  )


;; Clomacs
(use-package clomacs)

;; FOR SICP with GUILE
;;https://github.com/emacsmirror/geiser-guile
(use-package geiser-guile
  :ensure t
  :config
  (setq geiser-guile-binary (or (executable-find "guile") "/opt/homebrew/bin/guile"))
  ;; Configure load paths for project-specific modules (discussed in Section 6)
  (add-to-list 'geiser-guile-load-path "~/sicp-work/lib"))
;;

;; Racket for Picture Language 
(use-package geiser-racket
  :ensure t
  :config
  (setq geiser-active-implementations '(guile racket))
  (setq geiser-racket-binary (executable-find "racket")))

;;
