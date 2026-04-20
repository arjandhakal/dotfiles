;;; init.el --- Bootstrap for literate Emacs config -*- lexical-binding: t; -*-

;; Increase GC threshold during startup
(setq gc-cons-threshold (* 100 1024 1024))

;; Package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Separate file for Customize
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Load the literate configuration
;; Use file-truename to resolve symlinks (for stow-managed dotfiles)
(org-babel-load-file
 (expand-file-name "config.org"
                    (file-name-directory (file-truename user-init-file))))

;;; init.el ends here
