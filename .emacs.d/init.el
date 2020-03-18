;;; init.el --- Configuration for Emacs
;;
;;; Commentary:
;;
;; Configuration for Emacs
;;
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(setq package-enable-at-startup nil)

; (package-initialize)

;; Recompile .el file(s) if changes occur
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/kiran") 0)

;; Prevent loading of outdated .elc files
;; (setq load-prefer-newer t)

;; Experiment: Turn off mouse interface early in startup to avoid momentary display
(scroll-bar-mode -1)

;; Cache 50MB before garbage collection
(setq gc-cons-threshold 50000000)

;; language
(setq current-language-environment "English")

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs #'y-or-n-p)

;; Turn off bell
(setq ring-bell-function #'ignore)

;; https://www.emacswiki.org/emacs/ExecPath
(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'load-path "~/.emacs.d/oci/")

(package-initialize)
(require 'init-packages)

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Custom configuration set by Emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'init-ui)
(require 'init-web)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium))

(set-face-attribute 'region nil :background "#3a3a3a")

(fringe-mode 10)

;; Save minibuffer history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length 150)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)

;; turn beep off
(setq visible-bell nil)

;; Follow compiler ouput
(setq compilation-scroll-output t)

;; don't disable narrow-to-region command
(put 'narrow-to-region 'disabled nil)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Start emacs server
(server-start)

(provide `.emacs)
;;; init.el ends hereb
