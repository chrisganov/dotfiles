(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or wantto do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize) 

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
	:ensure t
	:config (which-key-mode))

(use-package gruvbox-theme
 :ensure t)

(use-package undo-tree
 :ensure t)

(use-package goto-chg
 :ensure t)

(use-package evil
 :ensure t)

(require 'evil)
(evil-mode 1)

(use-package web-mode
  :mode (
				 ("\\.html$" . web-mode)
				 ("\\.tsx\\'" . web-mode))
	:hook (
    (web-mode-hook . electric-pair-mode)
    (web-mode-hook . (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)))))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(use-package web-beautify
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

(use-package js2-mode
	:ensure t
	:mode "\\.js\\'"
  :interpreter "node"
	:init
	(setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval"
                                     "clearInterval" "location" "__dirname" "console" "JSON" "window"
                                     "process" "fetch"))
  (setq-default js2-strict-trailing-comma-warning t)
	:config
	(use-package rjsx-mode :ensure t
    :mode "\\.jsx\\'")

  (use-package js2-refactor :ensure t)
  
  (use-package nodejs-repl :ensure t)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-mode-hook
            '(lambda ()
               (js2-refactor-mode)
               (js2r-add-keybindings-with-prefix "M-m")
               (key-chord-define js2-mode-map ";;" (λ (save-excursion (move-end-of-line nil) (insert ";"))))
               (key-chord-define js2-mode-map ",," (λ (save-excursion (move-end-of-line nil) (insert ",")))))))

(use-package rjsx-mode
	:ensure t
	:init)

(use-package emmet-mode
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :bind (:map evil-insert-state-map
    ("C-a" . emmet-expand-line))
  :hook (
    (sgm-mode . emmet-mode)
    (web-mode . emmet-mode)
    (css-mode . emmet-mode))
  :config
  ;; Auto-start on any markup modes
  ;; (add-hook 'sgml-mode-hook 'emmet-mode)
  ;; (add-hook 'web-mode-hook 'emmet-mode)
  ;; (add-hook 'css-mode-hook 'emmet-mode)
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? t)
  (setq emmet-self-closing-tag-style " /"))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (bind-key "{" #'paredit-open-curly json-mode-map)
  (bind-key "}" #'paredit-close-curly json-mode-map))

;; TypeScript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(defun setup-tide-mode ()
  (interactive)
  (defun tide-imenu-index () nil)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
	:after (typescript-mode company flycheck)
	:hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :config
  (progn
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    (add-hook 'rjsx-mode-hook #'setup-tide-mode)))

(use-package flycheck
 :ensure t)

(use-package rainbow-delimiters
 :ensure t
 :init
 (progn
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package shut-up
 :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package aggressive-indent
  :ensure t
  :init
  (progn
    (global-aggressive-indent-mode 1)))


(use-package indent-guide
  :ensure t
  :init
  (progn
    (indent-guide-global-mode)
    (setq indent-guide-recursive t)))

(use-package evil-leader
  :ensure t
  :init
  (progn
    (global-evil-leader-mode 1)
    (evil-leader/set-leader "<SPC>")))

(use-package ace-jump-mode
  :ensure t
  :init
  (progn
    (evil-leader/set-key "<SPC>" 'evil-ace-jump-char-mode)))

(use-package all-the-icons 
	:ensure t
	:defer 0.5)

(use-package all-the-icons-ivy
	:ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package company
	:ensure t
	:diminish ""
	:init
	:config
	(global-company-mode)
  (company-tng-configure-default)
	(setq company-tooltip-limit 10)
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

(use-package company-web
  :ensure t
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package company-tern
 :ensure t
 :config
 (add-to-list 'company-backends 'company-tern)
 (add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (commpany-mode))))

(use-package all-the-icons-dired :ensure t)

(use-package lsp-mode
	:ensure t
  :init (setq lsp-keymap-prefix "s-l")
  :hook (
				 (web-mode . lsp)
         (js2-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode
 :ensure t)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(global-display-line-numbers-mode)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(set-frame-font "Input Mono Compressed 14" nil t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq-default tab-width 2)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(load-theme 'gruvbox t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" default))
 '(package-selected-packages
	 '(tide doom-modeline shut-up rainbow-delimiters which-key web-mode use-package rjsx-mode gruvbox-theme flycheck evil emmet-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
