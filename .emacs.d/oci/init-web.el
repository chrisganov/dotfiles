;;;
;;; Configuration for editing html, js and css
;;;

;;; COMPANY ====================================
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-align-annotations t))

;;; HELM ====================================
(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

;;; PROJECTILE ====================================
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))


;;; PRETTIER ====================================
(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es6"
                           "--single-quote" "true"
                           "--print-width" "120"
                           "--tab-width" "2"
                           "--use-tabs" "false"
                           "--jsx-bracket-same-line" "false"
                           "--stylelint-integration" "true"
                           )))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))
;;; WEBMODE ====================================
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
 
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
	web-mode-enable-auto-indentation nil
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (add-hook 'web-mode-hook 'electric-pair-mode))

(defun surround-html (start end tag)
   "Wraps the specified region (or the current 'symbol/ word'
 with a properly formatted HTML tag."
   (interactive "r\nsTag: " start end tag)
   (save-excursion
     (narrow-to-region start end)
     (goto-char (point-min))
     (insert (format "<%s>" tag))
     (goto-char (point-max))
     (insert (format "</%s>" tag))
     (widen)))

;;; EMMET ====================================
(use-package emmet-mode
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point)
          ("C-." . emmet-expand-line))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;; MARKDOWN ====================================
(use-package markdown-mode
  :defer 1
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; TYPESCRIPT ====================================
(defun setup-tide-mode()
  (interactive)
  (defun tide-imenu-index () nil)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :mode(("\\.ts\\'" . typescript-mode))
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :init
  (add-hook 'typescript-mode-hook 'tide-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  :config
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

(use-package json-mode
  :mode "\\.json\\'" 
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
    (setq js2-basic-offset 2
          js2-bounce-indent-p t
          js2-strict-missing-semi-warning t
          js2-concat-multiline-strings nil
          js2-include-node-externs t
          js2-skip-preprocessor-directives t
          js2-strict-inconsistent-return-warning t))

;; JSX
; (use-package rjsx-mode
;   :ensure t
;   :mode(("\\.js\\'" . rjsx-mode)
;         ("\\.jsx\\'" . rjsx-mode))
;   :init
;   (add-hook 'rjsx-mode-hook 'prettier-js-mode)
;   (add-hook 'rjsx-mode-hook 'tide-mode))

;; Yaml
(use-package yaml-mode
  :defer 1)

;; Flycheck
(use-package flycheck
  :defer 1
  :init (setq
         flycheck-checkers
         '(typescript-tide
           javascript-tide
           jsx-tide
           javascript-eslint
           css-csslint
           emacs-lisp
           haml
           json-jsonlint
           yaml-jsyaml))
  :config
  (global-flycheck-mode)
  (progn (flycheck-add-mode 'javascript-eslint 'web-mode)))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package hl-todo-mode
  :ensure t
  :config
  (setq hl-todo-keyword-faces
	'(("TODO" . "#FF0000")
	  ("FIXME" . "#FF0000"))))

(provide 'init-web)
;;; init-web.el ends here
