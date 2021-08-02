;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-

(straight-use-package 'esup)
(straight-use-package 'whitespace-cleanup-mode)
(straight-use-package 'anzu)
(straight-use-package 'smartparens)
(straight-use-package 'avy)
(straight-use-package 'htmlize)
(straight-use-package '(color-rg
                        :type git
                        :host github
                        :repo "manateelazycat/color-rg"))


;; whitespace
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(with-eval-after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(defun p-show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(dolist (h '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook h 'p-show-trailing-whitespace))

;; anzu
(setq anzu-mode-lighter ""
      anzu-replace-to-string-separator " => ")
(add-hook 'after-init-hook 'global-anzu-mode)

;; smartparens
(smartparens-global-mode t)

(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode)

  (sp-local-pair 'ess-stata-mode "`" "'")
  (sp-with-modes '(lisp-mode emacs-lisp-mode lisp-interaction-mode)
    (sp-local-pair "'" nil :actions nil))

  (defun p-add-paren ()
    (interactive)
    (sp-wrap-with-pair "("))

  (defun p-add-bracket ()
    (interactive)
    (sp-wrap-with-pair "["))

  (defun p-add-curly ()
    (interactive)
    (sp-wrap-with-pair "{")))

;; avy
(setq avy-background t
      avy-style 'post
      avy-styles-alist '((avy-goto-line . pre)))

;; color-rg
(autoload 'color-rg-search-input-in-current-file "color-rg")
(autoload 'color-rg-search-symbol-in-current-file "color-rg")
(autoload 'color-rg-search-input "color-rg")
(autoload 'color-rg-search-symbol "color-rg")
(autoload 'color-rg-search-input-in-project "color-rg")
(autoload 'color-rg-search-symbol-in-project "color-rg")
(autoload 'color-rg-mode "color-rg")

(setq color-rg-mac-load-path-from-shell nil)

(with-eval-after-load 'evil
  (evil-set-initial-state 'color-rg-mode 'emacs))

;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "nc")  'avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "nl")  'avy-goto-line)
  (define-key evil-normal-state-map (kbd "ny")  'avy-kill-ring-save-region)
  (define-key evil-normal-state-map (kbd "goo") 'sp-raise-sexp)
  (define-key evil-normal-state-map (kbd "got") 'sp-transpose-sexp)
  (define-key evil-normal-state-map (kbd "goj") 'sp-join-sexp)
  (define-key evil-normal-state-map (kbd "gos") 'sp-split-sexp)
  (define-key evil-normal-state-map (kbd "nd")  'sp-down-sexp)
  (define-key evil-normal-state-map (kbd "nD")  'sp-up-sexp)

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "d" '(sp-splice-sexp :which-key "sp-splice-sexp")
    "k" '(p-add-paren :which-key "p-add-paren")
    "f" '(p-add-bracket :which-key "p-add-bracket")
    "h" '(p-add-curly :which-key "p-add-curly")
    "s"  '(:ignore t :which-key "color rg")
    "sf" '(color-rg-search-input-in-current-file :which-key "search input in file")
    "sF" '(color-rg-search-symbol-in-current-file :which-key "search symbol in file")
    "sd" '(color-rg-search-input :which-key "search input in directory")
    "sD" '(color-rg-search-symbol :which-key "search symbol in directory")
    "sp" '(color-rg-search-input-in-project :which-key "search input in project")
    "sP" '(color-rg-search-symbol-in-project :which-key "search symbol in project")
    "r"  '(:ignore t :which-key "find and replace")
    "rr" '(anzu-query-replace-regexp :which-key "query-replace-regex")
    "rR" '(anzu-query-replace :which-key "query-replace")
    "rb" '(anzu-query-replace-at-cursor :which-key "query-replace-at-point")
    "rf" '(anzu-query-replace-at-cursor-thing :which-key "query-replace-at-point-func")))


(provide 'init-misc)
;;; init-misc.el ends here
