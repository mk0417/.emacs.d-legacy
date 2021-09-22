;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-

(straight-use-package 'esup)
(straight-use-package 'whitespace-cleanup-mode)
(straight-use-package 'anzu)
(straight-use-package 'smartparens)
(straight-use-package 'avy)
(straight-use-package 'htmlize)
(straight-use-package 'elisp-demos)
(straight-use-package '(color-rg
                        :type git
                        :host github
                        :repo "manateelazycat/color-rg"))
(straight-use-package '(puni
                        :type git
                        :host github
                        :repo "AmaiKinono/puni"))


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

  (sp-with-modes '(markdown-mode)
    (sp-local-pair "`" nil :actions nil))

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

;; elisp-demos
(advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)

;; puni
(puni-global-mode)

;; overwrite puni default keybindings
(define-key puni-mode-map (kbd "C-k") nil)
(define-key puni-mode-map (kbd "C-w") nil)

(autoload 'puni-strict-forward-sexp "puni")
(autoload 'puni-soft-delete-by-move "puni")

;; https://github.com/AmaiKinono/puni/wiki/Useful-commands
(defun puni-my-kill-line ()
  (interactive)
  (let ((forward-line (lambda ()
                        (if (eolp) (forward-char) (end-of-line))))
        (backward-line (lambda ()
                         (if (bolp) (forward-char -1) (beginning-of-line)))))
    (and
     (or (puni-soft-delete-by-move forward-line 'strict-sexp 'beyond 'kill)
         (puni-soft-delete-by-move backward-line 'strict-sexp 'beyond 'kill)
         (save-excursion
           (let (beg end)
             (when (setq beg (puni-up-list 'backward))
               (setq end (puni-strict-forward-sexp)))
             (when (and beg end)
               (puni-delete-region beg end 'kill))))))))


;; keybindings
(global-set-key (kbd "C-c M-f") 'anzu-query-replace)
(global-set-key (kbd "C-c M-r") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c M-a") 'anzu-query-replace-at-cursor)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "nc")  'avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "nl")  'avy-goto-line)
  (define-key evil-normal-state-map (kbd "ny")  'avy-kill-ring-save-region)
  (define-key evil-normal-state-map (kbd "goo") 'puni-raise)
  (define-key evil-normal-state-map (kbd "got") 'puni-transpose)
  (define-key evil-normal-state-map (kbd "gos") 'puni-split)
  (define-key evil-normal-state-map (kbd "gov") 'puni-convolute)
  (define-key evil-normal-state-map (kbd "gof") 'puni-barf-forward)
  (define-key evil-normal-state-map (kbd "gob") 'puni-barf-backward)
  (define-key evil-normal-state-map (kbd "gol") 'puni-forward-sexp)
  (define-key evil-normal-state-map (kbd "goh") 'puni-backward-sexp)
  (define-key evil-normal-state-map (kbd "nd")  'puni-syntactic-forward-punct)
  (define-key evil-normal-state-map (kbd "nD")  'puni-syntactic-backward-punct)

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "d" '(puni-splice :which-key "puni-splice")
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
    "rf" '(anzu-query-replace-at-cursor-thing :which-key "query-replace-at-point-func")
    "p"  '(:ignore t :which-key "puni")
    "pa" '(puni-beginning-of-sexp :which-key "puni-beginning-of-sexp")
    "pe" '(puni-end-of-sexp :which-key "puni-end-of-sexp")
    "pk" '(puni-kill-line :which-key "puni-kill-line")
    "pp" '(puni-expand-region :which-key "puni-expand-region")
    "pb" '(puni-backward-kill-word :which-key "puni-backward-kill-word")
    "pw" '(puni-forward-kill-word :which-key "puni-forward-kill-word")
    "pm" '(puni-mark-sexp-around-point :which-key "puni-mark-sexp-around-point")
    "p." '(puni-mark-sexp-at-point :which-key "puni-mark-sexp-at-point")
    "p," '(puni-mark-list-around-point :which-key "puni-mark-list-at-point")
    "ps" '(puni-squeeze :which-key "puni-squeeze")
    "pd" '(puni-my-kill-line :which-key "puni-my-kill-line")))


(provide 'init-misc)
;;; init-misc.el ends here
