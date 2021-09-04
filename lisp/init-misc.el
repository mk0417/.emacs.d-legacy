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
(defun puni-mark-sexp-around-point ()
  (interactive)
  (let (beg end)
    (save-excursion
      (when (setq beg (puni-up-list 'backward))
        (setq end (puni-strict-forward-sexp))))
    (when (and beg end)
      (set-mark beg)
      (goto-char end)
      (activate-mark))))

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

;; https://github.com/AmaiKinono/puni/wiki/The-Puni-version-of-expand-region
(defun puni--bounds-of-sexp-at-point ()
  (save-excursion
    (let ((end (or (puni-strict-forward-sexp)
                   (point)))
          (beg (or (puni-strict-backward-sexp)
                   (point))))
      (cons beg end))))

(defun puni--bounds-of-sexps-around-point ()
  (save-excursion
    (let ((beg (progn (puni-beginning-of-sexp)
                      (point)))
          (end (progn (puni-end-of-sexp)
                      (point))))
      (cons beg end))))

(defun puni--find-bigger-balanced-region (beg end)
  (let (new-beg new-end)
    (save-excursion
      (goto-char beg)
      (setq new-beg (puni-up-list 'backward))
      (setq new-end (puni-strict-forward-sexp))
      (if (and new-beg new-end)
          (cons new-beg new-end)
        (cons beg end)))))

(defun puni--set-region (beg end)
  (if (and (use-region-p)
           (> (mark) (point)))
      (progn (set-mark end)
             (goto-char beg)
             (activate-mark))
    (set-mark beg)
    (goto-char end)
    (activate-mark)))

(defun puni--expand-region (beg end)
  (pcase-let*
      ((`(,sexp-beg . ,sexp-end) (save-excursion
                                   (goto-char beg)
                                   (puni--bounds-of-sexp-at-point)))
       (`(,sexps-beg . ,sexps-end) (save-excursion
                                     (goto-char beg)
                                     (puni--bounds-of-sexps-around-point))))
    (cond
     ((<= beg sexps-beg)
      (let ((region (puni--find-bigger-balanced-region beg end)))
        (puni--set-region (car region) (cdr region))))
     ((and (< sexps-beg beg) (<= beg sexp-beg))
      (puni--set-region sexps-beg sexps-end))
     (t
      (puni--set-region sexp-beg sexp-end)))))

(defun puni-expand-region ()
  (interactive)
  (if (use-region-p)
      (puni--expand-region (region-beginning) (region-end))
    (pcase-let
        ((`(,sexp-beg . ,sexp-end) (puni--bounds-of-sexp-at-point)))
      (if (eq sexp-beg sexp-end)
          (puni--expand-region sexp-beg sexp-end)
        (puni--set-region sexp-beg sexp-end)))))


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
    "rf" '(anzu-query-replace-at-cursor-thing :which-key "query-replace-at-point-func")
    "p"  '(:ignore t :which-key "puni")
    "pf" '(puni-forward-sexp :which-key "puni-forward-sexp")
    "pb" '(puni-backward-sexp :which-key "puni-backward-sexp")
    "pa" '(puni-beginning-of-sexp :which-key "puni-beginning-of-sexp")
    "pe" '(puni-end-of-sexp :which-key "puni-end-of-sexp")
    "p," '(puni-syntactic-forward-punct :which-key "puni-syntactic-forward-punct")
    "p." '(puni-syntactic-backward-punct :which-key "puni-syntactic-backward-punct")
    "pk" '(puni-kill-line :which-key "puni-kill-line")
    "pp" '(puni-expand-region :which-key "puni-expand-region")
    "pw" '(puni-backward-kill-word :which-key "puni-backward-kill-word")
    "pe" '(puni-forward-kill-word :which-key "puni-forward-kill-word")
    "pm" '(puni-mark-sexp-around-point :which-key "puni-mark-sexp-around-point")
    "pd" '(puni-my-kill-line :which-key "puni-my-kill-line")))


(provide 'init-misc)
;;; init-misc.el ends here
