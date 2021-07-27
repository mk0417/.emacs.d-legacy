;;; init-git.el --- Git -*- lexical-binding: t -*-

(straight-use-package 'diff-hl)
(straight-use-package 'magit)
(straight-use-package 'git-messenger)


;; magit
(setq-default magit-diff-refine-hunk t)

(autoload 'magit-status "magit")
(autoload 'magit-diff "magit")
(autoload 'magit-blame "magit")

;;; diff-hl
(autoload 'diff-hl-mode "diff-hl")

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'conf-mode-hook 'diff-hl-mode)

;; git-messenger
(setq git-messenger:show-detail t
      git-messenger:use-magit-popup t)

;; keybindings
(with-eval-after-load 'git-messenger
  (define-key git-messenger-map (kbd "y") 'git-messenger:copy-message))

(with-eval-after-load 'diff-hl
  (define-key evil-normal-state-map (kbd "gn") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "gp") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "gP") 'diff-hl-diff-goto-hunk))

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit :which-key "magit")
    "gm" '(magit-dispatch :which-key "magit dispatch")
    "gs" '(magit-show-commit :which-key "magit show commit")
    "gp" '(git-messenger:popup-message :which-key "git message")))


(provide 'init-git)
;;; init-git.el ends here
