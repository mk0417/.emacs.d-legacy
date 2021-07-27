;;; init-project.el --- Project management -*- lexical-binding: t -*-

(straight-use-package 'projectile)
(straight-use-package 'rg)
(straight-use-package 'citre)


;; projectile
(add-hook 'after-init-hook 'projectile-mode)

;; project search path
(setq projectile-project-search-path '("~/Git/"))

;; shorter modeline
(setq-default projectile-mode-line-prefix " Proj")

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(when (executable-find "rg")
  (setq-default projectile-generic-command "rg --files --hidden"))

;; citre
(defun citre-jump+ ()
  "Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
  (interactive)
  (condition-case _ (citre-jump)
    (error (call-interactively #'xref-find-definitions))))

(add-hook 'prog-mode #'citre-auto-enable-citre-mode)

(setq citre-readtags-program "/usr/local/bin/readtags"
      citre-ctags-program "/usr/local/bin/ctags"
      citre-completion-case-sensitive nil
      citre-use-project-root-when-creating-tags t
      citre-prompt-language-for-ctags-command t)

(with-eval-after-load 'projectile
  (setq citre-project-root-function #'projectile-project-root)
  (diminish 'citre-mode))

;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "p"   '(:ignore t :which-key "projects and packages")
    "pp"  '(projectile-switch-project :which-key "switch project")
    "pf"  '(projectile-find-file :which-key "projectile find file")))

(with-eval-after-load 'evil
  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "c"  '(:ignore t :which-key "citre")
    "cc" '(citre-update-this-tags-file :which-key "create or update tags file")
    "cC" '(citre-create-tags-file :which-key "re-create tags file")
    "ce" '(citre-edit-tags-file-recipe :which-key "edit tags file recipe")
    "cd" '(citre-jump :which-key "citre jump")
    "cj" '(citre-jump+ :which-key "citre jump+")
    "cb" '(citre-jump-back :which-key "citre jump back")
    "ca" '(citre-ace-peek :which-key "citre ace peek")
    "cp" '(citre-peek :which-key "citre peek")))


(provide 'init-project)
;;; init-project.el ends here
