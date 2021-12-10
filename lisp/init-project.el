;;; init-project.el --- Project management -*- lexical-binding: t -*-

(straight-use-package 'rg)
(straight-use-package 'citre)


;; citre
(require 'citre-config)

(defun citre-jump+ ()
  (interactive)
  (condition-case _ (citre-jump)
    (error (call-interactively #'xref-find-definitions))))

(setq citre-readtags-program (executable-find "readtags")
      citre-ctags-program (executable-find "ctags")
      citre-auto-enable-citre-mode-modes '(prog-mode)
      citre-completion-case-sensitive nil
      citre-use-project-root-when-creating-tags t
      citre-prompt-language-for-ctags-command t)

(with-eval-after-load 'citre
  (diminish 'citre-mode))


;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "p"   '(:ignore t :which-key "projects and packages")
    "pp"  '(project-switch-project :which-key "switch project")
    "pb"  '(project-switch-to-buffer :which-key "switch buffer in project")
    "pf"  '(project-find-file :which-key "project find file")))

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
