;;; init-dired.el --- Dired -*- lexical-binding: t -*-

(straight-use-package 'diredfl)


(require 'dired)

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(diredfl-global-mode)

(with-eval-after-load 'dired
  (general-create-definer p-dired-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'dired-mode-map)
  (p-dired-leader-def
    "m"  '(dired-mark-files-regexp :which-key "mark file by regex")
    "M"  '(dired-mark-files-containing-regexp :which-key "mark file containing by regex")
    "c"  '(dired-do-copy :which-key "copy file")
    "r"  '(dired-rename-file :which-key "rename file")
    "p"  '(dired-up-directory :which-key "parent directory")
    "n"  '(dired-create-empty-file :which-key "add new file")
    "N"  '(dired-create-directory :which-key "add new directory")
    "i"  '(dired-find-file :which-key "enter directory")))


(provide 'init-dired)
;;; init-dired.el ends here
