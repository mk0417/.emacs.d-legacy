;;; init-latex.el --- Latex config -*- lexical-binding: t -*-

(straight-use-package 'auctex)
(straight-use-package 'latex-preview-pane)


(with-eval-after-load 'evil
  (general-create-definer p-latex-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'LaTeX-mode-map)
  (p-latex-leader-def
   "p"  '(:ignore t :which-key "latex preview")
   "pp" '(latex-preview-pane-mode :which-key "toggle latex preview pane")))


(provide 'init-latex)
;;; init-latex.el ends here
