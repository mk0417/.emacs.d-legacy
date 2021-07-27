;;; init-markdown.el --- Markdown -*- lexical-binding: t -*-

(straight-use-package 'markdown-mode)


(add-to-list 'auto-mode-alist '("\\.md\\.html\\'"))

(setq markdown-italic-underscore t
      markdown-asymmetric-header t
      markdown-fontify-code-blocks-natively t)

(with-eval-after-load 'whitespace-cleanup-mode
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))


(provide 'init-markdown)
;;; init-markdown.el ends here
