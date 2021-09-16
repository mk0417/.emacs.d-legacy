;;; init-tree-sitter.el --- Emacs tree-sitter -*- lexical-binding: t -*-

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package '(evil-textobj-tree-sitter
                        :type git
                        :host github
                        :repo "meain/evil-textobj-tree-sitter"
                        :files (:defaults "queries")))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(diminish 'tree-sitter-mode)

(global-tree-sitter-mode)

;; enable highlight
;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(define-key evil-outer-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "i" (evil-textobj-tree-sitter-get-textobj "function.inner"))
(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
(define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
(define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
(define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))


(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
