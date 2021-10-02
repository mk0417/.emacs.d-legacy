;; init-window.el --- Window management -*- lexical-binding: t -*-

(straight-use-package 'winum)
(straight-use-package 'switch-window)
(straight-use-package 'popper)


;; winum
(add-hook 'after-init-hook 'winum-mode)

;; switch-window
(setq-default switch-window-shortcut-style 'alphabet
              switch-window-timeout nil)

;; popper
(setq popper-mode-line
      '(:eval (propertize " POP" 'face 'mode-line-emphasis)))

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Warnings\\*$"
        "\\*Python Doc\\*$"
        "\\*Backtrace\\*$"
        "\\*Shell Command Output\\*$"
        "\\*Compile-Log\\*$"
        "\\*Async Shell Command\\*"
        "^\\*jupyter-"
        help-mode
        debugger-mode
        compilation-mode))

(popper-mode 1)

(defun p-popper-window-height (win)
  ((fit-window-to-buffer
    win
    (floor (frame-height) 5))))

(setq popper-window-height 'p-popper-window-height)

(add-to-list 'display-buffer-alist
             '("\\*Python Doc\\*$"
               (display-buffer-in-side-window)
               (side . right)
               (window-width 50)))


;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "w"  '(:ignore t :which-key "window")
    "wr" '(balance-windows :which-key "balance window")
    ","  '(popper-toggle-latest :which-key "popper latest")
    "'"  '(popper-cycle :which-key "popper cycle")))


(provide 'init-window)
;;; init-window.el ends here
