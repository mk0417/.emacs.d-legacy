;; init-window.el --- Window management -*- lexical-binding: t -*-

(straight-use-package 'winum)
(straight-use-package 'switch-window)


;; winum
(add-hook 'after-init-hook 'winum-mode)

;; switch-window
(setq-default switch-window-shortcut-style 'alphabet
              switch-window-timeout nil)

;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "w"  '(:ignore t :which-key "window")
    "wr" '(balance-windows :which-key "balance window")))


(provide 'init-window)
;;; init-window.el ends here
