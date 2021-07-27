;; init-window.el --- Window management -*- lexical-binding: t -*-

(straight-use-package 'winum)
(straight-use-package 'switch-window)


;; winum
(add-hook 'after-init-hook 'winum-mode)

;; switch-window
(setq-default switch-window-shortcut-style 'alphabet
              switch-window-timeout nil)

;; keybindings
(global-set-key (kbd "C-x o") 'switch-window)

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "1"  '(winum-select-window-1 :which-key "select window 1")
    "2"  '(winum-select-window-2 :which-key "select window 2")
    "3"  '(winum-select-window-3 :which-key "select window 3")
    "4"  '(winum-select-window-4 :which-key "select window 4")
    "5"  '(winum-select-window-5 :which-key "select window 5")
    "w"  '(:ignore t :which-key "window")
    "ww" '(switch-window :which-key "switch window")))


(provide 'init-window)
;;; init-window.el ends here
