;; init-window.el --- Window management -*- lexical-binding: t -*-

(straight-use-package 'winum)
(straight-use-package 'switch-window)


;; winum
(add-hook 'after-init-hook 'winum-mode)

;; switch-window
(setq-default switch-window-shortcut-style 'alphabet
              switch-window-timeout nil)


(provide 'init-window)
;;; init-window.el ends here
