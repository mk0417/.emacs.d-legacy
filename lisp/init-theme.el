;;; init-theme.el --- Defaults for themes -*- lexical-binding: t -*-

(straight-use-package 'modus-themes)

(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-links '(faint bold background)
      modus-themes-prompts '(bold background)
      modus-themes-mode-line '(moody)
      modus-themes-completions 'opinionated
      modus-themes-fringes 'intense
      modus-themes-hl-line 'intense
      modus-themes-paren-match '(intense bold underline)
      modus-themes-region '(no-extend accented))

(setq modus-themes-org-agenda
      '((header-block . (variable-pitch scale-title))
        (header-date . (grayscale bold-today))
        (scheduled . rainbow)))

(setq modus-themes-scale-headings t
      modus-themes-scale-4 1.3
      modus-themes-scale-3 1.2
      modus-themes-scale-2 1.1
      modus-themes-org-blocks 'gray-background)

(setq modus-themes-headings '((t . line)))

(setq x-underline-at-descent-line t)

(modus-themes-load-themes)
(modus-themes-load-vivendi)

;; header-line background
(defun p-modus-header-line-bg ()
  (interactive)
  (let ((mytheme (symbol-name (car custom-enabled-themes))))
    (when (equal mytheme "modus-vivendi")
      (custom-set-faces
       '(header-line ((t (:background "#2c2c2c"))))))
    (when (equal mytheme "modus-operandi")
      (custom-set-faces
       '(header-line ((t (:background "#d5d5d5"))))))))

(defvar after-load-theme-hook nil)
(defadvice load-theme (after run-after-load-theme-hook activate)
  (run-hooks 'after-load-theme-hook))

(add-hook 'after-init-hook 'p-modus-header-line-bg)
(add-hook 'after-load-theme-hook 'p-modus-header-line-bg)


(provide 'init-theme)
;;; init-theme.el ends here
