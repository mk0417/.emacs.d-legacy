;;; init-ui.el --- UI config  -*- lexical-binding: t; -*-

;; minimal UI
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
;; enable menu bar otherwise yabai cannot caputre Emacs
;; (menu-bar-mode -1)

;; header-line
;; (defun p-header-line-off ()
;;   (interactive)
;;   (setq-default header-line-format nil))

;; (defun p-header-line-init ()
;;   (interactive)
;;   (if (equal buffer-file-truename nil)
;;       (setq-default header-line-format '("[%F]"))
;;     (setq-default header-line-format (list "[%F]" '(:eval (format "  %s" buffer-file-truename))))))

;; (add-hook 'buffer-list-update-hook 'p-header-line-init)

;; modeline
(require 'prot-modeline)
;; full path in mode-line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    '(:eval (format "  %s" buffer-file-truename))))

;; maximize frame at startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; no fringe
(fringe-mode '(0 . 0))
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default overflow-newline-into-fringe t)

;; highlight current line
(global-hl-line-mode 1)

;; favor vertical split
(setq split-height-threshold nil)

;; disable face of completions-first-difference
(custom-set-faces
   '(completions-first-difference ((t (:background nil :weight normal)))))

;; line number
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; column indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default display-fill-column-indicator-column 80
                display-fill-column-indicator-character ?\u254e))

;;cursor type in minibuffer
(defun p-minibuffer-cursor-type ()
  (setq cursor-type 'hbar))
(add-hook 'minibuffer-setup-hook 'p-minibuffer-cursor-type)

;; cursor blink
(setq blink-cursor-delay 0.2
      blink-cursor-interval 0.3
      blink-cursor-blinks 30)

;; titlebar
(push '(ns-transparent-titlebar . t) default-frame-alist)

(if (< emacs-major-version 28)
    (setq frame-title-format
          '((:eval (if (buffer-file-name)
                       (concat " " (abbreviate-file-name (buffer-file-name)))
                     " %b"))))
  ;; https://emacs.stackexchange.com/questions/33680/how-to-remove-the-icon-in-the-titlebar
  (setq frame-title-format nil
        us-use-proxy-icon nil))

;; font
;; (set-face-font 'default "Iosevka Comfy")

(setq prot-fonts-typeface-sets-alist
      '((small . ( :fixed-pitch-family "Hack"
                   :fixed-pitch-regular-weight regular
                   :fixed-pitch-heavy-weight bold
                   :fixed-pitch-height 75
                   :fixed-pitch-line-spacing 1
                   :variable-pitch-family "FiraGO"
                   :variable-pitch-height 1.05
                   :variable-pitch-regular-weight normal))
        (regular . ( :fixed-pitch-family "Hack"
                     :fixed-pitch-regular-weight regular
                     :fixed-pitch-heavy-weight bold
                     :fixed-pitch-height 105
                     :fixed-pitch-line-spacing nil
                     :variable-pitch-family "FiraGO"
                     :variable-pitch-height 1.05
                     :variable-pitch-regular-weight normal))
        (large . ( :fixed-pitch-family "Hack"
                   :fixed-pitch-regular-weight normal
                   :fixed-pitch-heavy-weight bold
                   :fixed-pitch-height 130
                   :fixed-pitch-line-spacing nil
                   :variable-pitch-family "FiraGO"
                   :variable-pitch-height 1.05
                   :variable-pitch-regular-weight normal))
        (large-alt . ( :fixed-pitch-family "Iosevka Comfy"
                       :fixed-pitch-regular-weight book
                       :fixed-pitch-heavy-weight extrabold
                       :fixed-pitch-height 155
                       :fixed-pitch-line-spacing nil
                       :variable-pitch-family "Noto Sans"
                       :variable-pitch-height 1.0
                       :variable-pitch-regular-weight normal))))

(autoload 'prot-fonts-restore-last "prot-fonts")
(autoload 'prot-fonts-fonts-per-monitor "prot-fonts")
(autoload 'prot-fonts-laptop-desktop-keys "prot-fonts")

(setq prot-fonts-laptop-desktop-keys-list (prot-fonts-laptop-desktop-keys))
(prot-fonts-fonts-per-monitor)
;; (setq prot-fonts-max-small-resolution-width 1366)
(define-key global-map (kbd "C-c f") #'prot-fonts-set-fonts)
(add-hook 'modus-themes-after-load-theme-hook #'prot-fonts-restore-last)


(provide 'init-ui)
;;; init-ui.el ends here
