;;; init-ui.el --- UI config  -*- lexical-binding: t; -*-

;; minimal UI
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(menu-bar-mode -1)

;; header-line
(defun p-header-line-off ()
  (interactive)
  (setq-default header-line-format nil))

(defun p-header-line-init ()
  (interactive)
  (if (equal buffer-file-truename nil)
      (setq-default header-line-format '("[%F]"))
    (setq-default header-line-format (list "[%F]" '(:eval (format "  %s" buffer-file-truename))))))

(add-hook 'buffer-list-update-hook 'p-header-line-init)

;; font
(set-face-font 'default "Iosevka Comfy")

;; no fringe
(fringe-mode '(0 . 0))

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


(provide 'init-ui)
;;; init-ui.el ends here
