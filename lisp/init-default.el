;;; init-default.el --- Better default -*- lexical-binding: t -*-

(straight-use-package 'diminish)
(straight-use-package 'which-key)


;; initial scratch buffer message
(setq initial-scratch-message
      (concat ";; Hello Peng, welcome to EMACS and happy hacking\n"
              (format ";; Emacs version: %s\n" (car (split-string emacs-version)))))

;; startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))))))

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (>= emacs-major-version 28)
    (setq comp-deferred-compilation t
          comp-async-jobs-number 6
          comp-async-report-warnings-errors nil
          native-comp-async-report-warnings-errors nil
          compilation-scroll-output t
          package-native-compile t
          load-prefer-newer t))

;; disable bell sound
(setq ring-bell-function 'ignore)

;; scroll
(setq
 ;; disable automatic cursor re-centering
 ;; https://www.reddit.com/r/emacs/comments/2dgy52/how_to_stop_emacs_automatically_recentering_the/
 scroll-step 1
 ;; https://github.com/hlissner/doom-emacs/blob/develop/core/core-ui.el
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 101
 scroll-preserve-screen-position t
 auto-window-vscroll nil)

;; preferences
(setq-default window-resize-pixelwise nil
              frame-resize-pixelwise t
              buffers-menu-max-size 30
              bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
              mouse-yank-at-point t
              tab-width 4
              case-fold-search t
              column-number-mode t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              indent-tabs-mode nil
              create-lockfiles nil
              auto-save-default nil
              make-backup-files nil
              save-interprogram-paste-before-kill t
              set-mark-command-repeat-pop t
              truncate-lines nil
	          truncate-partial-width-windows nil
              whitespace-line-column nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; recentf-mode
(add-hook 'after-init-hook 'recentf-mode)
;;(setq-default recentf-max-saved-items 50
;;              recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/" "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(setq-default recentf-max-saved-items 50
              recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/" "/tmp/" "/ssh:"))
(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa.*/.*" (getenv "HOME")))

;; show-paren-mode
(add-hook 'after-init-hook 'show-paren-mode)

;; rainbow-delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; trash
(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
(when (memq window-system '(mac ns))
  (setq ns-option-modifier 'nil
        ns-right-option-modifier 'meta
        mac-command-modifier 'super)
  (defun system-move-file-to-trash (path)
    (shell-command (concat "trash -vF \"" path "\"" "| sed -e 's/^/Trashed: /'")
                   nil
                   "*Trash Error Buffer*")))

;; key modification for mac
(when (memq window-system '(mac ns))
  (setq ns-option-modifier 'nil
        ns-right-option-modifier 'meta
        mac-command-modifier 'super))

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-u") 'universal-argument)
(global-set-key (kbd "C-c C-e") 'occur-edit-mode)
(define-key minibuffer-local-map (kbd "C-k") 'delete-backward-char)

(add-hook 'after-init-hook 'which-key-mode)
(setq-default which-key-idle-delay 0.8)
(with-eval-after-load 'which-key
  (diminish 'which-key-mode))

(diminish 'eldoc-mode)

;; disable cursor movement in minibuffer
(setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(provide 'init-default)
;;; init-default.el ends here
