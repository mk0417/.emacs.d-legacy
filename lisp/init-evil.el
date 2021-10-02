;;; init-evil.el --- Evil-mode/Vim -*- lexical-binding: t -*-

(straight-use-package 'evil)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'general)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-escape)
(straight-use-package 'evil-goggles)


;; evil
(setq evil-undo-system 'undo-redo
      evil-symbol-word-search t
      evil-respect-visual-line-mode t
      evil-want-C-u-scroll t)
(add-hook 'after-init-hook 'evil-mode)

(with-eval-after-load 'evil
  (setq evilmi-shortcut "m")
  (dolist (hook '(prog-mode-hook markdown-mode-hook org-mode-hook))
    (add-hook hook 'turn-on-evil-matchit-mode))

  (global-evil-surround-mode 1)

  (setq evil-escape-excluded-states '(normal multiedit emacs motion))
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "fd")
  (diminish 'evil-escape-mode)

  ;; goggles
  ;; change and delete are sluggish, so disable them
  ;; https://github.com/edkolev/evil-goggles/issues/18
  (setq evil-goggles-pulse t
        evil-goggles-duration 0.35
        evil-goggles-enable-substitute nil
        evil-goggles-enable-change nil
        evil-goggles-enable-delete nil)
  (evil-goggles-mode)
  (diminish 'evil-goggles-mode)
  (custom-set-faces
   '(evil-goggles-paste-face ((t (:background "#cf5a65"))))
   '(evil-goggles-yank-face ((t (:background "#cf5a65")))))

  ;; change cursor type and color
  ;; https://github.com/hlissner/doom-emacs/issues/1848
  (setq evil-normal-state-cursor '(box "#cf5a65")
        evil-insert-state-cursor '(hbar "#00ff00")
        evil-visual-state-cursor '(hollow "#cf5a65"))

  ;; stay normal state in occur buffer
  (dolist (mode '(occur-mode help-mode debugger-mode))
    (evil-set-initial-state mode 'normal))

  ;; ex-evil replace
  (defun p-ex-evil-buffer-replace ()
    (interactive)
    (evil-ex (concat "%s/")))

  (defun p-ex-evil-selection-replace ()
    (interactive)
    (evil-ex (concat "'<,'>s/")))

  ;; https://philjackson.github.io//evil/emacs/2021/07/11/start-evil-substitution-on-selection/
  (evil-define-operator start-ex-sub-on-region (beg end)
    (let ((region (buffer-substring beg end)))
      (evil-ex (concat "%s/" (replace-regexp-in-string "\/" "\\\\/" (regexp-quote region)) "/"))))


  (define-key evil-normal-state-map (kbd "S-c") nil)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "gl")  'evil-shift-right)
  (define-key evil-normal-state-map (kbd "gh")  'evil-shift-left)
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "gcy") 'evilnc-copy-and-comment-lines)
  (define-key evil-normal-state-map (kbd "gor") 'p-ex-evil-buffer-replace)
  (define-key evil-normal-state-map (kbd "gox") 'start-ex-sub-on-region)

  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-b") 'backward-char)
  (define-key evil-visual-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gcy") 'evilnc-copy-and-comment-lines)
  (define-key evil-visual-state-map (kbd "gor") 'p-ex-evil-selection-replace)
  (define-key evil-visual-state-map (kbd "goc") 'start-ex-sub-on-region)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-ex-completion-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)

  (evil-define-key 'normal 'global
    "Q" "@q")
  (evil-define-key 'visual 'global
    "Q" (kbd ":norm @q RET"))

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "SPC" 'execute-extended-command
    "f"  '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save buffer")
    "fo" '(find-file-other-window :which-key "open file in another window")
    "fc" '(copy-file :which-key "copy file")
    "b"  '(:ignore t :which-key "buffer")
    "bd" '(kill-this-buffer :which-key "kill buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bD" '(kill-buffer-and-window :which-key "kill buffer and window")
    "br" '(revert-buffer :which-key "revert buffer")
    "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "switch to scratch")
    "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "switch to messages")
    "§"  '((lambda () (interactive) (switch-to-buffer nil)) :which-key "switch to pervious buffer")
    "`"  '((lambda () (interactive) (switch-to-buffer nil)) :which-key "switch to pervious buffer")
    "d"  '(:ignore t :which-key "dired")
    "dd" '(dired :which-key "dired directory")
    "dj" '(dired-jump :which-key "dired jump")
    "r"  '(:ignore t :which-key "eval")
    "rb" '(eval-buffer :which-key "eval buffer")
    "rr" '(eval-region :which-key "eval region")
    "rf" '(eval-defun :which-key "eval function")
    "re" '(eval-expression :which-key "eval expression")
    "rl" '(eval-last-sexp :which-key "eval last sexp")
    "rs" '(shell-command :which-key "shell command")
    "e"  '(:ignore t :which-key "editing")
    "ec" '(whitespace-cleanup :which-key "clear whitespace")
    "ea" '(align-regexp :which-key "align-regexp")
    "w"  '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "delete window")
    "wv" '(evil-window-vsplit :which-key "split window right")
    "ws" '(evil-window-split :which-key "split window below")
    "wo" '(delete-other-windows :which-key "delete other windows")
    "p"  '(:ignore t :which-key "projects and packages")
    "ps" '(straight-pull-package-and-deps  :which-key "straight-pull-package-and-deps")
    "pr" '(straight-remove-unused-repos  :which-key "straight-remove-unused-repos")
    "pu" '(straight-pull-all  :which-key "straight update all packages")
    "t"  '(:ignore t :which-key "toggle")
    "tp"  '(variable-pitch-mode :which-key "pitch font mode")
    "tw" '(count-words :which-key "count words")
    "tl" '(count-lines-page :which-key "count lines")
    "q"  '(:ignore t :which-key "quit")
    "qq" '(kill-emacs :which-key "quit emacs"))

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "i" '(evilmi-select-items :which-key "select item")
    "a" '(beginning-of-defun :which-key "beginning of function")
    "e" '(end-of-defun :which-key "end of function")))


(provide 'init-evil)
;;; init-evil.el ends here
