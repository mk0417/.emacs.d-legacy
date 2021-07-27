;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t; -*-

(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)


;; vertico
(add-hook 'after-init-hook 'vertico-mode)

;; marginalia
(add-hook 'after-init-hook 'marginalia-mode)

;; orderless
(setq completion-styles '(substring orderless))

;; consult
(setq consult-imenu-config
      '((emacs-lisp-mode :toplevel "Functions"
                         :types ((?f "Functions" font-lock-function-name-face)
                                 (?m "Macros"    font-lock-keyword-face)
                                 (?p "Packages"  font-lock-constant-face)
                                 (?t "Types"     font-lock-type-face)
                                 (?v "Variables" font-lock-variable-name-face)))))

(setq-default consult-project-root-function 'projectile-project-root)

(with-eval-after-load 'consult
  (defmacro p-no-consult-preview (&rest cmds)
    `(with-eval-after-load 'consult
       (consult-customize ,@cmds :preview-key (kbd "M-v"))))
  (p-no-consult-preview
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file consult-xref
   consult--source-file
   consult--source-project-file
   consult--source-bookmark)

  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key (kbd "C-x l")   'consult-line))

(autoload 'consult--grep "consult")

(defun p-consult-at-point-line (&optional initial)
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun p-consult-rg-at-point-project (&optional dir)
  (interactive)
  (consult--grep "Ripgrep" consult-ripgrep-command dir (thing-at-point 'symbol)))

(defun p-consult-rg-current-dir (&optional initial)
  (interactive "P")
  (if (equal buffer-file-name nil)
      (consult--grep "Ripgrep current dir" consult-ripgrep-command "/Users/ml/" initial)
    (consult--grep "Ripgrep current dir" consult-ripgrep-command (file-name-directory buffer-file-name) initial)))

(defun p-consult-rg-other-dir (&optional initial)
  (interactive "P")
  (consult--grep "Ripgrep current dir" consult-ripgrep-command (read-directory-name "consult-rg directory:") initial))

(defun p-consult-rg-at-point-current-dir ()
  (interactive)
  (consult--grep "Ripgrep current dir" consult-ripgrep-command (file-name-directory buffer-file-name) (thing-at-point 'symbol)))

(defun p-consult-fd-local (&optional dir initial)
  (interactive "P")
  (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
    (consult-find dir initial)))

(defun p-consult-fd-global (&optional initial)
  (interactive "P")
  (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
    (consult-find "~/" initial)))

;; embark
(setq embark-prompter 'embark-completing-read-prompter
      embark-keymap-prompter-key ",")

(autoload 'embark-act "embark")
(autoload 'embark-export "embark")

(global-set-key (kbd "C-c C-m") 'embark-act)
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-c C-o") 'embark-export)
  (define-key vertico-map (kbd "C-o") 'embark-act))

(with-eval-after-load 'embark
  (require 'embark-consult)
  (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode))

;; keystrokes feedback
(setq echo-keystrokes 0.02)


;; keybindings
(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fr" '(consult-recent-file :which-key "recent file")
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "consult switch buffer")
    "bo" '(consult-buffer-other-window :which-key "open file in another window")
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "consult line")
    "sS" '(p-consult-at-point-line :which-key "consult at-point line")
    "sm"  '(consult-multi-occur :which-key "consult multi occur")
    "sp" '(consult-ripgrep :which-key "consult-rg project")
    "sP" '(p-consult-rg-at-point-project :which-key "consult-rg at-point project")
    "sd" '(p-consult-rg-current-dir :which-key "consult-rg current dir")
    "sD" '(p-consult-rg-at-point-current-dir :which-key "consult-rg at-point current dir")
    "so" '(p-consult-rg-other-dir :which-key "consult-rg other dir")
    "sf" '(p-consult-fd-global :which-key "consult-fd global files")
    "sF" '(p-consult-fd-local :which-key "consult-fd local files")
    "si" '(consult-imenu :which-key "consult imenu")
    "sl" '(consult-outline :which-key "consult outline")))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
