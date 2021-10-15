;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t; -*-

(straight-use-package '(vertico
                        :type git
                        :host github
                        :repo "minad/vertico"
                        :files ("*.el" "extensions/*.el")))
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'consult-dir)
(straight-use-package 'fzf)


;; vertico
(add-hook 'after-init-hook 'vertico-mode)

;; add » to indicate current candidate
;; https://github.com/minad/vertico/wiki
(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

;; marginalia
(add-hook 'after-init-hook 'marginalia-mode)

;; orderless
(setq completion-styles '(substring orderless))

;; consult
(with-eval-after-load 'consult
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (setq consult-imenu-config
        '((emacs-lisp-mode :toplevel "Functions"
                           :types ((?f "Functions" font-lock-function-name-face)
                                   (?m "Macros"    font-lock-keyword-face)
                                   (?p "Packages"  font-lock-constant-face)
                                   (?t "Types"     font-lock-type-face)
                                   (?v "Variables" font-lock-variable-name-face)))))

  (defmacro p-no-consult-preview (&rest cmds)
    `(with-eval-after-load 'consult
       (consult-customize ,@cmds :preview-key (kbd "M-v"))))
  (p-no-consult-preview consult-ripgrep
                        consult-git-grep
                        consult-grep
                        consult-bookmark
                        consult-recent-file
                        consult-xref
                        consult--source-file
                        consult--source-project-file
                        consult--source-bookmark
                        p-consult-rg-at-point-project
                        p-consult-rg-current-dir
                        p-consult-rg-other-dir
                        p-consult-rg-at-point-current-dir)

  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key (kbd "C-x l") 'consult-line))

(autoload 'consult--grep "consult")

(defun p-consult-at-point-line (&optional initial)
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun p-consult-rg-at-point-project (&optional dir)
  (interactive)
  (consult--grep "Ripgrep" #'consult--ripgrep-builder dir (thing-at-point 'symbol)))

(defun p-consult-rg-current-dir (&optional initial)
  (interactive "P")
  (if (equal buffer-file-name nil)
      (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder "/Users/ml/" initial)
    (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder (file-name-directory buffer-file-name) initial)))

(defun p-consult-rg-other-dir (&optional initial)
  (interactive "P")
  (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder (read-directory-name "consult-rg directory:") initial))

(defun p-consult-rg-at-point-current-dir ()
  (interactive)
  (consult--grep "Ripgrep current dir" #'consult--ripgrep-builder (file-name-directory buffer-file-name) (thing-at-point 'symbol)))

(defun p-consult-fd-local (&optional dir initial)
  (interactive "P")
  (if (equal buffer-file-name nil)
      (consult-find "~/" initial)
    (consult-find dir initial)))

(defun p-consult-fd-global (&optional initial)
  (interactive "P")
  (consult-find (read-directory-name "consult-find directory:") initial))

;; embark
(autoload 'embark-act "embark")
(autoload 'embark-export "embark")

(global-set-key (kbd "C-,") 'embark-act)
(global-set-key (kbd "C-c C-o") 'embark-export)

;; colorize the current vertico candidate differently when acting
(defun embark-vertico-indicator ()
  (let ((fr face-remapping-alist))
    (lambda (&optional keymap _targets prefix)
      (when (bound-and-true-p vertico--input)
        (setq-local face-remapping-alist
                    (if keymap
                        (cons '(vertico-current . embark-target) fr)
                      fr))))))

;; embark action integration with which-key
(defun embark-which-key-indicator ()
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (caar targets) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t))))


(with-eval-after-load 'embark
  (setq embark-keymap-prompter-key ",")
  (setq embark-indicators '(embark-which-key-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  (add-to-list 'embark-indicators #'embark-vertico-indicator)
  (require 'embark-consult)
  (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

;; keystrokes feedback interval
(setq echo-keystrokes 0.02)


;; keybindings
(global-set-key (kbd "C-c C-d") 'consult-dir)
(global-set-key (kbd "C-c C-j") 'consult-dir-jump-file)

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fr" '(consult-recent-file :which-key "recent file")
    "fd" '(consult-dir :which-key "find directory")
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "consult switch buffer")
    "bo" '(consult-buffer-other-window :which-key "open file in another window")
    "e"  '(:ignore t :which-key "editing")
    "er" '(vertico-repeat :which-key "vertico-repeat")
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
