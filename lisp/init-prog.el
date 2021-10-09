;;; init-prog.el --- Programming/coding -*- lexical-binding: t -*-

(straight-use-package 'jupyter)
(straight-use-package 'ess)
(straight-use-package '(ess-stata-mode
                        :type git
                        :host github
                        :repo "emacs-ess/ess-stata-mode"))
(straight-use-package 'ein)
(straight-use-package 'elpy)


;; Jupyter
;; https://github.com/nnicandro/emacs-zmq
;; https://github.com/nnicandro/emacs-zmq/issues/19
;; Do not download zmq module from released version that contains .so file
;; Emacs 28 needs .dylib
;; Answer No when first installation and build it to have .dylib file
(setq jupyter-eval-use-overlays t)

(defun p-jupyter-remove-line-overlay ()
  (interactive)
  (evil-open-below 0)
  (kill-whole-line)
  (evil-escape)
  (previous-line))

(defun p-jupyter-eval-block ()
  (interactive)
  (xah-select-block)
  (let (beg end)
    (setq beg (region-beginning) end (region-end))
    (jupyter-eval-region beg end)))


;; Python
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil)

  (add-hook 'python-mode-hook 'display-fill-column-indicator-mode)

  ;; elpy
  (elpy-enable)
  (diminish 'elpy-mode)

  ;; solve issue: send-region shows ^G
  ;; https://github.com/jorgenschaefer/elpy/issues/1550#issuecomment-478448647
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i"
        python-shell-prompt-detect-failure-warning nil
        ;; disable completion warning
        ;; https://github.com/jorgenschaefer/elpy/issues/887
        python-shell-completion-native-enable nil
        elpy-rpc-virtualenv-path 'current)

  ;; disable flymake in Python
  ;; https://github.com/jorgenschaefer/elpy/issues/828
  (remove-hook 'elpy-modules 'elpy-module-flymake)

  ;; disable highlight indentation
  ;; https://stackoverflow.com/questions/45214116/how-to-disable-emacs-elpy-vertical-guide-lines-for-indentation
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

  ;; single line eldoc
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; send current line
  (defun p-elpy-shell-send-line ()
    (interactive)
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))
    (elpy-shell-send-region-or-buffer)
    (beginning-of-line)
    (keyboard-quit))

  ;; keybindings
  (define-key python-mode-map "\C-c\C-j" 'p-elpy-shell-send-line)

  (general-create-definer p-python-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'python-mode-map)
  (p-python-leader-def
    "j"  '(:ignore t :which-key "jupyter")
    "jj" 'jupyter-run-repl
    "jr" 'jupyter-eval-line-or-region
    "jf" 'jupyter-eval-defun
    "je" 'p-jupyter-eval-block
    "jR" 'jupyter-repl-restart-kernel
    "jK" 'jupyter-repl-clear-cells
    "jI" 'jupyter-repl-interrupt-kernel
    "ji" 'jupyter-inspect-at-point
    "jC" 'jupyter-eval-remove-overlays
    "jc" 'p-jupyter-remove-line-overlay
    "jw" 'jupyter-repl-pop-to-buffer
    "e"  '(:ignore t :which-key "elpy")
    "ep" 'run-python
    "eg" 'elpy-shell-send-group-and-step
    "el" 'p-elpy-shell-send-line
    "er" 'elpy-shell-send-region-or-buffer))


;; R
(with-eval-after-load 'ess
  ;; disable flymake
  (add-hook 'ess-r-mode-hook (lambda () (flymake-mode -1)))
  (add-hook 'ess-mode-hook 'display-fill-column-indicator-mode)
  (setq ess-ask-for-ess-directory nil)
  ;; fix: Error running timer 'ess--idle-timer-function': (wrong-type-argument stringp nil)
  ;; https://github.com/emacs-ess/ESS/issues/1102
  (setq ess-can-eval-in-background nil))


;; Stata
;; https://github.com/hieutkt/.doom.d/blob/master/config.el
(setq inferior-STA-start-args ""
      inferior-STA-program (executable-find "stata-mp")
      ;; make stata src work in org
      inferior-STA-program-name "")

(with-eval-after-load 'ess
  (general-create-definer p-ess-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'ess-mode-map)
  (p-ess-leader-def
   "a"  'ess-cycle-assign
   "j"  '(:ignore t :which-key "eval")
   "jf" 'ess-eval-function
   "jl" 'ess-eval-line
   "jr" 'ess-eval-region-or-line-and-step))


(provide 'init-prog)
;;; init-prog.el ends here
