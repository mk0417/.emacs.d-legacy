;;; init.el --- Load configuration -*- lexical-binding: t -*-

;; produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

;; inherit variables from .zshrc
;; http://ergoemacs.org/emacs/emacs_env_var_paths.html
(let ((p-env-path
       '("/Users/ml/anaconda3/bin"
         "/Users/ml/anaconda3/bin/jupyter"
         "/usr/local/bin"
         "/usr/local/sbin"
         "/usr/bin"
         "/bin"
         "/usr/sbin"
         "/sbin"
         "/Applications/Stata/StataMP.app/Contents/MacOS/"
         "/Applications/Stata/StataMP.app/Contents/MacOS/stata"
         "/Library/TeX/texbin"
         "/Users/ml/.emacs.d/bin"
         "/Users/ml/.cargo/bin"
         "/Applications/Emacs.app/Contents/MacOS/bin")))
  (setenv "PATH" (mapconcat 'identity p-env-path ":") )
  (setq exec-path (append p-env-path (list "." exec-directory))))


(require 'init-ui)
(require 'init-default)
(require 'init-theme)
(require 'init-evil)
(require 'init-git)
(require 'init-org)
(require 'init-dired)
(require 'init-project)
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-window)
(require 'init-xah)
(require 'init-prog)
(require 'init-typing)
(require 'init-utils)
(require 'init-misc)
(require 'init-latex)
(require 'init-hydra)
(require 'init-snippet)
(require 'init-wrds)
(require 'init-markdown)
(require 'init-prot)


(provide 'init)
