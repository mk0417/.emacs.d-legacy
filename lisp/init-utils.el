;;; init-utils.el --- Useful functions -*- lexical-binding: t -*-

;; list all installed package
;; https://github.com/raxod502/straight.el/issues/262
(defun p-list-installed-packages ()
  (interactive)
  (require 'magit)
  (let ((magit-repository-directories
         (list (cons (straight--repos-dir) 1))))
    (magit-list-repositories)))

;; rename the current file
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(defun p-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; find file in my config
(defun p-find-file-in-config ()
  (interactive)
  (let ((default-directory (file-truename (file-name-directory user-init-file))))
    (call-interactively 'find-file)))

;; https://stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs
(defun p-surround-anything (s)
  (interactive)
  (if (region-active-p)
      (progn
        (insert-pair 1 s s)
        (backward-char))
    (progn
      (forward-sexp)
      (backward-sexp)
      (mark-sexp)
      (insert-pair 1 s s))))

(defun p-surround-equal ()
  (interactive)
  (p-surround-anything "="))

(defun p-surround-asterisk ()
  (interactive)
  (p-surround-anything "*"))

(defun p-surround-slash ()
  (interactive)
  (p-surround-anything "/"))

;; insert date
(defun p-insert-uk-date ()
  (interactive)
  (insert (format-time-string "%d-%m-%Y")))

(defun p-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; insert current buffer name
(defun p-insert-file-name ()
  (interactive)
  (insert (buffer-file-name)))

;; backward kill to the beginning of line
(defun p-kill-to-begin-of-line ()
  (interactive)
  (kill-line 0))

;; delete to tab
(defun p-delete-backward-to-tab ()
  (interactive)
  (when (evil-normal-state-p)
    (beginning-of-line-text)
    (kill-line 0)
    (insert "    "))
  (when (evil-insert-state-p)
    (kill-line 0)
    (insert "    ")))

;; clear line
(defun p-clear-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;; add four spaces
(defun p-insert-spaces ()
  (interactive)
  (insert "    "))

;; select functions
(defun p-select-function ()
  (interactive)
  (beginning-of-defun)
  (evilmi-select-items))

;; Display file name
(defun p-display-file-name ()
  (interactive)
  (message buffer-file-name))

;; insert html tag
;; https://www.youtube.com/watch?v=aJsVD8nIoHA
(defun p-html-region-insert-tag (begin end tag)
  (interactive "r\nsTag:")
  (goto-char end)
  (insert (concat "</" tag ">"))
  (let* ((real-end (set-marker (make-marker) (point))))
    (goto-char begin)
    (insert (concat "<" tag ">"))
    (goto-char real-end)))

;; reveal file in Finder
;; https://github.com/xuchunyang/emacs.d/blob/master/lisp/chunyang-mac.el
(defun p-reveal-file-in-finder (file)
  (interactive (list (or (buffer-file-name) ".")))
  (do-applescript
   (format (concat "tell application \"Finder\"\n"
                   "	activate\n"
                   "	reveal POSIX file \"%s\"\n"
                   "end tell")
           (expand-file-name file))))

;; google search
;; https://emacsredux.com/blog/2013/03/28/google/
(defun p-google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

;; youtube search
;; https://emacsredux.com/blog/2013/08/26/search-youtube/
(defun p-youtube-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))


;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-i") 'p-delete-backward-to-tab)
  (define-key evil-normal-state-map (kbd "goe") 'p-surround-equal)
  (define-key evil-normal-state-map (kbd "goa") 'p-surround-asterisk)
  (define-key evil-normal-state-map (kbd "goi") 'p-surround-slash)
  (define-key evil-normal-state-map (kbd "goc") 'p-clear-line)

  (define-key evil-visual-state-map (kbd "goe") 'p-surround-equal)
  (define-key evil-visual-state-map (kbd "goa") 'p-surround-asterisk)
  (define-key evil-visual-state-map (kbd "goi") 'p-surround-slash)

  (define-key evil-insert-state-map (kbd "C-u") 'p-kill-to-begin-of-line)
  (define-key evil-insert-state-map (kbd "C-i") 'p-delete-backward-to-tab)
  (define-key evil-insert-state-map (kbd "C-;") 'p-insert-spaces)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "f"  '(:ignore t :which-key "file")
    "fi" '(p-insert-file-name :which-key "insert file path and name")
    "fp" '(p-find-file-in-config :which-key "find config file")
    "fR" '(p-rename-this-file-and-buffer :which-key "rename file")
    "e"  '(:ignore t :which-key "editing")
    "ed" '(p-insert-date :which-key "insert date")
    "ek" '(p-insert-uk-date :which-key "insert UK date")
    "et" '(p-html-region-insert-tag :which-key "insert html tag")
    "s"  '(:ignore t :which-key "search")
    "sg" '(p-google-search :which-key "search on google")
    "sy" '(p-youtube-search :which-key "search on youtube")
    "t"  '(:ignore t :which-key "toggle")
    "tr" '(p-reveal-file-in-finder :which-key "reveal file in finder")
    "tn" '(p-display-file-name :which-key "show file name")
    "p"  '(:ignore t :which-key "projects and packages")
    "pS" '(p-list-installed-packages :which-key "list installed packages"))

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "."  '(p-select-function :which-key "p-select-function")))


(provide 'init-utils)
;;; init-utils.el ends here
