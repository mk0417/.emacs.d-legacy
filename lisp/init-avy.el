;;; init-avy.el --- Avy -*- lexical-binding: t -*-

(straight-use-package 'avy)


(require 'avy)

(setq avy-background t
      avy-style 'at
      avy-styles-alist '((avy-goto-line . pre)))

(setq avy-lead-faces
      '(avy-lead-face
        avy-lead-face-1
        avy-lead-face-1
        avy-lead-face-1
        avy-lead-face-1))

(setq avy-dispatch-alist nil)

;; https://karthinks.com/software/avy-can-do-anything/
(setq avy-keys '(?q ?e ?r ?y ?u ?o ?p ?a ?s ?d ?f ?g ?h ?j ?l ?x ?c ?v ?b ?n))

(defun avy-show-dispatch-help ()
  (let* ((len (length "avy-action-"))
         (fw (frame-width))
         (raw-strings (mapcar
                       (lambda (x)
                         (format "%2s: %-19s"
                                 (propertize
                                  (char-to-string (car x))
                                  'face 'aw-key-face)
                                 (substring (symbol-name (cdr x)) len)))
                       avy-dispatch-alist))
         (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
         (strings-len (length raw-strings))
         (per-row (floor fw max-len))
         display-strings)
    (cl-loop for string in raw-strings
             for N from 1 to strings-len do
             (push (concat string " ") display-strings)
             (when (= (mod N per-row) 0) (push "\n" display-strings)))
    (message "%s" (apply #'concat (nreverse display-strings)))))

;; Avy command
(global-set-key (kbd "C-c m") 'avy-goto-char-timer)

;; Kill text
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(autoload 'avy-action-kill-stay "avy")

(setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

;; Copy text
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

;; Yank text
(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

;; Transpose/Move text
(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

;; Mark text
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

;; Flyspell words
(defun avy-action-flyspell (pt)
  (save-excursion
    (goto-char pt)
    (when (require 'flyspell nil t)
      (flyspell-auto-correct-word)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

;; Bind to semicolon (flyspell uses C-;)
(setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)

;; Embark
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark)

;; Avy + Isearch
(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

;; Isearch in other windows
(defun isearch-forward-other-window (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") 'isearch-backward-other-window)

;; Google search: requires executable Tuxi
(defvar google-search-history nil
  "List of queries to google-search-string.")
(defun google-search-string (search-string)
  "Read SEARCH-STRING from the minibuffer and call the shell
command tuxi on it."
  (interactive (list (read-string "Google: " nil
                                  google-search-history
                                  (thing-at-point 'sexp))))
  (unless (executable-find "tuxi")
    (user-error "Cannot find shell command: tuxi"))
  (let ((search-output (string-trim-right
                        (shell-command-to-string
                         (concat
                          "tuxi -r "
                          (shell-quote-argument search-string))))))
    (with-current-buffer (get-buffer-create "*Tuxi Output*")
      (erase-buffer)
      (insert search-output)
      ;; (fill-region (point-min) (point-max))
      (if (<= (count-lines (point-min) (point-max)) 1)
          (message search-output)
        (goto-char (point-min))
        (display-buffer (current-buffer))
        (goto-address-mode 1)))))
(defun google-search-at-point (&optional beg end)
  "Call the shell command tuxi on the symbol at point. With an
active region use it instead."
  (interactive "r")
  (if-let ((search-string (if (use-region-p)
                              (buffer-substring-no-properties beg end)
                            (thing-at-point 'symbol))))
      (google-search-string search-string)
    ;; (message "No symbol to search for at point!")
    (call-interactively #'google-search-string)))

(defun avy-action-tuxi (pt)
  (cl-letf (((symbol-function 'keyboard-quit)
             #'abort-recursive-edit))
    (save-excursion
      (goto-char pt)
      (google-search-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?G avy-dispatch-alist) 'avy-action-tuxi)

(defun p-avy-goto-word-current-line ()
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

(defun p-avy-goto-word-block ()
  (interactive)
  (let ((block-beginning-position (progn (p-beginning-of-block) (line-beginning-position)))
        (block-end-position (progn (p-end-of-block) (line-end-position))))
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil block-beginning-position block-end-position))))

(defun p-avy-goto-bracket (&optional BEG END)
  (interactive)
  (let ((avy-command this-command))
    (avy-jump "\\((+\\|\\[+\\|)+\\|]+\\)" :beg BEG :end END)))
(add-to-list 'avy-orders-alist '(p-avy-goto-bracket . avy-order-closest))

(defun p-avy-goto-bracket-block ()
  (interactive)
  (let ((block-beginning-position (progn (p-beginning-of-block) (line-beginning-position)))
        (block-end-position (progn (p-end-of-block) (line-end-position))))
    (avy-with p-avy-goto-bracket
      (p-avy-goto-bracket block-beginning-position block-end-position))))


;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "f") 'nil)
  (define-key evil-normal-state-map (kbd "ft") 'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "fw") 'avy-goto-word-0)
  (define-key evil-normal-state-map (kbd "fl") 'avy-goto-line)
  (define-key evil-normal-state-map (kbd "fc") 'avy-copy-line)
  (define-key evil-normal-state-map (kbd "fs") 'avy-kill-ring-save-region)
  (define-key evil-normal-state-map (kbd "fr") 'avy-copy-region)
  (define-key evil-normal-state-map (kbd "f.") 'p-avy-goto-word-current-line)
  (define-key evil-normal-state-map (kbd "f,") 'p-avy-goto-word-block)
  (define-key evil-normal-state-map (kbd "fK") 'p-avy-goto-bracket)
  (define-key evil-normal-state-map (kbd "fk") 'p-avy-goto-bracket-block))


(provide 'init-avy)
;;; init-avy.el ends here
