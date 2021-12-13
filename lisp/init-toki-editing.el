;;; init-toki-editing.el --- Tokimacs editing -*- lexical-binding: t -*-
;; https://github.com/AmaiKinono/Tokimacs

(straight-use-package '(puni
                        :type git
                        :host github
                        :repo "AmaiKinono/puni"))


(require 'cl-lib)
(require 'puni)

;; puni
(puni-global-mode)

(defvar toki-smooth-scroll-step-length 2
  "How many lines to skip in a step when smooth scrolling.
See also `toki-smooth-scroll-interval'.")

(defvar toki-smooth-scroll-interval 0.016
  "Interval between steps when smooth scrolling.
See also `toki-smooth-scroll-step-length'.")

(defun toki/forward-block ()
  "Go forward a block.
Return the point if success.
A block is a continuous region with the same syntax, which
contains no more than 1 word.  See the implementation for
details."
  (unless (eobp)
    ;; A word may actually end at a position where the syntax on both sides are
    ;; "word", e.g., when subword-mode is enabled.
    (let ((word-end (save-excursion (when (forward-word) (point)))))
      (puni--forward-same-syntax word-end))))

(defun toki/backward-block ()
  "Backward version of `toki/forward-block'."
  (unless (bobp)
    (let ((word-beg (save-excursion (when (forward-word -1) (point)))))
      (puni--backward-same-syntax word-beg))))

(defun toki/scroll-down-or-prev-line (&optional arg)
  "Scroll down ARG lines, or go to previous line if it can't be done.
Scroll 1 line if ARG is nil."
  (let ((arg (or arg 1)))
    (condition-case nil
        (scroll-down arg)
      (beginning-of-buffer
       (forward-line (- arg))))))

(defun toki/bob-error ()
  "Signal an error if point is at the beginning of buffer."
  (when (bobp)
    (signal 'beginning-of-buffer nil)))

(defun toki/eob-error ()
  "Signal an error if point is and the end of buffer."
  (when (eobp)
    (signal 'end-of-buffer nil)))

;;;###autoload
(defun toki-forward-word ()
  "A finer version of `forward-word'.
If there's *only one* space, tab, '-' or '_' between point and
next word, move after it.  Then jump forward by a block.  A block
is a continuous region with the same syntax, like a word, a bunch
of whitespaces/punctuations, etc.
This doesn't fly over most punctuations, while `forward-word'
does."
  (interactive)
  (toki/eob-error)
  (unless (> (point) (- (point-max) 2))
    (when (and (member (char-after) '(?\s ?\t ?- ?_))
               (eq (puni--syntax-char-after (1+ (point))) ?w))
      (forward-char)))
  (toki/forward-block))

;;;###autoload
(defun toki-forward-delete-word ()
  "Delete word forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-delete-active-region)
    (puni-soft-delete-by-move #'toki-forward-word nil nil nil 'jump-and-reverse-delete)))

;;;###autoload
(defun toki-backward-word ()
  "A finer version of `backward-word'.
If there's *only one* space, tab, '-' or '_' between point and
previous word, move before it.  Then jump back by a block.  A
block is a continuous region with the same syntax, like a word, a
bunch of whitespaces/punctuations, etc.
This doesn't fly over most punctuations, while `backward-word'
does."
  (interactive)
  (toki/bob-error)
  (unless (< (point) (+ (point-min) 2))
    (when (and (member (char-before) '(?\s ?\t ?- ?_))
               (eq (puni--syntax-char-after (- (point) 2)) ?w))
      (backward-char)))
  (toki/backward-block))

;;;###autoload
(defun toki-backward-delete-word ()
  "Delete word backward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-delete-active-region)
    (puni-soft-delete-by-move #'toki-backward-word nil nil nil 'jump-and-reverse-delete)))

;;;###autoload
(defun toki-beginning-of-line ()
  "A smarter version of `beginning-of-line'.
Jump to the beginning of current line, or if the point is already
there, move to the first non-whitespace character in current
line."
  (interactive)
  (if (and (bolp) (not (puni--line-empty-p)))
      (skip-chars-forward "[:blank:]")
    (beginning-of-line)))

;;;###autoload
(defun toki-end-of-line ()
  "A smarter version of `end-of-line'.
Jump to the end of current line, or if the point is already
there, move to the last non-whitespace character in current
line."
  (interactive)
  (if (and (eolp) (not (puni--line-empty-p)))
      (skip-chars-backward "[:blank:]")
    (end-of-line)))

;;;###autoload
(defun toki-forward-subsentence ()
  "Jump forward by a subsentence.
It jumps to the next period, comma, exclamation mark, question
mark, colon, semicolon, or ellipsis."
  (interactive)
  (re-search-forward "\\.\\|,\\|\\!\\|\\?\\|:\\|;\\|…\
\\|。\\|，\\|、\\|！\\|？\\|：\\|；\\|⋯"))

;;;###autoload
(defun toki-backward-subsentence ()
  "Jump backward by a subsentence.
It jumps to the previous period, comma, exclamation mark, question
mark, colon, semicolon, or ellipsis."
  (interactive)
  (re-search-backward "\\.\\|,\\|\\!\\|\\?\\|:\\|;\\|…\
\\|。\\|，\\|、\\|！\\|？\\|：\\|；\\|⋯"))

;;;###autoload
(defun toki-smooth-scroll-half-page-down ()
  "Smoothly scroll down by half a page."
  (interactive)
  (when (eq (line-number-at-pos) 1)
    (signal 'beginning-of-buffer nil))
  (let* ((lines (max (round (/ (window-body-height) 2.3)) 10))
         (times (max 1 (/ lines toki-smooth-scroll-step-length)))
         (interval (* (- times 0.5) toki-smooth-scroll-interval))
         (timer
          (run-with-timer
           nil toki-smooth-scroll-interval
           (lambda ()
             (toki/scroll-down-or-prev-line toki-smooth-scroll-step-length)))))
    (run-with-timer interval nil (lambda () (cancel-timer timer)))))

;;;###autoload
(defun toki-smooth-scroll-half-page-up ()
  "Smoothly scroll down by half a page."
  (interactive)
  (let* ((lines (max (round (/ (window-body-height) 2.3)) 10))
         (times (max 1 (/ lines toki-smooth-scroll-step-length)))
         (interval (* (- times 0.5) toki-smooth-scroll-interval))
         (timer
          (run-with-timer
           nil toki-smooth-scroll-interval
           (lambda ()
             (scroll-up toki-smooth-scroll-step-length)))))
    (run-with-timer interval nil (lambda () (cancel-timer timer)))))

;;;###autoload
(defun toki-shrink-whitespace ()
  "Intelligently shrink whitespaces around point.
When in the middle of a line, delete whitespaces around point, or
add a space if there are no whitespaces.
When in an empty line, leave only one blank line, or delete it if
it's the only one.
When at the beginning/end of a line, first delete whitespaces
around it, then delete empty lines before/after it.  Finally join
with the line before/after it."
  (interactive)
  (let* ((beg
          (save-excursion (while (member (char-before) '(?\s ?\t))
                            (backward-char))
                          (point)))
         ;; el = empty lines
         (beg-with-el
          (save-excursion (while (member (char-before) '(?\n ?\s ?\t))
                            (backward-char))
                          (point)))
         (end
          (save-excursion (while (member (char-after) '(?\s ?\t))
                            (forward-char))
                          (point)))
         (end-with-el
          (save-excursion (while (member (char-after) '(?\n ?\s ?\t))
                            (forward-char))
                          (point))))
    (cond
     ((puni--line-empty-p)
      (delete-blank-lines))
     ((eq beg end)
      (cond
       ((eq beg-with-el end-with-el)
        (insert-char ?\s))
       (t
        (cond
         ((eq (point) beg-with-el)
          (save-excursion
            (forward-char)
            (if (puni--line-empty-p)
                (delete-blank-lines)
              (delete-char -1))))
         (t
          (save-excursion
            (backward-char)
            (if (puni--line-empty-p)
                (delete-blank-lines)
              (delete-char 1))))))))
     (t
      (delete-region beg end)))))

(defun p-puni-kill-line-and-insert ()
    (interactive)
    (puni-kill-line)
    (evil-insert 0))


;; overwrite puni default keybindings
(define-key puni-mode-map (kbd "C-k") nil)
(define-key puni-mode-map (kbd "C-w") nil)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "nj") 'toki-smooth-scroll-half-page-up)
  (define-key evil-normal-state-map (kbd "nk") 'toki-smooth-scroll-half-page-down)
  (define-key evil-normal-state-map (kbd "noa") 'toki-beginning-of-line)
  (define-key evil-normal-state-map (kbd "noe") 'toki-end-of-line)
  (define-key evil-normal-state-map (kbd "now") 'toki-forward-subsentence)
  (define-key evil-normal-state-map (kbd "nob") 'toki-backward-subsentence)
  (define-key evil-normal-state-map (kbd "nol") 'toki-forward-delete-word)
  (define-key evil-normal-state-map (kbd "noh") 'toki-backward-delete-word)
  (define-key evil-normal-state-map (kbd "noc") 'puni-kill-line)
  (define-key evil-normal-state-map (kbd "noi") 'p-puni-kill-line-and-insert)
  (define-key evil-normal-state-map (kbd "nok") 'puni-beginning-of-sexp)
  (define-key evil-normal-state-map (kbd "noj") 'puni-end-of-sexp)
  (define-key evil-normal-state-map (kbd "noo") 'puni-expand-region)
  (define-key evil-normal-state-map (kbd "nod") 'puni-syntactic-forward-punct)
  (define-key evil-normal-state-map (kbd "nos") 'puni-syntactic-backward-punct)
  (define-key evil-normal-state-map (kbd "nou") 'puni-forward-sexp)
  (define-key evil-normal-state-map (kbd "not") 'puni-backward-sexp)
  (define-key evil-normal-state-map (kbd "nog") 'toki-shrink-whitespace)
  (define-key evil-normal-state-map (kbd "no.") 'puni-mark-sexp-at-point)
  (define-key evil-normal-state-map (kbd "no,") 'puni-mark-sexp-around-point)
  (define-key evil-normal-state-map (kbd "no;") 'puni-mark-list-around-point)

  (define-key evil-visual-state-map (kbd "noo") 'puni-expand-region)
  (define-key evil-visual-state-map (kbd "nou") 'puni-forward-sexp)
  (define-key evil-visual-state-map (kbd "not") 'puni-backward-sexp)

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "d" '(puni-splice :which-key "delete around")
    "p"  '(:ignore t :which-key "puni")
    "pr" '(puni-raise :which-key "puni-raise")
    "pt" '(puni-transpose :which-key "puni-transpose")
    "pi" '(puni-split :which-key "puni-split")
    "pc" '(puni-convolute :which-key "puni-convolute")
    "pw" '(puni-barf-forward :which-key "puni-barf-forward")
    "pb" '(puni-barf-backward :which-key "puni-barf-backward")
    "pl" '(puni-slurp-forward :which-key "puni-slurp-forward")
    "ph" '(puni-slurp-backward :which-key "puni-slurp-backward")
    "ps" '(puni-squeeze :which-key "puni-squeeze")))


(provide 'init-toki-editing)
;;; init-toki-editing.el ends here
