;;; init-prot.el --- Protesilaos Stavrou -*- lexical-binding: t -*-
;; https://protesilaos.com/emacs/dotemacs

(setq display-time-format "%a %e %b, %H:%M")
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(setq display-time-interval 60)
(setq display-time-default-load-average nil)

;;; World clock
(setq zoneinfo-style-world-list
      '(("America/Los_Angeles" "Los Angeles")
        ("America/Chicago" "Chicago")
        ("Brazil/Acre" "Rio Branco")
        ("America/New_York" "New York")
        ("Brazil/East" "Brasília")
        ("Europe/London" "London")
        ("Europe/Lisbon" "Lisbon")
        ("Europe/Brussels" "Brussels")
        ("Europe/Athens" "Athens")
        ("Asia/Tehran" "Tehran")
        ("Asia/Tbilisi" "Tbilisi")
        ("Asia/Yekaterinburg" "Yekaterinburg")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Tokyo" "Tokyo")
        ("Asia/Vladivostok" "Vladivostok")))

(setq world-clock-list t)
(setq world-clock-time-format "%R %z  %A %d %B")
(setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
(setq world-clock-timer-enable t)
(setq world-clock-timer-second 60)

;; prot-comment-timestamp-keyword
(defvar prot-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+")))

(defun prot-common-line-regexp-p (type &optional n)
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type prot-common--line-regexp-alist))))))

(defcustom prot-comment-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type '(repeat string)
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-concise "%F"
  "Specifier for date in `prot-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available options."
  :type 'string
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-verbose "%F %T %z"
  "Like `prot-comment-timestamp-format-concise', but longer."
  :type 'string
  :group 'prot-comment)

(defvar prot-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prot-comment--keyword-prompt (keywords)
  (let ((def (car prot-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prot-comment--keyword-hist def)))

;;;###autoload
(defun prot-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.
When called interactively, the list of possible keywords is that
of `prot-comment-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`prot-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `prot-comment-timestamp-format-verbose'."
  (interactive
   (list
    (prot-comment--keyword-prompt prot-comment-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   prot-comment-timestamp-format-verbose
                 prot-comment-timestamp-format-concise))
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond
     ((or (eq beg (point-at-bol))
          (prot-common-line-regexp-p 'empty))
      (let* ((maybe-newline (unless (prot-common-line-regexp-p 'empty 1) "\n")))
        (insert
         (concat comment-start
                 (make-string
                  (comment-add nil)
                  (string-to-char comment-start))
                 comment-padding
                 string
                 comment-end))
        (indent-region beg (point))
        (when maybe-newline
          (save-excursion (insert maybe-newline)))))
     (t
      (comment-indent t)
      (insert (concat " " string))))))


;; narrow dwim
;;;###autoload
(defun prot-common-window-bounds ()
  (list (save-excursion
          (move-to-window-line 0)
          (point))
        (save-excursion
          (move-to-window-line -1)
          (point))))

;;;###autoload
(defun prot-simple-narrow-visible-window ()
  (interactive)
  (let* ((bounds (prot-common-window-bounds))
         (window-area (- (cadr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cadr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun prot-simple-narrow-dwim ()
  "Do-what-I-mean narrowing.
If region is active, narrow the buffer to the region's boundaries.
If no region is active, narrow to the visible portion of the window.
If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (let ((beg (region-beginning))
          (end (region-end)))
      (narrow-to-region beg end)))
   ((null (buffer-narrowed-p))
    (prot-simple-narrow-visible-window))
   (t
    (widen)
    (recenter))))


;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gcp") 'prot-comment-timestamp-keyword)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "e"  '(:ignore t :which-key "editing")
    "en" '(prot-simple-narrow-dwim :which-key "prot-simple-narrow-dwim")))


(provide 'init-prot)
;;; init-prot.el ends here
