;;; init-xah.el --- Functions from Xah Lee -*- lexical-binding: t -*-

;; new scratch buffer
(defun xah-new-empty-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
If region is active, extend selection downward by block.
URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2019-12-26 2021-04-04 2021-08-13"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1)
      ;; my-hack
      (previous-line)
      (end-of-line))))

;; open using external app in dired
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional @fname)
  (interactive)
  (let* (($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))
         $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath)))
         $file-list))))))

;; https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_html.el
(defun xah-forward-html-end-tag ()
  "Move cursor to the next HTML tag's content."
  (interactive)
  (forward-char 1)
  (search-forward "</")
  (backward-char 2))

(defun xah-backward-html-end-tag ()
  "Move cursor to the previous HTML tag's content."
  (interactive)
  (search-backward "</"))

;; http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html
(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>“”‘’‹›«»")
(defvar xah-left-brackets '("(" "{" "[" "<" "‹" "«"))
(defvar xah-right-brackets '(")" "}" "]" ">" "›" "»"))

(progn
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-beginning-of-line-or-block ()
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (equal last-command this-command ))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
            (progn
              (skip-chars-backward "\n\t ")
              (forward-char ))
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun xah-end-of-line-or-block ()
  (interactive)
  (if (or (equal (point) (line-end-position))
          (equal last-command this-command ))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" ))
    (end-of-line)))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor, including the inner text.
This command assumes the left of cursor is a right bracket, and there is a matching one before it.
What char is considered bracket or quote is determined by current syntax table.
URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-09-21"
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.
URL `http://xahlee.info/emacs/emacs/modernization_upcase-word.html'
Version: 2020-06-26"
  (interactive)
  (let ( (deactivate-mark nil) $p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun xah-get-bounds-of-block ()
  "Return the boundary (START . END) of current block.
Version: 2021-08-12"
  (let ( $p1 $p2 ($blankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq $p1 (if (re-search-backward $blankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq $p2 (if (re-search-forward $blankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons $p1 $p2 )))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else same as `xah-get-bounds-of-block'.
Version: 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char in current block or selection.
URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version: 2017-08-19 2021-08-12 2021-09-12 2021-11-28"
  (interactive)
  (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun xah-quote-lines (Begin End QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.
For example,
 cat
 dog
 cow
becomes
 \"cat\",
 \"dog\",
 \"cow\",
or
 (cat)
 (dog)
 (cow)
In lisp code, QuoteL QuoteR Sep are strings.
URL `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
Version: 2020-06-26 2021-07-21 2021-08-15 2021-09-15"
  (interactive
   (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($brackets
          '(
            "\"double\""
            "'single'"
            "(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "none"
            "other"
            )) $bktChoice $sep $sepChoice $quoteL $quoteR)
     (setq $bktChoice (ido-completing-read "Quote to use:" $brackets))
     (setq $sepChoice (ido-completing-read "line separator:" '("," ";" "none" "other")))
     (cond
      ((string-equal $bktChoice "none")
       (setq $quoteL "" $quoteR ""))
      ((string-equal $bktChoice "other")
       (let (($x (read-string "Enter 2 chars, for begin/end quote:")))
         (setq $quoteL (substring-no-properties $x 0 1)
               $quoteR (substring-no-properties $x 1 2))))
      (t (setq $quoteL (substring-no-properties $bktChoice 0 1)
               $quoteR (substring-no-properties $bktChoice -1))))
     (setq $sep
           (cond
            ((string-equal $sepChoice "none") "")
            ((string-equal $sepChoice "other") (read-string "Enter separator:"))
            (t $sepChoice)))
     (list $p1 $p2 $quoteL $quoteR $sep)))
  (let (($p1 Begin) ($p2 End) ($quoteL QuoteL) ($quoteR QuoteR) ($sep Sep))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert $quoteL)
            (end-of-line)
            (insert $quoteR $sep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))

(defun xah-change-newline-chars-to-one (Begin End)
  "Replace newline char sequence by just one.
URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2021-07-06"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil 1) (replace-match "\n")))))

(defun xah-reformat-whitespaces-to-one-space (Begin End)
  "Replace whitespaces by one space.
URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil 1)
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil 1)
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil 1)
        (replace-match " ")))))

(defun xah-reformat-to-multi-lines ( &optional Begin End MinLength)
  "Replace spaces by a newline at ~70 chars, on current block or selection.
If `universal-argument' is called first, ask user for max width.
URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2018-12-16 2021-07-06 2021-08-12"
  (interactive)
  (let ( $p1 $p2 $minlen )
    (setq $minlen (if MinLength MinLength (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil 1)
          (when (> (- (point) (line-beginning-position)) $minlen)
            (replace-match "\n" )))))))

(defun xah-reformat-lines (&optional Width)
  "Reformat current block or selection into short lines or 1 long line.
When called for the first time, change to one long line. Second call change it to multiple short lines. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length of line. By default, it is 70.
URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Created 2016 or before.
Version: 2021-07-05 2021-08-13"
  (interactive)
  ;; This command symbol has a property 'is-long-p, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( $isLong $width $p1 $p2)
    (setq $width (if Width Width (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 70 )))
    (setq $isLong (if (eq last-command this-command) (get this-command 'is-long-p) nil))
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (progn
      (if current-prefix-arg
          (xah-reformat-to-multi-lines $p1 $p2 $width)
        (if $isLong
            (xah-reformat-to-multi-lines $p1 $p2 $width)
          (xah-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-long-p (not $isLong)))))

(defun xah-cycle-hyphen-lowline-space (&optional Begin End)
  "Cycle hyphen/lowline/space chars in selection or inside quote/bracket or line, in that order.
After this command is called, press t to repeat it.
The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.
URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version: 2019-02-12 2021-08-20"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of $charArray.
  (let* ($p1
         $p2
         ($charArray ["-" "_" " "])
         ($n (length $charArray))
         ($regionWasActive-p (region-active-p))
         ($nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0))
         ($changeTo (elt $charArray $nowState)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
          (skip-chars-backward $skipChars (line-beginning-position))
          (setq $p1 (point))
          (skip-chars-forward $skipChars (line-end-position))
          (setq $p2 (point))
          (set-mark $p1))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward (elt $charArray (% (+ $nowState 2) $n)) (point-max) 1)
          (replace-match $changeTo t t))))
    (when (or (string-equal $changeTo " ") $regionWasActive-p)
      (goto-char $p2)
      (set-mark $p1)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $n)))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "t") 'xah-cycle-hyphen-lowline-space) $kmap)))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.
when there is no selection,
• if cursor is on a any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.
when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.
URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-02-04"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        ;; (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-syntax-forward "_w")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        ;; (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, does not consider nesting. For example, if text is
(a(b)c▮)
the selected char is “c”, not “a(b)c”.
URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-11-24 2021-07-11"
  (interactive)
  (let ( $skipChars $p1 )
    (setq $skipChars "^\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))


;; keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ",nf") 'xah-forward-right-bracket)
  (define-key evil-normal-state-map (kbd ",nb") 'xah-backward-left-bracket)
  (define-key evil-normal-state-map (kbd ",na") 'xah-beginning-of-line-or-block)
  (define-key evil-normal-state-map (kbd ",ne") 'xah-end-of-line-or-block)
  (define-key evil-normal-state-map (kbd ",nm") 'xah-goto-matching-bracket)
  (define-key evil-normal-state-map (kbd ",nc") 'xah-toggle-letter-case)
  (define-key evil-normal-state-map (kbd ",nn") 'xah-space-to-newline)
  (define-key evil-normal-state-map (kbd ",nq") 'xah-quote-lines)
  (define-key evil-normal-state-map (kbd ",n1") 'xah-change-newline-chars-to-one)
  (define-key evil-normal-state-map (kbd ",nl") 'xah-reformat-lines)
  (define-key evil-normal-state-map (kbd ",nh") 'xah-cycle-hyphen-lowline-space)
  (define-key evil-normal-state-map (kbd ",ns") 'xah-extend-selection)
  (define-key evil-normal-state-map (kbd ",ni") 'xah-select-text-in-quote)

  (define-key evil-visual-state-map (kbd ",na") 'xah-beginning-of-line-or-block)
  (define-key evil-visual-state-map (kbd ",ne") 'xah-end-of-line-or-block)
  (define-key evil-visual-state-map (kbd ",nc") 'xah-toggle-letter-case)
  (define-key evil-visual-state-map (kbd ",nh") 'xah-cycle-hyphen-lowline-space)
  (define-key evil-visual-state-map (kbd ",ns") 'xah-extend-selection)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "b"  '(:ignore t :which-key "buffer")
    "bn"  '(xah-new-empty-buffer :which-key "new buffer"))

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    ","  '(xah-select-block :which-key "xah-select-block")))


(provide 'init-xah)
;;; init-xah.el ends here
