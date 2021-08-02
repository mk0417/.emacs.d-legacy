;;; init-xah.el --- Functions from Xah Lee -*- lexical-binding: t -*-

(straight-use-package 'xah-replace-pairs)


;; new scratch buffer
(defun xah-new-empty-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

;; select block between blank lines
;; http://ergoemacs.org/emacs/modernization_mark-word.html
(defun xah-select-block ()
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move")
      (forward-line -1))))

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
(setq xah-brackets "()[]{}<>“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋❨❩❪❫❴❵❬❭❮❯❰❱❲❳⸢⸣⸤⸥⸦⸧⸨⸩｟｠⸜⸝⸌⸍﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")
(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" ))
(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))

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


(autoload 'xah-replace-pairs-region "xah-replace-pairs")
(autoload 'xah-replace-regexp-pairs-region "xah-replace-pairs")

(defun xah-replace-paren-to-bracket (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["(" "["]
                                [")" "]"])
                              'REPORT)))

(defun xah-replace-bracket-to-paren (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["[" "("]
                                ["]" ")"])
                              'REPORT)))

(defun xah-replace-true-to-false (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["True" "False"])
                              'REPORT)))

(defun xah-replace-false-to-true (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["False" "True"])
                              'REPORT)))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "nf")  'xah-forward-right-bracket)
  (define-key evil-normal-state-map (kbd "nb")  'xah-backward-left-bracket)
  (define-key evil-normal-state-map (kbd "nF")  'xah-replace-paren-to-bracket)
  (define-key evil-normal-state-map (kbd "nP")  'xah-replace-bracket-to-paren)
  (define-key evil-normal-state-map (kbd "nt")  'xah-replace-true-to-false)
  (define-key evil-normal-state-map (kbd "nT")  'xah-replace-false-to-true)

  (define-key evil-visual-state-map (kbd "nF")  'xah-replace-paren-to-bracket)
  (define-key evil-visual-state-map (kbd "nP")  'xah-replace-bracket-to-paren)

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
