;;; prot-orderless.el --- Extensions for Orderless -*- lexical-binding: t -*-
;; Copyright (C) 2020-2021  Protesilaos Stavrou
;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs

(defgroup prot-orderless ()
  "Tweaks for the Orderless completion style."
  :group 'minibuffer)

(defcustom prot-orderless-default-styles
  '(orderless-flex
    orderless-strict-leading-initialism
    orderless-regexp
    orderless-prefixes
    orderless-literal)
  "List that should be assigned to `orderless-matching-styles'."
  :type 'list
  :group 'prot-orderless)

(defcustom prot-orderless-alternative-styles
  '(orderless-literal
    orderless-prefixes
    orderless-strict-leading-initialism
    orderless-regexp)
  "Alternative list for `orderless-matching-styles'.
Unlike `prot-orderless-default-styles', this variable is intended
for use on a case-by-case basis, with the help of the function
`prot-orderless-with-styles'."
  :type 'list
  :group 'prot-orderless)

(defun prot-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun prot-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

(defun prot-orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defvar orderless-matching-styles)

;;;###autoload
(defun prot-orderless-with-styles (cmd &optional styles)
  "Call CMD with optional orderless STYLES.
STYLES is a list of pattern matching methods that is passed to
`orderless-matching-styles'.  Its fallback value is that of
`prot-orderless-alternative-styles'."
  (let ((orderless-matching-styles (or styles prot-orderless-alternative-styles))
        (this-command cmd))
    (call-interactively cmd)))


(provide 'prot-orderless)
;;; prot-orderless.el ends here
