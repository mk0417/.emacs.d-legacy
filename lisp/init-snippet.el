;;; init-snippet.el --- Snippet/abbrev -*- lexical-binding: t -*-

(straight-use-package 'yankpad)


;; yankpad
(setq yankpad-file "~/Dropbox/org/yankpad.org")

;; abbrev mode
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(("afaik" "As far as i know" )
    ("atm" "at the moment" )
    ("btw" "By the way," )
    ("eq" "==" )
    ("dashline" "--------------------------------------------------" )
    ("wrdspgcon" "psql postgresql://username:password@wrds-pgdata.wharton.upenn.edu:9737/wrds?sslmode=require")
    ("wrdspgtable" "select table_name, table_schema, table_type from information_schema.tables where table_name='crsp';")
    ("wrdspgschema" "select schema_name from information_schema.schemata order by schema_name limit 10;")))

;; define major mode abbrev
(when (boundp 'org-mode-abbrev-table)
  (clear-abbrev-table org-mode-abbrev-table))
(define-abbrev-table 'org-mode-abbrev-table
  '(("tit" "#+TITLE:\n#+AUTHOR: Peng Li\n#+OPTIONS: ^:nil")
    ("srcpy" "#+begin_src jupyter-python :session py :eval no-export\n\n#+end_src")
    ("srcsh" "#+begin_src sh\n\n#+end_src ")
    ("srcsta"  "#+begin_src jupyter-stata :session stata :kernel stata :eval no-export\n\n#+end_src")
    ("tabl" "|   |   |\n|---+---|\n|   |   |")
    ("noh" "***** :no_heading:")
    ("texeq" "\\begin{equation*}\n\n\\end{equation*}")
    ("texeqn" "\\begin{equation*}\n\n\\end{equation*}")
    ("texeqi" "$$$$")))

(setq save-abbrevs nil)


(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "e"   '(:ignore t :which-key "editing")
    "ey"  '(yankpad-insert :which-key "insert yankpad snippet")))


(provide 'init-snippet)
;;; init-snippet.el ends here
