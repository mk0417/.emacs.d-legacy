;;; init-org.el --- Org-mode -*- lexical-binding: t -*-

(straight-use-package 'org)
(straight-use-package 'org-superstar)
(straight-use-package 'org-tree-slide)
(straight-use-package 'olivetti)
(straight-use-package 'org-journal)


;; org
(setq org-directory "~/Dropbox/org"
      org-agenda-files '("~/Dropbox/org/todo.org")
      org-log-done t
      org-startup-indented t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-export-use-babel nil
      org-confirm-babel-evaluate nil
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WORKING(w)" "|" "DONE(d)" "CANCEL(c)")))

(with-eval-after-load 'org
  (require 'org-tempo)
  (require 'ob)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
  (add-to-list 'org-structure-template-alist '("b" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("j" . "src python :session py :eval no-export")))

(with-eval-after-load 'ob
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;; org-superstar
(add-hook 'org-mode-hook 'org-superstar-mode)

(setq org-superstar-remove-leading-stars t
      org-superstar-headline-bullets-list '("◉" "▷" "○")
      org-superstar-item-bullet-alist
      '((?+ . ?•)
        (?* . ?➤)
        (?- . ?–)))

;; org-journal
(setq org-journal-dir "~/Dropbox/org/journal"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-file-type 'monthly
      org-journal-enable-agenda-integration t)

;; presentation (org-tree-slide + olivetti)
(setq org-tree-slide-header nil
      org-tree-slide-slide-in-effect t
      org-tree-slide-heading-emphasis nil
      org-tree-slide-cursor-init t
      org-tree-slide-modeline-display 'outside
      org-tree-slide-skip-done nil
      org-tree-slide-skip-comments t
      org-tree-slide-skip-outline-level 5
      org-tree-slide-activate-message (propertize "Presentation mode ON" 'face 'success)
      org-tree-slide-deactivate-message (propertize "Presentation mode OFF" 'face 'success))

(setq olivetti-body-width 0.7
      olivetti-minimum-body-width 80
      olivetti-recall-visual-line-mode-entry-state t)

(add-hook 'org-tree-slide-play-hook (lambda ()
                                      (fringe-mode '(0 . 0))
                                      (text-scale-increase 3)
                                      (olivetti-mode 1)))
(add-hook 'org-tree-slide-stop-hook (lambda ()
                                      (text-scale-adjust 0)
                                      (olivetti-mode -1)))

(with-eval-after-load 'olivetti
  (dolist (m '(org-indent-mode visual-line-mode olivetti-mode text-scale-mode))
    (diminish m)))

;; capturing
(setq org-capture-templates
      `(("t" "todo" entry (file ,(concat org-directory "/todo.org"))
         "* TODO %^{Title}\nSCHEDULED: %^t\n")
        ("n" "note" entry (file ,(concat org-directory "/note.org"))
         "* %U\n")
        ("i" "idea" entry (file ,(concat org-directory "/idea.org"))
         "* %^{Title}\n%U\n")))

;; re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

;; ignore heading with no_heading tag when exporting
;; https://emacs.stackexchange.com/questions/9492/is-it-possible-to-export-content-of-subtrees-without-their-headings/17677
(defun p-org-export-no-heading (backend)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "no_heading"))
(add-hook 'org-export-before-processing-hook 'p-org-export-no-heading)

;; latex
;; https://github.com/GeneKao/orgmode-latex-templates
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("ethz"
               "\\documentclass[a4paper,11pt,titlepage]{memoir}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(add-to-list 'org-latex-classes '("ebook"
                                  "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")))


;; keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link))

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "n"   '(:ignore t :which-key "note")
    "na"  '(org-agenda :which-key "org agenda")
    "nc"  '(org-capture :which-key "org capture")
    "nj"  '(org-journal-new-entry :which-key "new journal"))

  (general-create-definer p-org-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'org-mode-map)
  (p-org-leader-def
    "."  '(org-toggle-narrow-to-subtree :which-key "narrow to substree")
    ";"  '(org-toggle-latex-fragment :which-key "latex preview")
    "j"  '(:ignore t :which-key "presentation")
    "jj" '(org-tree-slide-mode :which-key "presentation on")
    "jn" '(org-tree-slide-move-next-tree :which-key "next slide")
    "jp" '(org-tree-slide-move-previous-tree :which-key "previous slide")
    "t"  '(:ignore t :which-key "table")
    "tk" '(org-table-move-row-up :which-key "move row up")
    "tj" '(org-table-move-row-down :which-key "move row down")
    "tl" '(org-table-move-column-right :which-key "move column right")
    "th" '(org-table-move-column-left :which-key "move column left")
    "tc" '(org-table-convert-region :which-key "convert region to table")))


(provide 'init-org)
;;; init-org.el ends here
