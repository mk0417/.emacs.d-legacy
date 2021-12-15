;;; init-completion.el --- Completion framework -*- lexical-binding: t -*-

(straight-use-package 'company)
(straight-use-package 'company-statistics)


;; company
(setq company-backends
      '((company-files company-keywords company-capf company-yasnippet)
        (company-abbrev company-dabbrev)))

(setq company-idle-delay 0
      company-auto-complete nil
      company-auto-complete-chars nil
      company-auto-commit nil
      company-tooltip-limit 10
      company-tooltip-align-annotations t
      company-tooltip-margin 0
      company-minimum-prefix-length 2
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-require-match nil
      company-dabbrev-other-buffers nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-global-modes '(not erc-mode
                                 message-mode
                                 help-mode
                                 gud-mode
                                 vterm-mode))

(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
(setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
(setq dabbrev-backward-only nil)
(setq dabbrev-case-distinction 'case-replace)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace 'case-replace)
(setq dabbrev-check-other-buffers t)
(setq dabbrev-eliminate-newlines t)
(setq dabbrev-upcase-means-case-search t)
(let ((map global-map))
  (define-key map (kbd "C-x a d") #'dabbrev-expand)
  (define-key map (kbd "C-x a c") #'dabbrev-completion))

;; remove duplicate candidate.
(with-eval-after-load 'company
  (add-to-list 'company-transformers #'delete-dups))

(autoload 'company-mode "company")

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (company-statistics-mode)

  ;; tabnine
  (add-to-list 'company-backends #'company-tabnine)

  (custom-set-faces
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "#ffeead" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "#69adc6" :foreground "white"))))
   '(company-tooltip-annotation
     ((t (:background "#ffeead" :foreground "red"))))
   '(company-tooltip-common
     ((t (:background "#ffeead" :foreground "black")))))

  (define-key company-mode-map   (kbd "C-j") 'company-complete)
  (define-key company-mode-map   [remap completion-at-point] 'company-complete)
  (define-key company-mode-map   [remap indent-for-tab-command] 'company-indent-or-complete-common)
  (define-key company-active-map (kbd "C-m") 'company-complete-selection)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word)
  (define-key company-active-map (kbd "C-k") 'delete-backward-char)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-o") 'company-filter-candidates)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (diminish 'company-mode))


(provide 'init-completion)
;;; init-completion.el ends here
