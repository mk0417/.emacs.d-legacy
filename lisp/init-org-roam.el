;;; init-org-roam.el --- Org-roam -*- lexical-binding: t -*-

(straight-use-package 'org-roam)


(setq org-roam-v2-ack t)
(setq org-roam-directory "~/Dropbox/org-roam")
(setq org-roam-db-location "~/Dropbox/org-roam")

(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "n"   '(:ignore t :which-key "note")
    "nr"  '(:ignore t :which-key "org-roam")
    "nrt" '(org-roam-buffer-toggle :which-key "org roam buffer toggle")
    "nrf" '(org-roam-node-find :which-key "org roam node find")
    "nri" '(org-roam-node-insert :which-key "org roam node insert")))


(provide 'init-org-roam)
;;; init-org-roam.el ends here
