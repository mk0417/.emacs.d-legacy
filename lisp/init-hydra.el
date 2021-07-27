;;; init-hydra.el --- Hydra -*- lexical-binding: t -*-

(straight-use-package 'hydra)


(defhydra hydra-quick-files (:color blue)
  "
^Files^                              ^Folders^
^-----^------------------------------^-------^--------------
_i_: research idea                   _n_: my notes
_t_: todo                            _l_: literature
_s_: scratch note                    _j_: journal
_y_: yankpad                         ^ ^
  "
  ("i" (find-file "~/Dropbox/org/idea.org"))
  ("s" (find-file "~/Dropbox/org/note.org"))
  ("t" (find-file "~/Dropbox/org/todo.org"))
  ("y" (find-file "~/Dropbox/org/yankpad.org"))
  ("j" (dired "~/Dropbox/org/journal"))
  ("n" (dired "~/Dropbox/org/p-notes"))
  ("l" (dired "~/Dropbox/literature")))

(defhydra hydra-zoom (:color pink)
  ("i" text-scale-increase "zoom in")
  ("o" text-scale-decrease "zoom out")
  ("r" (text-scale-adjust 0) "zoom reset" :exit t))


(with-eval-after-load 'evil
  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "h"   '(:ignore t :which-key "hydra")
    "hf"  '(hydra-quick-files/body :which-key "hydra files")
    "ht"  '(hydra-zoom/body :which-key "hydra text zoom")))


(provide 'init-hydra)
;;; init-hydra.el ends here
