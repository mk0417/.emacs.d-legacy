;;; init-typing.el --- Efficient typing -*- lexical-binding: t -*-

(straight-use-package 'key-chord)


(add-hook 'after-init-hook 'key-chord-mode)
(setq key-chord-two-keys-delay 0.3)

(with-eval-after-load 'evil
  (dolist (m (list evil-insert-state-map evil-ex-completion-map minibuffer-local-map))
    (key-chord-define m "kk" "()\C-b")
    (key-chord-define m ",," "[]\C-b")
    (key-chord-define m "hh" "{}\C-b")
    (key-chord-define m ",." "[[]]\C-b\C-b")
    (key-chord-define m "gt" "!")
    (key-chord-define m "qa" "@")
    (key-chord-define m "lk" "$")
    (key-chord-define m "fh" "%")
    (key-chord-define m "pw" "^")
    (key-chord-define m "aa" "&")
    (key-chord-define m "cj" "*")
    (key-chord-define m "uu" "_")
    (key-chord-define m ",j" "-")
    (key-chord-define m "jj" "+")
    (key-chord-define m ",e" "=")
    (key-chord-define m ",v" "|")
    (key-chord-define m "bw" "~")
    (key-chord-define m ",l" "<")
    (key-chord-define m ",g" ">")
    (key-chord-define m ",t" "<>\C-b")
    (key-chord-define m ",s" "---")
    (key-chord-define m "ww" "?")
    (key-chord-define m "vv" "#")
    (key-chord-define m ",a" "--")
    (key-chord-define m ",c" "<=")
    (key-chord-define m ",b" ">=")
    (key-chord-define m ",d" "==")
    (key-chord-define m ";r" "<-")
    (key-chord-define m ";j" "%>%")
    (key-chord-define m "nj" "|>")
    (key-chord-define m "mj" "=>")
    (key-chord-define m ";f" "5")
    (key-chord-define m ";i" "6")
    (key-chord-define m ";s" "7")
    (key-chord-define m ";e" "8")
    (key-chord-define m ";n" "9")
    (key-chord-define m ";t" "0"))

  ;; british pound
  ;; £ is not working in key-chord
  (with-eval-after-load 'general
    (general-evil-setup t)
    (defun p-insert-pound ()
      (interactive)
      (insert "£"))
    (general-imap "y"
      (general-key-dispatch 'self-insert-command
        :timeout 0.3
        "b" 'p-insert-pound))))


(provide 'init-typing)
;;; init-typing.el.el ends here
