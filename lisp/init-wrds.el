;;; init-wrds.el --- WRDS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Connect to WRDS database
(setq sql-connection-alist
      '((wrds-pg (sql-product 'postgres)
                 (sql-username (with-temp-buffer (insert-file-contents "~/.pass/wrds/user.txt") (buffer-string)))
                 (sql-password (with-temp-buffer (insert-file-contents "~/.pass/wrds/password.txt") (buffer-string)))
                 (sql-server "wrds-pgdata.wharton.upenn.edu")
                 (sql-port 9737)
                 (sql-database "wrds")
                 (sql-sslmode "require"))))


(provide 'init-wrds)
;;; init-wrds.el ends here
