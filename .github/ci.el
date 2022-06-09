;;; ci.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Types
(add-to-list 'doom-cli-commit-types 'theme)
(add-to-list 'doom-cli-commit-scopeless-types 'theme)


;;; Scopes
(add-to-list 'doom-cli-commit-scopes "base")
(cl-loop with default-directory = (dir!)
         for file in (doom-files-in "../themes/" :match "-theme\\.el$")
         if (file-name-base file)
         if (string-remove-prefix "doom-" it)
         if (string-remove-suffix "-theme" it)
         do (add-to-list 'doom-cli-commit-scopes it))
(cl-loop with default-directory = (dir!)
         for file in (doom-files-in "../extensions/" :match "\\.el$")
         if (file-name-base file)
         if (string-remove-prefix "doom-themes-" it)
         do (add-to-list 'doom-cli-commit-scopes it))

;;; ci.el ends here
