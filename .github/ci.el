;;; ci.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Types
(add-to-list 'doom-cli-commit-types 'module)
(add-to-list 'doom-cli-commit-scopeless-types 'module)


;;; Scopes
(add-to-list 'doom-cli-commit-scopes "base")
(add-to-list 'doom-cli-commit-scopes #'ci-theme-scope)
(add-to-list 'doom-cli-commit-scopes #'ci-extension-scope)


;;; Helpers
(cl-defun ci-theme-scope (scope (&key type))
  "Only allow scopes for themes that actually exist."
  (cl-loop with default-directory = (dir!)
           for file in (doom-files-in "../themes/" :match "-theme\\.el$")
           if (file-name-base file)
           if (string-remove-prefix "doom-" it)
           if (string-remove-suffix "-theme" it)
           if (equal it scope)
           return t))

(cl-defun ci-extension-scope (scope (&key type))
  "Only allow scopes for extensions that actually exist."
  (cl-loop with default-directory = (dir!)
           for file in (doom-files-in "../extensions/" :match "\\.el$")
           if (file-name-base file)
           if (string-remove-prefix "doom-themes-" it)
           if (equal it scope)
           return t))

;;; ci.el ends here
