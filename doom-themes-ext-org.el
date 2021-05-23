;;; doom-themes-ext-org.el --- fix fontification issues in org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: August 3, 2017
;; Homepage: https://github.com/hlissner/doom-themes-ext-org
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Fixes a few fontification issues in org-mode and adds special fontification
;; for @-tags and #hashtags. Simply load this file to use it.
;;
;;   (with-eval-after-load 'org-mode
;;     (require 'doom-themes-ext-org))
;;
;; Or call `doom-themes-enable-org-config', which does nothing but load this
;; package (because it's autoloaded).
;;
;;   (with-eval-after-load 'org-mode
;;     (doom-themes-enable-org-config))
;;
;;; Code:

(defgroup doom-themes-org nil
  "Options for doom's org customizations."
  :group 'doom-themes)

(define-obsolete-variable-alias
  'doom-org-special-tags 'doom-themes-org-fontify-special-tags
  "2021-02-10")
(defcustom doom-themes-org-fontify-special-tags t
  "If non-nil, fontify #hashtags and @attags.
Uses `doom-themes-org-at-tag' and `doom-themes-org-hash-tag' faces."
  :type 'boolean
  :group 'doom-themes-org)

(defface doom-themes-org-at-tag '((t :inherit org-formula))
  "Face used to fontify @-tags in org-mode."
  :group 'doom-themes-org)

(defface doom-themes-org-hash-tag '((t :inherit org-tag))
  "Face used to fontify #hashtags in org-mode."
  :group 'doom-themes-org)


(defvar org-done-keywords)
(defvar org-font-lock-extra-keywords)
(defvar org-heading-keyword-regexp-format)
(defvar org-todo-regexp)
(defvar org-fontify-done-headline)
(defvar org-activate-links)
(declare-function org-delete-all "ext:org" (elts list))

;;
(defun doom-themes--org-tag-face (n)
  "Return the face to use for the currently matched tag.
N is the match index."
  (declare (pure t) (side-effect-free t))
  (pcase (match-string n)
    ("#" 'doom-themes-org-hash-tag)
    ("@" 'doom-themes-org-at-tag)))

(defun doom-themes-enable-org-fontification ()
  "Correct (and improve) org-mode's font-lock keywords.

  1. Re-set `org-todo' & `org-headline-done' faces, to make them respect
     (inherit) underlying faces.
  2. Make statistic cookies respect (inherit) underlying faces.
  3. Fontify item bullets (make them stand out)
  4. Fontify item checkboxes (and when they're marked done), like TODOs that are
     marked done.
  5. Fontify dividers/separators (5+ dashes)
  6. Fontify #hashtags and @at-tags, for personal convenience; see
     `doom-org-special-tags' to disable this."
  (let ((org-todo (format org-heading-keyword-regexp-format
                          org-todo-regexp))
        (org-done (format org-heading-keyword-regexp-format
                          (concat "\\(?:" (mapconcat #'regexp-quote org-done-keywords "\\|") "\\)"))))
    (setq
     org-font-lock-extra-keywords
     (append (org-delete-all
              (append `(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                         (0 (org-get-checkbox-statistics-face) t))
                        (,org-todo (2 (org-get-todo-face 2) t)))
                      (when org-fontify-done-headline
                        `((,org-done (2 'org-headline-done t))))
                      (when (memq 'date org-activate-links)
                        '((org-activate-dates (0 'org-date t)))))
              org-font-lock-extra-keywords)
             ;; respsect underlying faces!
             `((,org-todo (2 (org-get-todo-face 2) prepend)))
             (when org-fontify-done-headline
               `((,org-done (2 'org-headline-done prepend))))
             (when (memq 'date org-activate-links)
               '((org-activate-dates (0 'org-date prepend))))
             ;; Make checkbox statistic cookies respect underlying faces
             '(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                (0 (org-get-checkbox-statistics-face) prepend))
               ;; make plain list bullets stand out
               ("^ *\\([-+]\\|\\(?:[0-9]+\\|[a-zA-Z]\\)[).]\\)[ \t]" 1 'org-list-dt append)
               ;; and separators/dividers
               ("^ *\\(-----+\\)$" 1 'org-meta-line))
             ;; I like how org-mode fontifies checked TODOs and want this to
             ;; extend to checked checkbox items:
             (when org-fontify-done-headline
               '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                  1 'org-headline-done prepend)))
             ;; custom #hashtags & @at-tags for another level of organization
             (when doom-themes-org-fontify-special-tags
               '(("\\s-\\(\\([#@]\\)[^+ \n.,]+\\)" 1 (doom-themes--org-tag-face 2) prepend)))))))

(add-hook 'org-font-lock-set-keywords-hook #'doom-themes-enable-org-fontification)

;;;###autoload
(defun doom-themes-org-config ()
  "Load `doom-themes-ext-org'.")

(provide 'doom-themes-ext-org)
;;; doom-themes-ext-org.el ends here
