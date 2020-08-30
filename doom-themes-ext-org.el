;;; doom-themes-ext-org.el --- improve org-mode support for doom-themes -*- lexical-binding: t; -*-

(defgroup doom-themes-org nil
  "Options for doom's org customizations."
  :group 'doom-themes)

(defcustom doom-org-special-tags t
  "If non-nil, highlight #hashtags and @attags especially."
  :type 'boolean
  :group 'doom-themes-org)

;; TODO Remove this once released with org-mode
(defface org-upcoming-distant-deadline '((t :inherit font-lock-comment-face))
  "Face for items scheduled previously, not done, and have a distant deadline.
See also `org-agenda-deadline-faces'."
  :group 'doom-themes-org)

;;
(defsubst doom-themes--org-tag-face (n)
  (let ((kwd (match-string n)))
    (or (and (equal kwd "#") 'org-tag)
        (and (equal kwd "@") 'org-formula))))

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
                        (,org-todo (2 (org-get-todo-face 2) t))
                        (,org-done (2 'org-headline-done t)))
                      (when (memq 'date org-activate-links)
                        '((org-activate-dates (0 'org-date t)))))
              org-font-lock-extra-keywords)
             ;; respsect underlying faces!
             `((,org-todo (2 (org-get-todo-face 2) prepend))
               (,org-done (2 'org-headline-done prepend)))
             (when (memq 'date org-activate-links)
               '((org-activate-dates (0 'org-date prepend))))
             ;; Make checkbox statistic cookies respect underlying faces
             '(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                (0 (org-get-checkbox-statistics-face) prepend))
               ;; I like how org-mode fontifies checked TODOs and want this to extend to
               ;; checked checkbox items:
               ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                1 'org-headline-done prepend)
               ;; make plain list bullets stand out
               ("^ *\\([-+]\\|\\(?:[0-9]+\\|[a-zA-Z]\\)[).]\\)[ \t]" 1 'org-list-dt append)
               ;; and separators/dividers
               ("^ *\\(-----+\\)$" 1 'org-meta-line))
             ;; custom #hashtags & @at-tags for another level of organization
             (when doom-org-special-tags
               '(("\\s-\\(\\([#@]\\)[^+ \n.,]+\\)" 1 (doom-themes--org-tag-face 2) prepend)))))))


;; Bootstrap
(setq org-hide-leading-stars t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(add-hook 'org-font-lock-set-keywords-hook #'doom-themes-enable-org-fontification)

;;;###autoload
(defun doom-themes-org-config ()
  "Enable custom fontification & improves theme integration with org-mode.")

(provide 'doom-themes-ext-org)
;;; doom-themes-ext-org.el ends here
