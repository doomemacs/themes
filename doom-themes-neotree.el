;;; doom-themes-neotree.el -*- lexical-binding: t; -*-

(unless doom-themes--inhibit-warning
  (message "doom-themes: loading `doom-themes-neotree' directly is obsolete, call `doom-themes-neotree-config' instead"))

(defgroup doom-neotree nil
  "Options for doom's neotree theme"
  :group 'doom-themes)

;;
(defface doom-neotree-dir-face  '((t (:inherit neo-dir-link-face)))
  "Face for directory labels."
  :group 'doom-neotree)

(defface doom-neotree-file-face '((t (:inherit neo-file-link-face)))
  "Face for file name labels."
  :group 'doom-neotree)

;; file type faces
(defface doom-neotree-hidden-file-face '((t (:inherit font-lock-comment-face)))
  "Face for labels of hidden files. See `doom-neotree-file-face-re-alist'."
  :group 'doom-neotree)

(defface doom-neotree-text-file-face '((t (:inherit neo-file-link-face)))
  "Face for labels of text/documentation files (readmes, org files, etc). See
`doom-neotree-file-face-re-alist'."
  :group 'doom-neotree)

(defface doom-neotree-media-file-face '((t (:inherit neo-file-link-face)))
  "Face for labels of media files. See `doom-neotree-file-face-re-alist'."
  :group 'doom-neotree)

(defface doom-neotree-data-file-face '((t (:inherit neo-file-link-face)))
  "Face for labels of data files (json, yaml, xml, etc). See
`doom-neotree-file-face-re-alist'."
  :group 'doom-neotree)


;;
(defcustom doom-neotree-project-size 1.4
  "What :height to display the project icon at the top at."
  :type 'float
  :group 'doom-neotree)

(defcustom doom-neotree-folder-size 1.05
  "What :height to display the folder icons at."
  :type 'float
  :group 'doom-neotree)

(defcustom doom-neotree-chevron-size 0.8
  "What :height to display the chevron icons at."
  :type 'float
  :group 'doom-neotree)

(defcustom doom-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'doom-neotree)

(define-obsolete-variable-alias 'doom-neotree-enable-file-icons 'doom-neotree-file-icons)
(defcustom doom-neotree-file-icons t
  "If non-nil, a minimalistic neotree icon theme that takes after Atom will be
used. It only distinguishes text, source, pdfs, images and binary files, rather
than file type."
  :type '(choice
          (const :tag "Minimalistic file icons (like Atom's)" t)
          (const :tag "Disable file icons" nil))
  :group 'doom-neotree)

(defcustom doom-neotree-enable-folder-icons t
  "If non-nil, display folder icons next to each file. Different icons are used
depending on whether the folder is a repo, symlink or regular folder."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-open-chevron-icons t
  "If non-nil, display the chevron-down icon next to each expanded folder."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-closed-chevron-icons t
  "If non-nil, display the chevron-right icon next to each collapsed folder."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-variable-pitch nil
  "If non-nil, labels will use the `doom-neotree-dir-face' and
`doom-neotree-dir-face' faces, which inherit from the `variable-pitch' face."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-type-colors t
  "If non-nil, color each file/folder based on the categories determined by
`doom-neotree-file-face-re-alist'."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-file-face-re-alist
  '(("\\(/\\.[^$/]+\\|\\.\\(lock\\|resolved\\|o\\|pyc\\|elc\\)$\\|/\\(node_modules\\|vendor\\)[/$]\\)"
     . doom-neotree-hidden-file-face)
    ("\\(\\.\\(md\\|org\\|rst\\|log\\)\\|/[A-Z_-]+\\(\\.[a-z]+\\)?\\)$"
     . doom-neotree-text-file-face)
    ("\\.\\(png\\|jpe?g\\|gif\\|tiff\\|svg\\|bmp\\|mov\\|avi\\|mp[34]\\|webm\\|zip\\|tar\\(\\.gz\\)?\\|7z\\|rar\\)$"
     . doom-neotree-media-file-face)
    ("\\.\\([jc]son\\|\\(ya?\\|x\\|to\\)ml\\|xml\\)"
     . doom-neotree-data-file-face))
  "Regexps used to determine what category each file/folder belongs to, and what
face to assign them."
  :type '(repeat (cons (regexp :tag "Pattern")
                       (symbol :tag "Face")))
  :group 'doom-neotree)

(defvar doom--neotree-file-re
  `((code    . ,(concat "\\.\\(p?html?\\|xml\\|ya?ml\\|json\\|tpl\\|conf\\|erb\\|mustache\\|twig\\|ejs\\|haml\\|pug\\|jade\\)$"))
    (media   . ,(concat "\\.\\("
                        "png\\|jpe?g\\|gif\\|tiff\\|svg\\|bmp" ; images
                        "\\|mov\\|avi\\|mp[34]\\|webm"         ; media
                        "\\)$"
                        ))
    (archive . "\\.\\(zip\\|rar\\|7z\\|tar\\(\\.gz\\)?\\)$"))
  "An alist mapping file type to regular expressions, used to determine what
type of icon to display for the file if `doom-neotree-file-icons' is set to
`simple'.")


;;
(defun doom--neotree-no-fringes ()
  "Remove fringes in neotree. They get reset each time you select the neotree
pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 0 0))

(defun doom--neotree-setup (&rest _)
  (setq line-spacing doom-neotree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

(defun doom--neotree-folder-icon-for (dir chevron &optional faces)
  (let* ((path (expand-file-name dir))
         (chevron
          (if chevron
              (all-the-icons-octicon
               (format "chevron-%s" chevron)
               :v-adjust 0.1
               :face `(:inherit (,@faces)
                       :family ,(all-the-icons-octicon-family)
                       :height ,doom-neotree-chevron-size))
            spc))
         (icon
          (when doom-neotree-enable-folder-icons
            (all-the-icons-octicon
             (cond ((file-symlink-p path) "file-symlink-directory")
                   ((file-exists-p (format "%s/.git" path)) "file-submodule")
                   ((all-the-icons-dir-is-submodule path) "file-submodule")
                   (t "file-directory"))
             :v-adjust 0
             :face `(:inherit (,@faces)
                     :family ,(all-the-icons-octicon-family)
                     :height ,doom-neotree-folder-size)))))
    (concat chevron "\t" icon)))

(defun doom--neotree-file-icon-for (file-name &optional faces)
  (if file-name
      (propertize
       (cond ((string-match-p (cdr (assq 'code doom--neotree-file-re)) file-name)
              (all-the-icons-octicon "file-code"))
             ((string-match-p (cdr (assq 'media doom--neotree-file-re)) file-name)
              (all-the-icons-octicon "file-media"))
             ((string-match-p (cdr (assq 'archive doom--neotree-file-re)) file-name)
              (all-the-icons-octicon "file-zip"))
             ((string= (or (file-name-extension file-name) "") "pdf")
              (all-the-icons-octicon "file-pdf"))
             ((file-symlink-p file-name)
              (all-the-icons-octicon "file-symlink-file"))
             ((file-executable-p file-name)
              (all-the-icons-octicon "file-binary"))
             (t
              (all-the-icons-octicon "file-text")))
       'face `(:inherit (,@faces)
                        :family ,(all-the-icons-octicon-family)
                        :height 1.3)
       'display '(raise 0))
    (all-the-icons-fileicon "default")))

(defun doom--neo-insert-fold-symbol (type file-name &optional faces)
  "Custom hybrid unicode theme with leading whitespace."
  (let ((spc "\t")
        (vspc (propertize "  " 'face 'variable-pitch)))
    (or (and (eq type 'open)
             (insert
              (concat spc
                      (doom--neotree-folder-icon-for
                       file-name
                       (if doom-neotree-enable-open-chevron-icons "down")
                       faces)
                      vspc)))
        (and (eq type 'close)
             (insert
              (concat spc
                      (doom--neotree-folder-icon-for
                       file-name
                       (if doom-neotree-enable-closed-chevron-icons "right")
                       faces)
                      vspc)))
        (and (eq type 'leaf)
             (insert
              (concat (when (or doom-neotree-enable-open-chevron-icons
                                doom-neotree-enable-closed-chevron-icons)
                        spc)
                      (when doom-neotree-enable-folder-icons spc)
                      (when doom-neotree-file-icons
                        (concat spc (doom--neotree-file-icon-for file-name faces)))
                      vspc))))))

(defun doom--neo-get-file-face (name)
  (when doom-neotree-enable-type-colors
    (let ((name (concat "/" (file-relative-name name neo-buffer--start-node)))
          case-fold-search)
      (cdr-safe
       (cl-loop for re in doom-neotree-file-face-re-alist
                when (string-match-p (car re) name)
                return re)))))

(defun doom--neo-buffer--insert-root-entry (node)
  "Pretty-print pwd in neotree"
  (let ((project-name (or (car (last (split-string node "/" t))) "-"))
        (faces '(neo-root-dir-face)))
    (when doom-neotree-enable-variable-pitch
      (push 'variable-pitch faces))
    (when (display-graphic-p)
      (insert
       (concat (propertize " " 'face `(:inherit (,@faces)))
               (all-the-icons-octicon "repo"
                                      :height doom-neotree-project-size
                                      :face 'neo-root-dir-face
                                      :v-adjust -0.1)
               (propertize " " 'face 'neo-root-dir-face))))
    (insert (propertize (concat project-name "\n") 'face `(:inherit (,@faces))))))

(defun doom--neo-buffer--insert-dir-entry (node depth expanded)
  (let ((node-short-name (neo-path--file-short-name node))
        (faces '(doom-neotree-dir-face))
        (add-face (doom--neo-get-file-face node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    ;; (when (memq 'char neo-vc-integration)
    ;;   (insert-char ?\s 2))
    (when add-face (setq faces (list add-face)))
    ;; (when (memq 'face neo-vc-integration)
    ;;   (push (cdr vc) faces))
    (if (display-graphic-p)
        (doom--neo-insert-fold-symbol (if expanded 'open 'close) node faces)
      (neo-buffer--insert-fold-symbol (if expanded 'open 'close) node))
    (when doom-neotree-enable-variable-pitch
      (push 'variable-pitch faces))
    ;;
    (insert-button node-short-name
                   'follow-link t
                   'face `(:inherit (,@faces))
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun doom--neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node))
        (vc (if neo-vc-integration (neo-vc-for-node node)))
        (faces '(doom-neotree-file-face))
        (add-face (doom--neo-get-file-face node)))
    (insert-char ?\s (* (- depth 1) 2))
    (when add-face (setq faces (list add-face)))
    (when (and (memq 'face neo-vc-integration)
               (not (eq (cdr vc) 'neo-vc-up-to-date-face)))
      (push (cdr vc) faces))
    (if (display-graphic-p)
        (doom--neo-insert-fold-symbol 'leaf node faces)
      (neo-buffer--insert-fold-symbol 'leaf node))
    (when doom-neotree-enable-variable-pitch
      (push 'variable-pitch faces))
    ;;
    (insert-button node-short-name
                   'follow-link t
                   'face `(:inherit (,@faces))
                   'neo-full-path node
                   'keymap neotree-file-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))


;;
(eval-after-load "neotree"
  (lambda ()
    (unless (require 'all-the-icons nil t)
      (error "all-the-icons isn't installed"))

    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook #'doom--neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add #'neo-global--select-window :after #'doom--neotree-no-fringes)
    ;; Patch neotree to use `doom--neo-insert-fold-symbol'
    (advice-add #'neo-buffer--insert-file-entry :override #'doom--neo-buffer--insert-file-entry)
    (advice-add #'neo-buffer--insert-dir-entry  :override #'doom--neo-buffer--insert-dir-entry)
    ;; Shorter pwd in neotree
    (advice-add #'neo-buffer--insert-root-entry :override #'doom--neo-buffer--insert-root-entry)))

(provide 'doom-themes-neotree)
;;; doom-themes-neotree.el ends here
