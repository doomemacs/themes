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

(defface doom-neotree-executable-file-face '((t (:inherit neo-file-link-face)))
  "TODO"
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
(defcustom doom-neotree-file-icons 'simple
  "The style to use for the file icons. Can be nil (disabled), non-nil (for a
diverse iconset), or 'simple, which is closest's to Atom's style as it only
distinguishes text, source, pdfs, images and binary files."
  :type '(choice
          (const :tag "A diverse array of file icons based on file type" t)
          (const :tag "Minimalistic file icons (like Atom's)" 'simple)
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


(defun doom--neo-is-repo-dir-p (path)
  (or (file-exists-p (format "%s/.git" path))
      (all-the-icons-dir-is-submodule path)))

(defvar doom-neotree-dir-rules
  (eval-when-compile
    `(("/\\(?:node_modules\\|vendor\\)$"
       :face doom-neotree-hidden-file-face)
      ("/\\.[^$/#]+$"
       :face doom-neotree-hidden-file-face)
      (file-symlink-p
       :icon (all-the-icons-octicon "file-symlink-directory"))
      (doom--neo-is-repo-dir-p
       :icon (all-the-icons-octicon "file-submodule"))
      (t :icon (all-the-icons-octicon "file-directory"))))
  "TODO")

(defvar doom-neotree-file-rules
  (eval-when-compile
    `((file-symlink-p
       :icon (all-the-icons-octicon "file-symlink-file"))
      (file-executable-p
       :face doom-neotree-executable-file-face
       :icon (all-the-icons-octicon "file-binary"))
      ("\\.\\(?:md\\|org\\|rst\\|log\\)\\|/[A-Z_-]+\\(?:\\.[a-z]+\\)?$"
       :face doom-neotree-text-file-face
       :icon (all-the-icons-octicon "file-text"))
      (,(concat "\\." (regexp-opt '("htm" "html" "phtml" "tpl" "erb" "mustache"
                                    "twig" "ejs" "erb" "jsx" "haml" "inky-haml"
                                    "inky-slim" "slim" "pug" "jade"))
                "$")
       :icon (all-the-icons-octicon "file-code"))
      (,(concat "\\(?:/\\(?:Gemfile\\|Vagrantfile\\|Makefile\\|Rakefile\\|Cask\\|\\.[^$]+rc\\|\\)\\|"
                "\\." (regexp-opt '("json" "cson" "yaml" "yml" "xml" "toml"
                                    "tpl" "ini" "erb" "mustache" "twig" "ejs"
                                    "mk" "haml" "pug" "jade"))
                "\\)$")
       :icon (all-the-icons-octicon "file-code"))
      (,(concat "\\."
                (regexp-opt '("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff"
                              "svg" "bmp" "psd" "ai" "eps" "indd"         ; images
                              "mov" "avi" "mp4" "webm" "mkv"              ; video
                              "wav" "mp3" "ogg" "midi"))                  ; audio
                "$")
       :face doom-neotree-data-file-face
       :icon (all-the-icons-octicon "file-media"))
      (,(concat "\\.\\(?:[gl]?zip\\|bzip2\\|deb\\|dmg\\|iso\\|7z\\|rpm\\|pkg\\|dat\\|[rjt]ar\\(?:\\.gz\\)?\\)$")
       :face doom-neotree-data-file-face
       :icon (all-the-icons-octicon "file-zip"))
      ("\\.pdf$"
       :face doom-neotree-data-file-face
       :icon (all-the-icons-octicon "file-pdf"))
      ("\\.\\(?:lock\\|resolved\\|dll\\|so\\|pyc\\|elc\\|class\\|css\\.map\\)$"
       :face doom-neotree-hidden-file-face
       :icon (all-the-icons-octicon "file-binary"))
      ("/\\.[^$/#]+$"
       :face doom-neotree-hidden-file-face)
      (t :icon (all-the-icons-octicon "file-text"))))
  "TODO")


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

(defun doom-neotree-spec (node rules)
  (let (case-fold-search)
    (cl-loop for spec in rules
             for pred = (car spec)
             for plist = (cdr spec)
             when
             (cond ((eq pred 't))
                   ((symbolp pred) (funcall pred node))
                   ((stringp pred) (string-match-p pred node)))
             return plist)))

(defun doom--neotree-insert-file-icon (node icon &optional faces)
  (if node
      (cond ((eq doom-neotree-file-icons 'simple)
             (propertize
              (if icon
                  (apply (car icon) (cdr icon))
                (all-the-icons-octicon "file-text"))
              'face `(:inherit ,faces
                               :family ,(all-the-icons-octicon-family)
                               :height 1.3)
              'display '(raise 0)))
            (t (all-the-icons-icon-for-file node)))
    (all-the-icons-fileicon "default")))

(defun doom--neotree-insert-dir-icon (node type &optional faces)
  (concat (if type
              (all-the-icons-octicon
               (format "chevron-%s" (if (eq type 'open) "down" "right"))
               :v-adjust 0.1
               :height doom-neotree-chevron-size
               :face `(:inherit ,faces
                                :family ,(all-the-icons-octicon-family)
                                :height ,doom-neotree-chevron-size))
            "\t")
          "\t"
          (when doom-neotree-enable-folder-icons
            (all-the-icons-octicon
             (cond ((file-symlink-p node) "file-symlink-directory")
                   ((file-exists-p (format "%s/.git" node)) "file-submodule")
                   ((all-the-icons-dir-is-submodule node) "file-submodule")
                   ("file-directory"))
             :v-adjust 0
             :height doom-neotree-folder-size
             :face `(:inherit ,faces
                     :family ,(all-the-icons-octicon-family)
                     :height ,doom-neotree-folder-size)))))

(defun doom--neotree-insert-icon (type node &optional icon faces)
  "Custom hybrid unicode theme with leading whitespace."
  (let ((spc "\t")
        (vspc (propertize "  " 'face 'variable-pitch)))
    (cond ((eq type 'open)
           (insert
            (concat spc
                    (doom--neotree-insert-dir-icon
                     node (if doom-neotree-enable-open-chevron-icons type)
                     faces)
                    vspc)))
          ((eq type 'close)
           (insert
            (concat spc
                    (doom--neotree-insert-dir-icon
                     node (if doom-neotree-enable-closed-chevron-icons type)
                     faces)
                    vspc)))
          ((eq type 'leaf)
           (insert
            (concat (when (or doom-neotree-enable-open-chevron-icons
                              doom-neotree-enable-closed-chevron-icons)
                      spc)
                    (when doom-neotree-enable-folder-icons spc)
                    (when doom-neotree-file-icons
                      (concat spc (doom--neotree-insert-file-icon node icon faces)))
                    vspc))))))

;;
(defun doom-neotree-insert-root (node)
  ;; insert icon
  (when (display-graphic-p)
    (insert
     (concat (propertize "\t" 'face 'neo-root-dir-face)
             (all-the-icons-octicon
              "repo"
              :height doom-neotree-project-size
              :face 'neo-root-dir-face
              :v-adjust -0.1)
             (propertize " " 'face 'neo-root-dir-face))))
  ;; insert project name
  (insert
   (propertize
    (concat (or (neo-path--file-short-name node) "-")
            "\n")
    'face `(:inherit ,(append (if doom-neotree-enable-variable-pitch '(variable-pitch))
                              '(neo-root-dir-face))))))

(defun doom-neotree-insert-dir (node depth expanded)
  (let ((short-name (neo-path--file-short-name node))
        (faces '(doom-neotree-dir-face))
        icon-text)
    ;; insert indentation
    (insert-char ?\s (* (- depth 1) 2))
    ;; vcs integration
    (let ((vc (if neo-vc-integration (neo-vc-for-node node))))
      (when (memq 'char neo-vc-integration)
        (insert-char (car vc))
        (insert-char ?\s))
      (unless (and (memq 'face neo-vc-integration)
                   (not (eq (cdr vc) 'neo-vc-up-to-date-face))
                   (setq faces (list (cdr vc))))
        (cl-destructuring-bind (&key face icon)
            (doom-neotree-spec node doom-neotree-dir-rules)
          (if face (push face faces))
          (if icon (setq icon-text icon)))))
    ;; insert icon
    (let ((type (if expanded 'open 'close)))
      (if (display-graphic-p)
          (doom--neotree-insert-icon type node icon-text faces)
        (neo-buffer--insert-fold-symbol type node)))
    ;; insert label button
    (when doom-neotree-enable-variable-pitch
      (push 'variable-pitch faces))
    (insert-button short-name
                   'follow-link t
                   'face `(:inherit (,@faces))
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap)
    ;; metadata + newline
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun doom-neotree-insert-file (node depth)
  (let ((short-name (neo-path--file-short-name node))
        (vc (if neo-vc-integration (neo-vc-for-node node)))
        (faces '(doom-neotree-file-face))
        icon-text)
    ;; insert indentation
    (insert-char ?\s (* (- depth 1) 2))
    ;; vcs integration
    (unless (and (memq 'face neo-vc-integration)
                 (not (eq (cdr vc) 'neo-vc-up-to-date-face))
                 (setq faces (list (cdr vc))))
      (cl-destructuring-bind (&key face icon)
          (doom-neotree-spec node doom-neotree-file-rules)
        (if face (push face faces))
        (if icon (setq icon-text icon))))
    ;; insert icon
    (if (display-graphic-p)
        (doom--neotree-insert-icon 'leaf node icon-text faces)
      (neo-buffer--insert-fold-symbol 'leaf node))
    ;; insert label button
    (when doom-neotree-enable-variable-pitch
      (push 'variable-pitch faces))
    (insert-button short-name
                   'follow-link t
                   'face `(:inherit (,@faces))
                   'neo-full-path node
                   'keymap neotree-file-button-keymap)
    ;; metadata + newline
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

;;
(eval-after-load 'neotree
  (lambda ()
    (unless (require 'all-the-icons nil t)
      (error "all-the-icons isn't installed"))

    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook #'doom--neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add #'neo-global--select-window :after #'doom--neotree-no-fringes)
    ;; Patch neotree to use `doom--neotree-insert-icon'
    (advice-add #'neo-buffer--insert-file-entry :override #'doom-neotree-insert-file)
    (advice-add #'neo-buffer--insert-dir-entry  :override #'doom-neotree-insert-dir)
    ;; Shorter pwd in neotree                    override
    (advice-add #'neo-buffer--insert-root-entry :override #'doom-neotree-insert-root)))

(provide 'doom-themes-neotree)
;;; doom-themes-neotree.el ends here
