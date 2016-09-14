;;; doom-neotree.el

(require 'all-the-icons)

(defgroup doom-neotree nil
  "Options for doom's neotree theme"
  :group 'doom)

;;
(defface doom-neotree-folder-face '((t (:inherit neo-dir-link-face :height 1.1)))
  "Base face for neotree folder icons. Also see `doom-neotree-open-folder-face' and
`doom-neotree-closed-folder-face'."
  :group 'doom-neotree)

(defface doom-neotree-open-folder-face '((t (:inherit doom-neotree-folder-face)))
  "Face for the 'open folder' icon. See `doom-neotree-open-folder-prefix'."
  :group 'doom-neotree)

(defface doom-neotree-closed-folder-face '((t (:inherit doom-neotree-folder-face)))
  "Face for the 'closed folder' icon. See `doom-neotree-closed-folder-prefix'."
  :group 'doom-neotree)

(defface doom-neotree-chevron-face '((t (:inherit neo-dir-link-face)))
  "Face for chevron icons next to folders. See
`doom-neotree-closed-chevron-icon' and `doom-neotree-open-chevron-icon'."
  :group 'doom-neotree)

(defface doom-neotree-open-chevron-face '((t (:inherit doom-neotree-chevron-face :height 0.8)))
  "Face for neotree folder chevron icons"
  :group 'doom-neotree)

(defface doom-neotree-closed-chevron-face '((t (:inherit doom-neotree-chevron-face :height 1)))
  "Face for neotree folder chevron icons"
  :group 'doom-neotree)


;;
(defcustom doom-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'doom-neotree)

(defcustom doom-neotree-enable-file-icons nil
  "If non-nil, display file icons next to each file. This can look strange on
some displays or at certain font sizes. YMMV."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-open-folder-prefix
  (concat "\t"
          (propertize (all-the-icons-octicon "chevron-down")
                      'face `(:inherit doom-neotree-open-chevron-face :family ,(all-the-icons-octicon-family))
                      'display '(raise 0.1))
          "\t"
          (propertize (all-the-icons-faicon "folder")
                      'face `(:inherit doom-neotree-open-folder-face :family ,(all-the-icons-faicon-family))
                      'display '(raise -0.1))
          "\t")
  "The string to prefix open directories with. Can be nil."
  :type 'string
  :group 'doom-neotree)

(defcustom doom-neotree-closed-folder-prefix
  (concat "\t"
          (propertize (all-the-icons-octicon "chevron-right")
                      'face `(:inherit doom-neotree-closed-chevron-face :family ,(all-the-icons-octicon-family))
                      'display '(raise 0.1))
          "\t"
          (propertize (all-the-icons-faicon "folder")
                      'face `(:inherit doom-neotree-open-folder-face :family ,(all-the-icons-faicon-family))
                      'display '(raise -0.1))
          "\t")
  "The string to prefix closed directories with. Can be nil."
  :type 'string
  :group 'doom-neotree)

(defcustom doom-neotree-leaf-prefix
  "\t\t"
  "The string to prefix files with. Can be nil. Does not affect
`doom-neotree-enable-file-icons'."
  :type 'string
  :group 'doom-neotree)


;;
(defun doom--neotree-no-fringes ()
  "Remove fringes in neotree"
  (set-window-fringes neo-global--window 1 0))

(defun doom--neotree-setup (&rest _)
  (setq line-spacing doom-neotree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

(defun doom--neo-insert-fold-symbol (type file-name)
  "Custom hybrid unicode theme with leading whitespace."
  (or (and (eq type 'open)
           (insert doom-neotree-open-folder-prefix))
      (and (eq type 'close)
           (insert doom-neotree-closed-folder-prefix))
      (and (eq type 'leaf)
           (insert (concat doom-neotree-leaf-prefix
                           (when doom-neotree-enable-file-icons
                             (concat "\t" (all-the-icons-icon-for-file file-name)))
                           "\t")))))

(defun doom--neo-buffer--insert-root-entry (&rest _)
  "Pretty-print pwd in neotree"
  (insert
   (concat (propertize (concat "\t" (all-the-icons-octicon "repo"))
                       'face `(:inherit neo-root-dir-face :family ,(all-the-icons-octicon-family) :height 1.4)
                       'display '(raise -0.1))
           (propertize (concat " " (projectile-project-name) "\n")
                       'face '(:inherit neo-root-dir-face)))))

(defun doom--neo-buffer--insert-dir-entry (node depth expanded)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char ?\s 2))
    (neo-buffer--insert-fold-symbol
     (if expanded 'open 'close) node)
    (insert-button (concat node-short-name "/")
                   'follow-link t
                   'face neo-dir-link-face
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun doom--neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node))
        (vc (when neo-vc-integration (neo-vc-for-node node))))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char (car vc))
      (insert-char ?\s))
    (neo-buffer--insert-fold-symbol 'leaf node-short-name)
    (insert-button node-short-name
                   'follow-link t
                   'face (if (memq 'face neo-vc-integration)
                             (cdr vc)
                           neo-file-link-face)
                   'neo-full-path node
                   'keymap neotree-file-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))


;;
(eval-after-load "neotree"
  (lambda ()
    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook 'doom--neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add 'neo-global--select-window :after 'doom--neotree-no-fringes)
    ;; A custom and simple theme for neotree
    (advice-add 'neo-buffer--insert-fold-symbol :override 'doom--neo-insert-fold-symbol)
    ;; Custom icons for each file & dir
    (advice-add 'neo-buffer--insert-file-entry :override 'doom--neo-buffer--insert-file-entry)
    (advice-add 'neo-buffer--insert-dir-entry  :override 'doom--neo-buffer--insert-dir-entry)
    ;; Shorter pwd in neotree
    (advice-add 'neo-buffer--insert-root-entry :override 'doom--neo-buffer--insert-root-entry)))

(provide 'doom-neotree)
;;; doom-neotree.el ends here
