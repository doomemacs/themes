;;; doom-theme-setup-neotree.el

(eval-when-compile (require 'all-the-icons))

(defcustom doom-theme-neotree-root-icon
  (eval-when-compile (all-the-icons-octicon "inbox"))
  "Propertized string to use for folder root icon"
  :type 'symbol
  :group 'doom)

(defcustom doom-theme-neotree-open-folder-icon
  (eval-when-compile (all-the-icons-faicon "folder"))
  "Propertized string to use for open folder icon"
  :type 'symbol
  :group 'doom)

(defcustom doom-theme-neotree-closed-folder-icon
  (eval-when-compile (all-the-icons-faicon "folder"))
  "Propertized string to use for closed folder icon."
  :type 'symbol
  :group 'doom)

(defcustom doom-theme-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'doom)

(defcustom doom-theme-neotree-file-icons nil
  "If non-nil, display file icons next to each file. Can mess up spacing and
  look weird depending on your font."
  :type 'boolean
  :group 'doom)


(defun doom-theme*neo-insert-root-entry (&rest _)
  "Pretty-print pwd in neotree"
  (list (format " %s %s" doom-theme-neotree-root-icon (projectile-project-name))))

(defun doom-theme*neo-insert-fold-symbol (type)
  "Custom hybrid unicode theme with leading whitespace."
  (or (and (eq type 'open)
           (neo-buffer--insert-with-face (format " - %s " doom-theme-neotree-open-folder-icon)
                                         'neo-expand-btn-face))
      (and (eq type 'close)
           (neo-buffer--insert-with-face (format " + %s " doom-theme-neotree-closed-folder-icon)
                                         'neo-expand-btn-face))
      (and (eq type 'leaf)
           (neo-buffer--insert-with-face "   " 'neo-expand-btn-face))))

(defun doom-theme*neo-insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node))
        (vc (when neo-vc-integration (neo-vc-for-node node))))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (neo-buffer--insert-fold-symbol 'leaf)
    (insert (all-the-icons-icon-for-file node-short-name))
    (insert-char ? )
    (insert-button node-short-name
                   'follow-link t
                   'face (if (memq 'face neo-vc-integration)
                             (cdr vc)
                           neo-file-link-face)
                   'neo-full-path node
                   'keymap neotree-file-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun doom-theme*neotree-no-fringes ()
  "Remove fringes in neotree"
  (set-window-fringes neo-global--window 1 0))

(defun doom-theme|neotree-setup (&rest _)
  (setq line-spacing doom-theme-neotree-line-spacing)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))


;;
(eval-after-load "neotree"
  (lambda ()
    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook 'doom-theme|neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add 'neo-global--select-window :after 'doom-theme*neotree-no-fringes)
    ;; A custom and simple theme for neotree
    (advice-add 'neo-buffer--insert-fold-symbol :override 'doom-theme*neo-insert-fold-symbol)
    ;; Custom icons for each file
    (when doom-theme-neotree-file-icons
      (advice-add 'neo-buffer--insert-file-entry :override 'doom-theme*neo-insert-file-entry))
    ;; Shorter pwd in neotree
    (advice-add 'neo-buffer--insert-root-entry :filter-args 'doom-theme*neo-insert-root-entry)))

(provide 'doom-theme-setup-neotree)
;;; doom-theme-setup-neotree.el ends here
