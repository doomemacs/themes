;;; doom-neotree.el

(require 'all-the-icons)

(defcustom doom-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'doom)

(defcustom doom-neotree-enable-file-icons nil
  "If non-nil, display file icons next to each file."
  :type 'boolean
  :group 'doom)

(defcustom doom-neotree-enable-dir-icons t
  "If non-nil, display folder icons next to each file."
  :type 'boolean
  :group 'doom)

(defcustom doom-neotree-enable-dir-chevrons t
  "If non-nil, prefix directories with chevrons."
  :type 'boolean
  :group 'doom)

(defcustom doom-neotree-folder-size 1.1
  "What :height to display the folder icons at."
  :type 'float
  :group 'doom)


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
           (insert (concat
                    (when doom-neotree-enable-dir-chevrons
                      (concat "\t"
                              (propertize (all-the-icons-octicon "chevron-down")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.8)
                                          'display '(raise 0))))
                    (when doom-neotree-enable-dir-icons
                      (concat "\t"
                              (propertize (all-the-icons-faicon "folder")
                                          'face `(:family ,(all-the-icons-faicon-family) :height ,doom-neotree-folder-size)
                                          'display '(raise -0.1))))
                    "\t")))
      (and (eq type 'close)
           (insert (concat
                    (when doom-neotree-enable-dir-chevrons
                      (concat "\t"
                              (propertize (all-the-icons-octicon "chevron-right")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 1)
                                          'display '(raise 0))))
                    (when doom-neotree-enable-dir-icons
                      (concat "\t"
                              (propertize (all-the-icons-faicon "folder")
                                          'face `(:family ,(all-the-icons-faicon-family) :height ,doom-neotree-folder-size)
                                          'display '(raise -0.1))))
                    "\t")))
      (and (eq type 'leaf)
           (insert (concat
                    (when doom-neotree-enable-dir-icons "\t\t")
                    (when doom-neotree-enable-file-icons
                      (concat "\t" (all-the-icons-icon-for-file file-name)))
                    "\t"
                    )))))

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
