;;; doom.el

(require 'dash)

(defgroup doom nil
  "Options for doom"
  :group 'faces)

(defface doom-default '((t (:inherit default)))
  "Background face for source code windows."
  :group 'doom)

(defface doom-minibuffer-active '((t (:inherit mode-line)))
  "Face for active minibuffer. See `doom-enable-bright-minibuffer'."
  :group 'doom)

(defface doom-linum '((t (:inherit linum)))
  "Another linum face for darker windows (like popups)."
  :group 'doom)

(defface doom-nlinum-highlight '((t (:inherit linum)))
  "A face for the nlinum overlay on the current line."
  :group 'doom)

(defface doom-hl-line '((t (:inherit hl-line)))
  "A face for the current line highlight."
  :group 'doom)

;;
(defcustom doom-enable-bold t
  "If nil, bold will remove removed from all faces."
  :group 'doom
  :type 'boolean)

(defcustom doom-enable-italic t
  "If nil, italics will remove removed from all faces."
  :group 'doom
  :type 'boolean)


;; Color helper functions
;; Shamelessly *borrowed* from solarized
(defun doom-name-to-rgb (color &optional frame)
  (mapcar (lambda (x) (/ x (float (car (color-values "#ffffff")))))
          (color-values color frame)))

(defun doom-blend (color1 color2 alpha)
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (--zip-with (+ (* alpha it) (* other (- 1 alpha)))
                     (doom-name-to-rgb color1)
                     (doom-name-to-rgb color2))))

(defun doom-darken (color alpha)
  (doom-blend color "#000000" (- 1 alpha)))

(defun doom-lighten (color alpha)
  (doom-blend color "#FFFFFF" (- 1 alpha)))


(defun doom--face-remap-add-relative (orig-fn &rest args)
  "Advice function "
  (when (and (display-graphic-p) doom-buffer-mode)
    (let ((remap (assq (nth 0 args) face-remapping-alist)))
      (when remap (setf (nth 0 args) (cadr remap)))))
  (apply orig-fn args))
(advice-add 'face-remap-add-relative :around 'doom--face-remap-add-relative)

;;;###autoload
(defun doom-brighten-minibuffer ()
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default doom-minibuffer-active))))))

;;;###autoload
(define-minor-mode doom-buffer-mode
  "Brighten source buffers by remapping common faces (like default, hl-line and
linum) to their doom-theme variants."
  :lighter " doom"
  :init-value nil
  (if doom-buffer-mode
      (progn
        ;; Don't reset remapped faces on `kill-all-local-variables'
        (make-variable-buffer-local 'face-remapping-alist)
        (put 'face-remapping-alist 'permanent-local t)
        ;; Brighten up file buffers; darken special and popup buffers
        (set-face-attribute 'fringe nil :background (face-attribute 'doom-default :background))
        (setq-local face-remapping-alist
                    (append face-remapping-alist
                            '((default doom-default)
                              (hl-line doom-hl-line)
                              (linum doom-linum)))))
    (set-face-attribute 'fringe nil :background (face-attribute 'default :background))
    (put 'face-remapping-alist 'permanent-local nil)
    ;; Remove face remaps
    (mapc (lambda (key) (setq-local face-remapping-alist (assq-delete-all key face-remapping-alist)))
          '(default hl-line linum))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'doom)
;;; doom.el ends here
