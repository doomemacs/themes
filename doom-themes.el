;;; doom-themes.el --- a pack of themes inspired by Atom One
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: May 16, 2017
;; Version: 1.2.8
;; Keywords: dark blue atom one theme neotree nlinum icons
;; Homepage: https://github.com/hlissner/emacs-doom-theme
;; Package-Requires: ((emacs "24.4") (all-the-icons "1.0.0") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
;; emacs.d, inspired by the One Dark/Light UI and syntax themes in Atom.
;;
;; Includes optional dimming of non-source buffers, a neotree theme with font
;; icons, and (soon) a mode-line config.
;;
;; Currently available colorschemes:
;; + doom-one: inspired by Atom One Dark
;; + doom-vibrant: a more vibrant take on doom-one
;; + doom-molokai: based on molokai
;; + doom-tomorrow-night: Chris Kempson's Tomorrow Night (dark)
;;
;; Soon to come:
;; + doom-tomorrow-day: Chris Kempson's Tomorrow Day (light)
;; + doom-one-light: inspired by Atom One Light
;; + doom-tron: daylerees' Tron Legacy colorscheme
;; + doom-peacock: daylerees' Peacock colorscheme
;; + doom-spacegrey: I'm sure you've heard of it
;; + doom-mono-dark: A minimalistic, custom colorscheme
;; + doom-mono-light: A minimalistic, custom colorscheme
;;
;;
;; ## Configuration
;;
;; + global
;;     + `doom-enable-bold` (default: `t`): if nil, bolding will be disabled
;;     across all faces.
;;     + `doom-enable-italic` (default: `t`): if nil, italicization will be
;;     disabled across all faces.
;;
;;   Each colorscheme has their own sub-options, and can be looked up via
;;   `customize'.
;;
;; Example:
;;
;;   (require 'doom-themes)
;;   ;;; Settings (defaults)
;;   (setq doom-enable-bold t    ; if nil, bold is universally disabled
;;         doom-enable-italic t) ; if nil, italics is universally disabled
;;
;;   (load-theme 'doom-one t) ;; or doom-molokai, etc.
;;
;;   ;;; OPTIONAL
;;   ;; brighter source buffers (that represent files)
;;   (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;;   ;; ...if you use auto-revert-mode
;;   (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;;   ;; And you can brighten other buffers (unconditionally) with:
;;   (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)
;;
;;   ;; brighter minibuffer when active
;;   (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
;;
;;   ;; Enable custom neotree theme
;;   (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;
;;   ;; Enable nlinum line highlighting
;;   (doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode
;;
;;; Code:

(require 'cl-lib)

(defgroup doom-themes nil
  "Options for doom-themes"
  :group 'faces)

(defface doom-default '((t (:inherit default)))
  "Background face for source code windows."
  :group 'doom-themes)

(defface doom-minibuffer-active '((t (:inherit doom-default)))
  "Face for active minibuffer. See `doom-enable-bright-minibuffer'."
  :group 'doom-themes)

(defface doom-linum '((t (:inherit linum)))
  "Another linum face for darker windows (like popups)."
  :group 'doom-themes)

(defface doom-nlinum-highlight '((t (:inherit linum)))
  "A face for the nlinum overlay on the current line."
  :group 'doom-themes)

(defface doom-hl-line '((t (:inherit hl-line)))
  "A face for the current line highlight."
  :group 'doom-themes)

(defface doom-mode-line '((t (:inherit mode-line)))
  "A face for the mode-line when `doom-buffer-mode' is active."
  :group 'doom-themes)

(defface doom-mode-line-inactive '((t (:inherit mode-line-inactive)))
  "A face for the inactive mode-line when `doom-buffer-mode' is active."
  :group 'doom-themes)

(defface doom-org-hide '((t (:inherit org-hide)))
  "A face for hidden elements in org-mode when `doom-buffer-mode' is active."
  :group 'doom-themes)


;;
(defcustom doom-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

(defcustom doom-enable-italic t
  "If nil, italics will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

;;
(defvar doom--colors nil)
(defvar doom--inhibit-warning nil)


;; Color helper functions
;; Shamelessly *borrowed* from solarized
(defun doom-name-to-rgb (color &optional frame)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (mapcar (lambda (x) (/ x (float (car (color-values "#ffffff")))))
          (color-values color frame)))

(defun doom-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
           (cl-mapcar (lambda (it other) (+ (* alpha it) (* other (- 1 alpha))))
                      (doom-name-to-rgb color1)
                      (doom-name-to-rgb color2)))))

(defun doom-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (doom-blend color "#000000" (- 1 alpha)))

(defun doom-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (doom-blend color "#FFFFFF" (- 1 alpha)))

(defun doom--face-remap-add-relative (orig-fn &rest args)
  "Ensure that other themes, functions or packages that use
`face-remap-add-relative' (like `text-scale-set') don't undo doom's overriden
faces."
  (when (and (display-graphic-p) doom-buffer-mode)
    (let ((remap (assq (nth 0 args) face-remapping-alist)))
      (when remap (setf (nth 0 args) (cadr remap)))))
  (apply orig-fn args))
(advice-add 'face-remap-add-relative :around 'doom--face-remap-add-relative)

(defmacro def-doom-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a DOOM theme."
  (declare (doc-string 2))
  (load "doom-themes-common" nil t) ; force-refresh while debugging
  (let ((faces (doom-common-faces extra-faces))
        (vars (doom-common-variables extra-vars))
        (defs (mapcar (lambda (cl)
                        (if (> (length cl) 2)
                            (list (car cl) `(if gui ,(nth 1 cl) ,(nth 2 cl)))
                          cl))
                      defs)))
    `(let* ((gui (or (display-graphic-p) (= (tty-display-color-cells) 16777216)))
            (bold   doom-enable-bold)
            (italic doom-enable-italic)
            ,@defs)
       (setq doom--colors
             (mapcar (lambda (x) (list (car x) (eval (cadr x)))) ',defs))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces ',name ,@faces)
       ,(if vars `(custom-theme-set-variables ',name ,@vars))
       (provide-theme ',name))))

;;;###autoload
(defun doom-color (name)
  "Retrieve a specific color named NAME (a symbol) from the current DOOM theme."
  (nth 1 (assq name doom--colors)))

;;;###autoload
(defun doom-brighten-minibuffer ()
  "Highlight the minibuffer whenever it is in use."
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default doom-minibuffer-active))))))

;;;###autoload
(define-minor-mode doom-buffer-mode
  "Brighten source buffers by remapping common faces (like default, hl-line and
linum) to their doom-theme variants."
  :lighter "" ; should be obvious it's on
  :init-value nil
  ;; Don't reset remapped faces on `kill-all-local-variables'
  (put (make-variable-buffer-local 'face-remapping-alist)
       'permanent-local doom-buffer-mode)
  (if (not doom-buffer-mode)
      (progn
        (mapc (lambda (key)
                (setq face-remapping-alist
                      (assq-delete-all key face-remapping-alist)))
              '(default hl-line linum mode-line mode-line-inactive org-hide))
        (unless (cl-remove-if-not
                 (lambda (buf) (buffer-local-value 'doom-buffer-mode buf))
                 (buffer-list))
          (set-face-background 'fringe (face-background 'default))))
    (set-face-background 'fringe (face-background 'doom-default))
    (setq face-remapping-alist
          (append face-remapping-alist
                  '((default doom-default)
                    (hl-line doom-hl-line)
                    (linum doom-linum)
                    (mode-line doom-mode-line)
                    (mode-line-inactive doom-mode-line-inactive)
                    (org-hide doom-org-hide))))))

;;;###autoload
(defun doom-themes-neotree-config ()
  "Install DOOM neotree configuration."
  (let ((doom--inhibit-warning t))
    (require 'doom-neotree)))

;;;###autoload
(defun doom-themes-nlinum-config ()
  "Install DOOM nlinum configuration."
  (let ((doom--inhibit-warning t))
    (require 'doom-nlinum)))

;;;###autoload
(defun doom-buffer-mode-maybe ()
  "Enable `doom-buffer-mode' in the current buffer.

Does nothing if it doesn't represent a real, file-visiting buffer."
  (when (and (not doom-buffer-mode)
             buffer-file-name)
    (doom-buffer-mode +1)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'doom-themes)
;;; doom-themes.el ends here
