;;; doom-themes.el --- a pack of themes inspired by Atom One
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: January 7, 2016
;; Version: 1.1.5
;; Keywords: dark blue atom one theme
;; Homepage: https://github.com/hlissner/emacs-doom-theme
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (all-the-icons "1.0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; An opinionated UI plugin/pack of themes extracted from my emacs.d, inspired
;; by the One Dark/Light UI and syntax themes in Atom.
;;
;; Includes optional dimming of non-source buffers, a neotree theme with font
;; icons, and (soon) a mode-line config.
;;
;; Currently available colorschemes:
;; + doom-one: inspired by Atom One Dark
;; + doom-dark: based on Molokai
;;
;; Soon to come:
;; + doom-one-light**: inspired by Atom One Light
;; + doom-tron**: doom-one, but with daylerees' Tron Legacy colorscheme
;; + doom-peacock**: doom-one, but with daylerees' Peacock colorscheme
;;
;;
;; ## Configuration
;;
;; + `doom-enable-bold` (default: `t`)
;; + `doom-enable-italic` (default: `t`)
;; + `doom-enable-brighter-comments` (default: `nil`)
;;
;;
;; ## Installation
;;
;; 1. Install from MELPA `M-x package-install RET doom-themes`, or clone
;;    the repo somewhere in your `load-path`.
;;
;; 2. If you want the neotree theme, download and install the fonts included
;;    with all-the-icons.
;;
;; 3. `(require 'doom-themes)` and then load the theme you want.
;;
;; Example configuration:
;;
;;   (require 'doom-themes)
;;   (load-theme 'doom-one t) ;; or doom-dark, etc.
;;
;;   ;;; OPTIONAL
;;   ;; brighter source buffers
;;   (add-hook 'find-file-hook 'doom-buffer-mode)
;;   ;; brighter minibuffer when active
;;   (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
;;   ;; Custom neotree theme
;;   (require 'doom-neotree)
;;
;;; Code:

(require 'dash)

(defgroup doom-themes nil
  "Options for doom-themes"
  :group 'faces)

(defface doom-default '((t (:inherit default)))
  "Background face for source code windows."
  :group 'doom-themes)

(defface doom-minibuffer-active '((t (:inherit mode-line)))
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

(defface doom-org-hide '((t (:inherit org-hide)))
  "A face for hidden elements in org-mode. Only active if `doom-buffer-mode' is active."
  :group 'doom-themes)

;;
(defcustom doom-enable-bold t
  "If nil, bold will remove removed from all faces."
  :group 'doom-themes
  :type 'boolean)

(defcustom doom-enable-italic t
  "If nil, italics will remove removed from all faces."
  :group 'doom-themes
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
        ;; Update `doom-org-hide'
        (when (eq major-mode 'org-mode)
          (set-face-attribute 'doom-org-hide nil
                              :inherit 'org-hide
                              :background (face-attribute 'doom-default :background)
                              :foreground (face-attribute 'doom-default :background)))
        (setq-local face-remapping-alist
                    (append face-remapping-alist
                            '((default doom-default)
                              (hl-line doom-hl-line)
                              (linum doom-linum)
                              (org-hide doom-org-hide)))))
    (set-face-attribute 'fringe nil :background (face-attribute 'default :background))
    (put 'face-remapping-alist 'permanent-local nil)
    ;; Remove face remaps
    (mapc (lambda (key) (setq-local face-remapping-alist (assq-delete-all key face-remapping-alist)))
          '(default hl-line linum org-hide))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'doom-themes)
;;; doom-themes.el ends here
