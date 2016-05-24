;;; doom.el --- a dark theme inspired by Atom's One
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: May 22, 2016
;; Version: 1.0.0
;; Keywords: dark, blue, atom, one, seek
;; Homepage: https://github.com/hlissner/evil-snipe
;; Package-Requires: ((dash "2.12.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'dash)

(defgroup doom nil
  "Options for doom"
  :group 'faces)

(defface doom-default '((t (:inherit default)))
  "Face for source code windows."
  :group 'doom)

(defface doom-fringe '((t (:inherit doom-default)))
  "Face for source code window fringes."
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

;;

(defcustom doom-enable-bright-minibuffer t
  "If non-nil, minibuffer will be brighter when active."
  :group 'doom
  :type 'boolean)

(defcustom doom-enable-bright-buffers t
  "If non-nil, code and source buffers will be brighter than special, popup or
temporary buffers."
  :group 'doom
  :type 'boolean)

(defcustom doom-enable-bold t
  "If nil, bold will remove removed from all faces."
  :group 'doom
  :type 'boolean)

(defcustom doom-enable-italic t
  "If nil, italics will remove removed from all faces."
  :group 'doom
  :type 'boolean)

;;

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

;;

(defun doom-init (name)
  (deftheme name)

  (when (display-graphic-p)
    (put 'face-remapping-alist 'permanent-local t)
    (make-variable-buffer-local 'face-remapping-alist)

    ;; Brighten up file buffers; darken special and popup buffers
    (when doom-enable-bright-buffers
      (defun doom|brighten-buffer (&rest _)
        (setq-local face-remapping-alist
                    '((default doom-default)
                      (linum doom-linum))))

      (add-hook 'find-file-hook 'doom|brighten-buffer))

    ;; Brighter minibuffer when active + no fringe in minibuffer
    (when doom-enable-bright-minibuffer
      (defun doom|brighten-minibuffer ()
        (with-selected-window (minibuffer-window)
          (setq-local face-remapping-alist
                      '((default doom-minibuffer-active)))))

      (add-hook 'minibuffer-setup-hook 'doom|brighten-minibuffer))))

(provide 'doom)
;;; doom.el ends here
