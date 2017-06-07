;;; doom-themes.el --- a pack of themes inspired by Atom One
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: Jun 07, 2017
;; Version: 2.0.1
;; Keywords: dark blue atom one theme neotree icons faces
;; Homepage: https://github.com/hlissner/emacs-doom-theme
;; Package-Requires: ((emacs "24.4") (all-the-icons "1.0.0") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; DOOM Themes is an opinionated UI plugin and pack of themes extracted from my
;; [emacs.d], inspired by some of my favorite color themes including:
;;
;;   [X] `doom-one': inspired by Atom's One Dark themes
;;   [-] `doom-one-light': light version of doom-one
;;   [X] `doom-vibrant': a more vibrant version of `doom-one`
;;   [X] `doom-molokai': based on Textmate's monokai
;;   [X] `doom-nova': adapted from Nova (thanks to bigardone)
;;   [ ] `doom-x': reads your colors from ~/.Xresources
;;   [-] `doom-tomorrow-night' / `doom-tomorrow-day': by Chris Kempson
;;   [ ] `doom-spacegrey': I'm sure you've heard of it
;;   [ ] `doom-mono-dark' / `doom-mono-light': a minimalistic, monochromatic theme
;;   [ ] `doom-tron': based on Tron Legacy from daylerees' themes
;;   [ ] `doom-peacock': based on Peacock from daylerees' themes
;;
;; ## Install
;;
;;   `M-x package-install RET doom-themes`
;;
;; A comprehensive configuration example:
;;
;;   (require 'doom-themes)
;;
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;
;;   ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;;   ;; theme may have their own settings.
;;   (load-theme 'doom-one t)
;;
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;
;;   ;; Enable custom neotree theme
;;   (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;
;;; Code:

(require 'cl-lib)

(defgroup doom-themes nil
  "Options for doom-themes."
  :group 'faces)

(defface doom-modeline-error '((t (:inherit error :inverse-video t)))
  "Face to use for the mode-line when `doom-themes-visual-bell-config' is used."
  :group 'doom-themes)

;;
(defcustom doom-themes-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

(defcustom doom-themes-enable-italic t
  "If nil, italics will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

(define-obsolete-variable-alias 'doom-enable-italic 'doom-themes-enable-italic "1.2.9")
(define-obsolete-variable-alias 'doom-enable-bold   'doom-themes-enable-bold "1.2.9")

(defvar doom-themes--colors nil)
(defvar doom-themes--inhibit-warning nil)
(defvar doom-themes--bell-p nil)


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
    (cond ((or (listp color1) (listp color2))
           (mapcar (lambda (x)
                     (let ((c2 (if (listp color2) (pop color2) color2)))
                       (when c2 (doom-blend x c2 alpha))))
                   color1))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-mapcar (lambda (it other) (+ (* alpha it) (* other (- 1 alpha))))
                             (doom-name-to-rgb color1)
                             (doom-name-to-rgb color2))))

          (t color1))))

(defun doom-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (if (listp color)
      (mapcar (lambda (c) (doom-darken c alpha)) color)
    (doom-blend color "#000000" (- 1 alpha))))

(defun doom-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (if (listp color)
      (mapcar (lambda (c) (doom-lighten c alpha)) color)
    (doom-blend color "#FFFFFF" (- 1 alpha))))

;;;###autoload
(defun doom-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme."
  (let ((colors (cdr-safe (assq name doom-themes--colors))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

(defmacro def-doom-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a DOOM theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (require 'doom-themes-common)
  (let ((doom-themes--colors defs))
    `(let* ((gui (or (display-graphic-p) (= (tty-display-color-cells) 16777216)))
            (bold   doom-themes-enable-bold)
            (italic doom-themes-enable-italic)
            ,@defs)
       (setq doom-themes--colors (mapcar (lambda (d)
                                           (cons (car d)
                                                 (eval
                                                  (if (eq (cadr d) 'quote)
                                                      (caddr d)
                                                    (cadr d)))))
                                         ',defs))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces ',name ,@(doom-themes-common-faces extra-faces))
       (custom-theme-set-variables ',name ,@(doom-themes-common-variables extra-vars))
       (provide-theme ',name))))

(defun doom-themes-common-faces (&optional extra-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (mapcar
   #'doom-themes--build-face
   (cl-remove-duplicates (append doom-themes-common-faces extra-faces)
                         :key #'car)))

(defun doom-themes-common-variables (&optional extra-vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (mapcar
   #'doom-themes--build-var
   (cl-remove-duplicates (append doom-themes-common-vars extra-vars)
                         :key #'car)))

;;;###autoload
(defun doom-themes-neotree-config ()
  "Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype."
  (let ((doom-themes--inhibit-warning t))
    (require 'doom-themes-neotree)))

;;;###autoload
(defun doom-themes-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'doom-themes-visual-bell-fn
        visible-bell t))

;;;###autoload
(defun doom-themes-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (unless doom-themes--bell-p
    (let ((old-remap (copy-alist face-remapping-alist)))
      (setq doom-themes--bell-p t)
      (setq face-remapping-alist
            (append (delete (assq 'mode-line face-remapping-alist) face-remapping-alist)
                    '((mode-line doom-modeline-error))))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (remap buf)
                        (with-current-buffer buf
                          (when (assq 'mode-line face-remapping-alist)
                            (setq face-remapping-alist remap
                                  doom-themes--bell-p nil))
                          (force-mode-line-update)))
                      old-remap
                      (current-buffer)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))


;; DEPRECATED FUNCTIONS
;;;###autoload
(defun doom-brighten-minibuffer ()
  "Does nothing. `doom-brighten-minibuffer' has been moved to the `solaire-mode'
package as `solaire-mode-in-minibuffer'. This function is deprecated."
  (message "doom-themes: doom-brighten-minibuffer has moved to the solaire-mode package"))

;;;###autoload
(define-minor-mode doom-buffer-mode
  "Does nothing. `doom-buffer-mode' has been moved to the `solaire-mode'
package. This function is deprecated."
  :lighter "" ; should be obvious it's on
  :init-value nil
  (message "doom-themes: doom-buffer-mode has moved to the solaire-mode package"))

;;;###autoload
(defun doom-buffer-mode-maybe ()
  "Does nothing. `doom-buffer-mode' has been moved to the `solaire-mode'
package. This function is deprecated."
  (doom-buffer-mode +1))

;;;###autoload
(defun doom-themes-nlinum-config ()
  "Does nothing. This functionality has been moved to the `nlinum-hl' package.
This function is deprecated."
  (require 'doom-themes-nlinum))

(provide 'doom-themes)
;;; doom-themes.el ends here
