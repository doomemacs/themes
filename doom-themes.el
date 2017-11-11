;;; doom-themes.el --- an opinionated pack of modern color-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2017 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: November 09, 2017
;; Version: 2.0.8
;; Keywords: dark light blue atom one theme neotree icons faces nova
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
;;   [X] `doom-vibrant': a more vibrant version of `doom-one`
;;   [X] `doom-molokai': based on Textmate's monokai
;;   [X] `doom-nova': adapted from Nova (thanks to bigardone)
;;   [X] `doom-one-light': light version of doom-one
;;   [X] `doom-tomorrow-night': by Chris Kempson
;;   [ ] `doom-tomorrow-day`: by Chris Kempson
;;   [ ] `doom-x': reads your colors from ~/.Xresources
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
(require 'doom-themes-common)

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
  (cl-loop for x in (color-values color frame)
           collect (/ x (float (car (color-values "#ffffff"))))))

(defun doom-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (doom-blend (doom-color color1) (doom-color color2) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (doom-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (doom-name-to-rgb color1)
                           for other in (doom-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (t color1))))

(defun doom-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (doom-darken (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-darken c alpha)))

        (t
         (doom-blend color "#000000" (- 1 alpha)))))

(defun doom-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (doom-lighten (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-lighten c alpha)))

        (t
         (doom-blend color "#FFFFFF" (- 1 alpha)))))

;;;###autoload
(defun doom-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme."
  (let ((colors (if (listp name)
                    name
                  (cdr-safe (assq name doom-themes--colors)))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

;;;###autoload
(defun doom-ref (face prop &optional class)
  "TODO"
  (let ((spec (or (cdr (assq face doom-themes--common-faces))
                  (error "Couldn't find the '%s' face" face))))
    (when (memq (car spec) '(quote backquote \`))
      (user-error "Can't fetch the literal spec for '%s'" face))
    (when class
      (setq spec (cdr (assq class spec)))
      (unless spec
        (error "Couldn't find the '%s' class in the '%s' face"
               class face)))
    (unless (plist-member spec prop)
      (error "Couldn't find the '%s' property in the '%s' face%s"
             prop face (if class (format "'s '%s' class" class) "")))
    (plist-get spec prop)))

;;;###autoload
(defmacro doom-themes-set-faces (theme &rest faces)
  "Customize THEME (a symbol) with FACES."
  `(custom-theme-set-faces
    ,theme
    ,@(mapcar #'doom-themes--build-face faces)))

(defmacro def-doom-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a DOOM theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((doom-themes--colors defs))
    `(let* ((bold   doom-themes-enable-bold)
            (italic doom-themes-enable-italic)
            ,@defs)
       (setq doom-themes--colors
             (cl-loop for (var val) in ',defs
                      collect (cons var (eval val))))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces ',name ,@(doom-themes-common-faces extra-faces))
       (custom-theme-set-variables ',name ,@(doom-themes-common-variables extra-vars))
       (provide-theme ',name))))

;;;###autoload
(defun doom-themes-org-config ()
  "Enable custom fontification and improves doom-themes integration with org-mode."
  (require 'doom-themes-org))

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
            (append (delete (assq 'mode-line face-remapping-alist)
                            face-remapping-alist)
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
