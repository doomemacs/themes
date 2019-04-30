;;; doom-themes.el --- an opinionated pack of modern color-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2018 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: May 22, 2016
;; Modified: August 22, 2018
;; Version: 2.1.6
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
;; Flagship themes
;;   `doom-one'
;;   `doom-one-light'
;;   `doom-vibrant'
;;
;; Additional themes
;;   [X] `doom-city-lights' (added by fuxialexnder)
;;   [X] `doom-darcula' (added by fuxialexnder)
;;   [X] `doom-Iosvkem' (added by neutaaaaan)
;;   [X] `doom-molokai'
;;   [X] `doom-nord' (added by fuxialexnder)
;;   [X] `doom-nord-light' (added by fuxialexnder)
;;   [X] `doom-opera' (added by jwintz)
;;   [X] `doom-opera-light' (added by jwintz)
;;   [X] `doom-nova' (added by bigardone)
;;   [X] `doom-palenight' (added by brettm12345)
;;   [X] `doom-peacock' (added by teesloane)
;;   [X] `doom-solarized-light' (added by fuxialexnder)
;;   [X] `doom-sourcerer' (added by defphil)
;;   [X] `doom-spacegrey' (added by teesloane)
;;   [X] `doom-tomorrow-night'
;;   [X] `doom-tomorrow-day'
;;   [ ] `doom-mono-dark' / `doom-mono-light'
;;   [ ] `doom-tron'
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

(defcustom doom-themes-padded-modeline nil
  "Default value for padded-modeline setting for themes that support it."
  :group 'doom-themes
  :type '(or integer boolean))

(define-obsolete-variable-alias 'doom-enable-italic 'doom-themes-enable-italic "1.2.9")
(define-obsolete-variable-alias 'doom-enable-bold   'doom-themes-enable-bold "1.2.9")

(defvar doom-themes--colors nil)
(defvar doom-themes--inhibit-warning nil)
(defvar doom-themes--bell-p nil)


;; Color helper functions
;; Shamelessly *borrowed* from solarized
;;;###autoload
(defun doom-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

;;;###autoload
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

;;;###autoload
(defun doom-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (doom-darken (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-darken c alpha)))

        (t
         (doom-blend color "#000000" (- 1 alpha)))))

;;;###autoload
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
  (let ((spec (or (cdr (assq face doom-themes--faces))
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
(defun doom-themes-set-faces (theme &rest faces)
  "Customize THEME (a symbol) with FACES."
  (declare (indent defun))
  (apply #'custom-theme-set-faces
         (or theme 'user)
         (mapcar #'doom-themes--build-face faces)))

(defmacro def-doom-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a DOOM theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((doom-themes--colors defs))
    `(let* ((bold   doom-themes-enable-bold)
            (italic doom-themes-enable-italic)
            ,@defs)
       (setq doom-themes--colors
             (list ,@(cl-loop for (var val) in defs
                              collect `(cons ',var ,val))))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces
        ',name ,@(doom-themes-prepare-facelist extra-faces))
       (custom-theme-set-variables
        ',name ,@(doom-themes-prepare-varlist extra-vars))
       (unless bold (set-face-bold 'bold nil))
       (unless italic (set-face-italic 'italic nil))
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
(defun doom-themes-treemacs-config ()
  "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype."
  (require 'doom-themes-treemacs))

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

(provide 'doom-themes)
;;; doom-themes.el ends here
