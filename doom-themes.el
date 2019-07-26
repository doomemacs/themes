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
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
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
(require 'doom-themes-base)

(defgroup doom-themes nil
  "Options for doom-themes."
  :group 'faces)

(defcustom doom-themes-padded-modeline nil
  "Default value for padded-modeline setting for themes that support it."
  :group 'doom-themes
  :type '(or integer boolean))

;;
(defcustom doom-themes-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

(defcustom doom-themes-enable-italic t
  "If nil, italics will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)


;;
;;; API

(defvar doom-themes--colors nil)
(defvar doom--min-colors '(257 256 16))
(defvar doom--quoted-p nil)
(defvar doom-themes--faces nil)

(defun doom-themes--colors-p (item)
  (declare (pure t) (side-effect-free t))
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote doom-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((doom--quoted-p t))
                      (doom-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (doom--quoted-p)
                      (doom-themes--colors-p (cdr item))))

                   ((or (doom-themes--colors-p car)
                        (doom-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not doom--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item doom-themes--colors))))))

(defun doom-themes--apply-faces (new-faces &optional default-faces)
  (declare (pure t) (side-effect-free t))
  (let ((default-faces (or default-faces doom-themes-base-faces))
        (faces (make-hash-table :test #'eq :size (+ (length default-faces) (length new-faces))))
        (directives (make-hash-table :test #'eq)))
    (dolist (spec (append (mapcar #'copy-sequence default-faces) new-faces))
      (if (listp (car spec))
          (cl-destructuring-bind (face action &optional arg) (car spec)
            (unless (assq face new-faces)
              (puthash face (list action arg (cdr spec))
                       directives)))
        (puthash (car spec) (cdr spec) faces)))
    (cl-loop for face being the hash-keys of directives
             for (action target spec) = (gethash face directives)
             unless (memq action '(&inherit &extend &override))
             do (error "Invalid operation (%s) for '%s' face" action face)
             if (eq (car spec) 'quote)
             do (error "Can't extend literal face spec (for '%s')" face)
             ;; TODO Add &all/&light/&dark extension support
             else if (memq (car spec) '(&all &light &dark))
             do (error "Can't extend face with &all, &light or &dark specs (for '%s')" face)
             else do
             (puthash face
                      (let ((old-spec (gethash (or target face) faces))
                            (plist spec))
                        ;; remove duplicates
                        (while (keywordp (car plist))
                          (setq old-spec (plist-put old-spec (car plist) (cadr plist))
                                plist (cddr plist)))
                        old-spec)
                      faces))
    (let (results)
      (maphash (lambda (face plist)
                 (when (keywordp (car plist))
                   ;; TODO Clean up duplicates in &all/&light/&dark blocks
                   (dolist (prop (append (unless doom-themes-enable-bold   '(:weight normal :bold nil))
                                         (unless doom-themes-enable-italic '(:slant normal :italic nil))))
                     (when (and (plist-member plist prop)
                                (not (eq (plist-get plist prop) 'inherit)))
                       (plist-put plist prop
                                  (if (memq prop '(:weight :slant))
                                      (quote 'normal))))))
                 (push (cons face plist) results))
               faces)
      (nreverse results))))

(defun doom-themes--colorize (item type)
  (declare (pure t) (side-effect-free t))
  (when item
    (let ((doom--quoted-p doom--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote doom-color))
                    item)
                   ((eq (car item) 'doom-ref)
                    (doom-themes--colorize
                     (apply #'doom-ref (cdr item)) type))
                   ((let* ((item (append item nil))
                           (car (car item))
                           (doom--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t doom--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (doom-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not doom--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item doom-themes--colors))
             `(doom-color ',item ',type))

            (item)))))

(defun doom-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                (if (doom-themes--colors-p real-attrs)
                    (dolist (cl doom--min-colors `(list ,@(nreverse defs)))
                      (push `(list '((class color) (min-colors ,cl))
                                   (list ,@(doom-themes--colorize real-attrs cl)))
                            defs))
                  `(list (list 't (list ,@real-attrs))))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))

             ((let (all-attrs defs)
                (dolist (attrs face-body `(list ,@(nreverse defs)))
                  (cond ((eq (car attrs) '&all)
                         (setq all-attrs (append all-attrs (cdr attrs))))

                        ((memq (car attrs) '(&dark &light))
                         (let ((bg (if (eq (car attrs) '&dark) 'dark 'light))
                               (real-attrs (append all-attrs (cdr attrs) '())))
                           (cond ((doom-themes--colors-p real-attrs)
                                  (dolist (cl doom--min-colors)
                                    (push `(list '((class color) (min-colors ,cl) (background ,bg))
                                                 (list ,@(doom-themes--colorize real-attrs cl)))
                                          defs)))

                                 ((push `(list '((background ,bg)) (list ,@real-attrs))
                                        defs)))))))))))))


;;
;;; Color helper functions

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

          (color1))))

;;;###autoload
(defun doom-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (doom-darken (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-darken c alpha)))

        ((doom-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun doom-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (doom-lighten (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-lighten c alpha)))

        ((doom-blend color "#FFFFFF" (- 1 alpha)))))

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


;;
;;; Defining themes

(defun doom-themes-prepare-facelist (custom-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (setq doom-themes--faces (doom-themes--apply-faces custom-faces))
  (mapcar #'doom-themes--build-face doom-themes--faces))

(defun doom-themes-prepare-varlist (vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (declare (pure t) (side-effect-free t))
  (cl-loop for (var val) in (append doom-themes-base-vars vars)
           collect `(list ',var ,val)))

;;;###autoload
(defun doom-themes-set-faces (theme &rest faces)
  "Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of Doom
theme face specs. These is a simplified spec. For example:

  (doom-themes-set-faces 'user
    '(default :background red :foreground blue)
    '(doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    '(doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
    '(doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
    '(doom-modeline-buffer-project-root :foreground green :weight 'bold))"
  (declare (indent defun))
  (apply #'custom-theme-set-faces
         (or theme 'user)
         (eval
          `(let* ((bold   ,doom-themes-enable-bold)
                  (italic ,doom-themes-enable-italic)
                  ,@(cl-loop for (var . val) in doom-themes--colors
                             collect `(,var ',val)))
             (list ,@(mapcar #'doom-themes--build-face faces))))))

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
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))
;;; Auto color

(defun doom-gencolor (color)
  "Return three colors derived from COLOR: (COLOR 8-bit 4-bit)."
  (list color (doom-closest-8-bit-color color) (doom-closest-4-bit-color color)))

(defvar doom--8-bit-color-list
  '("#000000" "#800000" "#008000" "#808000" "#000080" "#800080"
    "#008080" "#c0c0c0" "#808080" "#ff0000" "#00ff00" "#ffff00"
    "#0000ff" "#ff00ff" "#00ffff" "#ffffff" "#000000" "#00005f"
    "#000087" "#0000af" "#0000d7" "#0000ff" "#005f00" "#005f5f"
    "#005f87" "#005faf" "#005fd7" "#005fff" "#008700" "#00875f"
    "#008787" "#0087af" "#0087d7" "#0087ff" "#00af00" "#00af5f"
    "#00af87" "#00afaf" "#00afd7" "#00afff" "#00d700" "#00d75f"
    "#00d787" "#00d7af" "#00d7d7" "#00d7ff" "#00ff00" "#00ff5f"
    "#00ff87" "#00ffaf" "#00ffd7" "#00ffff" "#5f0000" "#5f005f"
    "#5f0087" "#5f00af" "#5f00d7" "#5f00ff" "#5f5f00" "#5f5f5f"
    "#5f5f87" "#5f5faf" "#5f5fd7" "#5f5fff" "#5f8700" "#5f875f"
    "#5f8787" "#5f87af" "#5f87d7" "#5f87ff" "#5faf00" "#5faf5f"
    "#5faf87" "#5fafaf" "#5fafd7" "#5fafff" "#5fd700" "#5fd75f"
    "#5fd787" "#5fd7af" "#5fd7d7" "#5fd7ff" "#5fff00" "#5fff5f"
    "#5fff87" "#5fffaf" "#5fffd7" "#5fffff" "#870000" "#87005f"
    "#870087" "#8700af" "#8700d7" "#8700ff" "#875f00" "#875f5f"
    "#875f87" "#875faf" "#875fd7" "#875fff" "#878700" "#87875f"
    "#878787" "#8787af" "#8787d7" "#8787ff" "#87af00" "#87af5f"
    "#87af87" "#87afaf" "#87afd7" "#87afff" "#87d700" "#87d75f"
    "#87d787" "#87d7af" "#87d7d7" "#87d7ff" "#87ff00" "#87ff5f"
    "#87ff87" "#87ffaf" "#87ffd7" "#87ffff" "#af0000" "#af005f"
    "#af0087" "#af00af" "#af00d7" "#af00ff" "#af5f00" "#af5f5f"
    "#af5f87" "#af5faf" "#af5fd7" "#af5fff" "#af8700" "#af875f"
    "#af8787" "#af87af" "#af87d7" "#af87ff" "#afaf00" "#afaf5f"
    "#afaf87" "#afafaf" "#afafd7" "#afafff" "#afd700" "#afd75f"
    "#afd787" "#afd7af" "#afd7d7" "#afd7ff" "#afff00" "#afff5f"
    "#afff87" "#afffaf" "#afffd7" "#afffff" "#d70000" "#d7005f"
    "#d70087" "#d700af" "#d700d7" "#d700ff" "#d75f00" "#d75f5f"
    "#d75f87" "#d75faf" "#d75fd7" "#d75fff" "#d78700" "#d7875f"
    "#d78787" "#d787af" "#d787d7" "#d787ff" "#d7af00" "#d7af5f"
    "#d7af87" "#d7afaf" "#d7afd7" "#d7afff" "#d7d700" "#d7d75f"
    "#d7d787" "#d7d7af" "#d7d7d7" "#d7d7ff" "#d7ff00" "#d7ff5f"
    "#d7ff87" "#d7ffaf" "#d7ffd7" "#d7ffff" "#ff0000" "#ff005f"
    "#ff0087" "#ff00af" "#ff00d7" "#ff00ff" "#ff5f00" "#ff5f5f"
    "#ff5f87" "#ff5faf" "#ff5fd7" "#ff5fff" "#ff8700" "#ff875f"
    "#ff8787" "#ff87af" "#ff87d7" "#ff87ff" "#ffaf00" "#ffaf5f"
    "#ffaf87" "#ffafaf" "#ffafd7" "#ffafff" "#ffd700" "#ffd75f"
    "#ffd787" "#ffd7af" "#ffd7d7" "#ffd7ff" "#ffff00" "#ffff5f"
    "#ffff87" "#ffffaf" "#ffffd7" "#ffffff" "#080808" "#121212"
    "#1c1c1c" "#262626" "#303030" "#3a3a3a" "#444444" "#4e4e4e"
    "#585858" "#606060" "#666666" "#767676" "#808080" "#8a8a8a"
    "#949494" "#9e9e9e" "#a8a8a8" "#b2b2b2" "#bcbcbc" "#c6c6c6"
    "#d0d0d0" "#dadada" "#e4e4e4" "#eeeeee")
  "8-bit colors.")

(defvar doom--4-bit-color-list
  '("#000000" "#0000FF" "#00FF00" "#00FFFF" "#000080" "#008000"
    "#008080" "#800000" "#800080" "#808000" "#808080" "#C0C0C0"
    "#FF0000" "#FF00FF" "#FFFF00" "#FFFFFF")
  "4-bit colors.")

(defvar doom-color-distance-fn
  (lambda (c1 c2)
    (let ((r1 (nth 0 c1))
          (g1 (nth 1 c1))
          (b1 (nth 2 c1))
          (r2 (nth 0 c2))
          (g2 (nth 1 c2))
          (b2 (nth 2 c2)))
      (+ (expt (- r1 r2) 2)
         (expt (- g1 g2) 2)
         (expt (- b1 b2) 2))))
  "Function (color1 color2) -> number that returns the distance between color1 and color2.
There is no specification on the range of the returned number as long as greater number
implies greater distance.
Each color is like (R G B) where R, G, B are number.

More on https://en.wikipedia.org/wiki/Color_difference")

(defun doom-color-distance (color1 color2)
  "Return the distance between COLOR1 and COLOR2.
COLOR’s are in the form of ”#RRGGBB”."
  (funcall doom-color-distance-fn
           (doom-color-str-to-list color1)
           (doom-color-str-to-list color2)))

(defun doom-color-str-to-list (color)
  "Convert COLOR ”#RRGGBB” to (R G B)."
  (list (string-to-number (substring color 1 3) 16)
        (string-to-number (substring color 3 5) 16)
        (string-to-number (substring color 5 7) 16)))

(defun doom-color-list-to-str (color)
  "Convert COLOR (R G B) to ”#RRGGBB”."
  (format "#%.2x%.2x%.2x" (nth 0 color) (nth 1 color) (nth 2 color)))

(defun doom--closest-color-in-list (color color-list)
  "Return the color closest to COLOR from COLOR-LIST."
  (let (closest-color
        (min-distance 1.0e+INF))
    (dolist (a-color color-list)
      (let ((new-dist (doom-color-distance color a-color)))
        (when (< new-dist min-distance)
          (setq min-distance new-dist
                closest-color a-color))))
    closest-color))

(defun doom-closest-8-bit-color (color)
  "Return the closest 8 bit color to COLOR."
  (doom--closest-color-in-list color doom--8-bit-color-list))

(defun doom-closest-4-bit-color (color)
  "Return the closest 4 bit color to COLOR."
  (doom--closest-color-in-list color doom--4-bit-color-list))


(provide 'doom-themes)
;;; doom-themes.el ends here
