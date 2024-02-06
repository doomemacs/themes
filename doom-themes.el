;;; doom-themes.el --- an opinionated pack of modern color-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2022 Henrik Lissner
;;
;; Author: Henrik Lissner <contact@henrik.io>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Maintainer: Emmanuel Bustos Torres <ema2159@gmail.com>
;; Created: May 22, 2016
;; Version: 2.3.0
;; Keywords: themes faces
;; Homepage: https://github.com/doomemacs/themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; doomemacs/themes is an megapack of popular themes, including aesthetic
;; extensions for popular packages.
;;
;; Themes in this pack:
;;  - doom-1337 -- ported from VSCode's 1337 theme (ported by @ccmywish)
;;  - doom-acario-dark -- an original dark theme (ported by @gagbo)
;;  - doom-acario-light -- an original light theme (ported by @gagbo)
;;  - doom-ayu-dark -- inspired by Ayu Dark (ported by @ashton)
;;  - doom-ayu-light -- inspirted by Ayu Light (ported by @LoveSponge)
;;  - doom-ayu-mirage -- inspired by Ayu Mirage (ported by @LoveSponge)
;;  - doom-badger -- inspired by cann's Badger colorscheme (ported by @jsoa)
;;  - doom-challenger-deep -- inspired by Vim's Challenger Deep theme (ported by @fuxialexander)
;;  - doom-city-lights -- inspired by Atom's City Lights theme (ported by @fuxialexander)
;;  - doom-dark+ -- ported from equinusocio's VSCode Theme, dark+ (ported by @ema2159)
;;  - doom-dracula -- inspired by the popular Dracula theme (ported by @fuxialexander)
;;  - doom-earl-grey -- a gentle color scheme, for code (ported by @JuneKelly)
;;  - doom-ephemeral -- inspired by the Ephemeral Theme from elenapan's dotfiles (ported by @karetsu)
;;  - doom-fairy-floss -- a candy colored theme by sailorhg (ported by @ema2159)
;;  - doom-feather-dark -- a purple-tinted theme, inspired by doom-one (by @Plunne)
;;  - doom-feather-light -- a light variable of feather-dark, inspired by doom-one (by @Plunne)
;;  - doom-flatwhite -- inspired by Atom's Flatwhite Syntax theme (ported by @JuneKelly)
;;  - doom-gruvbox -- inspired by morhetz's Gruvbox (ported by @JongW)
;;  - doom-gruvbox-light -- inspired by morhetz's Gruvbox (light) (ported by @jsoa)
;;  - doom-henna -- based on VSCode's Henna theme (ported by @jsoa)
;;  - doom-homage-black -- a minimalistic, colorless theme inspired by eziam, tao, and jbeans (ported by @mskorzhinskiy)
;;  - doom-homage-white -- minimal white theme inspired by editors from 2000s (ported by @mskorzhinskiy)
;;  - doom-horizon -- ported from VSCode Horizon (ported by @karetsu)
;;  - doom-Iosvkem -- ported from the default dark theme for Adobe Brackets (ported by @neutaaaaan)
;;  - doom-ir-black -- ported from Vim's ir_black colorscheme (ported by @legendre6891)
;;  - doom-lantern -- based on Gitleptune's Lantern theme (ported by @paladhammika)
;;  - doom-laserwave -- a clean synthwave/outrun theme inspired by VSCode's Laserwave (ported by @hyakt)
;;  - doom-manegarm -- an original autumn-inspired dark theme (ported by @kenranunderscore)
;;  - doom-material -- adapted from equinusocio's Material themes (ported by @tam5)
;;  - doom-material-dark -- inspired by Material Theme by xrei (ported by @trev-dev)
;;  - doom-meltbus -- a dark (mostly) monochromatic theme (ported by @spacefrogg)
;;  - doom-miramare -- a port of Franbach's Miramare theme; a variant of Grubox (ported by @sagittaros)
;;  - doom-molokai -- inspired by Tomas Restrepo's Molokai (ported by @hlissner)
;;  - doom-monokai-classic -- port of Monokai Classic (ported by @ema2159)
;;  - doom-monokai-machine -- port of Monokai Classic (ported by @minikN)
;;  - doom-monokai-octagon -- port of Monokai Octagon (ported by @minikN)
;;  - doom-monokai-pro -- Port of Monokai Pro (ported by @minikN)
;;  - doom-monokai-ristretto -- Port of Monokai Ristretto (ported by @minikN)
;;  - doom-monokai-spectrum -- port of Monokai Spectrum (ported by @minikN)
;;  - doom-moonlight -- inspired by VS code's Moonlight (ported by @Brettm12345)
;;  - doom-nord -- dark variant of Nord (ported by @fuxialexander)
;;  - doom-nord-aurora -- a light variant of Nord (ported by @MoskitoHero)
;;  - doom-nord-light -- light variant of Nord (ported by @fuxialexander)
;;  - doom-nova -- inspired by Trevord Miller's Nova (ported by @bigardone)
;;  - doom-oceanic-next -- inspired by Oceanic Next (ported by @juanwolf)
;;  - doom-oksolar-dark -- an OKLab variant of Solarized dark (ported by @logc)
;;  - doom-oksolar-light -- an OKLab variant of Solarized light (ported by @logc)
;;  - doom-old-hope -- inspired by An Old Hope, in a galaxy far far away (ported by @teesloane)
;;  - doom-one -- inspired by Atom One Dark (ported by @hlissner)
;;  - doom-one-light -- inspired by Atom One Light (ported by @ztlevi)
;;  - doom-opera -- an original light theme (ported by @jwintz)
;;  - doom-opera-light -- an original light theme (ported by @jwintz)
;;  - doom-outrun-electric -- a high contrast, neon theme inspired by Outrun Electric on VSCode (ported by @ema2159)
;;  - doom-palenight -- adapted from equinusocio's Material themes (ported by @Brettm12345)
;;  - doom-peacock -- inspired by daylerees' Peacock (ported by @teesloane)
;;  - doom-pine -- a green flavor of doom-gruvbox (by @RomanHargrave)
;;  - doom-plain -- inspired by gko's plain theme for VSCode (ported by @das-s)
;;  - doom-plain-dark -- inspired by gko's plain theme for VSCode (ported by @das-s)
;;  - doom-rouge -- ported from VSCode's Rouge Theme (ported by @das-s)
;;  - doom-shades-of-purple -- a port of VSCode's Shades of Purple (ported by @jwbaldwin)
;;  - doom-snazzy -- inspired by Hyper Snazzy (ported by @ar1a)
;;  - doom-solarized-dark -- a dark variant of Solarized (ported by @ema2159)
;;  - doom-solarized-dark-high-contrast -- a high-contrast variant of Solarized Dark (ported by @jmorag)
;;  - doom-solarized-light -- a light variant of Solarized (ported by @fuxialexander)
;;  - doom-sourcerer -- a port of xero's Sourcerer (ported by @fm0xb)
;;  - doom-spacegrey -- I'm sure you've heard of it (ported by @teesloane)
;;  - doom-tokyo-night -- inspired by VSCode's Tokyo Night theme (ported by @FosterHangdaan)
;;  - doom-tomorrow-day -- a light variant of Tomorrow (ported by @emacswatcher)
;;  - doom-tomorrow-night -- One of the dark variants of Tomorrow (ported by @hlissner)
;;  - doom-vibrant -- a more vibrant variant of doom-one (ported by @hlissner)
;;  - doom-wilmersdorf -- port of Ian Pan's Wilmersdorf (ported by @ema2159)
;;  - doom-xcode -- based off of Apple's Xcode Dark Theme (ported by @kadenbarlow)
;;  - doom-zenburn -- port of the popular Zenburn theme (ported by @jsoa)
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
  :type '(choice integer boolean))

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
                   (dolist (prop (append (unless doom-themes-enable-bold   '(:weight normal :bold unspecified))
                                         (unless doom-themes-enable-italic '(:slant normal :italic unspecified))))
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

  (doom-themes-set-faces \\='user
    \\='(default :background red :foreground blue)
    \\='(doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    \\='(doom-modeline-buffer-file :inherit \\='mode-line-buffer-id :weight \\='bold)
    \\='(doom-modeline-buffer-path :inherit \\='mode-line-emphasis :weight \\='bold)
    \\='(doom-modeline-buffer-project-root :foreground green :weight \\='bold))"
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
       (unless bold (set-face-bold 'bold 'unspecified))
       (unless italic (set-face-italic 'italic 'unspecified))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'doom-themes)
;;; doom-themes.el ends here
