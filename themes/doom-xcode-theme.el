;;; doom-xcode-theme.el --- based off of Apple's Xcode Dark Theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: February 28, 2020 (#414)
;; Author: kadenbarlow <https://github.com/kadenbarlow>
;; Maintainer:
;; Source: Xcode.app
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-xcode-theme nil
  "Options for the `doom-xcode' theme."
  :group 'doom-themes)

(defcustom doom-xcode-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-xcode-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-xcode
  "A theme based off of the Xcode Dark Theme"

  ;; name        gui       256       16
  ((bg         '("#292A30" nil       nil          ))
   (bg-alt     '("#252629" nil       nil          ))
   (base0      '("#0d0d0d" "black"   "black"      ))
   (base1      '("#1b1b1b" "#1b1b1b"              ))
   (base2      '("#212122" "#1e1e1e"              ))
   (base3      '("#292b2b" "#292929" "brightblack"))
   (base4      '("#3f4040" "#3f3f3f" "brightblack"))
   (base5      '("#5c5e5e" "#525252" "brightblack"))
   (base6      '("#757878" "#6b6b6b" "brightblack"))
   (base7      '("#969896" "#979797" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"      ))
   (fg         '("#FFFFFF" "#ffffff" "white"))
   (fg-alt     (doom-darken fg 0.4))

   (red        '("#FC6A5D" "#FC6A5D" "red"))
   (orange     '("#FD8F3F" "#FD8F3F" "orange"))
   (yellow     '("#D0BF68" "#D0BF68" "yellow"))
   (green      '("#67B7A4" "#67B7A4" "green"))
   (blue       '("#5DD8FF" "#5DD8FF" "brightblue"))
   (teal       '("#59B0CF" "#59B0CF" "brightblue"))
   (magenta    '("#D0A8FF" "#D0A8FF" "magenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))


   (grey       '("#6C7986" "#6C7986" "brightblack"))
   (light-green'("#9EF1DD" "#9EF1DD" "lightgreen"))
   (violet     '("#A167E6" "#A167E6" "brightmagenta"))
   (dark-blue  '("#41A1C0" "#41A1C0" "darkblue"))
   (pink       '("#FC5FA3" "#FC5FA3" "pink"))

   ;; face categories
   (highlight      blue)
   (vertical-bar   `("#161616" ,@base0))
   (selection      `(,(car (doom-lighten bg 0.1)) ,@(cdr base4)))
   (builtin        light-green)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.14))
   (constants      violet)
   (functions      dark-blue)
   (keywords       pink)
   (methods        dark-blue)
   (operators      orange)
   (type           blue)
   (strings        red)
   (variables      dark-blue)
   (numbers        yellow)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-xcode-padded-modeline
      (if (integerp doom-xcode-padded-modeline)
          doom-xcode-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (((font-lock-keyword-face &override) :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground dark-blue :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal))

  ;; --- variables --------------------------
  ;; ()
  )

;;; doom-xcode-theme.el ends here
