;; doom-monokai-pro-theme.el --- based off of Monokai Pro -*- no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-monokai-pro-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-monokai-pro-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-monokai-pro-theme
  :type '(choice integer boolean))

(def-doom-theme doom-monokai-pro
  "A theme based off of Monokai Pro"

  ;; name        gui       256       16
  ((bg         '("#222222" nil       nil          ))
   (bg-alt     '("#2b2b2b" nil       nil          ))
   (base0      '("#0d0d0d" "black"   "black"      ))
   (base1      '("#1b1b1b" "#1b1b1b"              ))
   (base2      '("#212122" "#1e1e1e"              ))
   (base3      '("#292b2b" "#292929" "brightblack"))
   (base4      '("#3f4040" "#3f3f3f" "brightblack"))
   (base5      '("#5c5e5e" "#525252" "brightblack"))
   (base6      '("#757878" "#6b6b6b" "brightblack"))
   (base7      '("#969896" "#979797" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"      ))
   (fg         '("#b2bbc2" "#b2bbc2" "white"))
   (fg-alt     (doom-darken fg 0.4))

   (red        '("#cc6666" "#cc6666" "red"))
   (blue       '("#81a2be" "#88aabb" "blue"))
   (grey       '("#7a8590" "#7a8590" "brightblack"))
   (violet     '("#a8a0ec" "#a8a0ec" "violet"))
   (light-blue '("#90dbe6" "#90dbe6" "lightblue"))
   (green      '("#b4d982" "#b4d982" "green"))
   (yellow     '("#fad778" "#fad778" "yellow"))
   (orange     '("#ef9c73" "#ef9c73" "orange"))
   (pink       '("#ed6c88" "#ed6c88" "pink"))

   (dark-blue  '("#81a2be" "#88aabb" "blue"))
   (teal       '("#81a2be" "#88aabb" "blue"))
   (cyan       '("#81a2be" "#88aabb" "blue"))
   (dark-cyan  '("#81a2be" "#88aabb" "blue"))
   (magenta    '("#a8a0ec" "#a8a0ec" "violet"))

   ;; face categories
   (highlight      blue)
   (vertical-bar   `("#161616" ,@base0))
   (selection      `(,(car (doom-lighten bg 0.1)) ,@(cdr base4)))
   (builtin        light-blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.14))
   (constants      violet)
   (functions      green)
   (keywords       pink)
   (methods        green)
   (operators      pink)
   (type           light-blue)
   (strings        yellow)
   (variables      orange)
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
   (modeline-fg     fg)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-monokai-pro-padded-modeline
      (if (integerp doom-monokai-pror-padded-modeline)
          doom-monokai-pro-padded-modeline
        4))))

  ;; --- faces ------------------------------
  ((doom-modeline-buffer-path       :foreground blue :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground yellow :bold bold)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt))))

  ;; --- variables --------------------------
  ;; ()
  )

(provide 'doom-monokai-pro-theme)
;;; doom-monokai-pro-theme.el ends here
