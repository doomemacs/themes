;;; doom-xcode-theme.el --- ibased off of Apple's Xcode Dark Theme -*- no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-xcode-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-xcode-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-xcode-theme
  :type '(choice integer boolean))

(def-doom-theme doom-xcode
  "A theme based off of the Xcode Dark Theme"

  ;; name        gui       256       16
  ((bg         '("#1d1f21" nil       nil          ))
   (bg-alt     '("#161719" nil       nil          ))
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
   (orange     '("#de935f" "#dd9955" "brightred"))
   (yellow     '("#f0c674" "#f0c674" "yellow"))
   (green      '("#b5bd68" "#b5bd68" "green"))
   (blue       '("#81a2be" "#88aabb" "brightblue"))
   (teal       blue) ; FIXME replace with real teal
   (magenta    '("#c9b4cf" "#c9b4cf" "magenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))


   (grey       '("#7a8590" "#7a8590" "brightblack"))
   (light-green'("#88c0b3" "#88c0b3" "lightgreen"))
   (violet     '("#b294bb" "#b294bb" "brightmagenta"))
   (light-blue '("#7eddfc" "#7eddfc" "lightblue"))
   (dark-blue  '("#5db8d9" "#5db8d9" "darkblue"))
   (orange     '("#ef8775" "#ef8775" "orange"))
   (pink       '("#ef82af" "#ef82af" "pink"))

   ;; face categories
   (highlight      blue)
   (vertical-bar   `("#161616" ,@base0))
   (selection      `(,(car (doom-lighten bg 0.1)) ,@(cdr base4)))
   (builtin        light-green)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.14))
   (constants      dark-blue)
   (functions      violet)
   (keywords       pink)
   (methods        violet)
   (operators      fg)
   (type           light-blue)
   (strings        light-green)
   (variables      dark-blue)
   (numbers        light-green)
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
  ((doom-modeline-buffer-path       :foreground dark-blue :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :bold bold)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt))))

  ;; --- variables --------------------------
  ;; ()
  )

(provide 'doom-xcode-theme)
;;; doom-xcode-theme.el ends here
