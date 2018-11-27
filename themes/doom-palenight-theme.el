;;; doom-palenight-theme.el -*- lexical-binding: t; -*-
(require 'doom-themes)

;;
(defgroup doom-palenight-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-palenight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-palenight-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-palenight
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#292D3E" nil       nil))
   (bg-alt     '("#21242b" nil       nil))
   (base0      '("#1c1f2b" "black"   "black"))
   (base1      '("#1e212e" "#262626" "brightblack"))
   (base2      '("#242837" "#303030" "brightblack"))
   (base3      '("#3C435E" "#3a3a3a" "brightblack"))
   (base4      '("#4E5579" "#444444" "brightblack"))
   (base5      '("#676E95" "#585858" "brightblack"))
   (base6      '("#697098" "#626262" "brightblack"))
   (base7      '("#717CB4" "#767676" "brightblack"))
   (base8      '("#A6Accd" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base4)

   (red         '("#FF5370" "#ff0000" "red"))
   (orange      '("#F78C6C" "#ff5f00" "brightred"))
   (green       '("#C3E88D" "#afff00" "green"))
   (teal        '("#44B9B1" "#00d7af" "brightgreen"))
   (yellow      '("#FFCB6B" "#ffd700" "brightyellow"))
   (blue        '("#82aaff" "#5fafff" "brightblue"))
   (dark-blue   '("#b2ccd6" "#d7ffff" "blue"))
   (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   (violet      '("#bb80b3" "#d787af" "magenta"))
   (cyan        '("#78CCF0" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80CBC4" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   "#282B3C")
   (selection      base4)
   (builtin        blue)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      blue)
   (functions      blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      cyan)
   (type           yellow)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         "#3C435E")
   (error          "#FF5572")
   (warning        "#FFCA28")
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-palenight-padded-modeline
      (if (integerp doom-palenight-padded-modeline) doom-palenight-padded-modeline 4))))

  ;; --- faces ------------------------
  ((doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-keyword-face
    :slant 'italic
    :foreground keywords)

   (font-lock-comment-face
    :slant 'italic
    :foreground comments)

   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :slant 'italic
    :foreground doc-comments)


   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired
   (dired-k-commited :foreground vc-modified)
   (dired-k-modified :foreground vc-modified)
   (dired-k-added    :foreground vc-added)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic)))

(provide 'doom-palenight-theme)



;;; doom-palenight-theme.el ends here
