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
   (base0      '("#202431" "black"   "black"))
   (base1      '("#232635" "#202020" "brightblack"))
   (base2      '("#292D3E" "#292D3E" "brightblack"))
   (base3      '("#3a3f58" "#2e2e2e" "brightblack"))
   (base4      '("#4E5579" "#3f3f3f" "brightblack"))
   (base5      '("#676E95" "#676E95" "brightblack"))
   (base6      '("#697098" "#6b6b6b" "brightblack"))
   (base7      '("#717CB4" "#979797" "brightblack"))
   (base8      '("#A6Accd" "#DDDDDD" "white"))
   (fg         '("#EEFFFF" "#BFBFBF" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#2D2D2D" "white"))

   (grey base4)

   (red         '("#FF5370" "#FF6655" "red"))
   (orange      '("#F78C6C" "#DD8844" "brightred"))
   (green       '("#C3E88D" "#99BB66" "green"))
   (teal        '("#f07178" "#44B9B1" "brightgreen"))
   (yellow      '("#FFCB6B" "#ECBE7B" "brightyellow"))
   (blue        '("#82aaff" "#51AFEF" "brightblue"))
   (dark-blue   '("#b2ccd6" "#727280" "blue"))
   (magenta     '("#c792ea" "#C678DD" "brightmagenta"))
   (violet      '("#bb80b3" "#A9A1E1" "magenta"))
   (cyan        '("#78CCF0" "#46D9FF" "brightcyan"))
   (dark-cyan   '("#80CBC4" "#8FA1B3" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   "#282B3C")
   (selection      base7)
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

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-keyword-face
    :slant 'italic)

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

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :slant 'italic)))

  ;; --- extra variables ---------------------
  ;; ()



  (provide 'doom-palenight-theme)



;;; doom-palenight-theme.el ends here
