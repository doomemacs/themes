;;; doom-city-lights-theme.el --- inspired by Atom One Dark
(require 'doom-themes)

;;
(defgroup doom-city-lights-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-city-lights-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-city-lights-theme
  :type 'boolean)

(defcustom doom-city-lights-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-city-lights-theme
  :type 'boolean)

(defcustom doom-city-lights-comment-bg doom-city-lights-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-city-lights-theme
  :type 'boolean)

(defcustom doom-city-lights-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-city-lights-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-city-lights
  "A dark theme inspired by Atom City Lights"

  ;; name        default   256       16
  ((bg         '("#181E24" nil       nil            ))
   (bg-alt     '("#1D252C" nil       nil            ))
   (base0      '("#11171D" "black"   "black"        ))
   (base1      '("#212930" "#1e1e1e" "brightblack"  ))
   (base2      '("#333F4A" "#2e2e2e" "brightblack"  ))
   (base3      '("#3A4855" "#262626" "brightblack"  ))
   (base4      '("#41505E" "#3f3f3f" "brightblack"  ))
   (base5      '("#5F7487" "#525252" "brightblack"  ))
   (base6      '("#718CA1" "#6b6b6b" "brightblack"  ))
   (base7      '("#7997AD" "#979797" "brightblack"  ))
   (base8      '("#CBD1E1" "#dfdfdf" "white"        ))
   (fg         '("#B7C5D3" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#CBD1E1" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#D95468" "#ff6655" "red"          ))
   (orange     '("#D98E48" "#dd8844" "brightred"    ))
   (green      '("#8BD49C" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#EBBF83" "#ECBE7B" "yellow"       ))
   (blue       '("#5EC4FF" "#51afef" "brightblue"   ))
   (dark-blue  '("#5C748E" "#2257A0" "blue"         ))
   (magenta    '("#E27E8D" "#c678dd" "magenta"      ))
   (violet     '("#B62D65" "#a9a1e1" "brightmagenta"))
   (cyan       '("#70E1E8" "#46D9FF" "brightcyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-city-lights-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-city-lights-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      teal)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        base7)
   (variables      base8)
   (numbers        magenta)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-city-lights-brighter-modeline)
   (-modeline-pad
    (when doom-city-lights-padded-modeline
      (if (integerp doom-city-lights-padded-modeline) doom-city-lights-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-city-lights-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-city-lights-theme.el ends here
