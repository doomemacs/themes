;;; doom-nord-theme.el --- inspired by Atom One Dark
(require 'doom-themes)

;;
(defgroup doom-nord-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-nord-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-nord-theme
  :type 'boolean)

(defcustom doom-nord-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-nord-theme
  :type 'boolean)

(defcustom doom-nord-comment-bg doom-nord-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-nord-theme
  :type 'boolean)

(defcustom doom-nord-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-nord-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-nord
  "A dark theme inspired by Emacs Nord"

  ;; name        default   256       16
  ((bg         '("#2E3440" nil       nil            ))
   (bg-alt     '("#3B4253" nil       nil            ))
   (base0      '("#2E3440" "black"   "black"        ))
   (base1      '("#3B4253" "#1e1e1e" "brightblack"  ))
   (base2      '("#434C5F" "#2e2e2e" "brightblack"  ))
   (base3      '("#4C566B" "#262626" "brightblack"  ))
   (base4      '("#545F76" "#3f3f3f" "brightblack"  ))
   (base5      '("#67748F" "#525252" "brightblack"  ))
   (base6      '("#D8DEE9" "#6b6b6b" "brightblack"  ))
   (base7      '("#E5E9F0" "#979797" "brightblack"  ))
   (base8      '("#ECEFF4" "#dfdfdf" "white"        ))
   (fg         '("#E5E9F0" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#ECEFF4" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#C16069" "#ff6655" "red"          ))
   (orange     '("#D2876D" "#dd8844" "brightred"    ))
   (green      '("#A2BF8A" "#99bb66" "green"        ))
   (teal       '("#8EBCBB" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECCC87" "#ECBE7B" "yellow"       ))
   (blue       '("#80A0C2" "#51afef" "brightblue"   ))
   (dark-blue  '("#5C748E" "#2257A0" "blue"         ))
   (magenta    '("#B58DAE" "#c678dd" "magenta"      ))
   (violet     '("#5D80AE" "#a9a1e1" "brightmagenta"))
   (cyan       '("#86C0D1" "#46D9FF" "brightcyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       (if doom-nord-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-nord-brighter-comments dark-cyan base5) 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.5))
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
   (-modeline-bright doom-nord-brighter-modeline)
   (-modeline-pad
    (when doom-nord-padded-modeline
      (if (integerp doom-nord-padded-modeline) doom-nord-padded-modeline 4)))

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
    :background (if doom-nord-comment-bg (doom-lighten bg 0.05)))
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

;;; doom-nord-theme.el ends here
