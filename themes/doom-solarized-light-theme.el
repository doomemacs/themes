;;; doom-solarized-light-theme.el --- inspired by Atom One Dark
(require 'doom-themes)

;;
(defgroup doom-solarized-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-solarized-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-light-theme
  :type 'boolean)

(defcustom doom-solarized-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-light-theme
  :type 'boolean)

(defcustom doom-solarized-light-comment-bg doom-solarized-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-solarized-light-theme
  :type 'boolean)

(defcustom doom-solarized-light-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-solarized-light-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-solarized-light
  "A light theme inspired by Solarized light"

  ;; name        default   256       16
  ((bg         '("#FDF6E3" nil       nil            ))
   (bg-alt     '("#FFFBEA" nil       nil            ))
   (base0      '("#FFFBF0" "black"   "black"        ))
   (base1      '("#FCF8ED" "#1e1e1e" "brightblack"  ))
   (base2      '("#FCF7E8" "#2e2e2e" "brightblack"  ))
   (base3      '("#F2E6CE" "#262626" "brightblack"  ))
   (base4      '("#E1DBCD" "#3f3f3f" "brightblack"  ))
   (base5      '("#D6D6D6" "#525252" "brightblack"  ))
   (base6      '("#B0AFAF" "#6b6b6b" "brightblack"  ))
   (base7      '("#788484" "#979797" "brightblack"  ))
   (base8      '("#626C6C" "#dfdfdf" "white"        ))
   (fg         '("#6B7A7C" "#2d2d2d" "white"        ))
   (fg-alt     '("#7B8787" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#B4C342" "#44b9b1" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#268bd2" "#51afef" "brightblue"   ))
   (dark-blue  '("#E1E3E5" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#D7DDD7" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base3)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-solarized-light-brighter-comments dark-cyan base5))
   (doc-comments   (doom-blend dark-cyan fg 0.8))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.05) ,@(doom-darken (cdr base0) 0.05)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-solarized-light-brighter-modeline)
   (-modeline-pad
    (when doom-solarized-light-padded-modeline
      (if (integerp doom-solarized-light-padded-modeline) doom-solarized-light-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten bg 0.7)
      `(,(doom-lighten (car bg) 0.25) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten bg 0.7)
      `(,(doom-lighten (car bg) 0.25) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-lighten bg 0.02))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (org-block :background (doom-blend yellow bg 0.04))
   (org-block-background :background (doom-blend yellow bg 0.04))
   (org-block-begin-line :background (doom-blend yellow bg 0.08))
   (org-block-end-line :background (doom-blend yellow bg 0.08))
   (font-lock-comment-face
    :foreground comments
    :background (if doom-solarized-light-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   ((font-lock-type-face &override) :slant 'italic)
   ((font-lock-builtin-face &override) :slant 'italic)
   ((font-lock-function-name-face &override) :weight 'semi-bold)
   ((font-lock-keyword-face &override) :weight 'semi-bold )

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

   ;; posframe
   (ivy-posframe :background (doom-lighten bg 0.3))
   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-solarized-light-theme.el ends here
