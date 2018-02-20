;;; doom-darcula-theme.el --- inspired by Atom One Dark
(require 'doom-themes)

;;
(defgroup doom-darcula-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-darcula-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-darcula-theme
  :type 'boolean)

(defcustom doom-darcula-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-darcula-theme
  :type 'boolean)

(defcustom doom-darcula-comment-bg doom-darcula-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-darcula-theme
  :type 'boolean)

(defcustom doom-darcula-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-darcula-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-darcula
  "A dark theme inspired by Emacs Darcula"

  ;; name        default   256       16
  ((bg         '("#1E2029" nil       nil            ))
   (bg-alt     '("#282a36" nil       nil            ))
   (base0      '("#1E2029" "black"   "black"        ))
   (base1      '("#282a36" "#1e1e1e" "brightblack"  ))
   (base2      '("#373844" "#2e2e2e" "brightblack"  ))
   (base3      '("#44475a" "#262626" "brightblack"  ))
   (base4      '("#565761" "#3f3f3f" "brightblack"  ))
   (base5      '("#6272a4" "#525252" "brightblack"  ))
   (base6      '("#b6b6b2" "#6b6b6b" "brightblack"  ))
   (base7      '("#ccccc7" "#979797" "brightblack"  ))
   (base8      '("#f8f8f2" "#dfdfdf" "white"        ))
   (fg         '("#e2e2dc" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#f8f8f2" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#ff5555" "#ff6655" "red"          ))
   (orange     '("#ffb86c" "#dd8844" "brightred"    ))
   (green      '("#50fa7b" "#99bb66" "green"        ))
   (teal       '("#0189cc" "#44b9b1" "brightgreen"  ))
   (yellow     '("#f1fa8c" "#ECBE7B" "yellow"       ))
   (blue       '("#0189cc" "#51afef" "brightblue"   ))
   (dark-blue  '("#0189cc" "#2257A0" "blue"         ))
   (magenta    '("#ff79c6" "#c678dd" "magenta"      ))
   (violet     '("#bd93f9" "#a9a1e1" "brightmagenta"))
   (cyan       '("#8be9fd" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#8be9fd" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       base5)
   (doc-comments   (doom-lighten (if doom-darcula-brighter-comments dark-cyan base5) 0.25))
   (constants      cyan)
   (functions      green)
   (keywords       magenta)
   (methods        teal)
   (operators      violet)
   (type           blue)
   (strings        yellow)
   (variables      base8)
   (numbers        red)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-darcula-brighter-modeline)
   (-modeline-pad
    (when doom-darcula-padded-modeline
      (if (integerp doom-darcula-padded-modeline) doom-darcula-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken  0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken magenta 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-darcula-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (solaire-hl-line-face :background base3)
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

;;; doom-darcula-theme.el ends here
