;;; doom-wilmersdorf-theme.el --- inspired by Atom City Lights -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-wilmersdorf-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-wilmersdorf-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-wilmersdorf-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-wilmersdorf
  "A dark theme inspired by Atom City Lights"

  ;; name        default   256       16
  ((bg         '("#282b33" nil       nil            ))
   (bg-alt     '("#1f2024" nil       nil            ))
   (base0      '("#222228" "black"   "black"        ))
   (base1      '("#282b33" "#1e1e1e" "brightblack"  ))
   (base2      '("#34373e" "#2e2e2e" "brightblack"  ))
   (base3      '("#41454b" "#262626" "brightblack"  ))
   (base4      '("#515462" "#3f3f3f" "brightblack"  ))
   (base5      '("#888395" "#525252" "brightblack"  ))
   (base6      '("#929292" "#6b6b6b" "brightblack"  ))
   (base7      '("#727269" "#979797" "brightblack"  ))
   (base8      '("#eceff4" "#dfdfdf" "white"        ))
   (fg-alt     '("#c9d9ff" "#bfbfbf" "brightwhite"  ))
   (fg         '("#c6c6c6" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#e1c1ee" "#ff6655" "red"          ))
   (orange     '("#a6c1e0" "#dd8844" "brightred"    ))
   (green      '("#5b94ab" "#99bb66" "green"        ))
   (teal       '("#7ebebd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#cfcf9c" "#ECBE7B" "yellow"       ))
   (blue       '("#819cd6" "#51afef" "brightblue"   ))
   (light-blue '("#90a6db" "#ECBE7B" "yellow"       ))
   (dark-blue  '("#616c96" "#2257A0" "blue"         ))
   (magenta    '("#a6c1e0" "#c678dd" "magenta"      ))
   (violet     '("#b0a2e7" "#a9a1e1" "brightmagenta"))
   (cyan       '("#7289bc" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#6e7899" "#5699AF" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        teal)
   (comments       dark-cyan)
   (doc-comments   (doom-lighten dark-cyan 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           violet)
   (strings        green)
   (variables      magenta)
   (numbers        magenta)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-wilmersdorf-padded-modeline
      (if (integerp doom-wilmersdorf-padded-modeline) doom-wilmersdorf-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    `(,(doom-darken (car bg) 0.15) ,@(cdr base0)))
   (modeline-bg-l
    `(,(doom-darken (car bg) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background highlight)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground highlight)

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
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; tooltip
   (tooltip              :background bg-alt :foreground fg))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-wilmersdorf-theme.el ends here
