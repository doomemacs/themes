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
  ((bg         '("#282b33" "#282b33" nil            ))
   (bg-alt     '("#1f2024" "#1f2024" nil            ))
   (base0      '("#222228" "#222228" "black"        ))
   (base1      '("#282b33" "#282b33" "brightblack"  ))
   (base2      '("#34373e" "#34373e" "brightblack"  ))
   (base3      '("#41454b" "#41454b" "brightblack"  ))
   (base4      '("#515462" "#515462" "brightblack"  ))
   (base5      '("#888395" "#888395" "brightblack"  ))
   (base6      '("#929292" "#929292" "brightblack"  ))
   (base7      '("#727269" "#727269" "brightblack"  ))
   (base8      '("#eceff4" "#eceff4" "white"        ))
   (fg-alt     '("#c9d9ff" "#c9d9ff" "brightwhite"  ))
   (fg         '("#c6c6c6" "#c6c6c6" "white"        ))

   (grey       base4)
   (red        '("#e1c1ee" "#e1c1ee" "red"          ))
   (orange     '("#a6c1e0" "#a6c1e0" "brightred"    ))
   (green      '("#5b94ab" "#5b94ab" "green"        ))
   (teal       '("#7ebebd" "#7ebebd" "brightgreen"  ))
   (yellow     '("#cfcf9c" "#cfcf9c" "yellow"       ))
   (blue       '("#819cd6" "#819cd6" "brightblue"   ))
   (light-blue '("#90a6db" "#90a6db" "yellow"       ))
   (dark-blue  '("#616c96" "#616c96" "blue"         ))
   (magenta    '("#a6c1e0" "#a6c1e0" "magenta"      ))
   (violet     '("#b0a2e7" "#b0a2e7" "brightmagenta"))
   (cyan       '("#7289bc" "#7289bc" "brightcyan"   ))
   (dark-cyan  '("#6e7899" "#6e7899" "cyan"   ))

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
    `(,(doom-darken (car bg) 0.1) ,@(cdr base0)))
   (modeline-bg-l
    `(,(doom-darken (car bg) 0.15) ,@(cdr base0)))
   (modeline-bg-inactive   `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


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

   ;; mic-paren
   (paren-face-match    :foreground teal   :background base0 :weight 'ultra-bold)
   (paren-face-mismatch :foreground red :background violet   :weight 'ultra-bold)
   (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)

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
