;;; doom-rouge-theme.el --- inspired by Nord -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-rouge-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-rouge-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-rouge-theme
  :type 'boolean)

(defcustom doom-rouge-brighter-tabs nil
  "If non-nil, tabs will a more vivid background color."
  :group 'doom-rouge-theme
  :type 'boolean)

(defcustom doom-rouge-comment-bg doom-rouge-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-rouge-theme
  :type 'boolean)

(defcustom doom-rouge-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-rouge-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-rouge
  "A dark theme inspired by Nord."

  ;; name        default   256       16
  ((bg         '("#172030" nil       nil            )) ;; modified
   (bg-alt     '("#172030" nil       nil            ))
   (base0      '("#191C25" "black"   "black"        ))
   (base1      '("#242832" "#1e1e1e" "brightblack"  ))
   (base2      '("#2C333F" "#2e2e2e" "brightblack"  ))
   (base3      '("#373E4C" "#262626" "brightblack"  ))
   (base4      '("#434C5E" "#3f3f3f" "brightblack"  ))
   (base5      '("#64727d" "#64727d" "brightblack"  ))
   (base6      '("#9099AB" "#6b6b6b" "brightblack"  ))
   (base7      '("#D8DEE9" "#979797" "brightblack"  ))
   (base8      '("#F0F4FC" "#dfdfdf" "white"        ))
   (fg         '("#bbb" "#bbb" "white"        ))
   (fg-alt     '("#E5E9F0" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#c6797e" "#c6797e" "red"          ))
   (light-red  '("#C6878F" "#C6878F" "red"          ))
   (orange     '("#eabe9a" "#eabe9a" "brightred"    ))
   (green      '("#ADB9A4" "#ADB9A4" "green"        ))
   (teal       '("#8FBCBB" "#44b9b1" "brightgreen"  ))
   (yellow     '("#EBCB8B" "#ECBE7B" "yellow"       ))
   (blue       '("#81A1C1" "#51afef" "brightblue"   ))
   (dark-blue  '("#91d1bd" "#91d1bd" "blue"         ))
   (magenta     '("#b18bb1" "#b18bb1" "magenta"      ))
   (salmon     '("#F9B5AC" "#F9B5AC" "orange"       ))
   (violet     '("#5D80AE" "#a9a1e1" "brightmagenta"))
   (cyan       '("#88C0D0" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#507681" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      base4)
   (builtin        red)
   (comments       (if doom-rouge-brighter-comments dark-cyan (doom-lighten base5 0.2)))
   (doc-comments   (doom-lighten (if doom-rouge-brighter-comments dark-cyan base5) 0.25))
   (constants      orange)
   (functions      salmon)
   (keywords       magenta)
   (methods        red)
   (operators      green)
   (type           red)
   (strings        green)
   (variables      light-red)
   (numbers        magenta)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-rouge-padded-modeline
      (if (integerp doom-rouge-padded-modeline) doom-rouge-padded-modeline 4)))

   (tabs-bg (if doom-rouge-brighter-tabs red bg))
   (tabs-bar-bg (if doom-rouge-brighter-tabs bg red))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg base1)
   (modeline-bg-l `(,(doom-darken (car bg) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((lazy-highlight :background base4)
   (cursor :background red)


   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-rouge-comment-bg (doom-lighten bg 0.05)))
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

   (doom-modeline-project-root-dir :foreground base6)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ediff
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))

   ;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; centuar-tabs
   (centaur-tabs-selected :background tabs-bg)
   (centaur-tabs-selected-modified :background tabs-bg)
   (centaur-tabs-unselected-modified :background base0)
   (centaur-tabs-active-bar-face :background tabs-bar-bg)

   ;; neotree
   (neo-root-dir-face :foreground red))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-rouge-theme.el ends here
