;;; doom-solarized-dark-theme.el --- description -*- lexical-binding: t; -*-

(require 'doom-themes)

;;
(defgroup doom-solarized-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-solarized-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-dark-theme
  :type 'boolean)

(defcustom doom-solarized-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-dark-theme
  :type 'boolean)

(defcustom doom-solarized-dark-comment-bg doom-solarized-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-solarized-dark-theme
  :type 'boolean)

(defcustom doom-solarized-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-solarized-dark-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-solarized-dark
  "A dark theme inspired by Solarized dark"

  ;; name        default   256       16
  ((bg         '("#153c44" nil       nil            ))
   (bg-alt     '("#002b36" nil       nil            ))
   (base0      '("#2a4d53" "black"   "black"        ))
   (base1      '("#3f5e61" "#1e1e1e" "brightblack"  ))
   (base2      '("#546f70" "#2e2e2e" "brightblack"  ))
   (base3      '("#69807e" "#262626" "brightblack"  ))
   (base4      '("#7e918d" "#3f3f3f" "brightblack"  ))
   (base5      '("#94a19b" "#525252" "brightblack"  ))
   (base6      '("#a9b2a9" "#6b6b6b" "brightblack"  ))
   (base7      '("#bec3b8" "#979797" "brightblack"  ))
   (base8      '("#d3d4c6" "#dfdfdf" "white"        ))
   (fg         '("#e8e5d5" "#2d2d2d" "white"        ))
   (fg-alt     '("#fdf6e3" "#bfbfbf" "brightwhite"  ))
   (grey       base4)
   (red        '("#dc322f" "#ff6655" "red"          ))
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#268bd2" "#51afef" "brightblue"   ))
   (dark-blue  '("#E1E3E5" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#D7DDD7" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-solarized-dark-brighter-comments
                       (doom-lighten teal 0.25)
                     base6))
   (doc-comments   teal)
   (constants      violet)
   (functions      magenta)
   (keywords       green)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.1)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-solarized-dark-brighter-modeline)
   (-modeline-pad
    (when doom-solarized-dark-padded-modeline
      (if (integerp doom-solarized-dark-padded-modeline) doom-solarized-dark-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten bg-alt 0.7)
      (doom-lighten base3 0.2)))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten bg-alt 0.7)
      (doom-darken bg-alt 0.05)))
   (modeline-bg-inactive   (doom-darken bg 0.02))
   (modeline-bg-inactive-l (doom-darken bg 0.025)))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (hl-line :background nil)

   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg :background region :weight 'bold)

   (org-block :background (doom-blend yellow bg 0.04))
   (org-block-background :background (doom-blend yellow bg 0.04))
   (org-block-begin-line :background (doom-blend yellow bg 0.08))
   (org-block-end-line :background (doom-blend yellow bg 0.08))
   (font-lock-comment-face
    :slant 'italic
    :foreground comments
    :background (if doom-solarized-dark-comment-bg (doom-blend teal base0 0.07)))
   ((font-lock-doc-face &override) :foreground doc-comments)
   ((font-lock-type-face &override) :slant 'italic)
   ((font-lock-builtin-face &override) :slant 'italic)
   ((font-lock-function-name-face &override) :foreground type)


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

    (company-tooltip-search-selection :background (doom-darken selection 0.35))
    (company-tooltip-selection  :background (doom-darken selection 0.35) :weight 'bold)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; ivy-mode
   (ivy-current-match :background (doom-darken blue 0.65) :distant-foreground fg)
   (ivy-minibuffer-match-face-1
    :foreground comments
    :weight 'light)
   (ivy-minibuffer-match-face-2 :foreground magenta :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground green   :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground yellow  :background base3 :weight 'bold)
   (ivy-minibuffer-match-highlight :foreground violet :weight 'bold)

   ;; posframe
   (ivy-posframe :background modeline-bg-l)
   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-solarized-light-theme.el ends here

(provide 'doom-solarized-dark-theme)
;;; doom-solarized-dark-theme.el ends here
