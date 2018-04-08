;;; doom-nova-theme.el --- inspired by Nova
(require 'doom-themes)

;;
(defgroup doom-nova-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-nova-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-nova-theme
  :type 'boolean)

(defcustom doom-nova-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-nova-theme
  :type 'boolean)

(defcustom doom-nova-comment-bg doom-nova-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-nova-theme
  :type 'boolean)

(defcustom doom-nova-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-nova-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-nova
  "A dark theme inspired by Atom Nova Dark"

  ;; name        default   256       16
  ((bg         '("#3C4C55" nil       nil            ))
   (bg-alt     '("#334048" nil       nil            ))
   (base0      '("#1E272C" "black"   "black"        ))
   (base1      '("#29343B" "#1e1e1e" "brightblack"  ))
   (base2      '("#3C4C55" "#2e2e2e" "brightblack"  ))
   (base3      '("#556873" "#262626" "brightblack"  ))
   (base4      '("#6A7D89" "#3f3f3f" "brightblack"  ))
   (base5      '("#899BA6" "#525252" "brightblack"  ))
   (base6      '("#C5D4DD" "#6b6b6b" "brightblack"  ))
   (base7      '("#DAEBF4" "#979797" "brightblack"  ))
   (base8      '("#E6EEF3" "#dfdfdf" "white"        ))
   (fg         '("#C5D4DD" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#DAEBF4" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#DF8C8C" "#ff6655" "red"          ))
   (orange     '("#F2C38F" "#dd8844" "brightred"    ))
   (green      '("#A8CE93" "#99bb66" "green"        ))
   (teal       '("#81C1C9" "#44b9b1" "brightgreen"  ))
   (yellow     '("#DADA93" "#ECBE7B" "yellow"       ))
   (blue       '("#83AFE5" "#51afef" "brightblue"   ))
   (dark-blue  '("#6086AC" "#2257A0" "blue"         ))
   (magenta    '("#D18EC2" "#c678dd" "magenta"      ))
   (violet     '("#9A93E1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#7FC1CA" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#33666E" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      highlight)
   (builtin        blue)
   (comments       (if doom-nova-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-nova-brighter-comments dark-cyan base4) 0.25))
   (constants      highlight)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      green)
   (type           green)
   (strings        cyan)
   (variables      red)
   (numbers        highlight)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   ;; (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-nova-brighter-modeline)
   (-modeline-pad
    (when doom-nova-padded-modeline
      (if (integerp doom-nova-padded-modeline) doom-nova-padded-modeline 4)))

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

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-nova-comment-bg (doom-lighten bg 0.05)))
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

   ;; ivy-mode
   (ivy-current-match :background base1 :distant-foreground base0 :weight 'normal)

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

;;; doom-nova-theme.el ends here
