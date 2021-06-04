;;; doom-ir-black-theme.el --- port of the original ir_black theme -*- lexical-bindings: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-ir-black-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ir-black-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ir-black-theme
  :type 'boolean)

(defcustom doom-ir-black-comment-bg doom-ir-black-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ir-black-theme
  :type 'boolean)

(defcustom doom-ir-black-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ir-black-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-ir-black
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#000000" "black"   "black"        ))
   (bg-alt     '("#121212" "black"   "black"        ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (extra-1    '("#3D3D3D" "#3D3D3D" "brightblack"  ))
   (extra-2    '("#3D3D3D" "#3D3D3D" "brightblack"  ))

   (fg         '("#f6f3e8" "#f6f3e8" "brightwhite"  ))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"        ))
   (white      '("#ffffff" "#ffffff" "white"))

   (grey       base4)
   (red        '("#ff6c60" "#ff6c60" "red"          ))
   (orange     '("#E9C062" "#E9C062" "brightred"    ))
   (orange-alt '("#FFD2A7" "#FFD2A7" "brightred"    ))
   (green      '("#A8FF60" "#A8FF60" "green"        ))
   (green-alt  '("#99CC99" "#99CC99" "green"        ))
   (teal       '("#00A0A0" "#00A0A0" "brightgreen"  ))
   (yellow     '("#FFFFB6" "#FFFFB6" "yellow"       ))
   (blue       '("#96CBFE" "#96CBFE" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#FF73FD" "#FF73FD" "magenta"      ))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#C6C5FE" "#C6C5FE" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base5)
   (selection      cyan)
   (builtin        magenta)
   (comments       (if doom-ir-black-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-ir-black-brighter-comments dark-cyan base5) 0.25))
   (constants      green-alt)
   (functions      orange-alt)
   (keywords       blue)
   (methods        cyan)
   (operators      white)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        magenta)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-ir-black-padded-modeline
      (if (integerp doom-ir-black-padded-modeline) doom-ir-black-padded-modeline 4)))

   (modeline-fg     white)
   (modeline-fg-alt base5)

   (modeline-bg base4)
   (modeline-bg-l base4)
   (modeline-bg-inactive base3)
   (modeline-bg-inactive-l base3))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground "#FFFF00" :weight 'bold)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-ir-black-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

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

;;; doom-ir-black-theme.el ends here
