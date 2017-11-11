;; doom-vibrant-theme.el --- a more vibrant version of doom-one
(require 'doom-themes)

;;
(defgroup doom-vibrant-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-vibrant-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-vibrant-theme
  :type 'boolean)

(defcustom doom-vibrant-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-vibrant-theme
  :type 'boolean)

(defcustom doom-vibrant-comment-bg doom-vibrant-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-vibrant-theme
  :type 'boolean)

(defcustom doom-vibrant-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-vibrant-theme
  :type '(or integer boolean))


;;
(def-doom-theme doom-vibrant
  "A dark theme based off of doom-one with more vibrant colors."

  ;; name        gui       256       16
  ((bg         '("#242730" nil       nil))
   (bg-alt     '("#2a2e38" nil       nil))
   (base0      '("#1c1f24" "#101010" "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#21272d" "#21212d" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#484854" "#5e5e5e" "brightblack"  ))
   (base5      '("#62686E" "#666666" "brightblack"  ))
   (base6      '("#757B80" "#7b7b7b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#bbc2cf" "#bfbfbf" ))
   (fg-alt     '("#5D656B" "#5d5d5d" ))

   (grey       base4)
   (red        '("#ff665c" "#ff6655" ))
   (orange     '("#e69055" "#dd8844" ))
   (green      '("#7bc275" "#99bb66" ))
   (teal       '("#4db5bd" "#44b9b1" ))
   (yellow     '("#FCCE7B"           ))
   (blue       '("#51afef"           ))
   (dark-blue  '("#1f5582"           ))
   (magenta    '("#C57BDB"           ))
   (violet     '("#a991f1"           )) ;a9a1e1
   (cyan       '("#5cEfFF"           ))
   (dark-cyan  '("#6A8FBF"           ))

   ;; face categories
   (highlight      blue)
   (vertical-bar   (doom-darken bg 0.15))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-vibrant-brighter-comments dark-cyan base5))
   (doc-comments   (if doom-vibrant-brighter-comments (doom-lighten dark-cyan 0.15) (doom-lighten base4 0.3)))
   (constants      violet)
   (functions      cyan)
   (keywords       blue)
   (methods        violet)
   (operators      magenta)
   (type           yellow)
   (strings        green)
   (variables      base8)
   (numbers        orange)
   (region         "#3d4451")
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when doom-vibrant-padded-modeline
      (if (integerp doom-vibrant-padded-modeline) doom-vibrant-padded-modeline 4)))

   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (doom-blend blue grey (if doom-vibrant-brighter-modeline 0.4 0.08)))

   (modeline-bg
    (if doom-vibrant-brighter-modeline
        `("#383f58" ,@(cdr base1))
      `(,(car bg-alt) ,@(cdr base0))))
   (modeline-bg-l
    (if doom-vibrant-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.25))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (font-lock-comment-face
    :foreground comments
    :background (if doom-vibrant-comment-bg (doom-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   ;; Line number faces must explicitly disable its text style attributes
   ;; because nearby faces may "bleed" into the line numbers otherwise.
   (line-number
    :inherit 'default :foreground base4 :distant-foreground nil
    :bold nil :italic nil :underline nil :strike-through nil)
   (line-number-current-line
    :inherit 'bold :foreground blue :distant-foreground base8
    :italic nil :underline nil :strike-through nil)

   (doom-modeline-bar :background (if doom-vibrant-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if doom-vibrant-brighter-modeline base8 blue) :bold bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if doom-vibrant-brighter-modeline base8 highlight))

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
   (markdown-header-face :inherit 'bold :foreground red)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden-alt))


  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-vibrant-theme.el ends here
