;; DOOM Vibrant (a more vibrant version of DOOM One)
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

(defcustom doom-vibrant-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-vibrant-theme
  :type '(or integer boolean))

(defcustom doom-vibrant-linum-height 1.0
  "The :height to render line numbers with."
  :group 'doom-vibrant-theme
  :type 'boolean)

;;
(def-doom-theme doom-vibrant
  "A more vibrant (and dark) theme, based off of DOOM One's classic look."

  ;; name        gui       256       16
  ((bg         '("#22252c" nil       nil))
   (bg-alt     '("#282c34" nil       nil))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#21272d" "#21212d" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3D3D48" "#5d5d5d" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#bbc2cf" "#bfbfbf" ))
   (fg-alt     '("#5D656B" "#5d5d5d" ))

   (grey       base4)
   (red        '("#ff665c" "#ff6655" ))
   (orange     '("#e69055" "#dd8844" ))
   (green      '("#7bc275" "#99bb66" ))
   (teal       '("#4db5bd" "#44b9b1" ))
   (yellow     '("#ECBE7B"           ))
   (blue       '("#51afef"           ))
   (dark-blue  '("#1f5582"           ))
   (magenta    '("#C57BDB"           ))
   (violet     '("#a9a1e1"           ))
   (cyan       '("#46D9FF"           ))
   (dark-cyan  '("#5699AF"           ))

   ;; face categories
   (highlight      blue)
   (vertical-bar   base0)
   (current-line   base2)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-vibrant-brighter-comments dark-cyan base5))
   (doc-comments   (if doom-vibrant-brighter-comments teal (doom-lighten base5 0.2)))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
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
   (modeline-bright doom-vibrant-brighter-modeline)
   (modeline-pad
    (when doom-vibrant-padded-modeline
      (if (integerp doom-vibrant-padded-modeline) doom-vibrant-padded-modeline 4)))

   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (doom-blend blue grey (if modeline-bright 0.4 0.08)))

   (modeline-bg     (if modeline-bright (doom-darken blue 0.5) bg-alt)                 "brightblack")
   (modeline-bg-l   (if modeline-bright modeline-bg            (doom-darken bg 0.075)) "black")
   (modeline-bg-inactive   (doom-darken bg 0.25))
   (modeline-bg-inactive-l (doom-darken bg-alt 0.3)))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (linum :foreground base4
          :distant-foreground nil
          :bold nil
          :height doom-vibrant-linum-height)
   (linum-highlight-face :foreground blue
                         :distant-foreground base8
                         :bold nil
                         :height doom-vibrant-linum-height)

   (doom-modeline-bar :background (if modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if modeline-bright base8 blue) :bold bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   )


  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-vibrant-theme.el ends here
