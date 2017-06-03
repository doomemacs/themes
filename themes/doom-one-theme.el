;; DOOM One Dark (inspired by Atom One Dark)
(require 'doom-themes)

;;
(defgroup doom-one-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-one-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-one-theme
  :type '(or integer boolean))

(defcustom doom-one-linum-height 1.0
  "The :height to render line numbers with."
  :group 'doom-one-theme
  :type 'boolean)

;;
(def-doom-theme doom-one
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#21242b" nil       nil            ))
   (bg-alt     '("#282c34" "#242428" "black"        ))
   (fg         '("#bbc2cf" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#5B6268" "#3d3d3d" "white"        ))

   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#23272e" "#262626" "brightblack"  ))
   (base3      '("#3B3F46" "#3d3d3d" "brightblack"  ))
   (base4      '("#5B6268" "#525252" "brightblack"  ))
   (base5      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base6      '("#9ca0a4" "#979797" "brightblack"  ))
   (base7      '("#DFDFDF" "#dfdfdf" "white"        ))

   (red        '("#ff6c6b" "#ff6655" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (blue       '("#51afef" "#51afef" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "magenta"      ))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   (white      base7)
   (light-grey base4)
   (grey       base3)
   (dark-grey  base2)
   (black      base0)

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   dark-grey)
   (current-line   `(,(car base1) ,@(cdr black)))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-one-brighter-comments dark-cyan light-grey))
   (doc-comments   (doom-lighten (if doom-one-brighter-comments dark-cyan light-grey) 0.15))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         (doom-lighten bg-alt 0.1))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    light-grey)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bright doom-one-brighter-modeline)
   (modeline-pad
    (when doom-one-padded-modeline
      (if (integerp doom-one-padded-modeline) doom-one-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet grey (if modeline-bright 0.5 0.2)))

   (modeline-bg     (if modeline-bright (doom-darken blue 0.475) bg-alt))
   (modeline-bg-l   (apply #'doom-darken (if modeline-bright (list blue 0.45) (list bg 0.085))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l (doom-darken bg 0.025)))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (linum
    `((((min-colors 257))
       (:foreground ,(if gui "#42454E" light-grey) :distant-foreground nil :bold nil))
      (((min-colors 256))
       (:foreground "#414141" :distant-foreground nil :bold nil))))
   ;; (doom-linum-highlight
   ;;  `((((min-colors 257))
   ;;     (:foreground ,black :distant-foreground ,(doom-darken white 0.3) :bold ,bold :height ,doom-one-linum-height))
   ;;    (((min-colors 256))
   ;;     (:foreground ,(c fg 256) :distant-foreground ,white :bold nil))))

   (doom-modeline-bar :background (if modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if modeline-bright white highlight))

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
   (css-selector             :foreground blue))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-one-theme.el ends here
