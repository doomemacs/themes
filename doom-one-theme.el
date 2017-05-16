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

  ;; name      gui       term (256)
  ((bg         "#21242b" nil      )
   (bg-alt     "#282c34" nil      )
   (fg         "#bbc2cf" "#bfbfbf")
   (fg-alt     "#5B6268" "#3d3d3d")
   (black      "#1B2229" "black"  )
   (light-grey fg-alt    "#525252")
   (grey       "#3B3F46" fg-alt   )
   (dark-grey  "#23272e" "#262626")
   (white      "#DFDFDF" "#dfdfdf")
   (red        "#ff6c6b" "#ff6655")
   (orange     "#da8548" "#dd8844")
   (green      "#98be65" "#99bb66")
   (teal       "#4db5bd" "#44b9b1")
   (yellow     "#ECBE7B"          )
   (blue       "#51afef"          )
   (dark-blue  "#2257A0"          )
   (magenta    "#c678dd"          )
   (violet     "#a9a1e1"          )
   (cyan       "#46D9FF"          )
   (dark-cyan  "#5699AF"          )

   ;; face categories
   (highlight      blue)
   (vertical-bar   (doom-darken dark-grey 0.2))
   (current-line   dark-grey "black")
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-one-brighter-comments dark-cyan light-grey))
   (doc-comments   (if doom-one-brighter-comments (doom-lighten dark-cyan 0.1) (doom-lighten light-grey 0.2)))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         (doom-lighten bg 0.075))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    light-grey)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-fg     nil)
   (modeline-fg-alt light-grey grey)
   (modeline-bg     bg-alt "brightblack")
   (modeline-bg-alt (doom-darken bg 0.075) "black")
   (modeline-pad
    (when doom-one-padded-modeline
      (if (integerp doom-one-padded-modeline) doom-one-padded-modeline 4))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (linum :foreground (if gui "#42454E" light-grey)
          :distant-foreground nil
          :bold nil
          :height doom-one-linum-height)
   (doom-nlinum-highlight :foreground black
                          :distant-foreground (doom-darken white 0.2)
                          :bold nil
                          :height doom-one-linum-height)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-alt)))

   (doom-mode-line
    :inherit 'mode-line
    :background (doom-darken bg 0.075))
   (doom-mode-line-inactive
    :inherit 'mode-line-inactive
    :background (if doom-one-brighter-modeline violet bg))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue))


  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-one-theme.el ends here
