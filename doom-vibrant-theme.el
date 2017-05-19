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

  ;; name      gui       term (256)
  ((bg         "#22252c" nil      )
   (bg-alt     "#282c34" nil      )
   (fg         "#bbc2cf" "#bfbfbf")
   (fg-alt     "#5D656B" "#5d5d5d")
   (black      "#181e26" "black"  )
   (light-grey fg-alt    "#525252")
   (grey       "#3D3D48" fg-alt   )
   (dark-grey  "#21272d" "#21212d")
   (white      "#DFDFDF" "#dfdfdf")
   (red        "#ff665c" "#ff6655")
   (orange     "#e69055" "#dd8844")
   (green      "#7bc275" "#99bb66")
   (teal       "#4db5bd" "#44b9b1")
   (yellow     "#ECBE7B"          )
   (blue       "#51afef"          )
   (dark-blue  "#1f5582"          )
   (magenta    "#C57BDB"          )
   (violet     "#a9a1e1"          )
   (cyan       "#46D9FF"          )
   (dark-cyan  "#5699AF"          )

   ;; face categories
   (highlight      blue)
   (vertical-bar   black)
   (current-line   dark-grey)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-vibrant-brighter-comments dark-cyan light-grey))
   (doc-comments   (if doom-vibrant-brighter-comments teal (doom-lighten light-grey 0.2)))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      white)
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

   (linum :foreground (if gui (doom-darken light-grey 0.3) light-grey)
          :distant-foreground nil
          :bold nil
          :height doom-vibrant-linum-height)
   (doom-linum-highlight :foreground blue
                         :distant-foreground (doom-darken white 0.3)
                         :bold nil
                         :height doom-vibrant-linum-height)

   (doom-modeline-bar :background (if modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if modeline-bright white blue) :bold bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if modeline-bright white highlight))

   (doom-mode-line
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-l)))
   (doom-mode-line-inactive
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
