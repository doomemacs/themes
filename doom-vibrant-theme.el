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

(defcustom doom-vibrant-linum-height 1.0
  "The :height to render line numbers with."
  :group 'doom-vibrant-theme
  :type 'boolean)

;;
(def-doom-theme doom-vibrant
  "A more vibrant (and dark) theme, based off of DOOM One's classic look."

  ;; name      gui       term (256)
  ((bg         "#282c34" nil      )
   (bg-alt     "#22252c" nil      )
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
   (modeline-bg     (if doom-vibrant-brighter-modeline bg bg-alt)                        "brightblack")
   (modeline-bg-alt (if doom-vibrant-brighter-modeline bg-alt (doom-darken bg-alt 0.25)) "black")
   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (doom-blend blue grey 0.08)))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (linum :foreground (if gui (doom-darken light-grey 0.3) light-grey)
          :background bg-alt
          :bold nil
          :height doom-vibrant-linum-height)
   (doom-nlinum-highlight :foreground blue
                          :distant-foreground nil
                          :bold nil
                          :height doom-vibrant-linum-height)

   (mode-line          :background modeline-bg     :foreground modeline-fg)
   (mode-line-inactive :background modeline-bg-alt :foreground modeline-fg-alt)
   (doom-modeline-buffer-path :foreground blue :bold bold)

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
