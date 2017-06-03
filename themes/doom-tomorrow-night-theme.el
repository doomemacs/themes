;;; doom-tomorrow-night-theme.el
(require 'doom-themes)

(defgroup doom-tomorrow-night-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-tomorrow-night-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-tomorrow-night-theme
  :type '(or integer boolean))

(def-doom-theme doom-tomorrow-night
  "A light theme inspired by Atom One Light."

  ;; name      gui
  ((bg         "#2c2f32")
   (bg-alt     "#25282b")
   (fg         "#c5c8c6")
   (fg-alt     (doom-darken fg 0.6))
   (black      "#0d0f11")
   (light-grey "#969896")
   (grey       (doom-darken light-grey 0.4))
   (dark-grey  (doom-darken grey 0.7))
   (white      "#ffffff")
   (red        "#cc6666")
   (orange     "#de935f")
   (yellow     "#f0c674")
   (green      "#b5bd68")
   (blue       "#81a2be")
   (dark-blue  "#41728e")
   (teal       blue)
   (magenta    (doom-lighten "#b294bb" 0.3)) ; FIXME
   (violet     "#b294bb")
   (cyan       "#8abeb7")
   (dark-cyan  (doom-darken cyan 0.4))

   ;; face categories
   (highlight      dark-blue)
   (vertical-bar   (doom-darken dark-grey 0.2))
   (current-line   (doom-darken bg-alt 0.05))
   (selection      (doom-lighten bg 0.1))
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.1))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)


   ;; custom categories
   (modeline-bg     (doom-darken bg-alt 0.3))
   (modeline-bg-alt (doom-darken bg 0.2))
   (modeline-fg     blue)
   (modeline-fg-alt grey)
   (modeline-pad
    (when doom-tomorrow-night-padded-modeline
      (if (integerp doom-tomorrow-night-padded-modeline)
          doom-tomorrow-night-padded-modeline
        4))))

  ;; --- faces ------------------------------
  ((doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if modeline-pad `(:line-width ,modeline-pad :color ,modeline-bg-alt))))

  ;; --- variables --------------------------
  ;; ()
  )

(provide 'doom-tomorrow-night-theme)
;;; doom-tomorrow-night-theme.el ends here
