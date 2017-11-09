;;; doom-nova-theme.el --- inspired by Trevord Miller's Nova
(require 'doom-themes)

(defgroup doom-nova-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-nova-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-nova-theme
  :type '(or integer boolean))

(def-doom-theme doom-nova
  "A light theme inspired by Trevord Miller's Nova. See
<https://trevordmiller.com/projects/nova>."

  ;; name      gui
  ((bg         '("#3c4c55" nil       nil))
   (bg-alt     '("#44545d" "#445566" "black"))

   ;; FIXME Tweak these
   (base0      '("#0d0f11" "#0d0f11" "black"      )) ; FIXME black
   (base1      '("#1e272c" "#1b1b1b"              ))
   (base2      '("#212122" "#1e1e1e"              )) ;
   (base3      '("#292a2b" "#292929" "brightblack")) ;
   (base4      '("#3c4c55" "#3f3f3f" "brightblack")) ;
   (base5      '("#556873" "#525252" "brightblack"))
   (base6      '("#6A7D89" "#6b6b6b" "brightblack"))
   (base7      '("#899BA6" "#878797" "brightblack"))
   (base8      '("#e6eef3" "#efefef" "brightwhite")) ; FIXME white
   (fg         '("#c5c8c6" "#c5c6c6" "white"      )) ;; TODO set correct color
   (fg-alt     (doom-darken fg 0.6)) ;; TODO set correct color

   (light-grey "#E6EEF3")
   (grey       base7)
   (dark-grey  base3)

   (red        "#DF8C8C")
   (orange     "#F2C38F")
   (yellow     "#DADA93")
   (green      "#A8CE93")
   (blue       "#83AFE5")
   (dark-blue  (doom-darken blue 0.7))
   (teal       blue)
   (magenta    (doom-lighten "#b294bb" 0.3)) ; FIXME TODO set correct color
   (violet     "#9A93E1")
   (cyan       "#7FC1CA")
   (dark-cyan  (doom-darken cyan 0.4))

   ;; face categories
   (highlight      cyan)
   (vertical-bar   (doom-lighten bg-alt 0.1))
   (selection      (doom-lighten highlight 0.6))
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.1))
   (constants      highlight)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        cyan)
   (variables      red)
   (numbers        highlight)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    violet)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (current-line    (doom-lighten bg-alt 0.04))
   (modeline-bg     bg-alt)
   (modeline-bg-alt (doom-lighten bg 0.035))
   (modeline-fg     blue)
   (modeline-fg-alt (doom-lighten bg-alt 0.4))

   (-modeline-pad
    (when doom-nova-padded-modeline
      (if (integerp doom-nova-padded-modeline)
          doom-nova-padded-modeline
        4))))

  ;; --- faces ------------------------------
  ((doom-modeline-buffer-path       :foreground violet :bold nil)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   (doom-modeline-bar :inherit 'mode-line-highlight)

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
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (solaire-mode-line-face
    :background (doom-darken modeline-bg 0.1) :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :background (doom-lighten modeline-bg 0.05) :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-hl-line-face :inherit 'hl-line :background current-line)

   ;; linum
   (line-number :foreground grey :distant-foreground nil :bold nil :underline nil)
   (line-number-current-line :background current-line :foreground highlight :distant-foreground nil :bold nil :underline nil)

   ;; helm
   (helm-selection :foreground dark-grey :background selection)
   (helm-match     :foreground highlight)

   ;; company
   (company-tooltip-selection  :background selection :foreground dark-grey)

   ;; org-mode
   (org-level-1
    :foreground blue :background (doom-darken bg 0.025)
    :bold bold :height 1.2))

  ;; --- variables --------------------------
  ;; ()
  )

(provide 'doom-nova-theme)
;;; doom-nova-theme.el ends here
