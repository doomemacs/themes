;;; doom-iceberg-light-theme.el --- inspired by Iceberg-Light -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-iceberg-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-iceberg-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-iceberg-light-theme
  :type 'boolean)

(defcustom doom-iceberg-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-iceberg-light-theme
  :type 'boolean)

(defcustom doom-iceberg-light-comment-bg doom-iceberg-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-iceberg-light-theme
  :type 'boolean)

(defcustom doom-iceberg-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-iceberg-light-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-iceberg-light-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-iceberg-light-theme
    :type 'symbol))

;;
(def-doom-theme doom-iceberg-light
                "A light theme inspired by Iceberg-Light with enhanced contrast."

                ;; name        default   256       16
                ((bg         '("#E8E9EC" "#E8E9EC" nil))
                 (bg-alt     '("#DDE1E6" "#DDE1E6" nil))
                 (base0      '("#dcdfe7" "#dcdfe7" "white"))
                 (base1      '("#c6c8d1" "#c6c8d1" "brightblack"))
                 (base2      '("#A5C9D4" "#A5C9D4" "brightblack"))
                 (base3      '("#F8FCFD" "#F8FCFD" "brightblack"))
                 (base4      '("#6B7A8F" "#6B7A8F" "brightblack"))
                 (base5      '("#4C566A" "#4C566A" "brightblack"))
                 (base6      '("#9099AB" "#9099AB" "brightblack"))
                 (base7      '("#33374c" "#33374c" "brightblack"))
                 (base8      '("#1B2B3C" "#1B2B3C" "black"))

                 (fg         '("#161821" "#161821" "black"))
                 (fg-alt     '("#6B7089" "#6B7089" "brightwhite"))

                 ;; Enhanced colors for better contrast on light background
                 (grey       base4)
                 (red        '("#CC3850" "#CC3850" "red"))           ; Darker, more saturated red
                 (orange     '("#CC7833" "#CC7833" "brightred"))     ; Richer orange
                 (green      '("#5CBE00" "#5CBE00" "green"))         ; More vibrant green
                 (teal       '("#289999" "#289999" "brightgreen"))   ; Deeper teal
                 (yellow     '("#BE9117" "#BE9117" "yellow"))        ; Warmer yellow
                 (blue       '("#4271AE" "#4271AE" "brightblue"))    ; Richer blue
                 (dark-blue  '("#0A4CBE" "#0A4CBE" "blue"))          ; Deeper blue
                 (magenta    '("#8959A8" "#8959A8" "magenta"))       ; Richer magenta
                 (violet     '("#7A63EE" "#7A63EE" "brightmagenta")) ; More saturated violet
                 (cyan       '("#3E999F" "#3E999F" "brightcyan"))    ; Deeper cyan
                 (dark-cyan  '("#2C7088" "#2C7088" "cyan"))          ; Darker cyan for contrast

                 ;; face categories -- required for all themes
                 (highlight      violet)
                 (vertical-bar   (doom-lighten base2 0.1))
                 (selection      dark-blue)
                 (builtin        magenta)
                 (comments       (if doom-iceberg-light-brighter-comments dark-cyan base5))
                 (doc-comments   (doom-lighten (if doom-iceberg-light-brighter-comments dark-cyan base5) 0.25))
                 (constants      violet)
                 (functions      blue)
                 (keywords       magenta)
                 (methods        cyan)
                 (operators      dark-blue)
                 (type           teal)
                 (strings        green)
                 (variables      (doom-darken magenta 0.1))
                 (numbers        orange)
                 (region         (doom-lighten base2 0.2))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; Custom categories
                 (hidden     `(,(car bg) "black" "black"))
                 (-modeline-bright doom-iceberg-light-brighter-modeline)
                 (-modeline-pad
                  (when doom-iceberg-light-padded-modeline
                    (if (integerp doom-iceberg-light-padded-modeline) doom-iceberg-light-padded-modeline 4)))

                 (modeline-fg     nil)
                 (modeline-fg-alt (doom-blend violet base4 0.2))

                 (modeline-bg
    (if -modeline-bright
        (doom-darken bg-alt 0.15)
      (doom-darken bg-alt 0.1)))
                 (modeline-bg-l
                  (if -modeline-bright
                      (doom-darken bg-alt 0.1)
                    (doom-darken bg-alt 0.05)))
                 (modeline-bg-inactive   (doom-darken bg-alt 0.05))
                 (modeline-bg-inactive-l (doom-darken bg-alt 0.025)))

                ;; --- extra faces ------------------------
                (((region &override) :foreground nil :background base3 :distant-foreground nil)

                 ((line-number &override) :foreground base4)
                 ((line-number-current-line &override) :foreground violet :weight 'bold)
                 ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
                 ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)

                 (font-lock-comment-face
    :foreground comments
    :background (if doom-iceberg-light-comment-bg (doom-darken bg 0.05)))
                 (font-lock-doc-face
                  :inherit 'font-lock-comment-face
                  :foreground doc-comments
                  :slant 'italic)

                 ;; Enhanced syntax highlighting
                 (font-lock-keyword-face :foreground keywords :weight 'bold)
                 (font-lock-function-name-face :foreground functions :weight 'semi-bold)
                 (font-lock-variable-name-face :foreground variables)
                 (font-lock-constant-face :foreground constants :weight 'bold)
                 (font-lock-string-face :foreground strings :slant 'italic)
                 (font-lock-type-face :foreground type :weight 'semi-bold)
                 (font-lock-builtin-face :foreground builtin :weight 'semi-bold)

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

                 ;; --- major-mode faces -------------------
                 ;; css-mode / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)

                 ;; markdown-mode
                 (markdown-markup-face :foreground base5)
                 (markdown-header-face :inherit 'bold :foreground violet)
                 ((markdown-code-face &override) :background (doom-lighten base2 0.05))

                 ;; org-mode
                 (org-hide :foreground hidden)
                 (solaire-org-hide-face :foreground hidden))

                ;; --- variables --------------------------
                ;; ()
                )

;;; doom-iceberg-light-theme.el ends here
