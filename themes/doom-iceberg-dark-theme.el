;;; doom-iceberg-dark-theme.el --- inspired by Iceberg-Dark with improvements -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-iceberg-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-iceberg-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-iceberg-dark-theme
  :type 'boolean)

(defcustom doom-iceberg-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-iceberg-dark-theme
  :type 'boolean)

(defcustom doom-iceberg-dark-comment-bg doom-iceberg-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-iceberg-dark-theme
  :type 'boolean)

(defcustom doom-iceberg-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-iceberg-dark-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-iceberg-dark-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-iceberg-dark-theme
    :type 'symbol))

;;
(def-doom-theme doom-iceberg-dark
                "A dark theme inspired by Iceberg-Dark with improvements."

                ;; name        default   256       16
                ((bg-alt     '("#161821" nil       nil))
                 (bg         '("#0B0E14" nil       nil))  ; Darker background
                 (base0      '("#1B202A" "#191C25" "black"))
                 (base1      '("#242832" "#242832" "brightblack"))
                 (base2      '("#2C333F" "#2C333F" "brightblack"))
                 (base3      '("#161821" "#161821" "brightblack"))
                 (base4      '("#182031" "#182031" "brightblack"))
                 (base5      '("#4C566A" "#4C566A" "brightblack"))
                 (base6      '("#9099AB" "#9099AB" "brightblack"))
                 (base7      '("#D8DEE9" "#D8DEE9" "brightblack"))
                 (base8      '("#F0F4FC" "#F0F4FC" "white"))
                 (fg         '("#d2d4de" "#d2d4de" "white"))
                 (fg-alt     '("#d2d4de" "#d2d4de" "brightwhite"))

                 (grey       base4)
                 (red        '("#e27878" "#e27878" "red"))
                 (orange     '("#e2a478" "#e2a478" "brightred"))
                 (green      '("#b4be82" "#b4be82" "green"))
                 (lightblue  '("#77d8d3" "#77d8d3" "lightblue"))
                 (teal       '("#67B7DD" "#67B7DD" "brightgreen"))
                 (yellow     '("#e4c787" "#e4c787" "yellow"))
                 (blue       '("#84a0c6" "#84a0c6" "brightblue"))
                 (dark-blue  '("#5E81AC" "#5E81AC" "blue"))
                 (magenta    '("#a093c7" "#a093c7" "magenta"))
                 (violet     '("#9A7ECC" "#9A7ECC" "brightmagenta"))
                 (cyan       '("#88c0d0" "#88c0d0" "brightcyan"))
                 (dark-cyan  '("#507681" "#507681" "darkcyan"))

                 ;; face categories -- required for all themes
                 (highlight      blue)
                 (vertical-bar   (doom-darken base1 0.2))
                 (selection      dark-blue)
                 (builtin        cyan)
                 (comments       (if doom-iceberg-dark-brighter-comments dark-cyan (doom-lighten base5 0.2)))
                 (doc-comments   (doom-lighten (if doom-iceberg-dark-brighter-comments dark-cyan base5) 0.25))
                 (constants      blue)
                 (functions      teal)
                 (keywords       blue)
                 (methods        cyan)
                 (operators      blue)
                 (type           teal)
                 (strings        (doom-lighten green 0.05))
                 (variables      base7)
                 (numbers        magenta)
                 (region         (pcase doom-iceberg-dark-region-highlight
                                   (`frost teal)
                                   (`snowstorm base7)
                                   (_ base4)))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; custom categories
                 (hidden     `(,(car bg) "black" "black"))
                 (-modeline-bright doom-iceberg-dark-brighter-modeline)
                 (-modeline-pad
                  (when doom-iceberg-dark-padded-modeline
                    (if (integerp doom-iceberg-dark-padded-modeline) doom-iceberg-dark-padded-modeline 4)))

                 (region-fg
                  (when (memq doom-iceberg-dark-region-highlight '(frost snowstorm))
                    base0))

                 (modeline-fg     nil)
                 (modeline-fg-alt base6)

                 (modeline-bg
                  (if -modeline-bright
                      (doom-blend bg base5 0.2)
                    base1))
                 (modeline-bg-l
                  (if -modeline-bright
                      (doom-blend bg base5 0.2)
                    `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
                 (modeline-bg-inactive   (doom-darken bg 0.1))
                 (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))

                ;; --- extra faces ------------------------
                (((region &override) :foreground region-fg)

                 ((line-number &override) :foreground (doom-lighten 'base5 0.2))
                 ((line-number &override) :background (doom-lighten 'bg 0.05))
                 ((line-number-current-line &override) :foreground base7)
                 ((line-number-current-line &override) :background bg-alt)
                 ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
                 ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
                 ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
                 ((vimish-fold-fringe &override)  :foreground teal)

                 (font-lock-comment-face
                  :foreground comments
                  :background (if doom-iceberg-dark-comment-bg (doom-lighten bg 0.05)))
                 (font-lock-doc-face
                  :inherit 'font-lock-comment-face
                  :foreground doc-comments)

                 (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis
                  :foreground (if -modeline-bright base8 highlight))

                 (doom-modeline-project-root-dir :foreground base6)
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-l
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-l
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

                 ;; ediff
                 (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
                 (ediff-current-diff-A :background (doom-darken base0 0.25))

                 ;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

                 ;; highlight-symbol
                 (highlight-symbol-face :background (doom-lighten base4 0.1) :distant-foreground fg-alt)

                 ;; highlight-thing
                 (highlight-thing :background (doom-lighten base4 0.1) :distant-foreground fg-alt)

                 ;; ivy
                 ((ivy-current-match &override) :foreground region-fg :weight 'semi-bold)

                 ;; --- major-mode faces -------------------
                 ;; css-mode / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)

                 ;; markdown-mode
                 (markdown-markup-face :foreground base5)
                 (markdown-header-face :inherit 'bold :foreground red)
                 ((markdown-code-face &override) :background (doom-lighten base3 0.05))

                 ;; org-mode
                 (org-hide :foreground hidden)
                 (solaire-org-hide-face :foreground hidden))

                ;; --- extra variables ---------------------
                ())

;;; doom-iceberg-dark-theme.el ends here
