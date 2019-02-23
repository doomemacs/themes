;;; doom-moonlight-theme.el --- inspired by vscode moonlight
(require 'doom-themes)

;;
(defgroup doom-moonlight-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-moonlight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-moonlight-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-moonlight
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#1f2133" nil       nil))
   (bg-alt     '("#1e2132" nil       nil))
   (base0      '("#161a2a" "black"   "black"))
   (base1      '("#191a2a" "#262626" "brightblack"))
   (base2      '("#212337" "#303030" "brightblack"))
   (base3      '("#292e46" "#3a3a3a" "brightblack"))
   (base4      '("#2c314b" "#444444" "brightblack"))
   (base5      '("#383e5c" "#585858" "brightblack"))
   (base6      '("#7e8eda" "#626262" "brightblack"))
   (base7      '("#9da5d7" "#767676" "brightblack"))
   (base8      '("#aab3e5" "#a8a8a8" "white"))
   (fg         '("#e4f3fa" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#dcebff" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#ff757f" "#ff0000" "red"))
   (orange      '("#ff9668" "#ff5f00" "brightred"))
   (green       '("#c7f59b" "#afff00" "green"))
   (teal        '("#7af8ca" "#00d7af" "brightgreen"))
   (yellow      '("#ffbd76" "#ffd700" "brightyellow"))
   (blue        '("#70b0ff" "#5fafff" "brightblue"))
   (dark-blue   '("#a7c6e8" "#d7ffff" "blue"))
   (magenta     '("#baacff" "#d787d7" "brightmagenta"))
   (violet      '("#f989d3" "#d787af" "magenta"))
   (cyan        '("#34d3fb" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#a6eefb" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   base0)
   (selection      "#74a0f1")
   (builtin        blue)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      "#f3c1ff")
   (functions      blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      "#7fdaff")
   (type           yellow)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         base4)
   (error          "#c53b53")
   (warning        "#ad7c43")
   (success        green)
   (vc-modified    blue)
   (vc-added       teal)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-moonlight-padded-modeline
      (if (integerp doom-moonlight-padded-modeline) doom-moonlight-padded-modeline 4))))

  ;; --- base faces ------------------------
  (((lazy-highlight &override) :background base4 :foreground fg :distant-foreground fg :bold bold)
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (fringe :background base2)

   ((line-number &override) :foreground base5 :background base2)
   ((line-number-current-line &override) :foreground base7 :background bg-alt)

   (font-lock-keyword-face
    :slant 'italic
    :weight 'medium
    :foreground keywords)

   (font-lock-comment-face
    :slant 'italic
    :weight 'medium
    :foreground comments)

   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :slant 'italic
    :weight 'medium
    :foreground doc-comments)

   ;; --- major-mode faces ------------------------
   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; org-mode
   (org-block                    :background base2)
   (org-block-background         :background base2)
   (org-block-begin-line         :foreground comments :background base2)
   (org-level-1 :foreground cyan)
   (org-level-2 :foreground green)
   (org-level-3 :foreground "#ffdf9b")
   (org-level-4 :foreground "#f0c4ff")
   (org-level-5 :foreground magenta)
   (org-level-6 :foreground red)
   (org-level-7 :foreground violet)

   ;; --- plugin faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)


   ;; markdown-mode
   (markdown-header-face           :inherit 'bold :foreground blue)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground magenta :inherit 'italic)
   (markdown-list-face             :foreground red)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground dark-cyan)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground "#f3c1ff")
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;; rjsx-mode
   (rjsx-tag :foreground violet)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)))

(provide 'doom-moonlight-theme)



;;; doom-moonlight-theme.el ends here
