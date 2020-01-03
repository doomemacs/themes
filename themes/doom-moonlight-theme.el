;;; doom-moonlight-theme.el --- inspired by VS code's Moonlight -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-moonlight-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-moonlight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-moonlight-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-moonlight
  "A dark theme inspired by VS code's Moonlight"

  ;; name        default   256       16
  ((bg         '("#212337" nil       nil))
   (bg-alt     '("#1e2132" nil       nil))
   (base0      '("#161a2a" "black"   "black"))
   (base1      '("#191a2a" "#262626" "brightblack"))
   (base2      '("#1f2133" "#303030" "brightblack"))
   (base3      '("#292e46" "#3a3a3a" "brightblack"))
   (base4      '("#2c314b" "#444444" "brightblack"))
   (base5      '("#383e5c" "#585858" "brightblack"))
   (base6      '("#7e8eda" "#626262" "brightblack"))
   (base7      '("#9da5d7" "#767676" "brightblack"))
   (base8      '("#afbeee" "#a8a8a8" "white"))
   (fg         '("#e4f3fa" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#dcebff" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#ff5370" "#ff5f5f" "red"))
   (orange      '("#ff9668" "#ff875f" "brightred"))
   (green       '("#c7f59b" "#d7ff87" "green"))
   (teal        '("#7af8ca" "#87ffd7" "brightgreen"))
   (teal2       '("#7fdaff" "#87ffd7" "brightgreen"))
   (yellow      '("#ffbd76" "#ffaf87" "brightyellow"))
   (blue        '("#70b0ff" "#5fafff" "brightblue"))
   (dark-blue   '("#74a0f1" "#5fafff" "brightblue"))
   (magenta     '("#baacff" "#afafff" "brightmagenta"))
   (violet      '("#f989d3" "#ff87d7" "magenta"))
   (pink        '("#f3c1ff" "#ff87d7" "magenta"))
   (cyan        '("#34d3fb" "#5fd7ff" "brightcyan"))
   (cyan2       '("#89ddff" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#a6eefb" "#afffff" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   base0)
   (selection      dark-blue)
   (builtin        blue)
   (comments       base6)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        blue)
   (operators      cyan2)
   (type           magenta)
   (strings        teal)
   (variables      teal2)
   (numbers        orange)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    dark-blue)
   (vc-added       teal)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (doom-darken base2 0.1))
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
   ((outline-1 &override) :foreground cyan)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground pink)
   ((outline-5 &override) :foreground magenta)
   ((outline-6 &override) :foreground red)
   ((outline-7 &override) :foreground violet)
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

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
   (js2-function-param         :foreground pink)
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
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)

   ;; tooltip
   (tooltip              :background (doom-darken bg-alt 0.2) :foreground fg)))

(provide 'doom-moonlight-theme)

;;; doom-moonlight-theme.el ends here
