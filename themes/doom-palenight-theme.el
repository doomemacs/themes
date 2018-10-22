;;; doom-palenight-theme.el --- description -*- lexical-binding: t; -*-
(require 'doom-themes)

;;
(defgroup doom-palenight-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-palenight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-palenight-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-palenight
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#292D3E" nil       nil))
   (bg-alt     '("#21242b" nil       nil))
   (base0      '("#202431" "black"   "black"))
   (base1      '("#232635" "black"   "black"))
   (base2      '("#292D3E" "#292D3E" "brightblack"))
   (base3      '("#32374D" "#2e2e2e" "brightblack"))
   (base4      '("#3C435E" "#262626" "brightblack"))
   (base4      '("#444267" "#3f3f3f" "brightblack"))
   (base5      '("#676E95" "#676E95" "brightblack"))
   (base6      '("#697098" "#6b6b6b" "brightblack"))
   (base7      '("#6c739a" "#979797" "brightblack"))
   (base8      '("#eeffff" "#DDDDDD" "white"))
   (fg         '("#bfc7d5" "#bfbfbf" "brightwhite"))
   (fg-alt     '("#d9f5dd" "#2d2d2d" "white"))

   (grey        base4)
   (red         '("#ff869a" "#ff6655" "red"))
   (orange      '("#F78C6C" "#dd8844" "brightred"))
   (green       '("#C3E88D" "#99bb66" "green"))
   (teal        '("#ffcb6b" "#44b9b1" "brightgreen"))
   (yellow      '("#FFEB95" "#ECBE7B" "brightyellow"))
   (dark-yellow '("#ffcb6b" "#ffd700" "yellow"))
   (blue        '("#82AAFF" "#51afef" "brightblue"))
   (dark-blue   '("#697098" "#727280" "blue"))
   (magenta     '("#c792ea" "#c678dd" "brightmagenta"))
   (violet      '("#7986E7" "#a9a1e1" "magenta"))
   (cyan        '("#78ccf0" "#46D9FF" "brightcyan"))
   (dark-cyan   '("#80CBC4" "#8FA1B3" "cyan"))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base1 0.1))
   (builtin        blue)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      blue)
   (functions      blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      cyan)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          "#ff5572")
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-palenight-padded-modeline
      (if (integerp doom-palenight-padded-modeline) doom-palenight-padded-modeline 4))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :slant 'italic
    :foreground comments)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :slant 'italic
    :foreground doc-comments)

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

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)


   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; js2-mode
   (js2-function-param :foreground violet)
   (js2-jsdoc-tag :foreground magenta)
   (js2-jsdoc-type :foreground base8)
   (js2-jsdoc-value :foreground cyan)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)))


  ;; --- extra variables ---------------------
  ;; ()


;;; doom-palenight-theme.el ends here
