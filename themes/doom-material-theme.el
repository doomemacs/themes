;;; doom-material-theme.el --- adapted from equinusocio's Material themes -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: December 12, 2019 (#363)
;; Author: tam5 <https://github.com/tam5>
;; Maintainer:
;; Source: https://github.com/equinusocio/vsc-material-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-material-theme nil
  "Options for the `material' theme."
  :group 'doom-themes)

(defcustom doom-material-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-material-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-material
  "A dark theme inspired by Material Theme by equinusocio"
  :family 'doom-material
  :background-mode 'dark

  ;; name        default   256       16
  ((bg         '("#263238" nil       nil))
   (bg-alt     '("#1C262B" nil       nil))
   (base0      '("#171F24" "black"   "black"))
   (base1      '("#1C262B" "#262626" "brightblack"))
   (base2      '("#222D32" "#303030" "brightblack"))
   (base3      '("#171F24" "#3a3a3a" "brightblack"))
   (base4      '("#314048" "#444444" "brightblack"))
   (base5      '("#37474F" "#585858" "brightblack"))
   (base6      '("#556369" "#626262" "brightblack"))
   (base7      '("#737E84" "#767676" "brightblack"))
   (base8      '("#9BA3A7" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#ff5370" "#ff0000" "red"))
   (orange      '("#f78c6c" "#ff5f00" "brightred"))
   (green       '("#c3e88d" "#afff00" "green"))
   (teal        '("#44b9b1" "#00d7af" "brightgreen"))
   (yellow      '("#ffcb6b" "#ffd700" "brightyellow"))
   (blue        '("#82aaff" "#5fafff" "brightblue"))
   (dark-blue   '("#7986E7" "#d7ffff" "blue"))
   (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   (violet      '("#bb80b3" "#d787af" "magenta"))
   (cyan        '("#89DDFF" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80cbc4" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   base2)
   (selection      base4)
   (builtin        blue)
   (comments       base6)
   (doc-comments   base6)
   (constants      orange)
   (functions      blue)
   (keywords       cyan)
   (methods        blue)
   (operators      cyan)
   (type           magenta)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     base2)
   (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-material-padded-modeline
      (if (integerp doom-material-padded-modeline) doom-material-padded-modeline 4))))

  ;;;; Base theme face overrides
  (;;;; emacs
   (lazy-highlight :background base4 :foreground fg :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background (doom-darken bg-alt 0.2) :foreground fg)

   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; highlight-thing highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; ivy
   (ivy-current-match :background base5)
   ;;;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)
   ;;;; lsp
   (lsp-headerline-breadcrumb-symbols-face :foreground base7)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)))

;;; doom-material-theme.el ends here
