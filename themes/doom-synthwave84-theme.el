;;; doom-synthwave84-theme.el --- Synthwave '84 theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added:
;; Author: jbcorwin <https://github.com/jbcorwin>
;; Maintainer:
;; Source: https://github.com/robb0wen/synthwave-vscode
;;
;;; Commentary:
;; A retro-futuristic synthwave theme based strictly on robb0wen's VS Code theme.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-synthwave84-theme nil
  "Options for the `doom-synthwave84' theme."
  :group 'doom-themes)

(defcustom doom-synthwave84-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-synthwave84-theme
  :type 'boolean)

(defcustom doom-synthwave84-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-synthwave84-theme
  :type 'boolean)

(defcustom doom-synthwave84-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-synthwave84-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-synthwave84
  "A retro-futuristic synthwave theme based strictly on robb0wen's VS Code theme."

  ;; Core palette (Format: '("GUI" "256-TERM" "TERM"))
  ((bg         '("#262335" "#262335" "black"))
   (bg-alt     '("#1f212b" "#1f212b" "black"))
   (base0      '("#1c1926" "black"   "black"))
   (base1      '("#1f212b" "#1e1e1e" "brightblack"))
   (base2      '("#2a2139" "#2e2e2e" "brightblack"))
   (base3      '("#34294f" "#262626" "brightblack"))
   (base4      '("#495495" "#3f3f3f" "brightblack"))
   (base5      '("#614d85" "#525252" "brightblack"))
   (base6      '("#848bbd" "#6b6b6b" "brightblack"))
   (base7      '("#a2a9d8" "#979797" "brightblack"))
   (base8      '("#ffffff" "#dfdfdf" "white"))
   (fg         '("#ffffff" "#bfbfbf" "brightwhite"))
   (fg-alt     '("#848bbd" "#2d2d2d" "white"))

   ;; Semantic colors
   (grey       base6)
   (red        '("#fe4450" "#ff6655" "red"))
   (orange     '("#ff8b39" "#dd8844" "brightred"))
   (green      '("#72f1b8" "#99bb66" "green"))
   (teal       '("#36f9f6" "#44b9b1" "brightgreen"))
   (yellow     '("#fede5d" "#ecbe7b" "yellow"))
   (blue       '("#03edf9" "#51afef" "brightblue"))
   (dark-blue  '("#211b43" "#2257a0" "blue"))
   (magenta    '("#f92aad" "#c678dd" "brightmagenta"))
   (violet     '("#ff7edb" "#a9a1e1" "magenta"))
   (cyan       '("#36f9f6" "#46D9FF" "brightcyan"))
   (dark-cyan  '("#2babb0" "#5699AF" "cyan"))

   ;; Face categories
   (highlight      violet)
   (vertical-bar   base2)
   (selection      base4)
   (builtin        magenta)
   (comments       (if doom-synthwave84-brighter-comments dark-cyan base6))
   (doc-comments   (if doom-synthwave84-brighter-comments (doom-lighten dark-cyan 0.25) base6))
   (constants      '("#f97e72" "#ff99aa" "magenta"))
   (functions      teal)
   (keywords       magenta)
   (methods        teal)
   (operators      cyan)
   (type           violet)
   (strings        orange)
   (variables      fg)
   (numbers        constants)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; Modeline
   (modeline-bg     (if doom-synthwave84-brighter-modeline (doom-darken magenta 0.45) base2))
   (modeline-bg-alt (if doom-synthwave84-brighter-modeline (doom-darken magenta 0.6) base1))
   (modeline-fg     base8)
   (modeline-fg-alt base6)

   (modeline-pad    (when doom-synthwave84-padded-modeline
                      (if (integerp doom-synthwave84-padded-modeline) doom-synthwave84-padded-modeline 4))))

  ;; Custom face overrides
  ((lazy-highlight :background base4 :foreground fg :weight 'bold)
   (region :background region)
   (font-lock-comment-face :foreground comments :slant 'italic)
   (font-lock-doc-face :foreground doc-comments :slant 'italic)
   (font-lock-string-face :foreground strings)
   (font-lock-keyword-face :foreground keywords :weight 'bold)
   (font-lock-function-name-face :foreground functions)
   (font-lock-variable-name-face :foreground variables)
   (font-lock-constant-face :foreground constants)
   (font-lock-type-face :foreground type :weight 'bold)
   (mode-line :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive :background modeline-bg-alt :foreground modeline-fg-alt)))

;;; doom-synthwave84-theme.el ends here
