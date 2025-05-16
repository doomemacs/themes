;;; doom-opera-theme.el --- an original light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 16, 2018 (#189)
;; Author: jwintz <https://github.com/jwintz>
;; Maintainer:
;; Source: original
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-opera-theme nil
  "Options for the `doom-opera' theme."
  :group 'doom-themes)

(defcustom doom-opera-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-opera-theme
  :type 'boolean)

(defcustom doom-opera-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-opera-theme
  :type 'boolean)

(defcustom doom-opera-comment-bg doom-opera-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-opera-theme
  :type 'boolean)

(defcustom doom-opera-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-opera-theme
  :type '(choice integer boolean))

(defcustom doom-opera-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-opera-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-opera
  "A dark Opera theme."
  :family 'doom-opera
  :background-mode 'dark

  ;; name        default   256       16
  ((bg         '("#323334" nil       nil            ))
   (bg-alt     '("#222224" nil       nil            ))
   (base0      '("#000000" "black"   "black"        ))
   (base1      '("#1e1e1e" "#1e1e1e" "brightblack"  ))
   (base2      '("#2e2e2e" "#2e2e2e" "brightblack"  ))
   (base3      '("#262626" "#262626" "brightblack"  ))
   (base4      '("#3f3f3f" "#3f3f3f" "brightblack"  ))
   (base5      '("#525252" "#525252" "brightblack"  ))
   (base6      '("#6b6b6b" "#6b6b6b" "brightblack"  ))
   (base7      '("#979797" "#979797" "brightblack"  ))
   (base8      '("#dfdfdf" "#dfdfdf" "white"        ))
   (fg         '("#eceff4" "#dfdfdf" "white"        ))
   (fg-alt     '("#727269" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#C16069" "#ff6655" "red"          ))
   (orange     '("#D2876D" "#dd8844" "brightred"    ))
   (green      '("#A2BF8A" "#99bb66" "green"        ))
   (teal       '("#8EBCBB" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECCC87" "#ECBE7B" "yellow"       ))
   (blue       '("#80A0C2" "#51afef" "brightblue"   ))
   (dark-blue  '("#5C748E" "#2257A0" "blue"         ))
   (magenta    '("#B58DAE" "#c678dd" "magenta"      ))
   (violet     '("#5D80AE" "#a9a1e1" "brightmagenta"))
   (cyan       '("#86C0D1" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#507681" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       (if doom-opera-brighter-comments dark-cyan (doom-lighten base5 0.2)))
   (doc-comments   (doom-lighten (if doom-opera-brighter-comments dark-cyan base5) 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.5))
   (numbers        magenta)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-opera-brighter-modeline)
   (-modeline-pad
    (when doom-opera-padded-modeline
      (if (integerp doom-opera-padded-modeline) doom-opera-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg-alt 0.1)))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-opera-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-opera-theme.el ends here
