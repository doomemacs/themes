;;; doom-flexoki-light-theme.el --- based on Flexoki theme by Steph Ango -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Brent Monning <https://github.com/schrobingus>
;; Maintainer: Brent Monning <https://github.com/schrobingus>
;; Source: https://github.com/kepano/flexoki
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-flexoki-light-theme nil
  "Options for the `doom-flexoki-light' theme."
  :group 'doom-themes)

(defcustom doom-flexoki-light-brighter-modeline t
  "If non-nil, a brighter background and foreground
will be used to style the mode-line."
  :group 'doom-flexoki-light-theme
  :type 'boolean)

(defcustom doom-flexoki-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-flexoki-light-theme
  :type '(choice integer boolean))

(defcustom doom-flexoki-light-opaque-vertical-bar t
  "If non-nil, the vertical bar color (which applies
to window dividers) will be a unique color instead of
the background color."
  :group 'doom-flexoki-light-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-flexoki-light
    "A light theme based on the Flexoki theme."
  :family 'doom-flexoki-light
  :background-mode 'light

  ;; name        default   256           16
  ((bg         '("#FFFCF0" "white"       "black"        ))
   (fg         '("#100F0F" "black"       "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#F2F0E5" "#EEEEEE"       "black"        ))
   (fg-alt     '("#6F6E69" "#626262"       "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#FFFFFF" "white"       "black"        ))
   (base1      '("#FFFCF0" "white"       "brightblack"  ))
   (base2      '("#E6E4D9" "#E4E4E4"     "brightblack"  ))
   (base3      '("#DAD8CE" "#DADADA"     "brightblack"  ))
   (base4      '("#CECDC3" "#D7D7D7"     "brightblack"  ))
   (base5      '("#B7B5AC" "#AFAFAF"     "brightblack"  ))
   (base6      '("#6F6E69" "#626262"     "brightblack"  ))
   (base7      '("#100F0F" "#000000"     "brightblack"  ))
   (base8      '("#000000" "#000000"     "white"        ))

   (grey       base4)
   (red        '("#AF3029" "#AF0000" "red"          ))
   (orange     '("#BC5215" "#AF5F00" "brightred"    ))
   (green      '("#66800B" "#5F8700" "green"        ))
   (teal       '("#879A39" "#878700" "brightgreen"  ))
   (yellow     '("#AD8301" "#AF8700" "yellow"       ))
   (blue       '("#205EA6" "#005FAF" "brightblue"   ))
   (dark-blue  '("#4385BE" "#0087AF" "blue"         ))
   (magenta    '("#A02F6F" "#AF005F" "brightmagenta"))
   (violet     '("#5E409D" "#5F00AF" "magenta"      ))
   (cyan       '("#24837B" "#008787" "brightcyan"   ))
   (dark-cyan  '("#3AA99F" "#00AFAF" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (if doom-flexoki-light-opaque-vertical-bar base4 base1))
   (selection      base3)
   (builtin        orange)
   (comments       base6)
   (doc-comments   (doom-darken base6 0.15))
   (constants      magenta)
   (functions      orange)
   (keywords       green)
   (methods        green)
   (operators      red)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         (doom-darken bg-alt 0.1))
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    cyan)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg                  fg)
   (modeline-fg-inactive         (if doom-flexoki-light-brighter-modeline
				     base5
				   base6))
   (modeline-bg                  (if doom-flexoki-light-brighter-modeline
				     base2
				   base3))

   (-modeline-pad
    (when doom-flexoki-light-padded-modeline
      (if (integerp doom-flexoki-light-padded-modeline) doom-flexoki-light-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg :foreground modeline-fg-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-emphasis :foreground highlight)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-flexoki-light-theme.el ends here
