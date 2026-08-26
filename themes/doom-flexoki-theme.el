;;; doom-flexoki-theme.el --- an inky, warm dark theme based on Flexoki -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Sarath Prasath K <https://github.com/prasathsarath>
;; Maintainer: Sarath Prasath K <https://github.com/prasathsarath>
;; Source: https://stephango.com/flexoki
;; Source: https://github.com/kepano/flexoki
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-flexoki-theme nil
  "Options for the `doom-flexoki' theme."
  :group 'doom-themes)

(defcustom doom-flexoki-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-flexoki-theme
  :type 'boolean)

(defcustom doom-flexoki-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-flexoki-theme
  :type 'boolean)

(defcustom doom-flexoki-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-flexoki-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-flexoki
  "An inky, warm dark theme based on Flexoki."
  :family 'doom-flexoki
  :background-mode 'dark

  ;; name        default   256           16
  ((bg         '("#100F0F" "black"       "black"        ))
   (fg         '("#CECDC3" "#D7D7D7"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1C1B1A" "#1C1C1C"     "black"        ))
   (fg-alt     '("#B7B5AC" "#AFAFAF"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#000000" "black"       "black"        ))
   (base1      '("#100F0F" "black"       "brightblack"  ))
   (base2      '("#1C1B1A" "#1C1C1C"     "brightblack"  ))
   (base3      '("#282726" "#262626"     "brightblack"  ))
   (base4      '("#343331" "#303030"     "brightblack"  ))
   (base5      '("#575653" "#4E4E4E"     "brightblack"  ))
   (base6      '("#878580" "#878787"     "brightblack"  ))
   (base7      '("#CECDC3" "#D7D7D7"     "brightblack"  ))
   (base8      '("#FFFCF0" "#FFFFFF"     "white"        ))

   (grey       base6)
   ;; Flexoki's "extended palette" recommends the -400 tints for dark
   ;; backgrounds and the -600 tints for light ones.
   (red        '("#D14D41" "#D75F5F" "red"          ))
   (orange     '("#DA702C" "#D75F00" "brightred"    ))
   (yellow     '("#D0A215" "#D7AF00" "yellow"       ))
   (green      '("#879A39" "#878700" "green"        ))
   (teal       '("#66800B" "#5F8700" "brightgreen"  ))
   (cyan       '("#3AA99F" "#00AFAF" "brightcyan"   ))
   (dark-cyan  '("#24837B" "#008787" "cyan"         ))
   (blue       '("#4385BE" "#0087AF" "brightblue"   ))
   (dark-blue  '("#205EA6" "#005FAF" "blue"         ))
   (violet     '("#8B7EC8" "#8787D7" "magenta"      ))
   (magenta    '("#CE5D97" "#D75F87" "brightmagenta"))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   base2)
   (selection      base3)
   (builtin        orange)
   (comments       (if doom-flexoki-brighter-comments blue base6))
   (doc-comments   (doom-darken comments 0.2))
   (constants      magenta)
   (functions      orange)
   (keywords       green)
   (methods        teal)
   (operators      red)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         base3)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg     fg)
   (modeline-fg-alt base6)

   (modeline-bg
    (if doom-flexoki-brighter-modeline base4 base2))
   (modeline-bg-inactive base1)

   (-modeline-pad
    (when doom-flexoki-padded-modeline
      (if (integerp doom-flexoki-padded-modeline) doom-flexoki-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
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
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground dark-cyan :weight 'normal)
   ((markdown-code-face &override) :background base2)
   ;;;; org <built-in>
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :foreground comments :background base1)
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
    :background modeline-bg-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive))))

  ;;;; Base theme variable overrides
  ())

;;; doom-flexoki-theme.el ends here
