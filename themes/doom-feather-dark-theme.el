;;; doom-feather-dark-theme.el --- a purple-tinted theme, inspired by doom-one -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Date: November 12, 2022
;; Author: Lena SAVY-LARIGALDIE <https://gitlab.com/Plunne>
;; Maintainer: Lena SAVY-LARIGALDIE <https://gitlab.com/Plunne>
;; Source: https://github.com/Plunne/doom-feather-theme
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-feather-dark-theme nil
  "Options for the `doom-feather' theme."
  :group 'doom-themes)

(defcustom doom-feather-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-feather-dark-theme
  :type 'boolean)

(defcustom doom-feather-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-feather-dark-theme
  :type 'boolean)

(defcustom doom-feather-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-feather-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-feather-dark
  "A dark theme based on Doom One Dark."

  ;; name        default   256           16
  ((bg         '("#251D2F" "#262626"    "black"                         ))
   (fg         '("#c4b8d3" "#c6c6c6"    "brightwhite"   ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1A1521" "#1c1c1c"    "black"                         ))
   (fg-alt     '("#503F65" "#2d2d2d"    "white"                         ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#0F0C13" "#000000"    "black"                         ))
   (base1      '("#130F18" "#080808"    "brightblack"   ))
   (base2      '("#16121C" "#121212"    "brightblack"   ))
   (base3      '("#1E1826" "#1c1c1c"    "brightblack"   ))
   (base4      '("#342942" "#303030"    "brightblack"   ))
   (base5      '("#503F65" "#4e4e4e"    "brightblack"   ))
   (base6      '("#6C5689" "#666666"    "brightblack"   ))
   (base7      '("#9783b1" "#949494"    "brightblack"   ))
   (base8      '("#D3CADE" "#dadada"    "white"                         ))

   (grey       base4)
   (red        '("#ff6c6b" "#ff5f5f"    "red"                   ))
   (orange     '("#da8548" "#ff8700"    "brightred"             ))
   (green      '("#98be65" "#87af5f"    "green"                         ))
   (teal       '("#1abc9c" "#00af87"    "brightgreen"   ))
   (yellow     '("#ECBE7B" "#d7af5f"    "yellow"                ))
   (blue       '("#51afef" "#00afff"    "brightblue"    ))
   (dark-blue  '("#2257A0" "#005faf"    "blue"                  ))
   (magenta    '("#7A619A" "#875faf"    "brightmagenta"         ))
   (violet     '("#9783b1" "#8787af"    "magenta"               ))
   (cyan       '("#7dcfff" "#87d7ff"    "brightcyan"    ))
   (dark-cyan  '("#5699AF" "#5fafd7"    "cyan"                  ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-feather-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-feather-brighter-comments dark-cyan base5) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        cyan)
   (operators      teal)
   (type           teal)
   (strings        green)
   (variables      (doom-lighten cyan 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-feather-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-feather-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-feather-dark-padded-modeline
      (if (integerp doom-feather-dark-padded-modeline) doom-feather-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background (doom-lighten bg 0.00) :foreground base7 :weight 'bold)
   ((font-lock-comment-face &override)
    :background (if doom-feather-brighter-comments (doom-lighten bg 0.05) 'unspecified) :italic t)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-feather-brighter-modeline base8 highlight))
   ;;;; button (#include "strings")
   (button :foreground strings)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dashboard
   (dashboard-navigator :foreground teal)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-feather-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background base7 :distant-foreground bg-alt :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; Outlines
   (outline-1 :height 1.8 :foreground blue :weight 'bold)
   (outline-2 :height 1.2 :foreground magenta :weight 'bold)
   (outline-3 :height 1.1 :foreground violet :weight 'bold)
   (outline-4 :height 1.0 :foreground (doom-lighten blue 0.25) :weight 'bold)
   (outline-5 :height 1.0 :foreground (doom-lighten magenta 0.25) :weight 'bold)
   (outline-6 :height 1.0 :foreground (doom-lighten blue 0.5) :weight 'bold)
   (outline-7 :height 1.0 :foreground (doom-lighten magenta 0.5) :weight 'bold)
   (outline-8 :height 1.0 :foreground (doom-lighten blue 0.8) :weight 'bold)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground blue)
   (rainbow-delimiters-depth-5-face :foreground green)
   (rainbow-delimiters-depth-6-face :foreground red)
   (rainbow-delimiters-depth-7-face :foreground cyan)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground yellow)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; Treemacs
   (treemacs-root-face :foreground teal :weight 'bold :height 1.4)
   (doom-themes-treemacs-root-face :foreground teal :weight 'ultra-bold :height 1.2)
   ;;;; Tree-sitter
   (tree-sitter-hl-face:punctuation.bracket :foreground comments)
   (tree-sitter-hl-face:attribute :foreground blue)
   (tree-sitter-hl-face:function\.call :foreground blue)
   (tree-sitter-hl-face:function\.macro :foreground magenta)
   (tree-sitter-hl-face:type\.builtin :foreground teal :italic t)
   (tree-sitter-hl-face:variable\.special :foreground constants)
   (tree-sitter-hl-face:operator :foreground operators)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-feather-dark-theme.el ends here
