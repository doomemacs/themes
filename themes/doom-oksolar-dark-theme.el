;;; doom-oksolar-dark-theme.el --- an OKLab variant of Solarized dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added:
;; Author: logc <https://github.com/logc>
;; Maintainer:
;; Source: https://meat.io/oksolar.json
;; Source: https://meat.io/oksolar
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-oksolar-dark-theme nil
  "Options for the `doom-oksolar-dark' theme."
  :group 'doom-themes)

(defcustom doom-oksolar-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-oksolar-dark-theme
  :type 'boolean)

(defcustom doom-oksolar-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-oksolar-dark-theme
  :type 'boolean)

(defcustom doom-oksolar-dark-brighter-text nil
  "If non-nil, default text will be brighter."
  :group 'doom-oksolar-dark-theme
  :type 'boolean)

(defcustom doom-oksolar-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-oksolar-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-oksolar-dark
  "A dark theme inspired by OKSolar."

  ;; name        default   256       16
  ((bg         '("#002D38" "#002D38" "brightblack" ))
   (fg         (if doom-oksolar-dark-brighter-text
                   '("#BBBBBB" "#BBBBBB" "brightwhite")
                 '("#98A8A8" "#98A8A8" "brightwhite")))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#093946" "#093946" "white"       ))
   (fg-alt     '("#8FAAAB" "#8FAAAB" "white"       ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#002D38" "#002D38" "black"       ))
   (base1      '("#093946" "#093946" "brightblack" ))
   ;; NOTE: base 2 never used, left as-is in Solarized
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#5B7279" "#5B7279" "brightblack" ))
   (base4      '("#657377" "#657377" "brightblack" ))
   (base5      '("#98A8A8" "#98A8A8" "brightblack" ))
   (base6      '("#8FAAAB" "#8FAAAB" "brightblack" ))
   (base7      '("#F1E9D2" "#F1E9D2" "brightblack" ))
   (base8      '("#FBF7EF" "#FBF7EF" "white" ))

   (grey       base4)
   (red        '("#F23749" "#ff6655" "red"          ))
   (orange     '("#D56500" "#dd8844" "brightred"    ))
   (green      '("#819500" "#99bb66" "green"        ))
   (teal       '("#35A69C" "#33aa99" "brightgreen"  ))
   (yellow     '("#AC8300" "#ECBE7B" "yellow"       ))
   (blue       '("#2B90D8" "#51afef" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#DD459D" "#c678dd" "magenta"      ))
   (violet     '("#7D80D1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#259D94" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-oksolar-dark-brighter-comments blue base5))
   (doc-comments   teal)
   (constants      magenta)
   (functions      blue)
   (keywords       green)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        cyan)
   (variables      violet)
   (numbers        magenta)
   (region         (doom-lighten bg-alt 0.15))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-oksolar-dark-brighter-modeline)
   (-modeline-pad
    (when doom-oksolar-dark-padded-modeline
      (if (integerp doom-oksolar-dark-padded-modeline) doom-oksolar-dark-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-alt
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-oksolar-dark-brighter-comments (doom-lighten bg 0.05) 'unspecified))
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-constant-face &override) :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected
    :inherit 'centaur-tabs-selected :foreground blue)
   (centaur-tabs-modified-marker-unselected
    :inherit 'centaur-tabs-unselected :foreground blue)
   ;;;; company
   (company-tooltip-selection     :background dark-cyan)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background blue)
   (doom-modeline-evil-emacs-state  :foreground magenta)
   (doom-modeline-evil-insert-state :foreground blue)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; helm
   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ((markdown-code-face &override)   :background bg-alt)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground comments :background base0)
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
  ;; ()
  )

;;; doom-oksolar-dark-theme.el ends here
