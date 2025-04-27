;;; doom-solarized-dark-high-contrast-theme.el --- a high-contrast variant of Solarized Dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 29, 2021 (#558)
;; Author: jmorag <https://github.com/jmorag>
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-solarized-dark-high-contrast-theme nil
  "Options for the `doom-solarized-dark-high-contrast' theme."
  :group 'doom-themes)

(defcustom doom-solarized-dark-high-contrast-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-dark-high-contrast-theme
  :type 'boolean)

(defcustom doom-solarized-dark-high-contrast-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-dark-high-contrast-theme
  :type 'boolean)

(defcustom doom-solarized-dark-high-contrast-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-solarized-dark-high-contrast-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-solarized-dark-high-contrast
  "A dark theme inspired by VS Code Solarized Dark"

  ;; name        default   256       16
  ((bg         '("#002732" "#002732" "black"      ))
   (fg         '("#8d9fa1" "#8d9fa1" "brightwhite"))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#00212B" "#00212B" "black"       ))
   (fg-alt     '("#60767e" "#60767e" "white"       ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#01323d" "#01323d" "black"       ))
   (base1      '("#03282F" "#03282F" "brightblack" ))
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#13383C" "#13383C" "brightblack" ))
   (base4      '("#56697A" "#56697A" "brightblack" ))
   (base5      '("#62787f" "#62787f" "brightblack" ))
   (base6      '("#96A7A9" "#96A7A9" "brightblack" ))
   (base7      '("#788484" "#788484" "brightblack" ))
   (base8      '("#626C6C" "#626C6C" "white"       ))

   (grey       base4)
   (red        '("#ec423a" "#ec423a" "red"          ))
   (orange     '("#db5823" "#db5823" "brightred"    ))
   (green      '("#93a61a" "#93a61a" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#c49619" "#c49619" "yellow"       ))
   (blue       '("#3c98e0" "#3c98e0" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#e2468f" "#e2468f" "magenta"      ))
   (violet     '("#7a7ed2" "#7a7ed2" "brightmagenta"))
   (cyan       '("#3cafa5" "#3cafa5" "brightcyan"   ))
   (dark-cyan  '("#03373f" "#03373f" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       base5)
   (doc-comments   teal)
   (constants      blue)
   (functions      blue)
   (keywords       green)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        green)
   (variables      fg)
   (numbers        violet)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       "#119e44")
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-solarized-dark-high-contrast-brighter-modeline)
   (-modeline-pad
    (when doom-solarized-dark-high-contrast-padded-modeline
      (if (integerp doom-solarized-dark-high-contrast-padded-modeline) doom-solarized-dark-high-contrast-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-alt
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive     (doom-darken bg 0.1))
   (modeline-bg-inactive-alt `(,(car bg) ,@(cdr base1))))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :slant 'italic
    :background (if doom-solarized-dark-high-contrast-brighter-comments
                    (doom-lighten bg 0.05)
                  'unspecified))
   ((font-lock-keyword-face &override)  :weight 'bold)
   ((font-lock-constant-face &override) :weight 'bold)
   ((font-lock-type-face &override)     :slant 'italic)
   ((font-lock-builtin-face &override)  :slant 'italic)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (tooltip              :background bg-alt :foreground fg)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
                                          :foreground blue)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
                                            :foreground blue)
   ;;;; company
   (company-tooltip-selection :background dark-cyan)
   ;;;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background blue)
   (doom-modeline-evil-emacs-state  :foreground magenta)
   (doom-modeline-evil-insert-state :foreground blue)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; eshell-git-prompt powerline theme
   (eshell-git-prompt-powerline-dir-face :background "steel blue" :foreground bg)
   (eshell-git-prompt-powerline-clean-face :background "foreset green" :foreground bg)
   (eshell-git-prompt-powerline-not-clean-face :background "indian red" :foreground bg)
   ;;; helm
   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground violet)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; outline (affects org-mode)
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
   ;;;; git-gutter-fringe
   (git-gutter-fr:modified :foreground vc-modified)
   ;;;; rainbow-delimeters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground cyan)
   (rainbow-delimiters-depth-6-face :foreground violet)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   ;;;; vterm
   (vterm-color-black   :background (doom-lighten base0 0.75)   :foreground base0)
   (vterm-color-red     :background (doom-lighten red 0.75)     :foreground red)
   (vterm-color-green   :background (doom-lighten green 0.75)   :foreground green)
   (vterm-color-yellow  :background (doom-lighten yellow 0.75)  :foreground yellow)
   (vterm-color-blue    :background (doom-lighten blue 0.75)    :foreground blue)
   (vterm-color-magenta :background (doom-lighten magenta 0.75) :foreground magenta)
   (vterm-color-cyan    :background (doom-lighten cyan 0.75)    :foreground cyan)
   (vterm-color-white   :background (doom-lighten base8 0.75)   :foreground base8)
   ))

;;; doom-solarized-dark-high-contrast-theme.el ends here
