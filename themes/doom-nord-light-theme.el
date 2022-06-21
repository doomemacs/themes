;;; doom-nord-light-theme.el --- light variant of Nord -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: April 18, 2018 (#163)
;; Author: fuxialexander <https://github.com/fuxialexander>
;; Maintainer:
;; Source: https://github.com/arcticicestudio/nord
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-nord-light-theme nil
  "Options for the `doom-nord-light' theme."
  :group 'doom-themes)

(defcustom doom-nord-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-nord-light-theme
  :type 'boolean)

(defcustom doom-nord-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-nord-light-theme
  :type 'boolean)

(defcustom doom-nord-light-comment-bg doom-nord-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-nord-light-theme
  :type 'boolean)

(defcustom doom-nord-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-nord-light-theme
  :type '(choice integer boolean))

(defcustom doom-nord-light-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-nord-light-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-nord-light
  "A light theme inspired by Nord-Light."

  ;; name        default   256       16
  ((bg         '("#E5E9F0" nil       nil))
   (bg-alt     '("#D8DEE9" nil       nil))
   (base0      '("#F0F4FC" "black"   "black"))
   (base1      '("#E3EAF5" "#1e1e1e" "brightblack"))
   (base2      '("#D8DEE9" "#2e2e2e" "brightblack"))
   (base3      '("#C2D0E7" "#262626" "brightblack"))
   (base4      '("#B8C5DB" "#3f3f3f" "brightblack"))
   (base5      '("#AEBACF" "#525252" "brightblack"))
   (base6      '("#A1ACC0" "#6b6b6b" "brightblack"))
   (base7      '("#60728C" "#979797" "brightblack"))
   (base8      '("#485163" "#dfdfdf" "white"))
   (fg         '("#3B4252" "#2d2d2d" "white"))
   (fg-alt     '("#2E3440" "#bfbfbf" "brightwhite"))

   (grey base4)
   (red       '("#99324B" "#ff6655" "red"))
   (orange    '("#AC4426" "#dd8844" "brightred"))
   (green     '("#4F894C" "#99bb66" "green"))
   (teal      '("#29838D" "#44b9b1" "brightgreen"))
   (yellow    '("#9A7500" "#ECBE7B" "yellow"))
   (blue      '("#3B6EA8" "#51afef" "brightblue"))
   (dark-blue '("#5272AF" "#2257A0" "blue"))
   (magenta   '("#97365B" "#c678dd" "magenta"))
   (violet    '("#842879" "#a9a1e1" "brightmagenta"))
   (cyan      '("#398EAC" "#46D9FF" "brightcyan"))
   (dark-cyan '("#2C7088" "#5699AF" "cyan"))

   ;; face categories -- required for all themes
   (highlight (doom-blend blue bg 0.8))
   (vertical-bar (doom-darken bg 0.15))
   (selection (doom-blend blue bg 0.5))
   (builtin teal)
   (comments (if doom-nord-light-brighter-comments dark-cyan (doom-darken base5 0.2)))
   (doc-comments (doom-darken (if doom-nord-light-brighter-comments dark-cyan base5) 0.25))
   (constants magenta)
   (functions teal)
   (keywords blue)
   (methods teal)
   (operators blue)
   (type yellow)
   (strings green)
   (variables violet)
   (numbers magenta)
   (region (pcase doom-nord-light-region-highlight
             ((\` frost) (doom-lighten teal 0.5))
             ((\` snowstorm) base0)
             (_ base4)))
   (error red)
   (warning yellow)
   (success green)
   (vc-modified orange)
   (vc-added green)
   (vc-deleted red)

   ;; custom categories
   (hidden `(,(car bg) "black" "black"))
   (-modeline-bright doom-nord-light-brighter-modeline)
   (-modeline-pad
    (when doom-nord-light-padded-modeline
      (if (integerp doom-nord-light-padded-modeline) doom-nord-light-padded-modeline 4)))

   (modeline-fg nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-blend bg blue 0.7)
      `(,(doom-darken (car bg) 0.02) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-blend bg blue 0.7)
      `(,(doom-darken (car bg) 0.03) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.01)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-nord-light-comment-bg (doom-lighten bg 0.05)))
   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   (internal-border :foreground (doom-blend blue bg 0.2) :background (doom-blend blue bg 0.2))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   ((region &override)
    :foreground (if (memq doom-nord-light-region-highlight '(frost snowstorm))
                    bg-alt))

   ;;;; css-mode <built-in> / scss-mode <built-in>
   (css-proprietary-property :foreground orange)
   (css-property :foreground green)
   (css-selector :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-project-root-dir :foreground base6)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-minibuffer-match-face-1 :background nil :foreground (doom-blend fg bg 0.5) :weight 'light)
   (ivy-virtual :foreground (doom-blend blue bg 0.8))
   ;;;; ivy-posframe
   (ivy-posframe :background (doom-blend blue bg 0.2))
   ;;;; magit
   (magit-diff-hunk-heading-highlight :foreground bg :background blue :weight 'bold)
   (magit-diff-hunk-heading :foreground bg :background (doom-blend blue bg 0.3))
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; mic-paren
   ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ;;;; nav-flash
   (nav-flash-face :background region :foreground base8 :weight 'bold)
   ;;;; org <built-in>
   (org-hide :foreground hidden)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; vimish-fold
   ((vimish-fold-fringe &override) :foreground teal)
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light))

  ;;;; Base theme variable overrides-
  ()
  )

;;; doom-nord-light-theme.el ends here
