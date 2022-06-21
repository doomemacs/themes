;;; doom-ayu-light-theme.el --- inspirted by Ayu Light -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: February 5, 2021 (#499)
;; Author: LoveSponge <https://github.com/LoveSponge>
;; Maintainer:
;; Source: https://github.com/dempfi/ayu
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-ayu-light-theme nil
  "Options for the `doom-ayu-light' theme."
  :group 'doom-themes)

(defcustom doom-ayu-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ayu-light-theme
  :type 'boolean)

(defcustom doom-ayu-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-light-theme
  :type 'boolean)

(defcustom doom-ayu-light-comment-bg doom-ayu-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-light-theme
  :type 'boolean)

(defcustom doom-ayu-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-ayu-light
  "A light theme inspired by Ayu Light"

  ;; name        default   256       16
  (
   ;; common
   (common-accent   '("#ff9940" "orange"  "orange" ))
   (common-bg       '("#fafafa" "black"   "black"  ))
   (common-fg       '("#575f66" "grey"    "grey"   ))
   (common-ui       '("#ba9199" "grey"    "grey"   ))
   (test            '("#2ea8e6" "grey"    "grey"   ))
   ;; syntax
   (syntax-tag      '("#55b4d4" "cyan"    "blue"   ))
   (syntax-func     '("#f2ae49" "yellow"  "yellow" ))
   (syntax-entity   '("#399ee6" "blue"    "blue"   ))
   (syntax-string   '("#86b300" "green"   "green"  ))
   (syntax-regexp   '("#4cbf99" "teal"    "green"  ))
   (syntax-markup   '("#f07171" "red"     "red"    ))
   (syntax-keyword  '("#fa8d3e" "orange"  "orange" ))
   (syntax-special  '("#e6ba7e" "yellow"  "yellow" ))
   (syntax-comment  '("#abb0b6" "grey"    "grey"   ))
   (syntax-constant '("#a37acc" "magenta" "purple" ))
   (syntax-operator '("#ed9366" "orange"  "orange" ))
   (syntax-error    '("#f51818" "red"     "red"    ))
   ;; ui
   (ui-line               (doom-darken common-bg 0.07))
   (ui-panel-shadow       (doom-lighten common-bg 0.35))
   (ui-panel-border       (doom-lighten common-bg 0.45))
   (ui-gutter-normal      (doom-lighten common-ui 0.45))
   (ui-gutter-active      common-ui)
   (ui-selection-bg       (doom-blend common-bg test 0.7))
   (ui-selection-inactive (doom-lighten test 0.93))
   (ui-selection-border   (doom-lighten test 0.93))
   (ui-guide-active       (doom-lighten common-ui 0.75))
   (ui-guide-normal       (doom-lighten common-ui 0.35))
   (ui-org-block          (doom-lighten test 0.95))
   (elscreen-bg           (doom-lighten common-fg 0.65))
   (elscreen-fg           (doom-darken common-fg 0.85))
   ;; vcs
   (vcs-added    '("#99bf4d" "green" "green" ))
   (vcs-modified '("#709ecc" "blue"  "blue"  ))
   (vcs-removed  '("#f27983" "red"   "red"   ))

   (bg         common-bg)
   (bg-alt     common-bg)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-selection-border)
   (base4      ui-selection-inactive)
   (base5      ui-guide-active)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         common-fg)
   (fg-alt     common-ui)

   (grey       syntax-comment)
   (red        syntax-markup)
   (orange     syntax-keyword)
   (green      syntax-string)
   (teal       syntax-regexp)
   (yellow     syntax-func)
   (blue       syntax-entity)
   (dark-blue  (doom-darken syntax-entity 0.2))
   (magenta    syntax-constant)
   (violet     (doom-lighten syntax-constant 0.2))
   (cyan       syntax-tag)
   (dark-cyan  (doom-darken syntax-tag 0.2))

   ;; face categories -- required for all themes
   (highlight      common-accent)
   (vertical-bar   ui-panel-border)
   (selection      ui-selection-inactive)
   (builtin        syntax-func)
   (comments       (if doom-ayu-light-brighter-comments syntax-comment elscreen-bg))
   (doc-comments   (doom-lighten (if doom-ayu-light-brighter-comments syntax-comment elscreen-bg) 0.25))
   (constants      syntax-constant)
   (functions      syntax-func)
   (keywords       syntax-keyword)
   (methods        syntax-func)
   (operators      syntax-operator)
   (type           syntax-special)
   (strings        syntax-string)
   (variables      common-fg)
   (numbers        syntax-func)
   (region         ui-selection-bg)
   (error          syntax-error)
   (warning        yellow)
   (success        green)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-ayu-light-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-light-padded-modeline
      (if (integerp doom-ayu-light-padded-modeline) doom-ayu-light-padded-modeline 4)))

   (modeline-fg     common-ui)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten blue 0.475)
      `(,(doom-lighten (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten blue 0.45)
      `(,(doom-lighten (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-lighten (car bg) 0.1) ,@(cdr bg)))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-ayu-light-comment-bg (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))


   ;;;; company
   (company-tooltip :foreground common-fg :background common-bg)
   (company-tooltip-annotation :foreground common-fg)
   (company-tooltip-selection :background ui-line)
   (company-tooltip-search :foreground common-accent :weight 'bold)
   (company-scrollbar-bg :background common-bg)
   (company-scrollbar-fg :background syntax-comment)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; diff-mode <built-in>
   (diff-removed :foreground vcs-removed)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground green :weight 'normal)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background elscreen-bg :foreground elscreen-fg)
   ;;;; highlight-numbers
   (highlight-numbers-number :foreground syntax-func :weight 'normal)
   ;;;; ivy
   (ivy-current-match :background ui-line)
   (ivy-minibuffer-match-face-1 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground common-accent :weight 'bold)
   ;;;; js2-mode
   (js2-object-property :foreground common-fg)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten common-bg 0.05))
   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-headline-done :foreground syntax-comment)
   (org-document-info-keyword :foreground comments)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (rjsx-attr :foreground syntax-func)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (web-mode-html-attr-name-face :foreground syntax-func)))

;;; doom-ayu-light-theme.el ends here
