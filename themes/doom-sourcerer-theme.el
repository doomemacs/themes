;;; doom-sourcerer-theme.el --- a port of xero's Sourcerer -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: August 23, 2018 (#215)
;; Author: fm0xb <https://github.com/fm0xb>
;; Maintainer:
;; Source: https://github.com/xero/sourcerer
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-sourcerer-theme nil
  "Options for the `doom-sourcerer' theme."
  :group 'doom-themes)

(defcustom doom-sourcerer-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-sourcerer-theme
  :type 'boolean)

(defcustom doom-sourcerer-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-sourcerer-theme
  :type 'boolean)

(defcustom doom-sourcerer-comment-bg doom-sourcerer-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'doom-sourcerer-theme
  :type 'boolean)

(defcustom doom-sourcerer-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-sourcerer-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-sourcerer
  "A dark theme based off of xero's Sourcerer VIM colorscheme"

  ((bg         '("#171717"))
   (bg-alt     '("#222222"))
   (base0      '("#1d2127"))
   (base1      '("#1d2127"))
   (base2      '("#272727"))
   (base3      '("#32353f"))
   (base4      '("#494952"))
   (base5      '("#62686E"))
   (base6      '("#757B80"))
   (base7      '("#9ca0a4"))
   (base8      '("#faf4c6"))
   (fg         '("#c2c2b0"))
   (fg-alt     '("#5D656B"))

   (grey       '("#686858"))
   (red        '("#aa4450"))
   (orange     '("#ff9800"))
   (green      '("#87875f"))
   (green-br   '("#719611"))
   (teal       '("#578F8F" "#44b9b1" ))
   (yellow     '("#cc8800"           ))
   (blue       '("#87AFD7"           ))
   (dark-blue  '("#6688aa"           ))
   (magenta    '("#8787AF"           ))
   (violet     '("#8181a6"           ))
   (cyan       '("#87ceeb"           ))
   (dark-cyan  '("#528b8b"           ))
   ;; face categories
   (highlight      cyan)
   (vertical-bar   base0)
   (selection      base5)
   (builtin        blue)
   (comments       (if doom-sourcerer-brighter-comments dark-cyan grey))
   (doc-comments   (if doom-sourcerer-brighter-comments (doom-lighten dark-cyan 0.15) (doom-darken grey 0.1)))
   (constants      teal)
   (functions      base8)
   (keywords       blue)
   (methods        magenta)
   (operators      green-br)
   (type           violet)
   (strings        green)
   (variables      base8)
   (numbers        yellow)
   (region         base3)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when doom-sourcerer-padded-modeline
      (if (integerp doom-sourcerer-padded-modeline) doom-sourcerer-padded-modeline 4)))

   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (doom-blend blue grey (if doom-sourcerer-brighter-modeline 0.4 0.08)))

   (modeline-bg
    (if doom-sourcerer-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-l
    (if doom-sourcerer-brighter-modeline
        `("#383f58" ,@(cdr base1))
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0)))
   (modeline-bg-inactive-l (doom-darken bg 0.20)))

  ;;;; Base theme face overrides
  ((cursor :background blue)
   ((font-lock-comment-face &override)
    :background (if doom-sourcerer-comment-bg (doom-darken bg-alt 0.095)))
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)
   (mode-line
    :background base3 :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base3)))
   (mode-line-inactive
    :background bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-sourcerer-brighter-modeline base8 highlight))
   (mode-line-buffer-id :foreground green-br :bold bold)

   ;;;; company
   (company-tooltip-selection     :background base3)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-sourcerer-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if doom-sourcerer-brighter-modeline base8 blue) :bold bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)
   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg-alt)
   ((org-block-end-line &override) :background bg-alt)
   (org-hide :foreground hidden)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground dark-cyan)
   (rainbow-delimiters-depth-2-face :foreground teal)
   (rainbow-delimiters-depth-3-face :foreground dark-blue)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground orange)
   ;;;; rjsx-mode
   (rjsx-attr :foreground magenta :slant 'italic :weight 'medium)
   (rjsx-tag :foreground blue)
   (rjsx-tag-bracket-face :foreground base8)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-sourcerer-theme.el ends here
