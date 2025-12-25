;;; doom-railscasts-darker-theme.el --- Railscasts port w/ darker background -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: (port) Codex CLI
;; Source: vscode-railscasts-theme (https://github.com/ryanb/vscode-railscasts-theme)
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-railscasts-darker-theme nil
  "Options for the `doom-railscasts-darker' theme."
  :group 'doom-themes)

;;
;;; Theme definition

(def-doom-theme doom-railscasts-darker
  "Railscasts port with a darker background."

  ;; name        default   256       16

  ((bg         '("#1e1a1a" "#1e1a1a" "black"      ))
   (bg-alt     '("#1f1f1f" "#1f1f1f" "black"      ))
   (base0      '("#282828" "#282828" "black"      ))
   (base1      '("#333435" "#333435" "brightblack"))
   (base2      '("#404040" "#404040" "brightblack"))
   (base3      '("#565656" "#565656" "brightblack"))
   (base4      '("#807c79" "#807c79" "brightblack"))
   (base5      '("#e6e1dc" "#e6e1dc" "brightblack"))
   (base6      '("#f4f1ed" "#f4f1ed" "brightwhite"))
   (base7      '("#ffffff" "#ffffff" "white"      ))
   (base8      '("#da4939" "#da4939" "red"        ))
   (fg         '("#e6e1dc" "#e6e1dc" "white"      ))
   (fg-alt     '("#d9d4cf" "#d9d4cf" "brightwhite"))

   (grey       base4)
   (red        '("#da4939" "#da4939" "red"          ))
   (orange     '("#cc7833" "#cc7833" "brightred"    ))
   (green      '("#a5c261" "#a5c261" "green"        ))
   (teal       '("#519f50" "#519f50" "brightgreen"  ))
   (yellow     '("#ffc66d" "#ffc66d" "yellow"       ))
   (blue       '("#6d9cbe" "#6d9cbe" "brightblue"   ))
   (dark-blue  '("#82a7e2" "#82a7e2" "blue"         ))
   (magenta    '("#d0d0ff" "#d0d0ff" "magenta"      ))
   (violet     '("#d0d0ff" "#d0d0ff" "brightmagenta"))
   (cyan       '("#82a7e2" "#82a7e2" "cyan"         ))
   (dark-cyan  '("#2f33ab" "#2f33ab" "brightcyan"   ))

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      '("#3d4456" "#3d4456" "brightblack"))
   (region         '("#f5f5f5" "#f5f5f5" "white"))
   (region-fg      '("#333333" "#333333" "black"))
   (builtin        blue)
   (comments       '("#bc9458" "#bc9458" "yellow"))
   (doc-comments   (doom-lighten comments 0.1))
   (constants      blue)
   (functions      yellow)
   (keywords       orange)
   (methods        yellow)
   (operators      orange)
   (type           base7)
   (strings        green)
   (variables      violet)
   (numbers        green)
   (error          red)
   (warning        '("#e8bf6a" "#e8bf6a" "yellow"))
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red))


  ;;;; Base theme face overrides
  ((cursor :background base7)
   (hl-line :background base1)
   (region :background region :foreground region-fg)
   (line-number :foreground base2)
   (line-number-current-line :foreground fg)
   (show-paren-match :background (doom-darken selection 0.15) :foreground base7 :weight 'bold)

   (mode-line :background (doom-darken bg-alt 0.1) :foreground fg)
   (mode-line-inactive :background bg-alt :foreground base5)
   (tooltip :background (doom-darken bg-alt 0.15) :foreground fg)

   ;; doom-modeline
   (doom-modeline-bar :background highlight)

   ;; ivy-posframe
   (ivy-posframe-border :background base1)

   ;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg-alt :foreground base4)
   ((org-block-end-line &override)   :background bg-alt :foreground base4)

   ;; font styles from tmTheme
   ((font-lock-comment-face &override) :slant 'italic)
   ((font-lock-doc-face &override) :slant 'italic)))
