;;; doom-aura-theme.el --- inspired by the aura theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: Oct 9, 2022
;; Author: scturtle <https://github.com/scturtle>
;; Maintainer: hlissner <https://github.com/hlissner>
;; Source: https://github.com/daltonmenezes/aura-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-aura-theme nil
  "Options for the `doom-aura' theme."
  :group 'doom-themes)

;;
;;; Theme definition

(def-doom-theme doom-aura
  "A dark theme based on aura theme"

  ;; name        default   256       16
  ((bg         '("#21202e" "#222222" "black"        ))
   (bg-alt     '("#1E2029" "#112222" "black"        ))
   (base0      '("#1E2029" "#112222" "black"        ))
   (base1      '("#282a36" "#222233" "brightblack"  ))
   (base2      '("#373844" "#333344" "brightblack"  ))
   (base3      '("#44475a" "#444455" "brightblack"  ))
   (base4      '("#565761" "#555566" "brightblack"  ))
   (base5      '("#6272a4" "#6677aa" "brightblack"  ))
   (base6      '("#b6b6b2" "#bbbbbb" "brightblack"  ))
   (base7      '("#ccccc7" "#cccccc" "brightblack"  ))
   (base8      '("#edecee" "#eeeeee" "white"        ))
   (fg         '("#edecee" "#eeeeee" "white"        ))
   (fg-alt     '("#e2e2dc" "#ffffff" "brightwhite"  ))

   (grey       base4)
   (red        '("#ff6767" "#ff6666" "red"          ))
   (orange     '("#ffca85" "#ffcc88" "brightred"    ))
   (green      '("#61ffca" "#66ffcc" "green"        ))
   (teal       '("#a277ff" "#aa77ff" "brightgreen"  ))
   (yellow     '("#ffca85" "#ffcc88" "yellow"       ))
   (blue       '("#a277ff" "#aa77ff" "brightblue"   ))
   (dark-blue  '("#a277ff" "#aa77ff" "blue"         ))
   (magenta    '("#a277ff" "#aa77ff" "magenta"      ))
   (violet     '("#a277ff" "#aa77ff" "brightmagenta"))
   (cyan       '("#61ffca" "#66ffcc" "brightcyan"   ))
   (dark-cyan  '("#61ffca" "#66ffcc" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        orange)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      cyan)
   (functions      green)
   (keywords       magenta)
   (methods        teal)
   (operators      violet)
   (type           violet)
   (strings        yellow)
   (variables      (doom-lighten magenta 0.6))
   (numbers        violet)
   (region         `(,(car base3) ,@(cdr base1)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (region-alt `(,(car base3) ,@(cdr base4)))

   (modeline-fg 'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg-inactive `(,(doom-darken (car bg) 0.075) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))

  ;;;; Base theme face overrides
  (
   ;;;; company
   (company-tooltip-common :foreground yellow :bold)
   (company-tooltip-selection :background (doom-darken blue 0.3))
   ;;;; org <built-in>
   ((org-block &override) :background (doom-darken base1 0.125) :foreground base7)
   ((org-block-begin-line &override) :background (doom-darken base1 0.125) :foreground comments)
   ((org-code &override) :foreground yellow)
   (org-todo :foreground orange :bold 'inherit)
   ((org-link &override) :foreground orange)
   )

  ;;;; Base theme variable overrides-
  ()
  )

;;; doom-aura-theme.el ends here
