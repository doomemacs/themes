;;; doom-nova-theme.el --- inspired by Trevord Miller's Nova -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 6, 2017 (#64)
;; Author: bigardone <https://github.com/bigardone>
;; Maintainer:
;; Source: https://github.com/trevordmiller/nova-colors
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-nova-theme nil
  "Options for the `doom-nova' theme."
  :group 'doom-themes)

(defcustom doom-nova-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-nova-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-nova
  "A light theme inspired by Trevord Miller's Nova. See
<https://trevordmiller.com/projects/nova>."

  ;; name        default   256       16
  ((bg         '("#3c4c55" nil       nil           ))
   (fg         '("#c5d4dd" "#c5c6c6" "white"       ))

   (bg-alt     '("#44545d" "#445566" "black"       ))
   (fg-alt     '("#c5c8c6" "#c5c8c6" "white"       ))

   (base0      '("#0d0f11" "#0d0f11" "black"       ))
   (base1      '("#1e272c" "#1b1b1b" "black"       ))
   (base2      '("#212122" "#1e1e1e" "black"       ))
   (base3      '("#2f3f48" "#292929" "brightblack" ))
   (base4      '("#3c4c55" "#3f3f3f" "brightblack" ))
   (base5      '("#556873" "#525252" "brightblack" ))
   (base6      '("#6A7D89" "#6b6b6b" "brightblack" ))
   (base7      '("#899BA6" "#878797" "brightblack" ))
   (base8      '("#e6eef3" "#efefef" "brightwhite" ))

   (grey       base7)
   (red        '("#DF8C8C"))
   (orange     '("#F2C38F"))
   (yellow     '("#DADA93"))
   (green      '("#A8CE93"))
   (blue       '("#83AFE5"))
   (dark-blue  '("#759DCE"))
   (teal       '("#95BEBC"))
   (magenta    '("#D18EC2"))
   (violet     '("#9A93E1"))
   (cyan       '("#7FC1CA"))
   (dark-cyan  '("#659AA1"))

   ;; face categories
   (highlight      cyan)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      highlight)
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.1))
   (constants      highlight)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      green)
   (type           green)
   (strings        cyan)
   (variables      red)
   (numbers        highlight)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    violet)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (current-line    base5) ; (doom-lighten bg-alt 0.04)
   (modeline-fg     blue)
   (modeline-bg     base5) ; bg-alt
   (modeline-fg-alt (doom-lighten bg-alt 0.4))
   (modeline-bg-alt base4)

   (-modeline-pad
    (when doom-nova-padded-modeline
      (if (integerp doom-nova-padded-modeline)
          doom-nova-padded-modeline
        4))))


  ;;;; Base theme face overrides
  ((fringe :inherit 'default :foreground "#6c808d")
   ((line-number &override) :foreground "#6c808d")
   ((line-number-current-line &override) :foreground highlight :weight 'bold)
   (hl-line :background current-line)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (region :background (doom-lighten current-line 0.1) :foreground nil :distant-foreground nil :weight 'bold)

   ;;;; company
   (company-tooltip            :inherit 'tooltip :background (doom-lighten bg 0.075))
   (company-tooltip-selection  :background base5 :foreground base8 :weight 'bold)
   (company-tooltip-common     :foreground cyan :distant-foreground cyan :weight 'bold)
   (company-tooltip-search     :background highlight :foreground base1 :weight 'ultra-bold)
   (company-tooltip-search-selection :background highlight :foreground base1 :weight 'ultra-bold)
   (company-tooltip-mouse      :background base6 :foreground bg :distant-foreground fg)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground violet :bold nil)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   (doom-modeline-bar :inherit 'mode-line-highlight)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background base3 :weight 'bold)
   (ediff-current-diff-A :inherit 'hl-line)
   (ediff-even-diff-A    :background base3)
   ;;;; helm
   (helm-selection :background current-line :weight 'bold)
   (helm-match     :foreground highlight)
   (helm-source-header :foreground base0 :background base6)
   ;;;; highlight-thing highlight-symbol
   (highlight-symbol-face :background (doom-lighten current-line 0.1) :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background (doom-lighten current-line 0.1) :distant-foreground fg-alt)
   ;;;; mic-paren
   ((paren-face-match &override)    :foreground red :background (doom-darken violet 0.4))
   ((paren-face-mismatch &override) :foreground (doom-darken red 0.4) :background cyan)
   ;;;; ivy
   (ivy-current-match :background current-line :distant-foreground base0)
   ;;;; org <built-in>
   (org-headline-done :foreground base7)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)
   ;;;; solaire-mode
   (solaire-hl-line-face :inherit 'hl-line))

  ;; --- variables --------------------------
  ;; ()
  )

;;; doom-nova-theme.el ends here
