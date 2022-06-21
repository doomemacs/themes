;;; doom-lantern-theme.el --- based on Gitleptune's Lantern theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: February 22, 2022 (#708)
;; Author: paladhammika <https://github.com/paladhammika>
;; Maintainer:
;; Source: https://github.com/Gitleptune/lantern-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-lantern-theme nil
  "Options for the `doom-lantern' theme."
  :group 'doom-themes)

(defcustom doom-lantern-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-lantern-theme
  :type 'boolean)

(defcustom doom-lantern-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-lantern-theme
  :type 'boolean)

(defcustom doom-lantern-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-lantern-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-lantern
  "A brown and ochre theme based of https://github.com/Gitleptune/lantern-theme"

  ;; name        default   256           16
  ((bg         '("#261b17" "black"       "black"  ))
   (fg         '("#e4cbb3" "#bfbfbf"     "brightwhite"  ))

   (bg-alt     '("#291812" "black"       "black"        ))
   (fg-alt     '("#5B6268" "#2d2d2d"     "white"        ))

   (base0      '("#1b110e" "#1b110e"     "black"        ))
   (base1      '("#1E1310" "#1e1e1e"     "brightblack"  ))
   (base2      '("#2D1F1A" "#2D1F1A"     "brightblack"  ))
   (base3      '("#2D1E19" "#231713"     "brightblack"  ))
   (base4      '("#3F2F2A" "#3F2F2A"     "brightblack"  ))
   (base5      '("#554541" "#554541"     "brightblack"  ))
   (base6      '("#694949" "#694949"     "brightblack"  ))
   (base7      '("#987070" "#987070"     "brightblack"  ))
   (base8      '("#C18E76" "#C18E76"     "white"        ))

   (grey       base4)
   (brown      "#532f28")
   (dark-brown "#291812")
   (ochre      "#aa6a1f")
   (red        '("#cc241d" "#e04220" "red"          ))
   (orange     '("#e35b15" "#dd8844" "brightred"    ))
   (green      '("#98971a" "#b8bb26" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#d79921" "#d79921" "yellow"       ))
   (blue       '("#66a4a4" "#66a4a4" "brightblue"   ))
   (dark-blue  '("#4395a3" "#2257A0" "blue"         ))
   (magenta    '("#d3869b" "#d3869b" "brightmagenta"))
   (violet     '("#805b87" "#805b87" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   (highlight      ochre)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-brown)
   (builtin        orange)
   (comments       (if doom-lantern-brighter-comments dark-cyan base8))
   (doc-comments   (doom-lighten (if doom-lantern-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      orange)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        yellow)
   (region         brown)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-lantern-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-lantern-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-lantern-padded-modeline
      (if (integerp doom-lantern-padded-modeline) doom-lantern-padded-modeline 4))))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-lantern-brighter-comments (doom-lighten bg 0.05)))
   ;; (hl-line                                     :background brown)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-lantern-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-lantern-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
    ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#261b17" :foreground "#1b110e")

   ;;;; ivy
   (ivy-current-match :background ochre :distant-foreground brown :weight 'bold)
   (ivy-cursor :foreground bg-alt
               :background fg)
   (ivy-minibuffer-match-highlight  :foreground yellow :weight 'bold)
   (ivy-highlight-face              :foreground yellow :weight 'bold)

   ;;;; orderless
   (orderless-match-face-0 :weight 'bold :foreground (doom-blend ochre   fg 0.8) :background (doom-blend ochre  bg 0.1))
   (orderless-match-face-1 :weight 'bold :foreground (doom-blend red     fg 0.6) :background (doom-blend red    bg 0.1))
   (orderless-match-face-2 :weight 'bold :foreground (doom-blend green   fg 0.6) :background (doom-blend green  bg 0.1))
   (orderless-match-face-3 :weight 'bold :foreground (doom-blend cyan    fg 0.6) :background (doom-blend cyan   bg 0.1))

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)

   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red :weight 'bold :background (doom-lighten red 0.4))
   (ediff-current-diff-B        :foreground green :weight 'bold :background (doom-lighten green 0.4))
   (ediff-current-diff-C        :foreground blue :weight 'bold :background (doom-lighten blue 0.4))

   ;;;; org <built-in>
   (org-level-1 :bold t :foreground ochre :weight 'bold)
   (org-level-2 :foreground yellow)
   (org-level-3 :foreground orange)

   ;;;; solaire-mode
   (solaire-hl-line-face :background brown)
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

;;; doom-lantern-theme.el ends here
