;;; doom-snazzy-theme.el --- inspired by Hyper Snazzy

;;; Commentary:
;;; Code:
(require 'doom-themes)

;;
(defgroup doom-snazzy-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

;;
(def-doom-theme doom-snazzy
  "A dark theme inspired by Atom Snazzy Dark"

  ;; name        default   256       16
  ((bg         '("#282a36" nil       nil)) ;; this is the background for the hl-line, modeline, and minibuffer
   (bg-alt     '("#242631" nil       nil)) ;; this is the background for the line you arent currently on
   (base0      '("#282a36" nil       nil))
   (base1      '("#34353e" nil       nil))
   (base2      '("#43454f" nil       nil))
   (base3      '("#78787e" nil       nil))
   (base4      '("#a5a5a9" nil       nil))
   (base5      '("#e2e4e5" nil       nil))
   (base6      '("#eff0eb" nil       nil))
   (base7      '("#f1f1f0" nil       nil))
   (base8      '("#ff5c57" nil       nil))
   (base9      '("#ff9f43" nil       nil))
   (base10     '("#f3f99d" nil       nil))
   (base11     '("#5af78e" nil       nil))
   (base12     '("#9aedfe" nil       nil))
   (base13     '("#57c7ff" nil       nil))
   (base14     '("#ff6ac1" nil       nil))
   (base15     '("#b2643c" nil       nil))
   (fg         '("#f9f9f9" nil       nil))
   (fg-alt     '("#5B6268" nil       nil))

   (ui0 '("#f9f9f9" nil nil))
   (ui1 '("#f9f9ff" nil nil))
   (ui2 '("#eff0eb" nil nil))
   (ui3 '("#e2e4e5" nil nil))
   (ui4 '("#a1a6a8" nil nil))
   (ui5 '("#848688" nil nil))
   (ui6 '("#5e6c70" nil nil))
   (ui7 '("#536991" nil nil))
   (ui8 '("#606580" nil nil))
   (ui9 '("#3a3d4d" nil nil))
   (internalborder '("#1c1e27" nil nil))
   ;; 10 skipped for some reason
   (ui11 '("#282a36" nil nil))
   (ui12 '("#192224" nil nil))

   (grey       ui5)
   (red        '("#ff5c57" nil nil))
   (green      '("#5af78e" nil nil))
   (yellow     '("#f3f99d" nil nil))
   (blue       '("#57c7ff" nil nil))
   (dark-blue  '("#459fcc" nil nil))
   (magenta    '("#ff6ac1" nil nil))
   (cyan       '("#9aedfe" nil nil))
   (violet     '("#bd93f9" nil nil))
   (orange     '("#ffb86c" nil nil))
   (teal       '("#aad4d3" nil nil))
   (dark-cyan  '("#82c9d7" nil nil))

   ;; face categories -- required for all themes
   (highlight      blue) ;; when searching with (/) ?
   ;; (vertical-bar   (doom-darken base1 0.1)) ;; no idea what this is
   (vertical-bar (doom-darken base1 0.1)) ;; the bar that separates modeline and
                                          ;; minibuffer?
   (selection      dark-blue) ;; for like company autocomplete and stuff
   (builtin magenta) ;; saw this in company autocomplete if i moved my mouse
                     ;; over it
   (comments       ui8) ;; comments
   (doc-comments (doom-lighten yellow 0.25)) ;; easy to test with elisp
                                             ;; documentation or git commit
                                             ;; first line thing
   (constants      green)
   (functions      blue)
   (keywords       orange)
   (methods        blue) ;; wtf is the difference between this and function?
   (operators      magenta)
   (type           cyan)
   (strings        yellow)
   ;; (variables      (doom-lighten magenta 0.4))
   (variables      red)
   (numbers        yellow)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red))

  ;; extra faces
  ;; i have no idea what im doing with the modeline
  ((mode-line
    :background (doom-darken bg-alt 0.15))
   (mode-line-inactive
    :background (doom-darken bg-alt 0.1)
    :foreground base5)
   (doom-modeline-bar :background highlight)
   ;; line numbers
   (line-number :foreground ui9)
   (line-number-current-line :foreground fg)
   ;; rjsx stuff
   (rjsx-text :foreground fg)

   (ivy-posframe-border :background internalborder)

   ;; org
   (org-level-1 :foreground blue :background ui9 :height 1.25 :weight 'bold)
   (org-level-3 :foreground dark-blue))) ;; this is ff0000 from something above, dunno what

;;; doom-snazzy-theme.el ends here
