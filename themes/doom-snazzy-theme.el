;;; doom-snazzy-theme.el --- inspired by Hyper Snazzy

;;; Commentary:
;;; Code:
(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)

;;
(defgroup doom-snazzy-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

;;
(def-doom-theme doom-snazzy
  "A dark theme inspired by Atom Snazzy Dark"

  ;; name        default   256       16
  ((bg         '("#282a36" "#282a36" nil          )) ;; this is the background for the hl-line, modeline, and minibuffer
   (bg-alt     '("#242631" "#242631" nil          )) ;; this is the background for the line you arent currently on
   (base0      '("#282a36" "#282a36" "black"      ))
   (base1      '("#34353e" "#34353e" "brightblack"))
   (base2      '("#43454f" "#43454f" "brightblack"))
   (base3      '("#78787e" "#78787e" "brightblack"))
   (base4      '("#a5a5a9" "#a5a5a9" "brightblack"))
   (base5      '("#e2e4e5" "#e2e4e5" "brightblack"))
   (base6      '("#eff0eb" "#eff0eb" "brightblack"))
   (base7      '("#f1f1f0" "#f1f1f0" "brightblack"))
   (base8      '("#ff5c57" "#ff5c57" "white"      ))
   (fg         '("#f9f9f9" "#f9f9f9" "white"      ))
   (fg-alt     '("#d1d1d1" "#d1d1d1" "brightwhite"))

   (ui0 '("#848688" "#848688" "grey"))
   (ui1 '("#606580" "#606580" "grey"))
   (ui2 '("#3a3d4d" "#3a3d4d" "grey"))
   (ui3 '("#1c1e27" "#1c1e27" "black"))

   (grey       ui0)
   (red        '("#ff5c57" "#ff5c57" "red"          ))
   (green      '("#5af78e" "#5af78e" "brightred"    ))
   (yellow     '("#f3f99d" "#f3f99d" "green"        ))
   (blue       '("#57c7ff" "#57c7ff" "brightgreen"  ))
   (dark-blue  '("#459fcc" "#459fcc" "yellow"       ))
   (magenta    '("#ff6ac1" "#ff6ac1" "brightblue"   ))
   (cyan       '("#9aedfe" "#9aedfe" "blue"         ))
   (violet     '("#bd93f9" "#bd93f9" "magenta"      ))
   (orange     '("#ffb86c" "#ffb86c" "brightmagenta"))
   (teal       '("#aad4d3" "#aad4d3" "brightcyan"   ))
   (dark-cyan  '("#82c9d7" "#82c9d7" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue) ;; when searching with (/) ?
   ;; (vertical-bar   (doom-darken base1 0.1)) ;; no idea what this is
   (vertical-bar (doom-darken base1 0.1)) ;; the bar that separates modeline and
                                          ;; minibuffer?
   (selection      dark-blue) ;; for like company autocomplete and stuff
   (builtin magenta) ;; saw this in company autocomplete if i moved my mouse
                     ;; over it
   (comments       ui1) ;; comments
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
   (line-number :foreground ui2)
   (line-number-current-line :foreground fg)
   ;; rjsx stuff
   (rjsx-text :foreground fg)
   ;; tooltip
   (tooltip              :background (doom-darken bg-alt 0.2) :foreground fg)

   (ivy-posframe-border :background ui3)

   ;; org
   (org-level-1 :foreground blue :background ui2 :height 1.25 :weight 'bold)
   (org-level-3 :foreground dark-blue))) ;; this is ff0000 from something above, dunno what

;;; doom-snazzy-theme.el ends here
