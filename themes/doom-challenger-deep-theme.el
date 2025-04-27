;;; doom-challenger-deep-theme.el --- inspired by Vim's Challenger Deep theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: April 5, 2016 (#163)
;; Author: fuxialexander <https://github.com/fuxialexander>
;; Maintainer:
;; Source: https://github.com/challenger-deep-theme/vim
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-challenger-deep-theme nil
  "Options for the `doom-challenger-deep' theme."
  :group 'doom-themes)

(defcustom doom-challenger-deep-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-challenger-deep-theme
  :type 'boolean)

(defcustom doom-challenger-deep-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-challenger-deep-theme
  :type 'boolean)

(defcustom doom-challenger-deep-comment-bg doom-challenger-deep-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-challenger-deep-theme
  :type 'boolean)

(defcustom doom-challenger-deep-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-challenger-deep-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-challenger-deep
  "A dark theme inspired by VIM Challenger Deep"

  ;; name        default   256       16
  ((bg         '("#1E1C31" "#121212" "black"        ))
   (bg-alt     '("#12111E" "#111111" "black"        ))
   (base0      '("#100E23" "#080808" "black"        ))
   (base1      '("#292F37" "#262626" "brightblack"  ))
   (base2      '("#3D4551" "#3A3A3A" "brightblack"  ))
   (base3      '("#4C4B68" "#444466" "brightblack"  ))
   (base4      '("#565575" "#555577" "brightblack"  ))
   (base5      '("#858FA5" "#8888AA" "brightblack"  ))
   (base6      '("#9BA7BF" "#99AABB" "brightblack"  ))
   (base7      '("#B0BED8" "#BBBBDD" "brightblack"  ))
   (base8      '("#BAC9E4" "#BBCCEE" "white"        ))
   (fg-alt     '("#B2B2B2" "#BBBBBB" "brightwhite"  ))
   (fg         '("#CBE3E7" "#CCEEEE" "white"        ))

   (grey       base4)
   (red        '("#FF8080" "#FF8888" "red"          ))
   (orange     '("#FFB378" "#FFBB77" "brightred"    ))
   (green      '("#95FFA4" "#99FFAA" "green"        ))
   (teal       '("#63F2F1" "#66FFFF" "brightgreen"  ))
   (yellow     '("#FFE9AA" "#FFEEAA" "yellow"       ))
   (blue       '("#91DDFF" "#99DDFF" "brightblue"   ))
   (dark-blue  '("#65B2FF" "#66BBFF" "blue"         ))
   (magenta    '("#C991E1" "#CC99EE" "magenta"      ))
   (violet     '("#906CFF" "#9966FF" "brightmagenta"))
   (cyan       '("#AAFFE4" "#AAFFEE" "brightcyan"   ))
   (dark-cyan  '("#62D196" "#66DD99" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar base1)
   (selection      violet)
   (builtin        magenta)
   (comments       (if doom-challenger-deep-brighter-comments dark-blue base4))
   (doc-comments   (if doom-challenger-deep-brighter-comments (doom-darken dark-cyan 0.3) base5) )
   (constants      cyan)
   (functions      magenta)
   (keywords       red)
   (methods        magenta)
   (operators      teal)
   (type           blue)
   (strings        yellow)
   (variables      yellow)
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-challenger-deep-brighter-modeline)
   (-modeline-pad
    (when doom-challenger-deep-padded-modeline
      (if (integerp doom-challenger-deep-padded-modeline) doom-challenger-deep-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-challenger-deep-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   ((secondary-selection &override) :background base0)
   (tooltip :background base0 :foreground fg)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue :background 'unspecified)
   ;;;; org <built-in>
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :background base1 :foreground comments)
   (org-hide :foreground hidden)
   (org-link :foreground orange :underline t :weight 'bold)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-challenger-deep-theme.el ends here
