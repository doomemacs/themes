;; doom-gotham.el --- inspired by VS Code Outrun Electric -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-gotham-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-gotham-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-gotham-theme
  :type 'boolean)

(defcustom doom-gotham-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gotham-theme
  :type 'boolean)

(defcustom doom-gotham-comment-bg doom-gotham-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-gotham-theme
  :type 'boolean)

(defcustom doom-gotham-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gotham-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-gotham
  "A very dark and evil emacs theme. Based on doom-outrun-electric and gotham."

  ;; name        default   256       16
  ((bg         '("#0c0a20" "#0c0a20" nil           ))
   (bg-alt     '("#090819" "#090819" nil           ))
   (base0      '("#0c1014" "#131033" "black"       ))
   (base1      '("#11151c" "#161130" "brightblack" ))
   (base2      '("#091f2e" "#110d26" "brightblack" ))
   (base3      '("#1d84ad" "#3b4167" "brightblack" ))
   (base4      '("#245361" "#2d2844" "brightblack" ))
   (base5      '("#599cab" "#BA45A3" "brightblack" ))
   (base6      '("#99d1ce" "#6A6EA3" "brightblack" ))
   (base7      '("#d3ebe9" "#6564D1" "brightblack" ))
   (base8      '("#1d1d8a" "#919ad9" "white"       ))
   (fg-alt     '("#7984D1" "#7984D1" "white"       ))
   (fg         '("#f2f3f7" "#f2f3f7" "brightwhite" ))

   (grey       '("#546A90" "#546A90" "gray"          ))
   (red        '("#c23127" "#e61f44" "red"          ))
   (orange     '("#cf433e" "#ff9b50" "brightred"    ))
   (green      '("#2aa889" "#a7da1e" "green"        ))
   (teal       '("#A875FF" "#A875FF" "brightgreen"  ))
   (yellow     '("#ffd400" "#ffd400" "yellow"       ))
   (blue       '("#1ea8fc" "#1ea8fc" "brightblue"   ))
   (dark-blue  '("#3761a1" "#3F88AD" "blue"         ))
   (magenta    '("#ff2afc" "#ff2afc" "magenta"      ))
   (violet     '("#ee3a8c" "#df85ff" "brightmagenta"))
   (cyan       '("#33859e" "#42c6ff" "brightcyan"   ))
   (dark-cyan  '("#008b8b" "#204052" "cyan"         ))
   (cadet-blue '("#5f9ea0" "#ef9ea0" "brightblue"   ))
   (sky-blue   '("#00b2ee" "#ef9ea0" "blue"         ))
   (deep-blue  '("#4876ff" "#204052" "blue"         ))
   (blood-red  '("#590621" "#e61f44" "red"          ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-gotham-brighter-comments blue grey))
   (doc-comments   green)
   (constants      grey)
   (functions      sky-blue)
   (keywords       dark-blue)
   (methods        deep-blue)
   (operators      teal)
   (type           violet)
   (strings        fg-alt)
   (variables      cadet-blue)
   (numbers        magenta)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-gotham-brighter-modeline)
   (-modeline-pad
    (when doom-gotham-padded-modeline
      (if (integerp doom-gotham-padded-modeline) doom-gotham-padded-modeline 4)))

   (modeline-fg     nil)
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
  (((font-lock-comment-face &override)
    :background (if doom-gotham-comment-bg (doom-lighten bg 0.05)))
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-constant-face &override) :weight 'bold)
   ((font-lock-function-name-face &override) :foreground functions)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (mode-line-highlight :background blood-red :foreground bg :weight 'bold)
   (vertical-border :foreground base5)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background deep-blue)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground magenta)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground magenta)
   ;;;; company
   (company-tooltip-selection :background dark-cyan)
   (company-tooltip-common    :foreground magenta :distant-foreground base0 :weight 'bold)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background blood-red :foreground green)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background blood-red :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground deep-blue)
   ((outline-2 &override) :foreground sky-blue)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :background base0)
   (org-hide              :foreground hidden)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-gotham.el ends here
