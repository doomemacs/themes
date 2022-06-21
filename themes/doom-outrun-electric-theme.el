;;; doom-outrun-electric-theme.el --- a high contrast, neon theme inspired by Outrun Electric on VSCode -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: July 30, 2019 (c810f6b96648)
;; Author: ema2159 <https://github.com/ema2159>
;; Maintainer:
;; Source: https://github.com/samrap/outrun-theme-vscode
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-outrun-electric-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-outrun-electric-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-outrun-electric-theme
  :type 'boolean)

(defcustom doom-outrun-electric-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-outrun-electric-theme
  :type 'boolean)

(defcustom doom-outrun-electric-comment-bg doom-outrun-electric-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-outrun-electric-theme
  :type 'boolean)

(defcustom doom-outrun-electric-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-outrun-electric-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-outrun-electric
  "A vibrant, neon colored theme inspired by VS Code Outrun Electric."

  ;; name        default   256       16
  ((bg         '("#0c0a20" "#0c0a20" nil           ))
   (bg-alt     '("#090819" "#090819" nil           ))
   (base0      '("#131033" "#131033" "black"       ))
   (base1      '("#1f1147" "#161130" "brightblack" ))
   (base2      '("#110d26" "#110d26" "brightblack" ))
   (base3      '("#3b4167" "#3b4167" "brightblack" ))
   (base4      '("#2d2844" "#2d2844" "brightblack" ))
   (base5      '("#BA45A3" "#BA45A3" "brightblack" ))
   (base6      '("#6A6EA3" "#6A6EA3" "brightblack" ))
   (base7      '("#6564D1" "#6564D1" "brightblack" ))
   (base8      '("#919ad9" "#919ad9" "white"       ))
   (fg-alt     '("#7984D1" "#7984D1" "white"       ))
   (fg         '("#f2f3f7" "#f2f3f7" "brightwhite" ))

   (grey       '("#546A90" "#546A90" "gray"          ))
   (red        '("#e61f44" "#e61f44" "red"          ))
   (orange     '("#cf433e" "#ff9b50" "brightred"    ))
   (green      '("#a7da1e" "#a7da1e" "green"        ))
   (teal       '("#A875FF" "#A875FF" "brightgreen"  ))
   (yellow     '("#ffd400" "#ffd400" "yellow"       ))
   (blue       '("#1ea8fc" "#1ea8fc" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#3F88AD" "blue"         ))
   (magenta    '("#ff2afc" "#ff2afc" "magenta"      ))
   (violet     '("#df85ff" "#df85ff" "brightmagenta"))
   (cyan       '("#42c6ff" "#42c6ff" "brightcyan"   ))
   (dark-cyan  '("#204052" "#204052" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-outrun-electric-brighter-comments blue grey))
   (doc-comments   teal)
   (constants      violet)
   (functions      cyan)
   (keywords       magenta)
   (methods        cyan)
   (operators      magenta)
   (type           yellow)
   (strings        fg-alt)
   (variables      violet)
   (numbers        yellow)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-outrun-electric-brighter-modeline)
   (-modeline-pad
    (when doom-outrun-electric-padded-modeline
      (if (integerp doom-outrun-electric-padded-modeline) doom-outrun-electric-padded-modeline 4)))

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
    :background (if doom-outrun-electric-comment-bg (doom-lighten bg 0.05)))
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
   (mode-line-highlight :background magenta :foreground bg :weight 'bold)
   (vertical-border :foreground base5)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background magenta)
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
   (doom-modeline-bar :background magenta)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
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
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-outrun-electric-theme.el ends here
