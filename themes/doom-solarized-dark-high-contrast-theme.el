;;; doom-solarized-dark-high-contrast-theme.el --- inspired by VS Code Solarized Dark -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-solarized-dark-high-contrast-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-solarized-dark-high-contrast-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-dark-high-contrast-theme
  :type 'boolean)

(defcustom doom-solarized-dark-high-contrast-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-dark-high-contrast-theme
  :type 'boolean)

(defcustom doom-solarized-dark-high-contrast-brighter-text t
  "If non-nil, default text will be brighter."
  :group 'doom-solarized-dark-high-contrast-theme
  :type 'boolean)

(defcustom doom-solarized-dark-high-contrast-comment-bg doom-solarized-dark-high-contrast-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-solarized-dark-high-contrast-theme
  :type 'boolean)

(defcustom doom-solarized-dark-high-contrast-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-solarized-dark-high-contrast-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-solarized-dark-high-contrast
  "A dark theme inspired by VS Code Solarized Dark"

  ;; name        default   256       16
  ((bg         '("#002732" "#002732"       nil     ))
   (bg-alt     '("#00212B" "#00212B"       nil     ))
   (base0      '("#01323d" "#01323d"   "black"     ))
   (base1      '("#03282F" "#03282F" "brightblack" ))
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#13383C" "#13383C" "brightblack" ))
   (base4      '("#56697A" "#56697A" "brightblack" ))
   (base5      '("#62787f" "#62787f" "brightblack" ))
   (base6      '("#96A7A9" "#96A7A9" "brightblack" ))
   (base7      '("#788484" "#788484" "brightblack" ))
   (base8      '("#626C6C" "#626C6C" "white"       ))
   (fg-alt     '("#60767e" "#60767e" "white"       ))
   (fg         '("#8d9fa1" "#8d9fa1" "brightwhite"))

   (grey       base4)
   (red        '("#ec423a" "#ec423a" "red"          ))
   (orange     '("#db5823" "#db5823" "brightred"    ))
   (green      '("#93a61a" "#93a61a" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#c49619" "#c49619" "yellow"       ))
   (blue       '("#3c98e0" "#3c98e0" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#e2468f" "#e2468f" "magenta"      ))
   (violet     '("#7a7ed2" "#7a7ed2" "brightmagenta"))
   (cyan       '("#3cafa5" "#3cafa5" "brightcyan"   ))
   (dark-cyan  '("#03373f" "#03373f" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       base5)
   (doc-comments   teal)
   (constants      blue)
   (functions      blue)
   (keywords       green)
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        green)
   (variables      fg)
   (numbers        violet)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       "#119e44")
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-solarized-dark-high-contrast-brighter-modeline)
   (-modeline-pad
    (when doom-solarized-dark-high-contrast-padded-modeline
      (if (integerp doom-solarized-dark-high-contrast-padded-modeline) doom-solarized-dark-high-contrast-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((company-tooltip-selection     :background dark-cyan)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)

   (font-lock-comment-face
    :slant 'italic
    :foreground comments
    :background (if doom-solarized-dark-high-contrast-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-keyword-face
    :weight 'bold
    :foreground keywords)
   (font-lock-constant-face
    :weight 'bold
    :foreground constants)
   ((font-lock-type-face &override) :slant 'italic)
   ((font-lock-builtin-face &override) :slant 'italic)

   ;;;; Centaur tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
                                          :foreground blue)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
                                            :foreground blue)
   ;;;; Doom modeline
   (doom-modeline-bar :background blue)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (tooltip              :background bg-alt :foreground fg)
   ;;; --- major-mode faces -------------------
   ;;;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground violet)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;;;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;;;; org-mode
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground comments :background base0)
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;;;; rainbow-delimeters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground cyan)
   (rainbow-delimiters-depth-6-face :foreground violet)

   ;;;; git-gutter-fringe
   (git-gutter-fr:modified :foreground vc-modified)

   ;;;; eshell-git-prompt powerline theme
   (eshell-git-prompt-powerline-dir-face :background "steel blue" :foreground bg)
   (eshell-git-prompt-powerline-clean-face :background "foreset green" :foreground bg)
   (eshell-git-prompt-powerline-not-clean-face :background "indian red" :foreground bg)

   ;;;; vterm
   (vterm               :foreground fg)
   (vterm-color-default :foreground fg)
   (vterm-color-black   :background (doom-lighten base0 0.75)   :foreground base0)
   (vterm-color-red     :background (doom-lighten red 0.75)     :foreground red)
   (vterm-color-green   :background (doom-lighten green 0.75)   :foreground green)
   (vterm-color-yellow  :background (doom-lighten yellow 0.75)  :foreground yellow)
   (vterm-color-blue    :background (doom-lighten blue 0.75)    :foreground blue)
   (vterm-color-magenta :background (doom-lighten magenta 0.75) :foreground magenta)
   (vterm-color-cyan    :background (doom-lighten cyan 0.75)    :foreground cyan)
   (vterm-color-white   :background (doom-lighten base8 0.75)   :foreground base8)
   ))


;;; doom-solarized-dark-high-contrast-theme.el ends here
