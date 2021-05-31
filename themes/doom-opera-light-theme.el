;;; doom-opera-light-theme.el --- Opera-Light theme -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'doom-themes)

(defgroup doom-opera-light-theme nil
  "Options for the `doom-opera-light' theme."
  :group 'doom-themes)

(defcustom doom-opera-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-opera-light-theme
  :type 'boolean)

(defcustom doom-opera-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-opera-light-theme
  :type 'boolean)

(defcustom doom-opera-light-comment-bg doom-opera-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-opera-light-theme
  :type 'boolean)

(defcustom doom-opera-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-opera-light-theme
  :type '(choice integer boolean))

(defcustom doom-opera-light-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-opera-light-theme
  :type 'symbol)

(def-doom-theme doom-opera-light
  "A light Opera theme."

  ;; name        default   256       16
  ((bg         '("#fafafa" nil       nil ))
   (bg-alt     '("#eeeeee" nil       nil ))
   (base0      '("#fafafa" "#dfdfdf" nil ))
   (base1      '("#f5f5f5" "#979797" nil ))
   (base2      '("#eeeeee" "#6b6b6b" nil ))
   (base3      '("#e0e0e0" "#525252" nil ))
   (base4      '("#bdbdbd" "#3f3f3f" nil ))
   (base5      '("#9e9e9e" "#262626" nil ))
   (base6      '("#757575" "#2e2e2e" nil ))
   (base7      '("#616161" "#1e1e1e" nil ))
   (base8      '("#424242" "black"   nil ))
   (fg         '("#2a2a2a" "#2a2a2a" nil ))
   (fg-alt     '("#454545" "#757575" nil ))

   (grey       base4)
   (red        '("#99324b" "#ff6655" nil ))
   (orange     '("#ac4426" "#dd8844" nil ))
   (green      '("#4f894c" "#99bb66" nil ))
   (teal       '("#29838d" "#44b9b1" nil ))
   (yellow     '("#9a7500" "#ECBE7B" nil ))
   (blue       '("#3b6ea8" "#51afef" nil ))
   (dark-blue  '("#5272AF" "#2257A0" nil ))
   (magenta    '("#97365b" "#c678dd" nil ))
   (violet     '("#842879" "#a9a1e1" nil ))
   (cyan       '("#398eac" "#46D9FF" nil ))
   (dark-cyan  '("#2c7088" "#5699AF" nil ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        teal)
   (comments       (if doom-opera-light-brighter-comments dark-cyan (doom-lighten base5 0.2)))
   (doc-comments   (doom-lighten (if doom-opera-light-brighter-comments dark-cyan base5) 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.5))
   (numbers        magenta)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-opera-light-brighter-modeline)
   (-modeline-pad
    (when doom-opera-light-padded-modeline
      (if (integerp doom-opera-light-padded-modeline) doom-opera-light-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg-alt 0.1)))

  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if doom-opera-light-comment-bg (doom-lighten bg 0.05)))
   (lazy-highlight :background (doom-blend bg highlight 0.7) :weight 'bold)
   ((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; ivy
   (ivy-current-match :background base3)
   ;;;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-opera-light-theme.el ends here
