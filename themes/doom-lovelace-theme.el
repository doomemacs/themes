;; doom-lovelace-theme.el -*- no-byte-compile: t; -*-
(require 'doom-themes)

;; This is basically a copy-pasted doom theme
(defgroup doom-lovelace-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-lovelace-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-lovelace-theme
  :type 'boolean)

(defcustom doom-lovelace-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-lovelace-theme
  :type 'boolean)

(defcustom doom-lovelace-comment-bg doom-lovelace-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-lovelace-theme
  :type 'boolean)

(defcustom doom-lovelace-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-lovelace-theme
  :type '(choice integer boolean))

(def-doom-theme doom-lovelace

  ((bg         `("#1D1F28" nil nil))
   (bg-alt     `("#282A36" nil nil))
   (base0      `(,(doom-lighten "#1D1F28" 0.02) "black" "black"))
   (base1      `(,(doom-lighten "#1D1F28" 0.04) "brightblack"))
   (base2      `(,(doom-lighten "#1D1F28" 0.06) "brightblack"))
   (base3      `("#282A36" "brightblack"))
   (base4      `(,(doom-lighten "#1D1F28" 0.1) "brightblack"))
   (base5      `("#414458" "#525252" "brightblack"))
   (base6      `(,(doom-lighten "#414458" 0.1) "#6b6b6b" "brightblack"))
   (base7      `(,(doom-lighten "#414458" 0.2) "#979797" "brightblack"))
   (base8      `(,(doom-lighten "#414458" 0.3) "#dfdfdf" "white"))
   (fg         `("#FDFDFD" "#bfbfbf" "brightwhite"))
   (fg-alt     `("#BEBEC1" "#2d2d2d" "white"))

   (grey       `("#414458"))
   (red        `("#F37F97" "#ff6655" "red"          ))
   (orange     `("#FF8037" "#dd8844" "brightred"    ))
   (green      `("#5ADECD" "#99bb66" "green"        ))
   (teal       `("#18E3C8" "#44b9b1" "brightgreen"  ))
   (yellow     `("#F2A272" "#ECBE7B" "yellow"       ))
   (blue       `("#8897F4" "#51afef" "brightblue"   ))
   (dark-blue  `("#556FFF" "#2257A0" "blue"         ))
   (magenta    `("#C574DD" "#c678dd" "brightmagenta"))
   (violet     `("#B043D1" "#a9a1e1" "magenta"      ))
   (cyan       `("#79E6F3" "#46D9FF" "brightcyan"   ))
   (dark-cyan  `("#3FDCEE" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-lovelace-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-lovelace-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-lovelace-brighter-modeline)
   (-modeline-pad
    (when doom-lovelace-padded-modeline
      (if (integerp doom-lovelace-padded-modeline) doom-lovelace-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-lovelace-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

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

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-level-1 :weight 'bold :family "sans" :height 1.75 :foreground "#79E6F3")
   (org-level-2 :weight 'bold :family "sans" :height 1.50 :foreground "#8897F4")
   (org-level-3 :weight 'bold :family "sans" :height 1.25 :foreground "#C574DD")
   (org-level-4 :weight 'bold :family "sans" :height 1.10 :foreground "#F2A272")
   (org-level-5 :weight 'bold :family "sans" :foreground "#79E6F3")
   (org-level-6 :weight 'bold :family "sans" :foreground "#8897F4")
   (org-level-7 :weight 'bold :family "sans" :foreground "#C574DD")
   (org-level-8 :weight 'bold :family "sans" :foreground "#F2A272")
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-lovelace-theme.el ends here
