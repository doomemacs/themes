;;; doom-oceanic-next-theme.el --- inspired by Oceanic Next -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-oceanic-next-theme nil
  "Options for the `doom-oceanic-next' theme."
  :group 'doom-themes)

(defcustom doom-oceanic-next-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-oceanic-next-theme
  :type 'boolean)

(defcustom doom-oceanic-next-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-oceanic-next-theme
  :type 'boolean)

(defcustom doom-oceanic-next-comment-bg doom-oceanic-next-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-oceanic-next-theme
  :type 'boolean)

(defcustom doom-oceanic-next-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-oceanic-next-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-oceanic-next
  "A dark theme inspired by Oceanic Next "

  ;; name        default   256       16
  ((bg         '("#1B2B34" nil       nil            ))
   (bg-alt     '("#14232D" nil       nil            ))
   (base0      '("#1B2B34" "black"   "black"        ))
   (base1      '("#343D46" "#1e1e1e" "brightblack"  ))
   (base2      '("#4F5B66" "#2e2e2e" "brightblack"  ))
   (base3      '("#65737E" "#262626" "brightblack"  ))
   (base4      '("#A7ADBA" "#3f3f3f" "brightblack"  ))
   (base5      '("#C0C5CE" "#525252" "brightblack"  ))
   (base6      '("#CDD3DE" "#6b6b6b" "brightblack"  ))
   (base7      '("#D8DEE9" "#979797" "white"        ))
   (base8      base7)
   (fg-alt     base6)
   (fg         base8)

   (grey       base4)
   (red        '("#EC5f67" "#EC5f67" "red"          ))
   (orange     '("#F99157" "#F99157" "brightred"    ))
   (green      '("#99C794" "#99bb66" "green"        ))
   (teal       '("#5FB3B3" "#44b9b1" "brightgreen"  ))
   (yellow     '("#FAC863" "#ECBE7B" "yellow"       ))
   (blue       '("#6699CC" "#51afef" "brightblue"   ))
   (dark-blue  blue)
   (magenta    '("#E27E8D" "#c678dd" "magenta"      ))
   (violet     '("#C594C5" "#a9a1e1" "brightmagenta"))
   (cyan       teal)
   (dark-cyan  cyan)

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      base2)
   (builtin        red)
   (comments       (if doom-oceanic-next-brighter-comments dark-cyan base3))
   (doc-comments   (doom-lighten (if doom-oceanic-next-brighter-comments dark-cyan base3) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      teal)
   (type           yellow)
   (strings        green)
   (variables      orange)
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
   (-modeline-bright doom-oceanic-next-brighter-modeline)
   (-modeline-pad
    (when doom-oceanic-next-padded-modeline
      (if (integerp doom-oceanic-next-padded-modeline) doom-oceanic-next-padded-modeline 4)))

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
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt))))


  ;;;; Base theme face overrides
  (

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-oceanic-next-comment-bg (doom-lighten bg 0.05)))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   
   ;;;; VCS/magit readability
   (diff-refine-removed :foreground vc-deleted :background bg :inverse-video t)
   (diff-refine-added :foreground vc-added :background bg :inverse-video t)

   (magit-diff-removed-highlight :foreground vc-deleted :background base1 :weight 'bold)

   (magit-diff-base :foreground vc-modified :background bg-alt)
   (magit-diff-removed :foreground vc-deleted :background base1)
   (magit-diff-added :foreground vc-added :background base1)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground blue)
   ((markdown-code-face &override) :background (doom-lighten bg 0.05))
   ;;;; ivy
   (ivy-current-match :background base2 :distant-foreground base0 :weight 'bold)
   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg-alt)
   ((org-block-end-line &override) :background bg-alt)
   (org-hide :foreground hidden)
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
  ()
  )

;;; doom-oceanic-next-theme.el ends here
