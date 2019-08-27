;;; doom-ewal-theme.el --- Themes ain't clorschemes -*- lexical-binding: t; -*-

(require 'doom-themes)
(require 'ewal)

(defgroup doom-ewal-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ewal-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ewal-theme
  :type 'boolean)

(defcustom doom-ewal-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ewal-theme
  :type 'boolean)

(defcustom doom-ewal-comment-bg doom-ewal-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ewal-theme
  :type 'boolean)

(defcustom doom-ewal-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ewal-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-ewal
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         `(,(ewal-get-color 'background  0) nil nil))
   (bg-alt     `(,(ewal-get-color 'background -1) nil nil))
   (base0      `(,(ewal-get-color 'background -2) nil nil))
   (base1      `(,(ewal-get-color 'background -3) nil nil))
   (base2      `(,(ewal-get-color 'background -4) nil nil))
   (base3      `(,(ewal-get-color 'background -5) nil nil))
   (base4      `(,(ewal-get-color 'background +1) nil nil))
   (base5      `(,(ewal-get-color 'background +2) nil nil))
   (base6      `(,(ewal-get-color 'background +3) nil nil))
   (base7      `(,(ewal-get-color 'background +4) nil nil))
   (base8      `(,(ewal-get-color 'background +5) nil nil))
   (fg         `(,(ewal-get-color 'foreground  0) nil nil))
   (fg-alt     `(,(ewal-get-color 'foreground -1) nil nil))

   (grey       base4)
   (red        `(,(ewal-get-color 'red      0) nil nil))
   (orange     `(,(ewal-get-color 'red     +2) nil nil))
   (green      `(,(ewal-get-color 'green    0) nil nil))
   (teal       `(,(ewal-get-color 'green   +2) nil nil))
   (yellow     `(,(ewal-get-color 'yellow   0) nil nil))
   (blue       `(,(ewal-get-color 'blue     0) nil nil))
   (dark-blue  `(,(ewal-get-color 'blue    +2) nil nil))
   (magenta    `(,(ewal-get-color 'magenta  0) nil nil))
   (violet     `(,(ewal-get-color 'magenta +2) nil nil))
   (cyan       `(,(ewal-get-color 'cyan     0) nil nil))
   (dark-cyan  `(,(ewal-get-color 'cyan    -2) nil nil))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-ewal-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-ewal-brighter-comments dark-cyan base5) 0.25))
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
   (-modeline-bright doom-ewal-brighter-modeline)
   (-modeline-pad
    (when doom-ewal-padded-modeline
      (if (integerp doom-ewal-padded-modeline) doom-ewal-padded-modeline 4)))

   (modeline-fg     nil)
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
    :background (if doom-ewal-comment-bg (doom-lighten bg 0.05)))
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
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-ewal-theme.el ends here
