;;; doom-Iosvkem-theme.el --- Inspired by VIM Iosvkem -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-Iosvkem-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-Iosvkem-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-Iosvkem-theme
  :type 'boolean)

(defcustom doom-Iosvkem-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-Iosvkem-theme
  :type 'boolean)

(defcustom doom-Iosvkem-comment-bg doom-Iosvkem-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-Iosvkem-theme
  :type 'boolean)

(defcustom doom-Iosvkem-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-Iosvkem-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-Iosvkem
  "A dark theme inspired by VIM Iosvkem"

  ;; name        default   256       16
  ((bg         '("#1b1d1e" "#1b1d1e" nil))
   (bg-alt     '("#262829" "#262829" nil))
   (base0      '("#1b1d1e" "#1b1d1e" "black"))
   (base1      '("#202020" "#202020" "brightblack"))
   (base2      '("#303030" "#303030" "brightblack"))
   (base3      '("#303030" "#303030" "brightblack"))
   (base4      '("#505050" "#505050" "brightblack"))
   (base5      '("#505050" "#505050" "brightblack"))
   (base6      '("#808080" "#808080" "brightblack"))
   (base7      '("#808080" "#808080" "brightblack"))
   (base8      '("#DFDFDF" "#dfdfdf" "white"))
   (fg         '("#dddddd" "#dddddd" "white"))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"))

   (grey       base4)
   (red        '("#d02b61" "#d02b61" "red"))
   (orange     '("#da8548" "#dd8844" "brightred"))
   (green      '("#60aa00" "#60aa00" "green"))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"))
   (yellow     '("#d08928" "#d08928" "yellow"))
   (blue       '("#6c9ef8" "#6c9ef8" "brightblue"))
   (dark-blue  '("#6688aa" "#6688aa" "blue"))
   (magenta    '("#b77fdb" "#b77fdb" "magenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#00aa80" "#00aa80" "brightcyan"))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"))
   (urlblue    '("#57aadd" "#57aadd" "blue"))
   (iolime     '("#bbfc20" "#bbfc20" "green"))
   (iopurple   '("#bb20fc" "#bb20fc" "magenta"))
   (iocyan     '("#20bbfc" "#20bbfc" "cyan"))
   (iopink     '("#fc20bb" "#fc20bb" "red"))
   (ioteal     '("#20fcbb" "#20fcbb" "brightgreen"))

   ;; face categories -- required for all themes
   (highlight      iopink)
   (vertical-bar   base2)
   (selection      bg-alt)
   (builtin        magenta)
   (comments       (if doom-Iosvkem-brighter-comments dark-cyan base6))
   (doc-comments   (doom-lighten (if doom-Iosvkem-brighter-comments dark-cyan base6) 0.25))
   (constants      green)
   (functions      magenta)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           cyan)
   (strings        yellow)
   (variables      dark-cyan)
   (numbers        green)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-Iosvkem-brighter-modeline)
   (-modeline-pad
    (when doom-Iosvkem-padded-modeline
      (if (integerp doom-Iosvkem-padded-modeline) doom-Iosvkem-padded-modeline 4)))

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

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

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

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   (markdown-markup-face :foreground red)
   (markdown-header-face :inherit 'bold :foreground fg)
   (markdown-header-delimiter-face :inherit 'bold :foreground red)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; Iosvkem
   (font-lock-comment-face
    :foreground comments
    :background (if doom-Iosvkem-comment-bg (doom-lighten bg 0.05))
    :slant 'italic)

   (font-lock-function-name-face
    :foreground functions
    :weight 'bold)

   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments
    :slant 'normal)

   (org-link :foreground urlblue :underline t)

   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground magenta)
   ((outline-3 &override) :foreground dark-cyan)
   ((outline-6 &override) :foreground (doom-lighten dark-cyan 0.2))
   ((outline-7 &override) :foreground (doom-lighten blue 0.4))
   ((outline-8 &override) :foreground (doom-lighten magenta 0.4))
   ((org-block &override) :background bg-alt)
   ((org-quote &override) :background bg-alt)
   ((org-block-begin-line &override) :foreground comments :background bg)

   ((paren-face-match &override) :foreground iopink :background bg :weight 'ultra-bold)

   ((nav-flash-face &override) :background bg-alt :foreground iopink)

   ((highlight-numbers-number &override) :inherit 'normal :foreground numbers)

   ((hes-escape-backslash-face &override) :inherit 'normal :foreground red)
   ((hes-escape-sequence-face &override) :inherit 'normal :foreground red)

   ((js2-function-param &override) :foreground fg :slant 'italic)
   ((js2-object-property &override) :foreground fg)

   ((whitespace-tab &override) :background bg)

   ((line-number-current-line &override) :foreground iocyan :background bg)

   ((markdown-link-face &override) :foreground blue)
   ((markdown-url-face &override) :foreground base5)
   ((markdown-list-face &override) :foreground magenta)
   ((markdown-italic-face &override) :foreground cyan)
   ((markdown-bold-face &override) :foreground cyan)

   ((tooltip &override) :background bg)
   ((company-tooltip-selection &override) :foreground iopink)

   ((lazy-highlight &override) :background iocyan  :foreground bg :distant-foreground base0 :weight 'bold)))
;; --- extra variables ---------------------
;; ()


;;; doom-Iosvkem-theme.el ends here
