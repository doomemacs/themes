;;; doom-ayu-light-theme.el --- inspired by Atom One Dark -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-ayu-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ayu-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ayu-light-theme
  :type 'boolean)

(defcustom doom-ayu-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-light-theme
  :type 'boolean)

(defcustom doom-ayu-light-comment-bg doom-ayu-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-light-theme
  :type 'boolean)

(defcustom doom-ayu-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-light-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-ayu-light
  "A light theme inspired by Ayu Light"


  ;; name        default   256       16
  (

   ;; common
   (common-accent   '("#ff9940" "orange"  "orange" ))
   (common-bg       '("#fafafa" "black"   "black"  ))
   (common-fg       '("#575f66" "grey"    "grey"   ))
   (common-ui       '("#ba9199" "grey"    "grey"   ))
   (test '("#7399e6" "grey" "grey"))
   ;; syntax
   (syntax-tag      '("#55b4d4" "cyan"    "blue"   ))
   (syntax-func     '("#f2ae49" "yellow"  "yellow" ))
   (syntax-entity   '("#399ee6" "blue"    "blue"   ))
   (syntax-string   '("#86b300" "green"   "green"  ))
   (syntax-regexp   '("#4cbf99" "teal"    "green"  ))
   (syntax-markup   '("#f07171" "red"     "red"    ))
   (syntax-keyword  '("#fa8d3e" "orange"  "orange" ))
   (syntax-special  '("#ffe6b3" "yellow"  "yellow" ))
   (syntax-comment  '("#abb0b6" "grey"    "grey"   ))
   (syntax-constant '("#a37acc" "magenta" "purple" ))
   (syntax-operator '("#ed9366" "orange"  "orange" ))
   (syntax-error    '("#f51818" "red"     "red"    ))
   ;; ui
   (ui-line               (doom-lighten common-bg 0.1))
   ;; (ui-panel-bg           '((doom-lighten common-bg 0.35) nil nil            ))
   (ui-panel-shadow       (doom-darken common-fg 0.25))
   (ui-panel-border       (doom-darken common-bg 0.2))
   (ui-gutter-normal      (doom-lighten common-ui 0.4)) ;; alpha replacement
   (ui-gutter-active      (doom-lighten common-ui 0.8)) ;; alpha replacement
   (ui-selection-bg       '("#d1e4f4" "cyan" "blue")) ;; fade replacement
   (ui-selection-inactive (doom-lighten '("#000000" nil nil) 0.87)) ;; fade replacement
   (ui-selection-border   (doom-lighten '("#000000" nil nil) 0.82)) ;; fade replacement
   (ui-guide-active       (doom-lighten common-ui 0.35)) ;; alpha replacement
   (ui-guide-normal       (doom-lighten common-ui 0.18)) ;; alpha replacement
   ;; vcs
   (vcs-added    '("#99bf4d" "green" "green" ))
   (vcs-modified '("#709ecc" "blue"  "blue"  ))
   (vcs-removed  '("#f27983" "red"   "red"   ))

   (bg         common-bg)
   (bg-alt     common-bg)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-selection-inactive)
   (base4      ui-selection-border)
   (base5      ui-guide-active)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         common-fg)
   (fg-alt     common-ui)

   (grey       syntax-comment)
   (red        syntax-markup)
   (orange     syntax-keyword)
   (green      syntax-string)
   (teal       syntax-regexp)
   (yellow     syntax-func)
   (blue       syntax-entity)
   (dark-blue  (doom-lighten syntax-entity 1))
   (magenta    syntax-constant)
   (violet     (doom-darken syntax-constant 1))
   (cyan       syntax-tag)
   (dark-cyan  (doom-lighten syntax-tag 1))

   ;; face categories -- required for all themes
   (highlight      common-accent)
   (vertical-bar   ui-panel-border)
   (selection      ui-selection-inactive)
   (builtin        syntax-func)
   (comments       (if doom-ayu-light-brighter-comments syntax-comment syntax-comment))
   (doc-comments   (if doom-ayu-light-brighter-comments syntax-comment syntax-comment))
   (constants      syntax-constant)
   (functions      syntax-func)
   (keywords       syntax-keyword)
   (methods        syntax-func)
   (operators      syntax-operator)
   (type           syntax-special)
   (strings        syntax-string)
   (variables      common-fg)
   (numbers        syntax-func)
   (region         `(,(car base2) ,(car base1)))
   (error          syntax-error)
   (warning        yellow)
   (success        green)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-ayu-light-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-light-padded-modeline
      (if (integerp doom-ayu-light-padded-modeline) doom-ayu-light-padded-modeline 4)))

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
    :background (if doom-ayu-light-comment-bg (doom-lighten bg 0.05)))
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

   ;; LaTeX-mode
   (font-latex-math-face :foreground green)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten common-bg 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (rjsx-attr :foreground syntax-func)
   (js2-function-param :foreground syntax-constant)
  )
  ;; --- extra variables ---------------------
  ()
)

;;; doom-ayu-light-theme.el ends here
