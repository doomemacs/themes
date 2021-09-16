;;; doom-ayu-light-theme.el --- inspired by Ayu Mirage -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'doom-themes)


;;
;; Variables

(defgroup doom-ayu-mirage-theme nil
  "Options for the `doom-ayu-mirage' theme."
  :group 'doom-themes)

(defcustom doom-ayu-mirage-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-comment-bg doom-ayu-mirage-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-mirage-theme
  :type 'boolean)

(defcustom doom-ayu-mirage-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-mirage-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-ayu-mirage
  "A dark theme inspired by Ayu Mirage"

  ;; name        default   256       16
  (
   ;; common
   (common-accent   '("#ffcc66" "orange"  "orange" ))
   (common-bg       '("#1f2430" "black"   "black"  ))
   (common-fg       '("#cbccc6" "grey"    "grey"   ))
   (common-ui       '("#707a8c" "grey"    "grey"   ))
   (test            '("#7399e6" "grey"    "grey"   ))
   ;; syntax
   (syntax-tag      '("#5ccfe6" "cyan"    "blue"   ))
   (syntax-func     '("#ffd580" "yellow"  "yellow" ))
   (syntax-entity   '("#73d0ff" "blue"    "blue"   ))
   (syntax-string   '("#bae67e" "green"   "green"  ))
   (syntax-regexp   '("#95e6cb" "teal"    "green"  ))
   (syntax-markup   '("#f28779" "red"     "red"    ))
   (syntax-keyword  '("#ffa759" "orange"  "orange" ))
   (syntax-special  '("#ffe6b3" "yellow"  "yellow" ))
   (syntax-comment  '("#5c6773" "grey"    "grey"   ))
   (syntax-constant '("#d4bfff" "magenta" "purple" ))
   (syntax-operator '("#f29e74" "orange"  "orange" ))
   (syntax-error    '("#ff3333" "red"     "red"    ))
   ;; ui
   (ui-line               (doom-darken common-bg 0.25))
   (ui-panel-shadow       (doom-darken common-bg 0.35))
   (ui-panel-border       (doom-darken common-bg 0.45))
   (ui-gutter-normal      (doom-darken common-ui 0.45))
   (ui-gutter-active      common-ui)
   (ui-selection-bg       (doom-blend common-bg test 0.8))
   (ui-selection-inactive (doom-lighten test 0.93))
   (ui-selection-border   (doom-lighten test 0.93))
   (ui-guide-normal       (doom-darken common-ui 0.35))
   (ui-guide-active       (doom-darken common-ui 0.75))
   (ui-org-block          (doom-darken common-bg 0.10))
   (elscreen-bg           (doom-darken common-ui 0.55))
   (elscreen-fg           ui-line)
   ;; vcs
   (vcs-added    '("#a6cc70" "green" "green" ))
   (vcs-modified '("#77a8d9" "blue"  "blue"  ))
   (vcs-removed  '("#f27983" "red"   "red"   ))

   (bg         common-bg)
   (bg-alt     ui-line)
   (base0      ui-gutter-normal)
   (base1      ui-gutter-active)
   (base2      ui-selection-bg)
   (base3      ui-org-block)
   (base4      ui-selection-border)
   (base5      ui-guide-normal)
   (base6      ui-guide-normal)
   (base7      ui-panel-shadow)
   (base8      ui-panel-border)
   (fg         common-fg)
   (fg-alt     common-ui)

   (grey       ui-line)
   (red        syntax-markup)
   (orange     syntax-keyword)
   (green      syntax-string)
   (teal       syntax-regexp)
   (yellow     syntax-func)
   (blue       syntax-entity)
   (dark-blue  (doom-darken syntax-entity 0.2))
   (magenta    syntax-constant)
   (violet     (doom-lighten syntax-constant 0.2))
   (cyan       syntax-tag)
   (dark-cyan  test)

   ;; face categories -- required for all themes
   (highlight      common-accent)
   (vertical-bar   ui-panel-border)
   (selection      nil)
   (builtin        nil)
   (comments       (if doom-ayu-mirage-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-ayu-mirage-brighter-comments dark-cyan base5) 0.25))
   (constants      syntax-constant)
   (functions      syntax-func)
   (keywords       syntax-keyword)
   (methods        syntax-func)
   (operators      syntax-operator)
   (type           syntax-special)
   (strings        syntax-string)
   (variables      common-fg)
   (numbers        syntax-func)
   (region         ui-selection-bg)
   (error          syntax-error)
   (warning        yellow)
   (success        green)
   (vc-modified    vcs-modified)
   (vc-added       vcs-added)
   (vc-deleted     vcs-removed)

   ;; custom categories
   (hidden     (car bg))
   (-modeline-bright doom-ayu-mirage-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-mirage-padded-modeline
      (if (integerp doom-ayu-mirage-padded-modeline) doom-ayu-mirage-padded-modeline 4)))

   (modeline-fg     common-fg)
   (modeline-fg-alt common-accent)

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
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (diff-removed :foreground vcs-removed)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-ayu-mirage-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; company
   (company-tooltip :foreground common-fg :background common-bg)
   (company-tooltip-annotation :foreground common-fg)
   (company-tooltip-selection :background ui-line)
   (company-tooltip-search :foreground common-accent :weight 'bold)
   (company-scrollbar-bg :background common-bg)
   (company-scrollbar-fg :background syntax-comment)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg modeline-bg) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground green :weight 'normal)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background elscreen-bg :foreground elscreen-fg)
   ;;;; ivy
   (ivy-current-match :background common-bg)
   (ivy-minibuffer-match-face-1 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground common-accent :weight 'bold)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten common-bg 0.05))
   ;;;; org-mode
   (org-hide :foreground hidden)
   (org-headline-done :foreground syntax-comment)
   (org-document-info-keyword :foreground comments)
   ;;;; mic-paren
   ((paren-face-match &override) :foreground fg :background ui-selection-bg :weight 'ultra-bold)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-darken cyan 0.5))
   (rjsx-attr :foreground syntax-func)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; web-mode
   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground (doom-darken cyan 0.5))
   (web-mode-html-attr-name-face :foreground syntax-func)))

;;; doom-ayu-mirage-theme.el ends here
