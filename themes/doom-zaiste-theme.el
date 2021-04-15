;;; doom-zaiste-theme.el --- Zaiste theme -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(def-doom-theme doom-zaiste
  "A light theme inspired by Bluloco, Ayu, Doom One Light"

  ;; name        default   256       16
  (
   ;; common
   (common-accent   '("#0098dd" "blue"  "blue" ))
   (common-bg       '("#fafafa" "black"   "black"  ))
   (common-fg       '("#575f66" "grey"    "grey"   ))
   (common-ui       '("#383a42" "grey"    "grey"   ))
   (brand           '("#2ea8e6" "grey"    "grey"   ))

   (lightviolet     '("#a37acc" "magenta" "magenta" ))

   ;; syntax
   (syntax-tag      '("#55b4d4" "cyan"    "cyan"   ))
   (syntax-func     '("#C838C6" "yellow"  "yellow" ))
   (syntax-entity   '("#399ee6" "blue"    "blue"   ))
   (syntax-string   '("#86b300" "green"   "green"  ))
   (syntax-regexp   '("#4cbf99" "teal"    "teal"  ))
   (syntax-markup   '("#f07171" "red"     "red"    ))
   (syntax-keyword  '("#0098dd" "brightblue"    "brightblue" ))
   (syntax-special  '("#d52753" "yellow"  "yellow" ))
   (syntax-comment  '("#abb0b6" "grey"    "grey"   ))
   (syntax-constant '("#a37acc" "magenta" "magenta" ))
   (syntax-operator '("#7a82da" "purple"  "purple" ))
   (syntax-error    '("#f51818" "red"     "red"    ))
   ;; ui
   (ui-line               (doom-darken common-bg 0.07))
   (ui-panel-shadow       (doom-lighten common-bg 0.35))
   (ui-panel-border       (doom-lighten common-bg 0.45))
   (ui-gutter-normal      (doom-lighten common-ui 0.45))
   (ui-gutter-active      common-ui)
   (ui-selection-bg       (doom-blend common-bg brand 0.7))
   (ui-selection-inactive (doom-lighten brand 0.8))
   (ui-selection-border   (doom-lighten common-fg 0.8))
   (ui-guide-active       (doom-lighten common-ui 0.75))
   (ui-guide-normal       (doom-lighten common-ui 0.35))
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
   (dark-blue  (doom-darken syntax-entity 0.2))
   (magenta    syntax-constant)
   (violet     (doom-lighten syntax-constant 0.2))
   (cyan       syntax-tag)
   (dark-cyan  (doom-darken syntax-tag 0.2))

   ;; face categories -- required for all themes
   (highlight      common-accent)
   (vertical-bar   ui-panel-border)
   (selection      ui-selection-inactive)
   (builtin        syntax-func)
   (comments       syntax-comment )
   (doc-comments   syntax-comment)
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
   (-modeline-bright nil)
   (-modeline-pad 4)

   (modeline-fg     common-ui)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten blue 0.475)
      `(,(doom-lighten (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten blue 0.45)
      `(,(doom-lighten (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-lighten (car bg) 0.1) ,@(cdr bg)))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (doom-lighten bg 0.05))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   ;; (solaire-mode-line-face
   ;;  :inherit 'mode-line
   ;;  :background modeline-bg-l
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   ;; (solaire-mode-line-inactive-face
   ;;  :inherit 'mode-line-inactive
   ;;  :background modeline-bg-inactive-l
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight) :weight 'normal)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'normal)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'normal)
   (doom-modeline-buffer-project-root :foreground blue :weight 'normal)
   (doom-modeline-project-dir :foreground blue :weight 'normal)

   ;; ivy-mode
   (ivy-current-match :background ui-line)
   (ivy-minibuffer-match-face-1 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground common-accent :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground common-accent :weight 'bold)

   ;; --- major-mode faces -------------------

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten common-bg 0.05))

   ;; org-mode
   (org-document-title :foreground (doom-darken common-fg 0.1) :weight 'bold :height 1.5)
   (org-document-info-keyword :foreground common-bg :height 0.2)
   (org-level-1 :foreground (doom-darken common-fg 0.1) :weight 'bold :height 1.25)
   (org-level-2 :foreground (doom-darken common-fg 0.1) :weight 'bold :height 1.1)
   (org-level-3 :foreground (doom-darken common-fg 0.1) :weight 'bold :height 1.0)
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)
   (org-headline-done :foreground syntax-comment)
   (org-block-begin-line
    :background (doom-darken common-bg 0.02)
    :foreground (doom-darken common-bg 0.02))
   (org-block
    :background (doom-darken common-bg 0.02))
   (org-code
    :foreground (doom-darken common-fg 0.5)
    :background (doom-darken common-bg 0.02))
   (org-priority-faces '((65 :foreground "#e45649")
                         (66 :foreground "#da8548")
                         (67 :foreground "#0098dd")))

   (js2-object-property :foreground common-fg)

   (rjsx-tag :foreground cyan)
   (rjsx-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (rjsx-attr :foreground syntax-func)

   (web-mode-html-tag-face :foreground cyan)
   (web-mode-html-tag-bracket-face :foreground (doom-lighten cyan 0.5))
   (web-mode-html-attr-name-face :foreground syntax-func)

   (company-tooltip :foreground common-fg :background common-bg)
   (company-tooltip-annotation :foreground common-fg)
   (company-tooltip-selection :background ui-line)
   (company-tooltip-search :foreground common-accent :weight 'bold)
   (company-scrollbar-bg :background common-bg)
   (company-scrollbar-fg :background syntax-comment)

   (highlight-numbers-number :foreground syntax-func :weight 'normal)

   ;; diff-mode
   (diff-removed :foreground vcs-removed)
   )
  )

;;; doom-zaiste-theme.el ends here

(with-eval-after-load 'org
  (setq org-superstar-headline-bullets-list '("⁖")
        org-hide-emphasis-markers t))

(with-eval-after-load 'org-fancy-priorities
  (setq org-fancy-priorities-list '("●" "●" "●")))
