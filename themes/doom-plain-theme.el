;;; doom-plain-theme.el --- inspired by gko's plain theme for VSCode

(require 'doom-themes)

(defgroup doom-plain-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-plain-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-plain-theme
  :type 'boolean)

(defcustom doom-plain-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-plain-theme
  :type '(or integer boolean))

(def-doom-theme doom-plain
  "Theme inspired by gko's plain."

  ;; name        default   256       16
  ((bg         '("#ffffff" nil       nil ))
   (bg-alt     '("#f3f3f3" nil       nil ))
   (base0      '("#969896" nil nil ))
   (base1      '("#f1f3f5" nil nil ))
   (base2      '("#444444" nil nil ))
   (base3      '("#cccccc" nil nil ))
   (base4      '("#e7e7e7" nil nil ))
   (base5      '("#c5c8c6" nil nil ))
   (base6      '("#fafafa" nil nil ))
   (base7      '("#dfdfdf" nil nil ))
   (base8      '("#fafafa" nil nil ))
   (fg         '("#282a2e" nil nil ))
   (fg-alt     (doom-lighten fg 0.15))

   (grey       fg)
   (red        fg)
   (blue       fg)
   (dark-blue  fg)
   (orange     fg)
   (green      fg)
   (teal       fg)
   (yellow     fg)
   (magenta    fg)
   (violet     fg)
   (cyan       fg)
   (dark-cyan  fg)

   ;; face categories -- required for all themes
   (highlight      base2)
   (vertical-bar   (doom-lighten fg 0.3))
   (selection      base1)
   (builtin        base0)
   (comments       base5)
   (doc-comments   base5)
   (constants      base0)
   (functions      fg)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        base0)
   (variables      base0)
   (numbers        base0)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg)
   (vc-added       (doom-lighten fg 0.6))
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-plain-brighter-modeline)
   (-modeline-pad
    (when doom-plain-padded-modeline
      (if (integerp doom-plain-padded-modeline) doom-plain-padded-modeline 4)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1)))
   (modeline-fg     nil)
   (modeline-fg-alt (doom-darken modeline-bg-inactive 0.35)))

  ;;;; Base theme face overrides
  (((font-lock-constant-face &override)      :slant 'italic)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-function-name-face &override) :slant 'italic)
   ((font-lock-type-face &override)          :slant 'italic)
   (hl-line :background base8)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)
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
   (ivy-posframe :background bg-alt)
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   ;;;; outline <built-in>
   (outline-1 :slant 'italic :foreground fg-alt)
   (outline-2 :inherit 'outline-1 :foreground base2)
   (outline-3 :inherit 'outline-2)
   (outline-4 :inherit 'outline-3)
   (outline-5 :inherit 'outline-4)
   (outline-6 :inherit 'outline-5)
   (outline-7 :inherit 'outline-6)
   (outline-8 :inherit 'outline-7)
   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :foreground base5)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-plain-theme.el ends here
