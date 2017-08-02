;;; doom-one-light-theme.el --- inspired by Atom One Light
(require 'doom-themes)

;;
(defgroup doom-one-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-one-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-one-light-theme
  :type 'boolean)

(defcustom doom-one-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-one-light-theme
  :type 'boolean)

(defcustom doom-one-light-comment-bg doom-one-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-one-light-theme
  :type 'boolean)

(defcustom doom-one-light-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-one-light-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-one-light
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#fafafa" nil       nil            ))
   (bg-alt     '("#f0f0f0" nil       nil            ))
   (base0      '("#1b2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5b6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#dfdfdf" "#dfdfdf" "white"        ))
   (fg         '("#383a42" "#424242" "black"  ))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"        ))

   (grey       base4)
   (red        '("#e45649" "#e45649" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#50a14f" "#50a14f" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#986801" "#986801" "yellow"       ))
   (blue       '("#4078f2" "#4078f2" "brightblue"   ))
   (dark-blue  '("#a0bcf8" "#a0bcf8" "blue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#b751b6" "#b751b6" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      (doom-lighten blue 0.2))
   (vertical-bar   base2)
   (selection      (doom-darken bg 0.08))
   (builtin        magenta)
   (comments       (if doom-one-light-brighter-comments dark-cyan base5))
   (doc-comments   (doom-darken (if doom-one-light-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-darken magenta 0.36))
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base8) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    base5)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-one-light-brighter-modeline)
   (-modeline-pad
    (when doom-one-light-padded-modeline
      (if (integerp doom-one-light-padded-modeline) doom-one-light-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(car bg-alt) ,@(cdr base8))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg) 0.125) ,@(cdr base8))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base7))))

  ;; --- extra faces ------------------------
  ((lazy-highlight :background violet :foreground bg :distant-foreground base0 :bold bold)

   ;; evil-mode
   (evil-search-highlight-persist-highlight-face :background violet)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground base0 :background magenta)

   (cursor :background magenta)

   (show-paren-match :background red :foreground bg :weight 'bold)

   ;; helm
   (helm-selection :foreground base0 :background bg-alt)
   (helm-match     :foreground magenta :bold t :underline t)
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)

   ;; ivy
   (ivy-current-match :background bg-alt)
   (ivy-minibuffer-match-face-1 :background bg :weight 'bold :underline t)
   (swiper-line-face :background bg-alt)
   (swiper-match-face-2 :background bg :weight 'bold :foreground magenta :underline t)

   ;; company
   (company-preview-common :background bg-alt :foreground magenta)
   (company-tooltip-common :inherit 'company-tooltip :weight 'bold :underline nil)
   (company-tooltip-common-selection :inherit 'company-tooltip-selection :weight 'bold :underline nil)

   ;; neotree
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)

   ;; yasnippet
   (yas-field-highlight-face :background bg-alt)

   ;; magit
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-one-light-comment-bg (doom-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (linum :inherit 'default :foreground base8 :distant-foreground nil :bold nil)
   (nlinum-hl-face :foreground base5)

   (isearch :foreground bg :background magenta)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   ;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   (solaire-hl-line-face :background bg-alt)
   (solaire-line-number-face :background bg)
   (solaire-default-face :background bg)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-darken base3 0.05))

   ;; org-mode
   (org-block            :background bg)
   (org-block-begin-line :foreground fg :slant 'italic)
   (org-level-1          :background bg :foreground red :bold t :height 1.2)
   (org-level-3          :bold 'bold :foreground violet :height 1.1)
   (org-ellipsis         :underline nil :background bg :foreground red)
   )
  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-one-light-theme.el ends here
