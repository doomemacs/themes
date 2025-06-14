;ar modeline-bg)

;;
(defgroup doom-gruvbox-material-theme nil
  "Options for doom-gruvbox-material."
  :group 'doom-themes)

(defcustom doom-gruvbox-material-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gruvbox-material-theme
  :type 'boolean)

(defcustom doom-gruvbox-material-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gruvbox-material-theme
  :type '(choice integer boolean))

(defcustom doom-gruvbox-material-dark-variant nil
  "A choice of \"hard\" or \"soft\" can be used to change the
background contrast. All other values default to \"medium\"."
  :group 'doom-gruvbox-material-theme
  :type  'string)

;;
(def-doom-theme doom-gruvbox-material
  "Dark theme with pastel 'retro groove' colors."

  ;; name        gui       256       16
  (
   ;; Standardized official colours from gruvbox-material-vscode
   (bg_s           '("#32302f" "#323232" nil))
   (bg_m           '("#292828" "#282828" nil))
   (bg_h           '("#202020" "#1e1e1e" nil))
   (bg0            '("#101010" "#1d1d1d" "brightblack"))
   (bg1            '("#1c1c1c" "#282828" "brightblack"))
   (bg2            '("#32302f" "#383838" "brightblack"))
   (bg3            '("#383432" "#383838" "brightblack"))
   (bg4            '("#3c3836" "#5c5c5c" "brightblack"))
   (bg5            '("#45403d" "#5c5c5c" "brightblack"))
   (bg6            '("#504945" "#6f6f6f" "brightblack"))
   (bg7            '("#5a524c" "#6f6f6f" "brightblack"))
   (bg8            '("#665c54" "#6f6f6f" "brightblack"))
   (bg9            '("#7c6f64" "#6f6f6f" "brightblack"))
   (grey0          '("#7c6f64" "#6f6f6f" "grey"))
   (grey1          '("#928374" "#6f6f6f" "grey"))
   (grey2          '("#a89984" "#6f6f6f" "brightgrey"))

   (fg_m           '("#d4be98" "#cccccc" "brightwhite"))
   (fg0            '("#ddc7a1" "#909090" "brightwhite"))
   (fg1            '("#c5b18d" "#cccccc" "brightwhite"))
   (red            '("#ea6962" "#e74c3c" "red"))
   (orange         '("#e78a4e" "#fd971f" "orange"))
   (yellow         '("#d8a657" "#fabd2f" "yellow"))
   (green          '("#a9b665" "#b8bb26" "green"))
   (aqua           '("#89b482" "#8ec07c" "green"))
   (blue           '("#7daea3" "#83a598" "brightblue"))
   (purple         '("#d3869b" "#d3869b" "brightmagenta"))
   (dimRed         '("#b85651" "#e74c3c" "red"))
   (dimOrange      '("#bd6f3e" "#fd971f" "orange"))
   (dimYellow      '("#c18f41" "#fabd2f" "yellow"))
   (dimGreen       '("#8f9a52" "#b8bb26" "green"))
   (dimAqua        '("#72966c" "#8ec07c" "cyan"))
   (dimBlue        '("#68948a" "#83a598" "brightblue"))
   (dimPurple      '("#ab6c7d" "#d3869b" "brightmagenta"))


   ;; Mapped colours onto those which Doom Themes expects
   (bg
    (pcase doom-gruvbox-material-dark-variant
      ("hard"      bg_h)
      ("soft"      bg_s)
      (_           bg_m)))
   (bg-alt
    (pcase doom-gruvbox-material-dark-variant
      ("hard"      bg1)
      ("soft"      bg_m)
      (_           bg_h))) ;; should be darker than bg
   (bg-alt2
    (pcase doom-gruvbox-material-dark-variant
      ("hard"      bg_m)
      ("soft"      bg3)
      (_           bg_s))) ;; should be lighter than bg

   (fg             fg_m)
   (fg-alt         fg1 )

   (base0          bg0 )
   (base1          bg1 )
   (base2          bg2 )
   (base3          bg5 )
   (base4          bg7 )
   (base5          bg9 )
   (base6          fg1 )
   (base7          fg_m)
   (base8          fg0 )

   (violet         purple)
   (teal           aqua)
   (magenta        purple)
   (grey           grey1)
   (cyan           aqua)
   (dark-cyan      dimAqua)
   (dark-blue      dimBlue)
   (dark-green     dimGreen)

   ;; face categories
   ;; Please see https://www.sainnhe.dev/post/contributing-guide/#color-palette
   ;; - Red: keywords
   (keywords       red)
   ;; - Orange: operators, modifiers
   (operators      orange)
   (builtin        orange)
   ;; - Yellow: types, classes
   (type           yellow)
   ;; - Green: functions, methods
   (functions      green)
   (methods        green)
   ;; - Aqua: strings, characters
   (strings        aqua)
   ;; - Blue: properties, members
   (variables      blue)
   ;; - Purple: values, includes, preproc, special variables
   (constants      purple)
   (numbers        purple)

   ;; - Self-defined
   (highlight      yellow)
   (vertical-bar   bg-alt2)
   (selection      bg-alt2)
   (comments       (if doom-gruvbox-material-brighter-comments magenta grey))
   (doc-comments   (if doom-gruvbox-material-brighter-comments (doom-lighten magenta 0.2) (doom-lighten fg-alt 0.25)))
   (region         bg-alt2)
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    dimAqua)
   (vc-added       dimGreen)
   (vc-deleted     dimRed)

   ;; custom categories
   (-modeline-pad
    (when doom-gruvbox-material-padded-modeline
      (if (integerp doom-gruvbox-material-padded-modeline)
          doom-gruvbox-material-padded-modeline
        4)))
   (modeline-bg bg-alt2)
   (modeline-fg (doom-lighten fg-alt 0.25))
   (modeline-inactive-bg (doom-darken modeline-bg 0.15))
   (modeline-inactive-fg base6)

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;;;; Base theme face overrides
  ((button :foreground cyan :underline t :weight 'bold)
   (cursor :background "white")
   (hl-line :background base3)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background base3 :foreground yellow)
   (isearch :foreground base0 :background orange)
   (lazy-highlight :background yellow :foreground base0 :distant-foreground base0 :bold bold)
   ((link &override) :foreground violet)
   (minibuffer-prompt :foreground cyan)
   (mode-line
    :background bg-alt :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-inactive-bg)))
   (secondary-selection :background (doom-blend bg bg-alt2 0.6))

   ;;;; company
   (company-preview-common :foreground cyan)
   (company-tooltip-common :foreground cyan)
   (company-tooltip-common-selection :foreground cyan)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background base3)
   (company-scrollbar-fg :background cyan)
   (company-tooltip-selection :background bg-alt2)
   (company-tooltip-mouse :background bg-alt2 :foreground nil)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground keywords)
   ;;;; doom-emacs
   (+workspace-tab-selected-face :background dark-green :foreground "white")
   ;;;; doom-modeline
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-error :background bg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-info :bold t :foreground cyan)
   (doom-modeline-bar :background dark-green)
   (doom-modeline-panel :background dark-green :foreground fg)
   ;;;; doom-themes
   (doom-themes-neotree-file-face :foreground fg)
   (doom-themes-neotree-hidden-file-face :foreground (doom-lighten fg-alt 0.25))
   (doom-themes-neotree-media-file-face :foreground (doom-lighten fg-alt 0.25))
   ;;;; emacs-lisp-mode
   (highlight-quoted-symbol :foreground dark-cyan)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (doom-blend red bg 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.2))
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background yellow)
   (evil-ex-substitute-replacement :foreground cyan :strike-through nil :inherit 'evil-ex-substitute-matches)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground "white" :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)
   ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,blue)   :background base3)
   ;;;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)
   ;;;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)
   ;;;; highlight-thing
   (highlight-thing :background (doom-lighten base3 0.03) :distant-foreground fg-alt)
   ;;;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base3 0.03) :distant-foreground fg-alt)
   ;;;; ivy
   (ivy-current-match :background bg-alt2)
   (ivy-subdir :background nil :foreground cyan)
   (ivy-action :background nil :foreground cyan)
   (ivy-grep-line-number :background nil :foreground cyan)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow)
   (ivy-minibuffer-match-face-2 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (counsel-key-binding :foreground cyan)
   ;;;; ivy-posframe
   (ivy-posframe :background base3)
   (ivy-posframe-border :background base1)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground dark-cyan)
   ;;;; magit
   (magit-section-heading             :foreground cyan :weight 'bold)
   (magit-branch-current              :underline green :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base3 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background bg-alt2 :foreground fg)
   (magit-diff-context                :foreground base3 :foreground fg-alt)
   ;;;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground green)
   (markdown-header-delimiter-face :foreground orange)
   (markdown-blockquote-face :inherit 'italic :foreground grey)
   (markdown-list-face :foreground grey)
   (markdown-url-face :foreground violet)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'underline :foreground grey)
   ((markdown-code-face &override) :background (doom-blend bg bg-alt2 0.6))
   ;;;; mu4e-view
   (mu4e-header-key-face :foreground red :weight 'bold)
   ;;;; neotree
   (neo-root-dir-face   :foreground cyan)
   (doom-neotree-dir-face :foreground cyan)
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground violet)
   ((outline-2 &override) :foreground cyan)
   ((outline-3 &override) :foreground green)
   ((outline-4 &override) :foreground (doom-lighten violet 0.2))
   ((outline-5 &override) :foreground (doom-lighten dark-cyan 0.25))
   ((outline-6 &override) :foreground (doom-lighten violet 0.4))
   ((outline-7 &override) :foreground (doom-lighten dark-cyan 0.5))
   ((outline-8 &override) :foreground (doom-lighten violet 0.6))
   ; ((outline-1 &override) :foreground green)
   ; ((outline-2 &override) :foreground green)
   ; ((outline-3 &override) :foreground yellow)
   ; ((outline-4 &override) :foreground yellow)
   ; ((outline-5 &override) :foreground dark-yellow)
   ; ((outline-6 &override) :foreground dark-yellow)
   ;;;; org <built-in>
   ((org-code &override) :foreground orange)
   (org-block :background (doom-blend bg bg-alt 0.4))

   (org-date :foreground green)
   (org-document-info :foreground red)
   (org-document-title :foreground red)
   (org-drawer :foreground (doom-lighten cyan 0.4))
   (org-ellipsis :underline nil :foreground orange)
   (org-formula :foreground green)
   (org-meta-line :foreground comments)
   (org-list-dt :foreground cyan)
   ; (org-list-dt :foreground yellow)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-table :foreground cyan)
   (org-tag :foreground (doom-darken comments 0.15) :weight 'normal)
   ; (org-tag :foreground yellow :bold nil)
   (org-todo :foreground green :bold 'inherit)
   ; (org-todo :foreground yellow :bold 'inherit)
   (org-verbatim :foreground yellow)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground orange)
   (rainbow-delimiters-depth-2-face :foreground magenta)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground blue)
   ;;;; show-paren <built-in>
   ((show-paren-match &override) :foreground nil :background base5 :bold t)
   ((show-paren-mismatch &override) :foreground nil :background "red")
   ;;;; swiper
   (swiper-line-face :background bg-alt2)
   ;;;; undo-tree
   (undo-tree-visualizer-active-branch-face :foreground cyan)
   (undo-tree-visualizer-current-face :foreground yellow)
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background bg-alt2 :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground "white" :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)
   ;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground blue)
   (web-mode-html-tag-face         :foreground cyan)
   (web-mode-html-attr-name-face   :foreground cyan)
   (web-mode-json-key-face         :foreground green)
   (web-mode-json-context-face     :foreground cyan)
   ;;;; which-key
   (which-key-key-face                   :foreground green)
   (which-key-group-description-face     :foreground red)
   (which-key-command-description-face   :foreground blue)
   (which-key-local-map-description-face :foreground orange))

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-gruvbox-material-theme.el ends here
