;; doom-gruvbox-theme.el --- inspired by morhetz Gruvbox -*- no-byte-compile: t; -*-
(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)

;;
(defgroup doom-gruvbox-theme nil
  "Options for doom-gruvbox."
  :group 'doom-themes)

(defcustom doom-gruvbox-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gruvbox-theme
  :type 'boolean)

(defcustom doom-gruvbox-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gruvbox-theme
  :type '(choice integer boolean))

(defcustom doom-gruvbox-dark-variant nil
  "A choice of \"hard\" or \"soft\" can be used to change the
background contrast. All other values default to \"medium\"."
  :group 'doom-gruvbox-theme
  :type  'string)

;;
(def-doom-theme doom-gruvbox
  "Dark theme with pastel 'retro groove' colors."

  ;; name        gui       256       16
  ((bg
    (cond ((equal doom-gruvbox-dark-variant "hard") '("#1d2021" "#1e1e1e" nil))   ; bg0_h
          ((equal doom-gruvbox-dark-variant "soft") '("#32302f" "#323232" nil))   ; bg0_s
          (t                                        '("#282828" "#282828" nil)))) ; bg0
   (bg-alt
    (cond ((equal doom-gruvbox-dark-variant "hard") '("#282828" "#282828" nil))
          ((equal doom-gruvbox-dark-variant "soft") '("#403d3d" "#404040" nil))
          (t                                        '("#32302f" "#323232" nil))))
   (bg-alt2    '("#504945" "#504945" "brown"      )) ; bg2 (for region, selection etc.)

   (base0      '("#0d1011" "black"   "black"      )) ; (self-defined)
   (base1      '("#1d2021" "#1d1d1d" "brightblack")) ; bg0_h
   (base2      '("#282828" "#282828" "brightblack")) ; bg0
   (base3      '("#3c3836" "#383838" "brightblack")) ; bg1
   (base4      '("#665c54" "#5c5c5c" "brightblack")) ; bg3
   (base5      '("#7c6f64" "#6f6f6f" "brightblack")) ; bg4
   (base6      '("#928374" "#909090" "brightblack")) ; gray
   (base7      '("#d5c4a1" "#cccccc" "brightblack")) ; fg2
   (base8      '("#fbf1c7" "#fbfbfb" "brightwhite")) ; fg0
   (fg         '("#ebdbb2" "#dfdfdf" "brightwhite")) ; fg/fg1
   (fg-alt     '("#d5c4a1" "#cccccc" "brightwhite")) ; fg2

   ;; Standardized official colours from gruvbox
   (grey       '("#928374" "#909090" "brightblack"))   ; gray
   (red        '("#fb4934" "#e74c3c" "red"))           ; bright-red
   (magenta    '("#cc241d" "#cc241d" "magenta"))       ; red
   (violet     '("#d3869b" "#d3869b" "brightmagenta")) ; bright-purple
   (orange     '("#fe8019" "#fd971f" "orange"))        ; bright-orange
   (yellow     '("#fabd2f" "#fabd2f" "yellow"))        ; bright-yellow
   (teal       '("#8ec07c" "#8ec07c" "green"))         ; bright-aqua
   (green      '("#b8bb26" "#b8bb26" "green"))         ; bright-green
   (dark-green '("#98971a" "#98971a" "green"))         ; green
   (blue       '("#83a598" "#83a598" "brightblue"))    ; bright-blue
   (dark-blue  '("#458588" "#458588" "blue"))          ; blue
   (cyan       '("#8ec07c" "#8ec07c" "brightcyan"))    ; bright-aqua
   (dark-cyan  '("#689d6a" "#689d6a" "cyan"))          ; aqua

   ;; face categories
   (highlight      yellow)
   (vertical-bar   grey)
   (selection      bg-alt2)
   (builtin        orange)
   (comments       (if doom-gruvbox-brighter-comments magenta grey))
   (doc-comments   (if doom-gruvbox-brighter-comments (doom-lighten magenta 0.2) (doom-lighten fg-alt 0.25)))
   (constants      violet)
   (functions      cyan)
   (keywords       red)
   (methods        cyan)
   (operators      cyan)
   (type           yellow)
   (strings        green)
   (variables      cyan)
   (numbers        violet)
   (region         bg-alt2)
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    (doom-darken blue 0.15))
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     (doom-darken red 0.15))

   ;; custom categories
   (-modeline-pad
    (when doom-gruvbox-padded-modeline
      (if (integerp doom-gruvbox-padded-modeline)
          doom-gruvbox-padded-modeline
        4)))

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))

  ;; --- extra faces ------------------------
  (
   ;;;;;;;; Editor ;;;;;;;;
   (cursor :background "white")
   (hl-line :background bg-alt)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background bg-alt2 :foreground fg :bold t)

   ;; Vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background bg-alt2 :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground "white" :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)

   ;;;;;;;; Doom-modeline ;;;;;;;;
   (mode-line
    :background bg-alt2 :foreground (doom-lighten fg-alt 0.25)
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base3)))

   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base2)))

   ;; File-name
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   ;; Misc
   (doom-modeline-error :background bg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-info :bold t :foreground cyan)
   (doom-modeline-bar :background dark-green)
   (doom-modeline-panel :background dark-green :foreground fg)

   ;;;;;;;; Search ;;;;;;;;
   ;; /find
   (isearch :foreground base0 :background orange)
   (evil-search-highlight-persist-highlight-face :background yellow)
   (lazy-highlight :background yellow :foreground base0 :distant-foreground base0 :bold bold)
   (evil-ex-substitute-replacement :foreground cyan :inherit 'evil-ex-substitute-matches)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground "white" :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)

   ;;;;;;;; Mini-buffers ;;;;;;;;
   (minibuffer-prompt :foreground cyan)
   (solaire-hl-line-face :background bg-alt2)

   ;; ivy
   (ivy-current-match :background bg-alt2)
   (ivy-subdir :background nil :foreground cyan)
   (ivy-action :background nil :foreground cyan)
   (ivy-grep-line-number :background nil :foreground cyan)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow)
   (ivy-minibuffer-match-face-2 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (counsel-key-binding :foreground cyan)

   ;; swiper
   (swiper-line-face :background bg-alt2)

   ;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)

   ;; neotree
   (neo-root-dir-face   :foreground cyan)
   (doom-neotree-dir-face :foreground cyan)
   (neo-dir-link-face   :foreground cyan)
   (doom-neotree-file-face :foreground fg)
   (doom-neotree-hidden-file-face :foreground (doom-lighten fg-alt 0.25))
   (doom-neotree-media-file-face :foreground (doom-lighten fg-alt 0.25))
   (neo-expand-btn-face :foreground magenta)

   ;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)

   ;;;;;;;; Brackets ;;;;;;;;
   ;; Rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground cyan)
   (rainbow-delimiters-depth-4-face :foreground red)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground cyan)
   (rainbow-delimiters-depth-7-face :foreground red)
   ;; Bracket pairing
   ((show-paren-match &override) :foreground nil :background base5 :bold t)
   ((show-paren-mismatch &override) :foreground nil :background "red")

   ;;;;;;;; which-key ;;;;;;;;
   (which-func :foreground cyan)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground (doom-lighten fg-alt 0.25))
   (which-key-local-map-description-face :foreground cyan)

   ;;;;;;;; Company ;;;;;;;;
   (company-preview-common :foreground cyan)
   (company-tooltip-common :foreground cyan)
   (company-tooltip-common-selection :foreground cyan)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background bg-alt)
   (company-scrollbar-fg :background cyan)
   (company-tooltip-selection :background bg-alt2)
   (company-tooltip-mouse :background bg-alt2 :foreground nil)

   ;;;;;;;; Misc ;;;;;;;;
   (+workspace-tab-selected-face :background dark-green :foreground "white")

   ;; Undo tree
   (undo-tree-visualizer-active-branch-face :foreground cyan)
   (undo-tree-visualizer-current-face :foreground yellow)

   ;; General UI
   (button :foreground cyan :underline t :bold t)

   ;; ediff
   (ediff-fine-diff-A    :background (doom-blend red bg 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.2))

   ;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,cyan)  :background base3)

   ;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)

   ;; magit
   (magit-section-heading             :foreground yellow :weight 'bold)
   (magit-branch-current              :underline cyan :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base3 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background bg-alt2 :foreground fg)
   (magit-diff-context                :foreground bg-alt :foreground fg-alt)


   ;;;;;;;; Major mode faces ;;;;;;;;
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground keywords)

   ;; elisp-mode
   (highlight-quoted-symbol :foreground dark-cyan)

   ;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base3 0.03) :distant-foreground fg-alt)

   ;; highlight-thing
   (highlight-thing :background (doom-lighten base3 0.03) :distant-foreground fg-alt)

   ;; LaTeX-mode
   (font-latex-math-face :foreground dark-cyan)

   ;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground cyan)
   (markdown-list-face :foreground red)
   (markdown-url-face :foreground red)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-lighten base2 0.045))

   ;; mu4e-view
   (mu4e-header-key-face :foreground red)

   ;; org-mode
   ((outline-1 &override) :foreground yellow)
   ((outline-2 &override) :foreground cyan)
   ((outline-3 &override) :foreground cyan)
   (org-ellipsis :underline nil :foreground orange)
   (org-tag :foreground yellow :bold nil)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow)

   ;; web-mode
   (web-mode-html-tag-bracket-face :foreground blue)
   (web-mode-html-tag-face         :foreground cyan)
   (web-mode-html-attr-name-face   :foreground cyan)
   (web-mode-json-key-face         :foreground green)
   (web-mode-json-context-face     :foreground cyan))
  ;; --- extra variables --------------------
  ;; ()
  )
;;; doom-gruvbox-theme.el ends here
