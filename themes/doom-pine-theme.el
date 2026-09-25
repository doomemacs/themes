;; doom-pine-theme.el --- a green flavor of doom-gruvbox -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Date: November 12, 2022
;; Author: Roman Hargrave <https://github.com/RomanHargrave>
;; Maintainer:
;; Source: doom-gruvbox
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

;;
(defgroup doom-pine-theme nil
  "Options for doom-pine."
  :group 'doom-themes)

(defcustom doom-pine-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-pine-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-pine
  "Emacs theme for mountain folks."

  ;; name        gui       256       16
  ((bg         '("#0c1400" "#0f0f0f" nil          )) ; bg0
   (fg         '("#d5c9b0" "#dfdfdf" "brightwhite")) ; fg/fg1

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#121e00" "#1a1a1a" nil          )) ; bg1
   (bg-alt2    '("#222b14" "#005f00" nil          )) ; bg2 (for region, selection etc.)
   (fg-alt     '("#d5c4a1" "#cccccc" "brightwhite")) ; fg2

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#0d1011" "black"   "black"      )) ; (self-defined)
   (base1      '("#0c1400" "#0c1400" "brightblack")) ; bg0
   (base2      '("#0c1400" "#0c1400" "brightblack")) ; bg0
   (base3      '("#121e00" "#121e00" "brightblack")) ; bg1
   (base4      '("#353e29" "#5f875f" "brightblack")) ; bg3
   (base5      '("#49503d" "#5f8787" "brightblack")) ; bg4
   (base6      '("#928374" "#909090" "brightblack")) ; gray
   (base7      '("#d5c4a1" "#cccccc" "brightblack")) ; fg2
   (base8      '("#fbf1c7" "#fbfbfb" "brightwhite")) ; fg0

   (grey       '("#928374" "#909090" "brightblack"))   ; gray
   (red        '("#ce4e2b" "#e74c3c" "red"))           ; bright-red
   (magenta    '("#cc241d" "#cc241d" "magenta"))       ; red
   (violet     '("#d3869b" "#d3869b" "brightmagenta")) ; bright-purple
   (orange     '("#fe8019" "#fd971f" "orange"))        ; bright-orange
   (yellow     '("#e5aa2b" "#fabd2f" "yellow"))        ; bright-yellow
   (teal       '("#8ec07c" "#8ec07c" "green"))         ; bright-aqua
   (green      '("#86914e" "#87af87" "white"))         ;
   (dark-green '("#98971a" "#98971a" "green"))         ; green
   (blue       '("#83a598" "#83a598" "brightblue"))    ; bright-blue
   (dark-blue  '("#458588" "#458588" "blue"))          ; blue
   (cyan       '("#8ec07c" "#8ec07c" "brightcyan"))    ; bright-aqua
   (dark-cyan  '("#689d6a" "#689d6a" "cyan"))          ; aqua

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      fg-alt)
   (vertical-bar   bg-alt2)
   (selection      bg-alt2)
   (builtin        orange)
   (comments       base5)
   (doc-comments   (doom-lighten fg-alt 0.25))
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

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-bg-active   `(,(car bg-alt2)))
   (modeline-fg-active   `(,(car base4)))
   (modeline-bg-inactive `(,(car bg-alt)))
   (modeline-fg-inactive `(,(car fg-alt)))

   (-modeline-pad
    (when doom-pine-padded-modeline
      (if (integerp doom-pine-padded-modeline)
          doom-pine-padded-modeline
        4)))

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))

  ;; --- extra faces ------------------------
  (;;;;;;;; Editor ;;;;;;;;
   ((secondary-selection &override) :background (doom-lighten base3 0.05))
   ((default &override) :foreground fg)
   (cursor :background (doom-lighten bg 0.5))
   (hl-line :background bg-alt)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background bg-alt2 :foreground fg :bold t)

   ;; Vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background bg-alt2 :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground "white" :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)

   ;;;;;;;; Doom-modeline ;;;;;;;;
   (mode-line
    :background modeline-bg-active :foreground modeline-fg-active
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-active)))

   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))

   ;; File-name
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)

   ;; Misc
   (doom-modeline-bar :background dark-green)

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

;;; doom-pine-theme.el ends here
