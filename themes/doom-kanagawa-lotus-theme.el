;;; doom-kanagawa-lotus-theme.el --- inspired by rebelot/kanagawa.nvim (lotus) -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: September 30 2025
;; Author: Anskrevy <https://github.com/Anskrevy
;; Maintainer:
;; Source: https://github.com/rebelot/kanagawa.nvim
;;
;;; Commentary:
;;
;; Kanagawa Lotus - A serene light theme features warm cream backgrounds with
;; muted, earthy colors optimized for bright environments and outdoor work.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-kanagawa-lotus-theme nil
  "Options for the `doom-kanagawa-lotus' theme."
  :group 'doom-themes)

(defcustom doom-kanagawa-lotus-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanagawa-lotus-theme
  :type 'boolean)

(defcustom doom-kanagawa-lotus-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanagawa-lotus-theme
  :type 'boolean)

(defcustom doom-kanagawa-lotus-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kanagawa-lotus-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-kanagawa-lotus
  "A light theme with serene, warm tones inspired by Kanagawa Lotus."

  ;; name        default   256       16
  ;; Lotus White - Light warm backgrounds
  ((bg         '("#f2ecbc" "#ffffaf" "white"        ))
   (fg         '("#545464" "#5f5f5f" "black"        ))

   (bg-alt     '("#d5cea3" "#d7d7af" "brightwhite"  ))
   (fg-alt     '("#43436c" "#5f5f87" "brightblack"  ))

   (base0      '("#d5cea3" "#d7d7af" "white"        ))
   (base1      '("#dcd5ac" "#d7d7af" "brightwhite"  ))
   (base2      '("#e5ddb0" "#d7d7af" "brightwhite"  ))
   (base3      '("#e7dba0" "#d7d7af" "white"        ))
   (base4      '("#e4d794" "#d7d7af" "white"        ))
   (base5      '("#716e61" "#767676" "brightblack"  ))
   (base6      '("#8a8980" "#878787" "brightblack"  ))
   (base7      '("#a09cac" "#afafaf" "brightblack"  ))
   (base8      '("#545464" "#5f5f5f" "black"        ))

   (grey       base6)

   ;; Core semantic colors
   (red        '("#c84053" "#d75f5f" "red"          ))
   (orange     '("#cc6d00" "#d75f00" "brightred"    ))
   (green      '("#6f894e" "#5f875f" "green"        ))
   (teal       '("#597b75" "#5f8787" "brightgreen"  ))
   (yellow     '("#77713f" "#878700" "yellow"       ))
   (blue       '("#6693bf" "#5f87af" "brightblue"   ))
   (dark-blue  '("#4d699b" "#5f87af" "blue"         ))
   (magenta    '("#624c83" "#5f5f87" "brightmagenta"))
   (violet     '("#766b90" "#5f5f87" "magenta"      ))
   (cyan       '("#5a7785" "#5f8787" "brightcyan"   ))
   (dark-cyan  '("#5a7785" "#5f8787" "cyan"         ))

   ;; Extended Lotus-specific colors
   (lotus-pink        '("#b35b79" "#af5f87" "magenta"     ))
   (lotus-blue5       '("#5d57a3" "#5f5faf" "blue"        ))
   (lotus-violet1     '("#a09cac" "#afafaf" "brightmagenta"))
   (lotus-violet3     '("#c9cbd1" "#c6c6c6" "brightwhite" ))
   (lotus-blue1       '("#c7d7e0" "#d7d7d7" "brightcyan"  ))
   (lotus-blue2       '("#b5cbd2" "#afd7d7" "brightcyan"  ))
   (lotus-blue3       '("#9fb5c9" "#afd7d7" "brightblue"  ))
   (lotus-red2        '("#d7474b" "#d75f5f" "red"         ))
   (lotus-red3        '("#e82424" "#d70000" "brightred"   ))
   (lotus-red4        '("#d9a594" "#d7afaf" "brightred"   ))
   (lotus-orange2     '("#e98a00" "#ff8700" "brightyellow"))
   (lotus-yellow2     '("#836f4a" "#875f5f" "yellow"      ))
   (lotus-yellow3     '("#de9800" "#d7af00" "brightyellow"))
   (lotus-yellow4     '("#f9d791" "#ffd787" "brightyellow"))
   (lotus-green2      '("#6e915f" "#87875f" "green"       ))
   (lotus-green3      '("#b7d0ae" "#afd7af" "brightgreen" ))
   (lotus-gray        '("#dcd7ba" "#d7d7af" "white"       ))
   (lotus-gray3       '("#8a8980" "#878787" "brightblack" ))
   (lotus-teal1       '("#4e8ca2" "#5f87af" "brightcyan"  ))
   (lotus-cyan        '("#d7e3d8" "#d7d7d7" "brightwhite" ))

   ;; Diagnostic colors
   (samurai-red       '("#e82424" "#d70000" "red"         ))
   (ronin-yellow      '("#e98a00" "#ff8700" "brightyellow"))

   ;; VCS colors
   (lotus-vcs-added   '("#6e915f" "#87875f" "green"       ))
   (lotus-vcs-removed '("#d7474b" "#d75f5f" "red"         ))
   (lotus-vcs-changed '("#de9800" "#d7af00" "brightyellow"))

   ;; Autumn colors
   (autumn-green      '("#76946A" "#5f875f" "green"       ))
   (autumn-red        '("#C34043" "#af5f5f" "red"         ))
   (autumn-yellow     '("#DCA561" "#d7af5f" "yellow"      ))

   ;; Face categories
   (highlight      dark-blue)
   (vertical-bar   base3)
   (selection      lotus-violet3)
   (builtin        blue)
   (comments       (if doom-kanagawa-lotus-brighter-comments dark-cyan lotus-gray3))
   (doc-comments   (if doom-kanagawa-lotus-brighter-comments dark-cyan fg-alt))
   (constants      orange)
   (functions      dark-blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      lotus-yellow2)
   (type           teal)
   (strings        green)
   (variables      fg)
   (numbers        lotus-pink)
   (region         lotus-violet3)
   (error          lotus-red3)
   (warning        lotus-orange2)
   (success        green)
   (vc-modified    lotus-yellow3)
   (vc-added       lotus-green2)
   (vc-deleted     lotus-red2)

   ;; Mode-line configuration
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-kanagawa-lotus-brighter-modeline
                                 (doom-lighten dark-blue 0.4)
                               base1))
   (modeline-bg-alt          (if doom-kanagawa-lotus-brighter-modeline
                                 (doom-lighten dark-blue 0.45)
                               (doom-darken base1 0.05)))
   (modeline-bg-inactive     base1)
   (modeline-bg-inactive-alt (doom-darken base1 0.05))

   (-modeline-pad
    (when doom-kanagawa-lotus-padded-modeline
      (if (integerp doom-kanagawa-lotus-padded-modeline)
          doom-kanagawa-lotus-padded-modeline
        4))))

  ;;;; Base theme face overrides
  (
   ;; Basic faces
   (default :background bg :foreground fg)
   (cursor :background dark-blue :foreground bg)
   (hl-line :background base3)
   (region :background region :distant-foreground fg)
   ((line-number &override) :foreground base7 :background bg-alt)
   ((line-number-current-line &override) :foreground violet :background base3 :bold t)
   (fringe :background bg-alt :foreground base4)
   (vertical-border :foreground base3)

   ;; Font lock
   ((font-lock-builtin-face &override) :foreground blue)
   ((font-lock-comment-face &override) :foreground comments :italic t
    :background (if doom-kanagawa-lotus-brighter-comments (doom-darken bg 0.05)))
   ((font-lock-comment-delimiter-face &override) :foreground comments :italic t)
   ((font-lock-constant-face &override) :foreground orange)
   ((font-lock-doc-face &override) :foreground doc-comments :italic t)
   ((font-lock-function-name-face &override) :foreground dark-blue)
   ((font-lock-keyword-face &override) :foreground magenta :weight 'semi-bold)
   ((font-lock-string-face &override) :foreground green :italic t)
   ((font-lock-type-face &override) :foreground teal :weight 'semi-bold)
   ((font-lock-variable-name-face &override) :foreground fg)
   ((font-lock-warning-face &override) :foreground lotus-orange2)
   ((font-lock-negation-char-face &override) :foreground red)
   ((font-lock-preprocessor-face &override) :foreground red)
   ((font-lock-regexp-grouping-backslash &override) :foreground lotus-yellow2)

   ;; Mode-line
   (mode-line
    :background modeline-bg :foreground modeline-fg :bold t
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanagawa-lotus-brighter-modeline base8 highlight))
   (mode-line-buffer-id :foreground teal :bold t)
   (mode-line-highlight :foreground yellow)

   ;; Highlighting
   (highlight :background base3 :foreground violet)
   (match :background yellow :foreground bg :bold t)
   (trailing-whitespace :background base3)
   (lazy-highlight :background lotus-blue2 :foreground fg :bold t)
   ((isearch &override) :background autumn-yellow :foreground bg :bold t)
   ((isearch-fail &override) :background samurai-red :foreground bg)
   (show-paren-match :background teal :foreground bg :bold t)
   (show-paren-mismatch :background samurai-red :foreground bg)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background dark-blue :foreground fg)
   (centaur-tabs-selected :background bg :foreground fg :bold t)
   (centaur-tabs-selected-modified :background bg :foreground fg)
   (centaur-tabs-modified-marker-selected :background bg :foreground autumn-yellow)
   (centaur-tabs-close-selected :inherit 'centaur-tabs-selected)
   (centaur-tabs-unselected :background base0 :foreground base5)
   (centaur-tabs-default :background base0 :foreground base5)
   (centaur-tabs-unselected-modified :background base0 :foreground samurai-red)
   (centaur-tabs-modified-marker-unselected :background base0 :foreground base5)
   (centaur-tabs-close-unselected :background base0 :foreground base5)
   (centaur-tabs-close-mouse-face :background nil :foreground samurai-red)

   ;;;; company
   (company-tooltip :background lotus-blue1 :foreground fg)
   (company-tooltip-common :foreground autumn-yellow :bold t)
   (company-tooltip-quick-access :foreground violet)
   (company-tooltip-scrollbar-thumb :background autumn-red)
   (company-tooltip-scrollbar-track :background lotus-blue1)
   (company-tooltip-search :background yellow :foreground bg :distant-foreground fg)
   (company-tooltip-selection :background lotus-blue3 :foreground fg :bold t)
   (company-tooltip-mouse :background lotus-blue1 :foreground bg :distant-foreground fg)
   (company-tooltip-annotation :foreground red :distant-foreground base1)
   (company-scrollbar-bg :inherit 'tooltip)
   (company-scrollbar-fg :background samurai-red)
   (company-preview :foreground yellow)
   (company-preview-common :foreground autumn-yellow :bold t)
   (company-preview-search :inherit 'company-tooltip-search)
   (company-template-field :inherit 'match)

   ;;;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property :foreground green)
   (css-selector :foreground dark-blue)

   ;;;; dashboard
   (dashboard-heading :foreground violet :bold t)
   (dashboard-items-face :foreground fg)
   (dashboard-banner-logo-title :bold t :height 200)
   (dashboard-no-items-face :foreground base5)

   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-kanagawa-lotus-brighter-modeline modeline-bg violet) :bold t)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold :foreground teal)
   (doom-modeline-buffer-project-root :foreground orange :weight 'bold)
   (doom-modeline-buffer-modified :foreground yellow :bold t)
   (doom-modeline-buffer-major-mode :foreground teal :bold t)
   (doom-modeline-panel :inherit 'bold :background orange :foreground base0)
   (doom-modeline-info :bold t :foreground green)
   (doom-modeline-warning :foreground yellow :bold t)
   (doom-modeline-urgent :foreground red :bold t)
   (doom-modeline-evil-insert-state :foreground samurai-red)
   (doom-modeline-evil-visual-state :foreground green)
   (doom-modeline-evil-replace-state :foreground lotus-orange2)
   (doom-modeline-evil-normal-state :foreground cyan)
   (doom-modeline-evil-motion-state :foreground cyan)

   ;;;; ediff
   (ediff-current-diff-A :background lotus-red4 :foreground fg)
   (ediff-current-diff-B :background lotus-green3 :foreground fg)
   (ediff-current-diff-C :background lotus-cyan :foreground fg)
   (ediff-fine-diff-A :background lotus-red2 :foreground bg :bold t)
   (ediff-fine-diff-B :background lotus-green2 :foreground bg :bold t)
   (ediff-fine-diff-C :background lotus-yellow4 :foreground bg :bold t)

   ;;;; evil
   (evil-ex-lazy-highlight :background lotus-green3 :foreground green :bold t)
   (evil-ex-substitute-matches :background lotus-red4 :foreground red :bold t)
   (evil-ex-substitute-replacement :foreground orange :strike-through nil)
   (evil-search-highlight-persist-highlight-face :background lotus-yellow4 :foreground fg)

   ;;;; flycheck
   (flycheck-error (:underline `(:style wave :color ,lotus-red3)))
   (flycheck-warning (:underline `(:style wave :color ,lotus-orange2)))
   (flycheck-info (:underline `(:style wave :color ,cyan)))
   (flycheck-fringe-error :foreground lotus-red3)
   (flycheck-fringe-warning :foreground lotus-orange2)
   (flycheck-fringe-info :foreground cyan)

   ;;;; git-gutter
   (git-gutter:added :foreground lotus-green2 :background bg-alt)
   (git-gutter:modified :foreground lotus-yellow3 :background bg-alt)
   (git-gutter:deleted :foreground lotus-red2 :background bg-alt)

   ;;;; indent-guides
   (highlight-indent-guides-character-face :foreground base3)
   (highlight-indent-guides-stack-character-face :foreground base3)
   (highlight-indent-guides-stack-odd-face :foreground base3)
   (highlight-indent-guides-stack-even-face :foreground base4)
   (highlight-indent-guides-even-face :foreground base2)
   (highlight-indent-guides-odd-face :foreground base3)

   ;;;; ivy
   (ivy-current-match :background dark-blue :foreground bg :bold t)
   (ivy-action :background nil :foreground fg)
   (ivy-grep-line-number :background nil :foreground green)
   (ivy-minibuffer-match-face-1 :background nil :foreground red)
   (ivy-minibuffer-match-face-2 :background nil :foreground green)
   (ivy-minibuffer-match-face-3 :background nil :foreground cyan)
   (ivy-minibuffer-match-face-4 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (ivy-grep-info :foreground cyan)
   (ivy-confirm-face :foreground teal)
   (ivy-posframe :background lotus-blue1)
   (ivy-posframe-border :background base3)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   (font-latex-script-char-face :foreground cyan)

   ;;;; lsp-mode and lsp-ui
   (lsp-face-highlight-textual :background lotus-blue2)
   (lsp-face-highlight-read :background lotus-blue2)
   (lsp-face-highlight-write :background lotus-blue2)
   (lsp-headerline-breadcrumb-path-error-face (:foreground base5 :background base0 :underline (:color green :style wave)))
   (lsp-headerline-breadcrumb-path-face :background base0 :foreground fg)
   (lsp-headerline-breadcrumb-symbols-error-face :foreground samurai-red)
   (lsp-ui-doc-background :background base0 :foreground fg)
   (lsp-ui-doc-header :background base0 :foreground fg :bold t)
   (lsp-ui-doc-border :foreground base4)
   (lsp-ui-sideline-code-action :foreground yellow)
   (lsp-ui-sideline-current-symbol :foreground dark-blue)
   (lsp-ui-sideline-symbol :foreground dark-cyan)
   (lsp-ui-peek-filename :foreground cyan)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-header-face-1 :foreground red :height 1.3 :bold t)
   (markdown-header-face-2 :foreground violet :height 1.15 :bold t)
   (markdown-header-face-3 :foreground yellow :height 1.05)
   ((markdown-code-face &override) :background base0 :foreground green)
   (markdown-inline-code-face :background base0 :foreground green)
   (markdown-blockquote-face :foreground lotus-violet1)

   ;;;; org-mode
   (org-block :background base0 :foreground base5)
   (org-block-begin-line :background lotus-blue2 :foreground dark-blue)  ; Light blue background
   (org-block-end-line :background lotus-red4 :foreground red)           ; Light red/pink background
   (org-code :background base0 :foreground green)
   (org-meta-line :background lotus-green3 :foreground green)            ; Light green background
   (org-level-1 :foreground red :height 1.3 :bold t)
   (org-level-2 :foreground violet :height 1.15 :bold t)
   (org-level-3 :foreground yellow :height 1.05)
   (org-level-4 :foreground fg)
   (org-level-5 :foreground fg)
   (org-level-6 :foreground yellow)
   (org-level-7 :foreground orange)
   (org-level-8 :foreground green)
   (org-todo :foreground orange :bold t)
   (org-done :foreground dark-cyan :strike-through t)
   (org-headline-done :foreground dark-cyan :strike-through t)
   (org-ellipsis :foreground lotus-blue2 :bold t)
   (org-date :foreground lotus-blue2)
   (org-footnote :foreground teal)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-mismatched-face :foreground samurai-red)
   (rainbow-delimiters-unmatched-face :foreground teal)
   (rainbow-delimiters-base-error-face :foreground samurai-red)
   (rainbow-delimiters-base-face :foreground base5)
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground dark-cyan)
   (rainbow-delimiters-depth-3-face :foreground lotus-violet1)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground teal)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground red)
   (rainbow-delimiters-depth-8-face :foreground blue)
   (rainbow-delimiters-depth-9-face :foreground violet)

   ;;;; solaire-mode
   (solaire-default-face :background base1)
   (solaire-hl-line-face :background base3)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

   ;;;; treemacs
   (treemacs-root-face :foreground fg :bold t :height 1.2)
   (treemacs-directory-face :foreground fg)
   (treemacs-directory-collapsed-face :foreground fg)
   (treemacs-file-face :foreground fg)
   (treemacs-git-added-face :foreground orange)
   (treemacs-git-renamed-face :foreground fg)
   (treemacs-git-ignored-face :foreground base5)
   (treemacs-git-unmodified-face :foreground fg)
   (treemacs-git-modified-face :foreground green)

   ;;;; vertico
   (vertico-current :background lotus-blue2 :foreground yellow :bold t)
   (vertico-multiline :background samurai-red)
   (vertico-group-title :background lotus-blue2 :foreground dark-blue :bold t)
   (vertico-group-separator :background lotus-blue2 :foreground dark-blue :strike-through t)
   (vertico-posframe-border :background base3)
   (vertico-posframe :background lotus-blue1)

   ;;;; which-key
   (which-key-command-description-face :foreground dark-blue)
   (which-key-group-description-face :foreground red)
   (which-key-local-map-description-face :foreground yellow)
   (which-key-key-face :foreground teal)
   (which-key-posframe :background lotus-violet3)
   (which-key-posframe-border :background lotus-violet3)

   ;;;; whitespace-mode
   (whitespace-space :foreground base5)
   (whitespace-tab :foreground base5)
   (whitespace-newline :foreground base5)
   (whitespace-trailing :background base3)
   (whitespace-line :background lotus-red4 :foreground samurai-red)
   )

  ;;;; Base theme variable overrides
  ())

;;; doom-kanagawa-lotus-theme.el ends here
