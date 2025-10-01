;;; doom-kanagawa-abyss-theme.el --- inspired by rebelot/kanagawa.nvim and others -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 4 2023
;; Author: Anskrevy <https://github.com/Anskrevy
;; Maintainer:
;; Source: https://github.com/rebelot/kanagawa.nvim
;;
;;; Commentary:
;;
;; Kanagawa Abyss - A hybrid theme combining Dragon's deep, dark backgrounds
;; with Wave's vibrant, bright color palette. This creates a high-contrast
;; experience with vivid colors against deep blacks.
;;
;; Original theme by rebelot see: https://github.com/rebelot/kanagawa.nvim
;; Inspiration taken from modified version in https://github.com/NvChad/base46
;; and konrad1977 https://github.com/konrad1977/emacs .
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-kanagawa-abyss-theme nil
  "Options for the `doom-kanagawa-abyss' theme."
  :group 'doom-themes)

(defcustom doom-kanagawa-abyss-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanagawa-abyss-theme
  :type 'boolean)

(defcustom doom-kanagawa-abyss-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanagawa-abyss-theme
  :type 'boolean)

(defcustom doom-kanagawa-abyss-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kanagawa-abyss-theme
  :type '(choice integer boolean))

(defcustom doom-kanagawa-abyss-red-cursor nil
  "If non-nil, cursor will be red."
  :group 'doom-kanagawa-abyss-theme
  :type 'boolean)

;;
;;; Theme definition

(def-doom-theme doom-kanagawa-abyss
  "A dark theme combining Dragon's deep blacks with Wave's vibrant colors."

  ;; name        default   256       16
  ;; Dragon's deeper backgrounds for more contrast
  ((bg         '("#181616" "#1c1c1c" "black"        ))
   (fg         '("#DCD7BA" "#d0d0d0" "brightwhite"  ))

   (bg-alt     '("#0d0c0c" "#080808" "black"        ))
   (fg-alt     '("#C8C093" "#c6c6c6" "white"        ))

   ;; Base colors - using Dragon's darker gradient
   (base0      '("#0d0c0c" "#080808" "black"        ))
   (base1      '("#12120f" "#121212" "brightblack"  ))
   (base2      '("#1D1C19" "#1c1c1c" "brightblack"  ))
   (base3      '("#282727" "#303030" "brightblack"  ))
   (base4      '("#393836" "#3a3a3a" "brightblack"  ))
   (base5      '("#625e5a" "#5f5f5f" "brightblack"  ))
   (base6      '("#727169" "#767676" "brightblack"  ))
   (base7      '("#C8C093" "#c6c6c6" "brightblack"  ))
   (base8      '("#DCD7BA" "#d0d0d0" "white"        ))

   (grey       base6)

   ;; Core semantic colors - Wave's bright, vibrant palette
   (red        '("#FF5D62" "#ff5f5f" "red"          ))
   (orange     '("#FFA066" "#ff875f" "brightred"    ))
   (green      '("#98BB6C" "#87af87" "green"        ))
   (teal       '("#7AA89F" "#87af87" "brightgreen"  ))
   (yellow     '("#E6C384" "#d7af87" "yellow"       ))
   (blue       '("#7FB4CA" "#87afd7" "brightblue"   ))
   (dark-blue  '("#7E9CD8" "#8787d7" "blue"         ))
   (magenta    '("#957FB8" "#8787af" "brightmagenta"))
   (violet     '("#9CABCA" "#afafd7" "magenta"      ))
   (cyan       '("#A3D4D5" "#afd7d7" "brightcyan"   ))
   (dark-cyan  '("#658594" "#5f8787" "cyan"         ))

   ;; Extended Wave palette colors
   (sakura-pink      '("#D27E99" "#d787af" "magenta"      ))
   (peach-red        '("#FF5D62" "#ff5f5f" "red"          ))
   (wave-aqua-1      '("#6A9589" "#5f8787" "cyan"         ))
   (wave-blue-1      '("#223249" "#303030" "blue"         ))
   (wave-blue-2      '("#2D4F67" "#5f5f87" "brightblue"   ))
   (spring-violet-1  '("#938AA9" "#8787af" "magenta"      ))
   (spring-violet-2  '("#9CABCA" "#afafd7" "brightmagenta"))
   (boat-yellow-1    '("#938056" "#87875f" "yellow"       ))
   (boat-yellow-2    '("#C0A36E" "#afaf5f" "yellow"       ))
   (oni-violet-2     '("#b8b4d0" "#afafd7" "brightmagenta"))
   (comet            '("#54536D" "#4e4e4e" "brightblack"  ))

   ;; Diagnostic colors (Wave)
   (samurai-red      '("#E82424" "#d70000" "red"          ))
   (ronin-yellow     '("#FF9E3B" "#ff8700" "brightyellow" ))

   ;; VCS colors (Wave)
   (autumn-green     '("#76946A" "#5f875f" "green"        ))
   (autumn-red       '("#C34043" "#af5f5f" "red"          ))
   (autumn-yellow    '("#DCA561" "#d7af5f" "yellow"       ))

   ;; Diff colors (Wave)
   (winter-green     '("#2B3328" "#303030" "brightblack"  ))
   (winter-yellow    '("#49443C" "#3a3a3a" "brightblack"  ))
   (winter-red       '("#43242B" "#3a3a3a" "brightblack"  ))
   (winter-blue      '("#252535" "#303030" "brightblack"  ))

   ;; Face categories - Wave color mappings with high contrast
   (highlight      dark-blue)
   (vertical-bar   base3)
   (selection      wave-blue-1)
   (builtin        blue)
   (comments       (if doom-kanagawa-abyss-brighter-comments wave-aqua-1 base6))
   (doc-comments   (if doom-kanagawa-abyss-brighter-comments wave-aqua-1 comet))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         wave-blue-1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    autumn-yellow)
   (vc-added       autumn-green)
   (vc-deleted     autumn-red)

   ;; Mode-line configuration
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-kanagawa-abyss-brighter-modeline
                                 (doom-darken dark-blue 0.45)
                               base0))
   (modeline-bg-alt          (if doom-kanagawa-abyss-brighter-modeline
                                 (doom-darken dark-blue 0.475)
                               (doom-darken base0 0.1)))
   (modeline-bg-inactive     base1)
   (modeline-bg-inactive-alt (doom-darken base1 0.1))

   (-modeline-pad
    (when doom-kanagawa-abyss-padded-modeline
      (if (integerp doom-kanagawa-abyss-padded-modeline)
          doom-kanagawa-abyss-padded-modeline
        4))))

  ;;;; Base theme face overrides
  (
   ;; Basic faces - high contrast on deep black
   (default :background bg :foreground fg)
   (cursor :background (if doom-kanagawa-abyss-red-cursor red blue) :foreground base0)
   (hl-line :background base3)
   (region :background region :distant-foreground fg)
   ((line-number &override) :foreground base5 :background base0)
   ((line-number-current-line &override) :foreground violet :background base3 :bold t)
   (fringe :background base0 :foreground base4)
   (vertical-border :foreground base3)

   ;; Font lock
   ((font-lock-builtin-face &override) :foreground blue)
   ((font-lock-comment-face &override) :foreground comments :italic t
    :background (if doom-kanagawa-abyss-brighter-comments (doom-lighten bg 0.05)))
   ((font-lock-comment-delimiter-face &override) :foreground comments :italic t)
   ((font-lock-constant-face &override) :foreground violet)
   ((font-lock-doc-face &override) :foreground doc-comments :italic t)
   ((font-lock-function-name-face &override) :foreground magenta)
   ((font-lock-keyword-face &override) :foreground blue :weight 'semi-bold)
   ((font-lock-string-face &override) :foreground green :italic t)
   ((font-lock-type-face &override) :foreground yellow :weight 'semi-bold)
   ((font-lock-variable-name-face &override) :foreground (doom-lighten magenta 0.4))
   ((font-lock-warning-face &override) :foreground ronin-yellow)
   ((font-lock-negation-char-face &override) :foreground peach-red)
   ((font-lock-preprocessor-face &override) :foreground boat-yellow-2)
   ((font-lock-regexp-grouping-backslash &override) :foreground boat-yellow-2)

   ;; Mode-line
   (mode-line
    :background modeline-bg :foreground modeline-fg :bold t
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanagawa-abyss-brighter-modeline base8 highlight))
   (mode-line-buffer-id :foreground teal :bold t)
   (mode-line-highlight :foreground boat-yellow-2)

   ;; Highlighting
   (match :background boat-yellow-2 :foreground base0 :bold t)
   (trailing-whitespace :background comet)
   (lazy-highlight :background wave-blue-2 :foreground fg :bold t)
   ((isearch &override) :background autumn-yellow :foreground base0 :bold t)
   ((isearch-fail &override) :background samurai-red :foreground bg)
   (show-paren-match :background wave-aqua-1 :foreground base0 :bold t)
   (show-paren-mismatch :background samurai-red :foreground fg-alt)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background blue :foreground fg)
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
   (company-tooltip :background base3 :foreground fg)
   (company-tooltip-common :foreground autumn-yellow :bold t)
   (company-tooltip-quick-access :foreground violet)
   (company-tooltip-scrollbar-thumb :background autumn-red)
   (company-tooltip-scrollbar-track :background base3)
   (company-tooltip-search :background boat-yellow-2 :foreground base0 :distant-foreground fg)
   (company-tooltip-selection :background wave-blue-2 :foreground fg :bold t)
   (company-tooltip-mouse :background base3 :foreground base0 :distant-foreground fg)
   (company-tooltip-annotation :foreground peach-red :distant-foreground base1)
   (company-scrollbar-bg :inherit 'tooltip)
   (company-scrollbar-fg :background samurai-red)
   (company-preview :foreground boat-yellow-2)
   (company-preview-common :foreground autumn-yellow :bold t)
   (company-preview-search :inherit 'company-tooltip-search)
   (company-template-field :inherit 'match)

   ;;;; corfu
   (corfu-current :background base3)
   (corfu-border :background base3)
   (corfu-default :background base2)

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
   (doom-modeline-bar :background (if doom-kanagawa-abyss-brighter-modeline modeline-bg spring-violet-1) :bold t)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold :foreground teal)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   (doom-modeline-buffer-modified :foreground boat-yellow-2 :bold t)
   (doom-modeline-buffer-major-mode :foreground teal :bold t)
   (doom-modeline-panel :inherit 'bold :background orange :foreground base0)
   (doom-modeline-info :bold t :foreground green)
   (doom-modeline-warning :foreground yellow :bold t)
   (doom-modeline-urgent :foreground red :bold t)
   (doom-modeline-evil-insert-state :foreground samurai-red)
   (doom-modeline-evil-visual-state :foreground green)
   (doom-modeline-evil-replace-state :foreground ronin-yellow)
   (doom-modeline-evil-normal-state :foreground cyan)
   (doom-modeline-evil-motion-state :foreground cyan)

   ;;;; ediff
   (ediff-current-diff-A :background winter-red :foreground fg)
   (ediff-current-diff-B :background winter-green :foreground fg)
   (ediff-current-diff-C :background winter-blue :foreground fg)
   (ediff-fine-diff-A :background autumn-red :foreground bg :bold t)
   (ediff-fine-diff-B :background autumn-green :foreground bg :bold t)
   (ediff-fine-diff-C :background winter-yellow :foreground bg :bold t)

   ;;;; evil
   (evil-ex-lazy-highlight :background autumn-green :foreground winter-green :bold t)
   (evil-ex-substitute-matches :background autumn-red :foreground winter-red :bold t)
   (evil-ex-substitute-replacement :foreground orange :strike-through nil)
   (evil-search-highlight-persist-highlight-face :background boat-yellow-2 :foreground base0)
   (evil-snipe-first-match-face :foreground blue :background bg-alt)

   ;;;; flycheck
   (flycheck-error (:underline `(:style wave :color ,samurai-red)))
   (flycheck-warning (:underline `(:style wave :color ,ronin-yellow)))
   (flycheck-info (:underline `(:style wave :color ,wave-aqua-1)))
   (flycheck-fringe-error :foreground samurai-red)
   (flycheck-fringe-warning :foreground ronin-yellow)
   (flycheck-fringe-info :foreground wave-aqua-1)

   ;;;; git-gutter
   (git-gutter:added :foreground autumn-green :background base0)
   (git-gutter:modified :foreground autumn-yellow :background base0)
   (git-gutter:deleted :foreground autumn-red :background base0)

   ;;;; indent-guides
   (highlight-indent-guides-character-face :foreground base4)
   (highlight-indent-guides-stack-character-face :foreground base4)
   (highlight-indent-guides-stack-odd-face :foreground base4)
   (highlight-indent-guides-stack-even-face :foreground comet)
   (highlight-indent-guides-even-face :foreground base3)
   (highlight-indent-guides-odd-face :foreground comet)

   ;;;; ivy
   (ivy-current-match :background dark-blue :foreground base0 :bold t)
   (ivy-action :background nil :foreground fg)
   (ivy-grep-line-number :background nil :foreground green)
   (ivy-minibuffer-match-face-1 :background nil :foreground red)
   (ivy-minibuffer-match-face-2 :background nil :foreground green)
   (ivy-minibuffer-match-face-3 :background nil :foreground cyan)
   (ivy-minibuffer-match-face-4 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (ivy-grep-info :foreground cyan)
   (ivy-confirm-face :foreground teal)
   (ivy-posframe :background base3)
   (ivy-posframe-border :background base4)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   (font-latex-script-char-face :foreground cyan)

   ;;;; lsp-mode and lsp-ui
   (lsp-face-highlight-textual :background wave-blue-2)
   (lsp-face-highlight-read :background wave-blue-2)
   (lsp-face-highlight-write :background wave-blue-2)
   (lsp-headerline-breadcrumb-path-error-face (:foreground base5 :background base0 :underline (:color green :style wave)))
   (lsp-headerline-breadcrumb-path-face :background base0 :foreground fg)
   (lsp-headerline-breadcrumb-symbols-error-face :foreground samurai-red)
   (lsp-ui-doc-background :background base0 :foreground fg)
   (lsp-ui-doc-header :background base0 :foreground fg :bold t)
   (lsp-ui-doc-border :foreground base4)
   (lsp-ui-sideline-code-action :foreground boat-yellow-2)
   (lsp-ui-sideline-current-symbol :foreground blue)
   (lsp-ui-sideline-symbol :foreground dark-cyan)
   (lsp-ui-peek-filename :foreground cyan)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-header-face-1 :foreground red :height 1.3 :bold t)
   (markdown-header-face-2 :foreground violet :height 1.15 :bold t)
   (markdown-header-face-3 :foreground boat-yellow-2 :height 1.05)
   ((markdown-code-face &override) :background base0 :foreground green)
   (markdown-inline-code-face :background base0 :foreground green)
   (markdown-blockquote-face :foreground oni-violet-2)

   ;;;; org-mode
   (org-block :background base0 :foreground fg)
   (org-block-begin-line :background winter-blue :foreground blue)
   (org-block-end-line :background winter-red :foreground peach-red)
   (org-code :background base0 :foreground green)
   (org-meta-line :background winter-green :foreground green)
   (org-level-1 :foreground peach-red :height 1.3 :bold t)
   (org-level-2 :foreground violet :height 1.15 :bold t)
   (org-level-3 :foreground boat-yellow-2 :height 1.05)
   (org-level-4 :foreground fg)
   (org-level-5 :foreground fg)
   (org-level-6 :foreground yellow)
   (org-level-7 :foreground orange)
   (org-level-8 :foreground green)
   (org-todo :foreground orange :bold t)
   (org-done :foreground dark-cyan :strike-through t)
   (org-headline-done :foreground dark-cyan :strike-through t)
   (org-ellipsis :foreground wave-blue-2 :bold t)
   (org-date :foreground wave-blue-2)
   (org-footnote :foreground teal)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-mismatched-face :foreground samurai-red)
   (rainbow-delimiters-unmatched-face :foreground teal)
   (rainbow-delimiters-base-error-face :foreground samurai-red)
   (rainbow-delimiters-base-face :foreground base5)
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground dark-cyan)
   (rainbow-delimiters-depth-3-face :foreground spring-violet-1)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground teal)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground red)
   (rainbow-delimiters-depth-8-face :foreground cyan)
   (rainbow-delimiters-depth-9-face :foreground violet)

   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)

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
   (vertico-current :background wave-blue-1 :foreground yellow :bold t)
   (vertico-multiline :background samurai-red)
   (vertico-group-title :background winter-blue :foreground cyan :bold t)
   (vertico-group-separator :background winter-blue :foreground cyan :strike-through t)
   (vertico-posframe-border :background base4)
   (vertico-posframe :background base3)

   ;;;; vterm
   (vterm-color-bright-black :inherit 'term-color-bright-black :foreground base5)

   ;;;; which-key
   (which-key-command-description-face :foreground dark-blue)
   (which-key-group-description-face :foreground red)
   (which-key-local-map-description-face :foreground yellow)
   (which-key-key-face :foreground teal)
   (which-key-posframe :background wave-blue-1)
   (which-key-posframe-border :background wave-blue-1)

   ;;;; whitespace-mode
   (whitespace-space :foreground base5)
   (whitespace-tab :foreground base5)
   (whitespace-newline :foreground base5)
   (whitespace-trailing :background comet)
   (whitespace-line :background winter-red :foreground samurai-red)
   )

  ;;;; Base theme variable overrides
  ())

;;; doom-kanagawa-abyss-theme.el ends here
