;; doom-monokai-machine-theme.el --- Machine filter of Monokai Pro -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;; Code:
(defgroup doom-monokai-machine-theme nil
  "Options for doom-molokai."
  :group 'doom-themes)

(defcustom doom-monokai-machine-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-monokai-machine-theme
  :type 'boolean)

(defcustom doom-monokai-machine-comment-bg doom-monokai-machine-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'doom-monokai-machine-theme
  :type 'boolean)

(defcustom doom-monokai-machine-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-monokai-machine-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-monokai-machine
  "A dark, vibrant theme inspired by Textmate's Monokai."

  ;; name        gui       256       16
  ((bg         '("#273136" nil       nil            ))
   (bg-alt     '("#1E2528" nil       nil            ))
   (base0      '("#131313" "#121212" "black"        ))
   (base1      '("#161b1e" "#1c1c1c" "black"        ))
   (base2      '("#2e363b" "#262626" "brightblack"  ))
   (base3      '("#3a4449" "#3a3a3a" "brightblack"  ))
   (base4      '("#545f62" "#585858" "brightblack"  ))
   (base5      '("#5a6568" "#585858" "brightblack"  ))
   (base6      '("#6b7678" "#6c6c6c" "brightblack"  ))
   (base7      '("#8b9798" "#8a8a8a" "brightblack"  ))
   (base8      '("#b4c1c0" "#bcbcbc" "white"        ))
   (fg         '("#f2fffc" "#ffffff" "brightwhite"  ))
   (fg-alt     '("#c6c6c6" "#c6c6c6" "white"        ))

   (grey       base4)
   (red        '("#ff6d7e" "#ff69bf" "red"          ))
   (orange     '("#ffb270" "#ff7f50" "brightred"    ))
   (green      '("#a2e57b" "#90ee90" "green"        ))
   (yellow     '("#ffed72" "#f0e68c" "yellow"       ))
   (violet     '("#baa0f8" "#9370db" "magenta"      ))
   (cyan       '("#7cd5f1" "#40e0d0" "brightcyan"   ))
   (magenta     cyan)
   (blue        cyan)
   (dark-blue   cyan)
   (teal        cyan)
   (dark-cyan   cyan)

   ;; face categories
   (highlight      yellow)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base2)
   (builtin        violet)
   (comments       (if doom-monokai-machine-brighter-comments violet base6))
   (doc-comments   (if doom-monokai-machine-brighter-comments (doom-lighten violet 0.1) (doom-lighten base6 0.25)))
   (constants      violet)
   (functions      green)
   (keywords       magenta)
   (methods        green)
   (operators      red)
   (type           cyan)
   (strings        yellow)
   (variables      fg)
   (numbers        violet)
   (region         base3)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-monokai-machine-padded-modeline
      (if (integerp doom-monokai-machine-padded-modeline) doom-monokai-machine-padded-modeline 4)))


   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;;;; Base theme face overrides
  ((cursor                                       :background fg)
   ;; I-search
   (match                                        :foreground fg :background base3)
   (isearch                                      :inherit 'match :box `(:line-width 2 :color ,yellow))
   (lazy-highlight                               :inherit 'match)
   (isearch-fail                                 :foreground red)
   ;; current line
   (hl-line                                      :background base3)
   ;; line-numbers
   ((line-number &override)                      :foreground base4 :distant-foreground nil)
   ((line-number-current-line &override)         :foreground base7 :distant-foreground nil)
   ;; mode-line
   (mode-line                                    :background base3 :foreground fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color red)))
   (mode-line-inactive                           :background bg :foreground fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color red)))

   ;;;; centaur-tabs
   (centaur-tabs-selected-modified               :inherit 'centaur-tabs-selected :foreground yellow)
   (centaur-tabs-unselected-modified             :inherit 'centaur-tabs-unselected :foreground yellow)
   (centaur-tabs-active-bar-face                 :background yellow)
   (centaur-tabs-modified-marker-selected        :inherit 'centaur-tabs-selected :foreground fg)
   (centaur-tabs-modified-marker-unselected      :inherit 'centaur-tabs-unselected :foreground fg)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property                     :foreground keywords)
   ;;;; deadgrep
   (deadgrep-match-face                          :inherit 'match :box `(:line-width 2 :color ,yellow))

   ;;;; doom-modeline
   (doom-modeline-bar                            :background yellow)
   (doom-modeline-buffer-file                    :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path                    :inherit 'bold :foreground green)
   (doom-modeline-buffer-project-root            :foreground green :weight 'bold)
   (doom-modeline-buffer-modified                :inherit 'bold :foreground orange)
   ;;;; doom-emacs
   (doom-dashboard-menu-title                    :foreground yellow)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A                            :background (doom-blend red bg 0.3) :weight 'bold)
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background violet)
   ;;;; evil-snipe
   (evil-snipe-first-match-face                  :foreground base0 :background green)
   (evil-snipe-matches-face                      :foreground green :underline t)
   ;;;; flycheck
   ((flycheck-error &override)                   :background base3)
   ((flycheck-warning &override)                 :background base3)
   ((flycheck-info &override)                    :background base3)
   ;;;; helm
   (helm-swoop-target-line-face                  :foreground red :inverse-video t)
   ;;;; ivy
   (ivy-action                                   :foreground violet)
   (ivy-confirm-face                             :foreground green)
   (ivy-current-match                            :background base3)
   (ivy-cursor                                   :foreground bg :background fg)
   (ivy-grep-info                                :foreground red)
   (ivy-grep-line-number                         :foreground red)
   (ivy-highlight-face                           :background base3 :foreground fg)
   (ivy-match-required-face                      :foreground red)
   (ivy-minibuffer-match-face-1                  :foreground yellow)
   (ivy-minibuffer-match-face-2                  :foreground yellow :weight 'bold)
   (ivy-minibuffer-match-face-3                  :foreground green)
   (ivy-minibuffer-match-face-4                  :foreground green :weight 'bold)
   (ivy-minibuffer-match-highlight               :foreground base6 :background base3)
   (ivy-modified-buffer                          :foreground fg)
   (ivy-modified-outside-buffer                  :foreground fg)
   (ivy-org                                      :foreground base3 :italic italic)
   (ivy-prompt-match                             :foreground bg :background yellow)
   (ivy-remote                                   :foreground violet)
   (ivy-separator                                :foreground base3)
   (ivy-subdir                                   :foreground green)
   (ivy-virtual                                  :foreground violet)
   (ivy-yanked-word                              :foreground base6 :background base3)
   ;;;; lsp-mode
   (lsp-face-highlight-read                     :background base3)
   (lsp-face-highlight-textual                  :background base3)
   (lsp-face-highlight-write                    :background base4)
   ;;;; lsp-ui
   ;; HIGHLY recommended: (setq lsp-ui-peek-fontify 'always)
   (lsp-ui-peek-header                          :foreground fg :background base5)
   (lsp-ui-peek-footer                          :inherit 'lsp-ui-peek-header)
   (lsp-ui-peek-selection                       :foreground bg :background yellow)
   (lsp-ui-peek-list                            :background base3)
   (lsp-ui-peek-peek                            :inherit 'lsp-ui-peek-list)
   (lsp-ui-peek-highlight                       :inherit 'isearch)
   (lsp-ui-peek-filename                        :foreground base8 :weight 'bold)
   ;;;; markdown-mode
   (markdown-blockquote-face                     :inherit 'italic :foreground dark-blue)
   (markdown-list-face                           :foreground red)
   (markdown-pre-face                            :foreground cyan)
   (markdown-link-face                           :inherit 'bold :foreground blue)
   ((markdown-code-face &override)               :background (doom-lighten base2 0.045))
   ;;;; neotree
   (neo-dir-link-face                            :foreground cyan)
   (neo-expand-btn-face                          :foreground red)
   ;;;; outline <built-in>
   ((outline-1 &override)                        :foreground yellow)
   ((outline-2 &override)                        :foreground blue)
   ((outline-3 &override)                        :foreground green)
   ((outline-4 &override)                        :foreground fg)
   (outline-5                                    :inherit 'outline-4)
   (outline-6                                    :inherit 'outline-5)
   (outline-7                                    :inherit 'outline-6)
   (outline-8                                    :inherit 'outline-7)
   ;;;; org <built-in>
   (org-ellipsis                                 :foreground orange)
   (org-tag                                      :foreground yellow :weight 'normal)
   ((org-quote &override)                        :inherit 'italic :foreground base7 :background org-quote)
   (org-todo                                     :foreground yellow)
   (org-list-dt                                  :foreground yellow)
   ;;;;;; php-mode
   (php-php-tag                                  :foreground orange)
   (php-function-name                            :foreground green)
   (php-function-call                            :foreground green)
   (php-string                                   :foreground yellow)
   (php-keyword                                  :foreground blue)
   (php-builtin                                  :foreground violet)
   (php-method-call                              :foreground green)
   (php-static-method-call                       :foreground green)
   (php-variable-name                            :foreground fg)
   (php-property-name                            :foreground fg)
   (php-variable-sigil                           :foreground base8)
   (php-operator                                 :foreground red)
   (php-paamayim-nekudotayim                     :foreground red)
   (php-type                                     :foreground blue :italic italic)
   (php-class                                    :foreground red)
   (php-constant                                 :foreground violet)
   (php-constant-assign                          :foreground blue)
   (php-magical-constant                         :foreground violet)
   (php-$this                                    :foreground base8 :italic italic)
   (php-$this-sigil                              :foreground base8 :italic italic)
   (php-errorcontrol-op                          :foreground red)
   (php-doc-annotation-tag                       :foreground blue)
   (php-doc-variable-sigil                       :foreground base6)
   (php-doc-$this                                :foreground base6)
   (php-doc-$this-sigil                          :foreground base6)
   (php-doc-class-name                           :foreground base6)
   ;; As soon as https://github.com/emacs-php/php-mode/pull/606
   ;; is merged these can be uncommented.
   ;;(php-class-declaration-spec                   :foreground red)
   ;;(php-class-modifier                           :foreground red)
   ;;(php-namespace-declaration                    :foreground red)
   ;;(php-import-declaration                       :foreground red)
   ;;(php-method-modifier                          :foreground red :italic italic)
   ;;(php-method-access                            :foreground red :italic italic)
   ;;(php-method-static                            :foreground red :italic italic)
   ;;(php-property-access                          :foreground red :italic italic)
   ;;(php-property-const                           :foreground red :italic italic)
   ;;(php-property-static                          :foreground red :italic italic)
   ;;(php-block-delimiter                          :foreground base7)
   ;;(php-flow-control-statement                   :foreground red)
   ;;(php-block-statement                          :foreground red)
   ;;(php-include-statement                        :foreground green)
   ;;(php-constant-keyword                         :foreground violet)
   ;;(php-number                                   :foreground violet)
   ;;(php-string-quote                             :foreground base7)
   ;;(php-type-operator                            :foreground red)
   ;;(php-print-statement                          :foreground green)
   ;;(php-return-type-colon                        :foreground red)
   ;;(php-function-keyword                         :foreground blue :italic italic)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face              :foreground red)
   (rainbow-delimiters-depth-2-face              :foreground orange)
   (rainbow-delimiters-depth-3-face              :foreground green)
   (rainbow-delimiters-depth-4-face              :foreground cyan)
   (rainbow-delimiters-depth-5-face              :foreground red)
   (rainbow-delimiters-depth-6-face              :foreground orange)
   (rainbow-delimiters-depth-7-face              :foreground green)
   ;;;; show-paren-mode
   (show-paren-match                             :weight 'bold :foreground green)
   (show-paren-mismatch                          :weight 'bold :foreground red)
   ;;;; swiper
   (swiper-background-match-face-1               :inherit 'match :weight 'bold)
   (swiper-background-match-face-2               :inherit 'match)
   (swiper-background-match-face-3               :inherit 'match :foreground green)
   (swiper-background-match-face-4               :inherit 'match :weight 'bold :foreground green)
   (swiper-match-face-1                          :inherit 'isearch :weight 'bold)
   (swiper-match-face-2                          :inherit 'isearch)
   (swiper-match-face-3                          :inherit 'isearch :foreground green)
   (swiper-match-face-4                          :inherit 'isearch :weight 'bold :foreground green)
   (swiper-line-face                             :inherit 'hl-line)
   ;;;; term <built-in>
   (term-color-black                            :foreground base3)
   (term-color-blue                             :foreground blue)
   (term-color-cyan                             :foreground violet)
   (term-color-green                            :foreground green)
   (term-color-magenta                          :foreground red)
   (term-color-red                              :foreground red)
   (term-color-white                            :foreground fg)
   (term-color-yellow                           :foreground yellow)
   ;;;; treemacs
   (treemacs-git-added-face                     :foreground green)
   (treemacs-git-conflict-face                  :foreground red)
   (treemacs-git-ignored-face                   :foreground base6)
   (treemacs-git-modified-face                  :foreground violet)
   (treemacs-git-renamed-face                   :foreground orange)
   (treemacs-git-untracked-face                 :inherit 'treemacs-git-renamed-face)
   (treemacs-on-failure-pulse-face              :foreground base0 :background red)
   (treemacs-on-success-pulse-face              :foreground base0 :background green)
   ;;;; web-mode
   (web-mode-html-tag-face                      :foreground red)
   (web-mode-html-tag-bracket-face              :foreground base7)
   (web-mode-html-attr-name-face                :foreground cyan :italic italic)
   (web-mode-html-attr-equal-face               :inherit 'web-mode-html-tag-bracket-face)
   ;; Apparently web-mode has no face for values of css properties.
   (web-mode-css-selector-face                  :foreground green)
   (web-mode-css-property-name-face             :foreground base7))

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-monokai-machine-theme.el ends here
