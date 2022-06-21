;;; doom-flatwhite-theme.el --- inspired by Atom's Flatwhite Syntax theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: August 9, 2020 (#507)
;; Author: JuneKelly <https://github.com/JuneKelly>
;; Maintainer:
;; Source: https://github.com/biletskyy/flatwhite-syntax
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-flatwhite-theme nil
  "Options for the `doom-flatwhite' theme."
  :group 'doom-themes)

(defcustom doom-flatwhite-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-flatwhite-theme
  :type 'boolean)

(defcustom doom-flatwhite-no-highlight-variables nil
  "If non-nil, removes highlight on variable names"
  :group 'doom-flatwhite-theme
  :type 'boolean)

(defcustom doom-fw-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-flatwhite-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-flatwhite
  "A minimal light syntax theme"

  ;; name        default   256       16
  (
   (fw-base1           '("#605a52" "#666666" "black"))
   (fw-base2           '("#93836c" "#999966" "brightblack"))
   (fw-base3           '("#b9a992" "#cc9999" "brightblack"))
   (fw-base4           '("#dcd3c6" "#cccccc" "brightblack"))
   (fw-base5           '("#e4ddd2" "#cccccc" "brightblack"))
   (fw-base6           '("#f1ece4" "#ffffcc" "brightblack"))
   (fw-base7           '("#f7f3ee" "#ffffff" "brightblack"))

   (fw-accent          '("#6a4dff" "#6666ff" "brightblue"))

   (fw-orange-text     '("#5b5143" "#666633" "brightblack"))
   (fw-orange-text-sec '("#957f5f" "#996666" "brightblack"))
   (fw-orange          '("#f08c00" "#ff9900" "orange"))
   (fw-orange-blend    '("#f7e0c3" "#ffcccc" "brightorange"))

   (fw-red-text        '("#5b4343" "#663333" "brightblack"))
   (fw-red-text-sec    '("#955f5f" "#996666" "brightblack"))
   (fw-red             '("#f00000" "#ff0000" "red"))
   (fw-red-blend       '("#f6cfcb" "#ffcccc" "brightred"))

   (fw-green-text      '("#525643" "#666633" "brightblack"))
   (fw-green-text-sec  '("#81895d" "#999966" "brightblack"))
   (fw-green           '("#84bd00" "#99cc00" "green"))
   (fw-green-blend     '("#e2e9c1" "#ccffcc" "brightgreen"))

   (fw-teal-text       '("#465953" "#336666" "brightblack"))
   (fw-teal-text-sec   '("#5f8c7d" "#669966" "brightblack"))
   (fw-teal            '("#00bda4" "#00cc99" "cyan"))
   (fw-teal-blend      '("#d2ebe3" "#ccffcc" "brightcyan"))

   (fw-blue-text       '("#4c5361" "#336666" "brightblack"))
   (fw-blue-text-sec   '("#7382a0" "#669999" "brightblack"))
   (fw-blue            '("#75a3ff" "#6699ff" "blue"))
   (fw-blue-blend      '("#dde4f2" "#ccccff" "brightblue"))

   (fw-purple-text     '("#614c61" "#663366" "brightblack"))
   (fw-purple-text-sec '("#9c739c" "#996699" "brightblack"))
   (fw-purple          '("#ce5cff" "#cc66ff" "purple"))
   (fw-purple-blend    '("#f1ddf1" "#ffccff" "brightpurple"))

   (bg         `(,(car fw-base7) nil       nil            ))
   (bg-alt     `(,(car fw-base6) nil       nil            ))
   (base0      fw-base6)
   (base1      fw-base5 )
   (base2      fw-base4 )
   (base3      fw-base3 )
   (base4      fw-base2 )
   (base5      fw-base1 )
   (base6      '("#202328"       nil "brightblack"  ))
   (base7      '("#1c1f24"       nil "brightblack"  ))
   (base8      '("#1b2229"       nil "black"        ))
   (fg         `(,(car fw-base1) nil "black"        ))
   (fg-alt     `(,(car fw-base2) nil "brightblack"  ))

   (grey       base3)
   (red        fw-red-text-sec)
   (orange     fw-orange-text-sec)
   (green      fw-green-text-sec)
   (teal       fw-teal-text-sec)
   (yellow     fw-orange-text-sec)
   (blue       fw-blue-text-sec)
   (dark-blue  fw-blue-text-sec)
   (magenta    fw-purple-text-sec)
   (violet     fw-purple-text-sec)  ;; TODO fix these
   (cyan       fw-teal-text-sec)
   (dark-cyan  fw-teal-text-sec)

   (fw--light-accent (doom-lighten fw-accent 0.85))

   ;; face categories -- required for all themes
   (highlight       blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base3)
   (doc-comments   (doom-darken comments 0.15))
   (constants      violet)
   (functions      magenta)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           orange)
   (strings        green)
   (variables      (doom-darken magenta 0.36))
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-flatwhite-brighter-modeline)
   (-no-highlight-variables doom-flatwhite-no-highlight-variables)
   (-modeline-pad
    (when doom-fw-padded-modeline
      (if (integerp doom-fw-padded-modeline) doom-fw-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt fw-base2)

   (modeline-bg
    (if -modeline-bright
        (doom-darken base2 0.05)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base2 0.1)
      base2))
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))


  ;;;; Base theme face overrides
  ((font-lock-builtin-face :inherit 'italic :foreground fg :extend t)
   ((font-lock-doc-face &override) :slant 'italic)
   (font-lock-type-face :inherit 'default)
   (font-lock-variable-name-face
    :foreground (if -no-highlight-variables fg fw-blue-text)
    :background (if -no-highlight-variables bg fw-blue-blend))
   (font-lock-warning-face              :background fw-red-blend
                                        :foreground fw-red-text)
   (font-lock-negation-char-face        :inherit 'default)
   (font-lock-preprocessor-face         :inherit 'default)
   (font-lock-preprocessor-char-face    :inherit 'default)
   (font-lock-regexp-grouping-backslash :inherit 'default)
   (font-lock-regexp-grouping-construct :inherit 'default)
   (font-lock-constant-face             :background fw-teal-blend
                                        :foreground fw-teal-text)
   (font-lock-function-name-face        :foreground fg
                                        :weight 'semi-bold)
   (font-lock-keyword-face              :background fw-purple-blend
                                        :foreground fw-purple-text)
   (font-lock-string-face               :background fw-green-blend
                                        :foreground fw-green-text )

   (lazy-highlight :background fw--light-accent
                   :foreground fw-blue-text
                   :distant-foreground base0
                   :weight 'bold)

   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)

   (mode-line
    :background modeline-bg
    :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive
    :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; swiper
   (swiper-line-face    :background fw--light-accent
                        :foreground fw-blue-text)
   (swiper-match-face-1 :inherit 'unspecified
                        :background base0
                        :foreground fg)
   (swiper-background-match-face-1 :inherit 'unspecified
                                   :background base0
                                   :foreground fg-alt)
   (swiper-match-face-2 :inherit 'unspecified
                        :background fw-orange-blend
                        :foreground fw-orange-text
                        :weight 'bold)
   (swiper-background-match-face-2 :inherit 'unspecified
                                   :background fw-orange-blend
                                   :foreground fw-orange-text-sec
                                   :weight 'bold)
   (swiper-match-face-3 :inherit 'unspecified
                        :background fw-green-blend
                        :foreground fw-green-text
                        :weight 'bold)
   (swiper-background-match-face-3 :inherit 'unspecified
                                   :background fw-green-blend
                                   :foreground fw-green-text-sec
                                   :weight 'bold)
   (swiper-match-face-4 :inherit 'unspecified
                        :background fw-teal-blend
                        :foreground fw-teal-text
                        :weight 'bold)
   (swiper-background-match-face-4 :inherit 'unspecified
                                   :background fw-teal-blend
                                   :foreground fw-teal-text-sec
                                   :weight 'bold)
   ;;;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-annotation            :foreground fw-purple-text-sec )
   (company-tooltip-annotation-selection  :foreground fw-purple-text )
   (company-tooltip-common                :foreground highlight
                                          :distant-foreground base0
                                          :weight 'bold)
   (company-tooltip-search     :background highlight
                               :foreground bg
                               :distant-foreground fg
                               :weight 'bold)
   (company-tooltip-search-selection :background fw-blue-blend)
   (company-tooltip-selection  :background fw--light-accent
                               :weight 'bold)
   (company-tooltip-mouse      :background magenta
                               :foreground bg
                               :distant-foreground fg)
   (company-tooltip-annotation :foreground violet
                               :distant-foreground bg)
   (company-scrollbar-bg       :inherit 'tooltip)
   (company-scrollbar-fg       :background highlight)
   (company-preview            :foreground comments)
   (company-preview-common     :background base3
                               :foreground highlight)
   (company-preview-search     :inherit 'company-tooltip-search)
   (company-template-field     :inherit 'match)
   ;;;; clojure
   (clojure-keyword-face :foreground fw-orange-text
                         :background fw-orange-blend)
   ;;;; css-mode <built-in> / scss-mode
   (css-property             :foreground fg
                             :inherit 'italic)
   (css-proprietary-property :foreground fw-orange-text
                             :background fw-orange-blend)
   (css-selector             :foreground fw-purple-text
                             :background fw-purple-blend)
   ;;;; company-box
   (company-box-candidate :foreground fg)
   ;;;; doom-emacs
   (doom-dashboard-banner      :foreground comments)
   (doom-dashboard-menu-title  :foreground fw-purple-text-sec)
   (doom-dashboard-menu-desc   :foreground fw-green-text-sec)
   (doom-dashboard-footer-icon :foreground (doom-darken yellow 0.4))
   (doom-dashboard-loaded      :foreground fw-orange-text)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-path       :foreground fw-blue-text-sec
                                    :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path )
   (doom-modeline-info              :foreground fw-green-text-sec)
   (doom-modeline-project-dir       :foreground fw-purple-text-sec)
   (doom-modeline-evil-insert-state :foreground fw-teal)
   ;;;; diff-mode
   (diff-removed :foreground red
                 :background fw-red-blend)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red
                                :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green
                                :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue
                                :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal
                                :background (doom-lighten teal 0.8))
   ;;;; elixir
   (elixir-atom-face :foreground fw-blue-text
                     :background fw-blue-blend)
   (elixir-attribute-face :foreground fw-teal-text
                          :background fw-teal-blend)
   ;;;; fill column
   (hl-fill-column-face :foreground fg
                        :background fw--light-accent)
   ;;;; git-commit
   (git-commit-summary :foreground fg)
   ;;;; highlight-numbers-mode
   (highlight-numbers-number :foreground fw-teal-text
                             :background fw-teal-blend)
   ;;;; highlight-quoted-mode
   (highlight-quoted-symbol :background fw-blue-blend
                            :foreground fw-blue-text)
   (highlight-quoted-quote  :foreground fw-teal-blend
                            :foreground fw-teal-text)
   ;;;; ivy
   (ivy-current-match :background fw-base5
                      :distant-foreground nil
                      :extend t)
   (ivy-minibuffer-match-face-1
    :background nil
    :foreground fg
    :weight 'light)
   (ivy-minibuffer-match-face-2
    :inherit 'ivy-minibuffer-match-face-1
    :foreground fw-orange-text
    :background fw-orange-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-face-3
    :inherit 'ivy-minibuffer-match-face-2
    :foreground fw-blue-text
    :background fw-blue-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-face-4
    :inherit 'ivy-minibuffer-match-face-2
    :foreground fw-green-text
    :background fw-green-blend
    :weight 'semi-bold)
   (ivy-minibuffer-match-highlight :foreground bg
                                   :background fw-purple-text-sec)
   (ivy-highlight-face :foreground fw-purple-text)
   (ivy-confirm-face :foreground success)
   (ivy-match-required-face :foreground error)
   (ivy-virtual :inherit 'italic :foreground doc-comments)
   (ivy-modified-buffer :inherit 'bold :foreground vc-modified)
   ;;;; ivy-posframe
   (ivy-posframe               :background base0)
   ;;;; js2-mode
   (js2-function-param    :foreground fg)
   (js2-function-call     :foreground fg )
   (js2-object-property   :foreground fg :inherit 'italic)
   (js2-jsdoc-tag         :foreground doc-comments)
   (js2-external-variable :foreground fg)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)
   ;;;; magit
   (magit-bisect-bad        :background fw-red-blend
                            :foreground fw-red-text)
   (magit-bisect-good       :background fw-green-blend
                            :foreground fw-green-text)
   (magit-bisect-skip       :background fw-orange-blend
                            :foreground fw-orange-text)
   (magit-blame-date        :background fw-base4
                            :foreground fw-red-text)
   (magit-blame-heading     :background fw-base4
                            :foreground fw-orange-text)
   (magit-branch-current    :background bg-alt
                            :foreground fw-blue-text)
   (magit-branch-local      :background bg-alt
                            :foreground fw-teal-text)
   (magit-branch-remote     :background bg-alt
                            :foreground fw-green-text)
   (magit-cherry-equivalent :background fw-base7
                            :foreground fw-purple-text)
   (magit-cherry-unmatched  :background fw-base7
                            :foreground fw-teal-text)

   (magit-diff-added             :foreground fw-green-text-sec
                                 :background fw-green-blend
                                 :extend t)
   (magit-diff-added-highlight   :foreground fw-green-text
                                 :background fw-green-blend
                                 :weight 'bold :extend t)

   (magit-diff-base              :foreground fw-orange-text-sec
                                 :background fw-orange-blend
                                 :extend t)
   (magit-diff-base-highlight    :foreground fw-orange-text
                                 :background fw-orange-blend
                                 :weight 'bold
                                 :extend t)

   (magit-diff-context           :foreground (doom-darken fg 0.4)
                                 :background bg
                                 :extend t)
   (magit-diff-context-highlight :foreground fg
                                 :background bg-alt
                                 :extend t)
   (magit-diff-file-heading           :foreground fw-purple-text-sec
                                      :background fw-purple-blend
                                      :weight 'bold
                                      :extend t)
   (magit-diff-file-heading-selection :foreground fw-purple-text
                                      :background fw-purple-blend
                                      :weight 'bold
                                      :extend t)
   (magit-diff-hunk-heading           :foreground fw-purple-text-sec
                                      :background fw-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-selection :foreground fw-purple-text-sec
                                      :background fw-purple-blend
                                      :extend t)
   (magit-diff-hunk-heading-highlight :foreground fw-purple-blend
                                      :background fw-purple-text-sec
                                      :weight 'bold
                                      :extend t)

   (magit-diff-removed                :foreground fw-red-text-sec
                                      :background fw-red-blend
                                      :extend t)
   (magit-diff-removed-highlight      :foreground fw-red-text
                                      :background fw-red-blend
                                      :weight 'bold
                                      :extend t)

   (magit-diff-lines-heading          :foreground yellow
                                      :background red
                                      :extend t)
   (magit-diffstat-added              :foreground fw-green)
   (magit-diffstat-removed            :foreground fw-red)
   (magit-dimmed                      :foreground comments)
   (magit-hash                        :foreground fg-alt)
   (magit-header-line :background fw-blue-blend
                      :foreground fw-blue-text
                      :weight 'bold
                      :box `(:line-width 3 :color ,fw-blue-blend))
   (magit-log-author :foreground fw-orange-text-sec)
   (magit-log-date   :foreground fw-blue-text-sec)
   (magit-log-graph  :foreground comments)
   (magit-process-ng :inherit 'error)
   (magit-process-ok :inherit 'success)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-reflog-commit :foreground green)
   (magit-reflog-merge :foreground green)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :inherit 'error)
   (magit-refname :foreground comments)
   (magit-section-heading :foreground blue
                          :weight 'bold
                          :extend t)
   (magit-section-heading-selection :foreground orange
                                    :weight 'bold
                                    :extend t)
   (magit-section-highlight :inherit 'hl-line)
   (magit-sequence-drop :foreground red)
   (magit-sequence-head :foreground blue)
   (magit-sequence-part :foreground orange)
   (magit-sequence-stop :foreground green)
   (magit-signature-bad :inherit 'error)
   (magit-signature-error :inherit 'error)
   (magit-signature-expired :foreground orange)
   (magit-signature-good :inherit 'success)
   (magit-signature-revoked :foreground magenta)
   (magit-signature-untrusted :foreground yellow)
   (magit-tag :foreground yellow)
   (magit-filename :foreground violet)
   (magit-section-secondary-heading :foreground violet
                                    :weight 'bold
                                    :extend t)
   ;;;; makefile-*-mode
   (makefile-targets :foreground fw-purple-text
                     :background fw-purple-blend)
   ;;;; markdown-mode
   (markdown-header-face           :inherit 'bold
                                   :foreground fw-purple-text
                                   :background fw-purple-blend)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground fw-green-text
                                   :background fw-green-blend)
   (markdown-list-face             :foreground fg
                                   :inherit 'bold)
   (markdown-link-face             :foreground fw-blue-text
                                   :background fw-blue-blend)
   (markdown-url-face              :foreground fw-blue-text
                                   :background fw-blue-blend)
   (markdown-italic-face           :inherit 'italic
                                   :foreground fg)
   (markdown-bold-face             :inherit 'bold
                                   :foreground fg)
   (markdown-markup-face           :foreground fg
                                   :inherit 'bold)
   (markdown-blockquote-face       :inherit 'italic
                                   :foreground doc-comments)
   (markdown-pre-face              :foreground fg)
   (markdown-code-face             :background fw-orange-blend
                                   :foreground fw-orange-text
                                   :extend t)
   (markdown-reference-face        :foreground doc-comments)
   (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face)
                                   :extend nil)
   (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
   (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
   (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
   (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
   (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)
   ;;;; org-mode
   ((outline-1 &override) :foreground red)
   ((outline-2 &override) :foreground orange)
   (org-ellipsis :underline nil :background bg     :foreground red)
   ((org-block-begin-line &override)
    :background fw-orange-blend
    :foreground fw-orange-text
    :weight 'semi-bold)
   ((org-block &override)
    :background fw-orange-blend
    :foreground fw-orange-text)
   ((org-quote &override)
    :background fw-orange-blend
    :foreground fw-orange-text)
   ;;;; racket
   (racket-keyword-argument-face :foreground fw-orange-text
                                 :background fw-orange-blend)
   (racket-selfeval-face :foreground fw-teal-text
                         :background fw-teal-blend)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground fw-blue-text-sec)
   (rainbow-delimiters-depth-2-face :foreground fw-purple-text-sec)
   (rainbow-delimiters-depth-3-face :foreground fw-green-text-sec)
   (rainbow-delimiters-depth-4-face :foreground fw-orange-text-sec)
   (rainbow-delimiters-depth-5-face :foreground fw-teal-text-sec)
   (rainbow-delimiters-depth-6-face :foreground fw-red-text-sec)
   (rainbow-delimiters-depth-7-face :foreground fw-green-text-sec)
   (rainbow-delimiters-unmatched-face  :foreground red
                                       :weight 'bold
                                       :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
   ;;;; rjsx-mode
   (rjsx-tag  :background fw-purple-blend
              :foreground fw-purple-text)
   (rjsx-text :inherit 'default)
   (rjsx-tag-bracket-face :background bg
                          :foreground fg-alt)
   (rjsx-attr :background bg
              :foreground fg
              :inherit 'italic)
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
   (web-mode-current-element-highlight-face :background dark-blue
                                            :foreground bg)
   (web-mode-css-property-name-face :foreground fg
                                    :inherit 'italic)
   (web-mode-doctype-face           :background bg
                                    :foreground comments)
   (web-mode-html-tag-face          :background fw-purple-blend
                                    :foreground fw-purple-text)
   (web-mode-html-attr-name-face    :background bg
                                    :foreground fg
                                    :inherit 'italic)
   (web-mode-html-attr-value-face   :inherit 'font-lock-string-face)
   (web-mode-html-entity-face       :background fw-orange-blend
                                    :foreground fw-orange-text
                                    :inherit 'italic)
   (web-mode-block-control-face     :background bg
                                    :foreground fw-base1)
   (web-mode-html-tag-bracket-face  :background bg
                                    :foreground fg-alt)
   (web-mode-symbol-face            :foreground fw-blue-text
                                    :background fw-blue-blend)
   (web-mode-string-face            :inherit 'font-lock-string-face)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; which-key
   (which-key-key-face                   :foreground fw-green-text-sec)
   (which-key-group-description-face     :foreground fw-purple-text-sec)
   (which-key-command-description-face   :foreground fg)
   (which-key-local-map-description-face :foreground fw-orange-text-sec)
   (which-key-separator-face             :background bg-alt :foreground comments)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (unless (default-value 'indent-tabs-mode) base0))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0)))

  ;;;; Base theme variable overrides-
  ()
  )

;;; doom-flatwhite-theme.el ends here
