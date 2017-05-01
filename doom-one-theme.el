;; DOOM One Dark (inspired by Atom One Dark)
(require 'doom-themes)

;;
(defgroup doom-one-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-one-brighter-modeline nil
  "If non-nil, the mode-line is brighter than normal."
  :group 'doom-one-theme
  :type 'boolean)

(defcustom doom-one-brighter-comments nil
  "If non-nil, comments are brighter and easier to see."
  :group 'doom-one-theme
  :type 'boolean)

;;
(def-doom-theme doom-one
  "A dark theme inspired by Atom One Dark"
  ((bg         "#282c34")
   (bg-alt     "#21242b")
   (fg         "#bbc2cf")
   (fg-alt     "#BBBBBB")
   (black      (if window-system "#1B2229"))
   (dark-grey  (if window-system "#23272e" "#262626"))
   (grey       "#3B3F46")
   (light-grey (if window-system "#5B6268" "#525252"))
   (white      "#DFDFDF")
   (red        "#ff6c6b")
   (orange     "#da8548")
   (green      "#98be65")
   (teal       "#4db5bd")
   (yellow     "#ECBE7B")
   (blue       "#51afef")
   (dark-blue  "#1f5572")
   (magenta    "#c678dd")
   (violet     "#a9a1e1")
   (cyan       "#46D9FF")
   (dark-cyan  "#5699AF")

   ;; face categories
   (highlight      blue)
   (vertical-bar   (or black "black"))
   (current-line   dark-grey)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-one-brighter-comments dark-cyan light-grey))
   (doc-comments   (if doom-one-brighter-comments (doom-lighten dark-cyan 0.1) (doom-lighten light-grey 0.2)))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      white)
   (numbers        orange)
   (region         (doom-lighten bg 0.075))
   ;; tabs
   (tab-unfocused-bg "#353a42") ; FIXME
   (tab-unfocused-fg "#1e2022") ; FIXME
   ;; main search regions
   (search-bg      blue)
   (search-fg      black)
   ;; vcs
   (vc-modified    grey)
   (vc-added       green)
   (vc-deleted     red))

  ;; --- faces ------------------------------
  (`(doom-default
     ((((type graphic)) :inherit default :background ,bg)
      (t                :inherit default)))
   `(doom-hl-line
     ((((type graphic)) :background ,current-line)
      (t                :background "black")))
   `(doom-linum
     ((((type graphic)) :inherit linum :background ,bg)
      (t                :inherit linum)))
   `(doom-minibuffer-active ((,c (:background ,bg))))
   `(doom-nlinum-highlight  ((,c (:foreground ,(doom-darken white 0.25) :bold nil))))
   `(doom-modeline-buffer-path       ((,c (:foreground ,(if bold white cyan) :bold ,bold))))
   `(doom-modeline-buffer-project    ((,c (:foreground ,fg))))
   `(doom-modeline-buffer-modified   ((,c (:foreground ,red))))
   `(doom-modeline-buffer-major-mode ((,c (:foreground ,(if bold white blue) :bold ,bold))))
   `(doom-modeline-highlight         ((,c (:foreground ,blue))))
   `(doom-modeline-panel             ((,c (:foreground ,black :background ,blue))))
   `(doom-modeline-bar               ((,c (:background ,blue))))
   `(doom-modeline-eldoc-bar         ((,c (:background ,yellow))))
   `(doom-neotree-dir-face           ((,c (:foreground ,blue))))
   `(doom-neotree-file-face          ((,c (:foreground ,fg)))) ;"#888888"
   `(doom-neotree-hidden-file-face   ((,c (:foreground ,light-grey))))
   `(doom-neotree-text-file-face     ((,c (:foreground ,white)))) ;"#888888"
   `(doom-neotree-media-file-face    ((,c (:foreground ,light-grey))))
   `(doom-neotree-data-file-face     ((,c (:foreground ,violet))))

   ;; --- base faces -------------------------
   `(bold                   ((,c (:weight ,(if bold 'bold 'normal) :color ,white))))
   `(italic                 ((,c (:slant  ,(if italic 'italic 'normal)))))
   `(bold-italic            ((,c (:inherit (bold italic)))))

   `(default
      ((((class color) (type graphic)) :background ,bg-alt :foreground ,fg)
       (((class color) (type tty)) :background "black")
       (t :foreground ,fg)))
   `(fringe                 ((,c (:inherit default :foreground ,comments))))
   `(region                 ((,c (:background ,region))))
   `(highlight              ((,c (:background ,blue :foreground ,black))))
   `(hl-line                ((,c (:background ,black))))
   `(cursor                 ((,c (:background ,blue))))
   `(shadow                 ((,c (:foreground ,light-grey))))
   `(minibuffer-prompt      ((,c (:foreground ,blue))))
   `(tooltip
     ((((type graphic)) :inherit doom-default :background ,bg-alt)
      (t                :inherit doom-default)))
   `(error                  ((,c (:foreground ,red))))
   `(warning                ((,c (:foreground ,yellow))))
   `(success                ((,c (:foreground ,green))))
   `(secondary-selection    ((,c (:background ,blue :foreground ,black))))
   `(lazy-highlight         ((,c (:background ,dark-blue :foreground ,white))))
   `(match                  ((,c (:foreground ,green :background ,black :bold ,bold))))
   `(trailing-whitespace    ((,c (:background ,red))))
   `(vertical-border        ((,c (:foreground ,vertical-bar :background ,vertical-bar))))
   `(show-paren-match       ((,c (:foreground ,red :background ,black :bold ,bold))))
   `(show-paren-mismatch    ((,c (:foreground ,black :background ,red :bold ,bold))))

   `(font-lock-builtin-face           ((,c (:foreground ,builtin))))
   `(font-lock-comment-face           ((,c (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((,c (:foreground ,comments))))
   `(font-lock-doc-face               ((,c (:foreground ,doc-comments))))
   `(font-lock-doc-string-face        ((,c (:foreground ,doc-comments))))
   `(font-lock-constant-face          ((,c (:foreground ,constants))))
   `(font-lock-function-name-face     ((,c (:foreground ,functions))))
   `(font-lock-keyword-face           ((,c (:foreground ,keywords))))
   `(font-lock-string-face            ((,c (:foreground ,strings))))
   `(font-lock-type-face              ((,c (:foreground ,type))))
   `(font-lock-variable-name-face     ((,c (:foreground ,variables))))
   `(font-lock-warning-face           ((,c (:inherit warning))))
   `(font-lock-negation-char-face          ((,c (:foreground ,operators :bold ,bold))))
   `(font-lock-preprocessor-face           ((,c (:foreground ,operators :bold ,bold))))
   `(font-lock-preprocessor-char-face      ((,c (:foreground ,operators :bold ,bold))))
   `(font-lock-regexp-grouping-backslash   ((,c (:foreground ,operators :bold ,bold))))
   `(font-lock-regexp-grouping-construct   ((,c (:foreground ,operators :bold ,bold))))

   `(mode-line
     ((((type graphic)) :background ,(if doom-one-brighter-modeline bg bg-alt))
      (t                :background ,black)))
   `(mode-line-inactive
     ((((type graphic))
       :foreground ,light-grey
       :background ,(if doom-one-brighter-modeline bg dark-grey))
      (t
       :foreground ,grey
       :background ,black)))
   `(header-line ((,c (:inherit mode-line))))

   `(linum
     ((((type graphic)) :bold nil :foreground ,(doom-lighten grey 0.05) :background ,bg-alt)
      (t                :bold nil :foreground ,light-grey)))

   ;; dired
   `(dired-directory             ((,c (:foreground ,orange))))
   `(dired-ignored               ((,c (:foreground ,light-grey))))

   ;; Search
   `(isearch                     ((,c (:background ,search-bg :foreground ,black :bold ,bold))))
   `(isearch-lazy-highlight-face ((,c (:background ,dark-grey))))
   `(yas-field-highlight-face    ((,c (:inherit match))))

   ;; Terminal colors
   `(term-color-black            ((,c (:foreground ,black :background ,black))))
   `(term-color-red              ((,c (:foreground ,red :background ,red))))
   `(term-color-green            ((,c (:foreground ,green :background ,green))))
   `(term-color-yellow           ((,c (:foreground ,yellow :background ,yellow))))
   `(term-color-blue             ((,c (:foreground ,blue :background ,blue))))
   `(term-color-magenta          ((,c (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan             ((,c (:foreground ,cyan :background ,cyan))))
   `(term-color-white            ((,c (:foreground ,white :background ,white))))

   ;; `window-divider'
   `(window-divider              ((,c (:foreground ,vertical-bar))))
   `(window-divider-first-pixel  ((,c (:foreground ,vertical-bar))))
   `(window-divider-last-pixel   ((,c (:foreground ,vertical-bar))))


   ;; --- plugin faces -----------------------
   ;; avy
   `(avy-lead-face-0    ((,c (:background ,search-bg :foreground ,search-fg))))
   `(avy-lead-face-1    ((,c (:background ,search-bg :foreground ,search-fg))))
   `(avy-lead-face-2    ((,c (:background ,search-bg :foreground ,search-fg))))
   `(avy-lead-face      ((,c (:background ,search-bg :foreground ,search-fg))))

   ;; company
   `(company-tooltip             ((,c (:inherit tooltip))))
   `(company-tooltip-common      ((,c (:foreground ,blue))))
   `(company-tooltip-search      ((,c (:foreground ,search-fg :background ,highlight))))
   `(company-tooltip-selection   ((,c (:background ,selection))))
   `(company-tooltip-mouse       ((,c (:background ,magenta :foreground ,bg))))
   `(company-tooltip-annotation  ((,c (:foreground ,violet))))
   `(company-scrollbar-bg        ((,c (:inherit tooltip))))
   `(company-scrollbar-fg        ((,c (:background ,blue))))
   `(company-preview             ((,c (:foreground ,blue))))
   `(company-preview-common      ((,c (:foreground ,magenta :background ,dark-grey))))
   `(company-preview-search      ((,c (:inherit company-tooltip-search))))

   ;; diff-hl
   `(diff-hl-change              ((,c (:foreground ,vc-modified))))
   `(diff-hl-delete              ((,c (:foreground ,vc-deleted))))
   `(diff-hl-insert              ((,c (:foreground ,vc-added))))

   ;; dired+
   `(diredp-file-name            ((,c (:foreground ,white))))
   `(diredp-dir-name             ((,c (:foreground ,white :bold ,bold))))
   `(diredp-ignored-file-name    ((,c (:foreground ,light-grey))))
   `(diredp-compressed-file-suffix ((,c (:foreground ,light-grey))))
   `(diredp-symlink              ((,c (:foreground ,violet))))
   `(diredp-dir-heading          ((,c (:foreground ,blue :bold ,bold))))
   `(diredp-file-suffix          ((,c (:foreground ,violet))))
   `(diredp-read-priv            ((,c (:foreground ,magenta))))
   `(diredp-write-priv           ((,c (:foreground ,green))))
   `(diredp-exec-priv            ((,c (:foreground ,yellow))))
   `(diredp-rare-priv            ((,c (:foreground ,red :bold ,bold))))
   `(diredp-dir-priv             ((,c (:foreground ,blue :bold ,bold))))
   `(diredp-no-priv              ((,c (:foreground ,light-grey))))
   `(diredp-number               ((,c (:foreground ,magenta))))
   `(diredp-date-time            ((,c (:foreground ,blue))))

   ;; dired-k
   `(dired-k-directory           ((,c (:foreground ,blue))))

   ;; ediff
   `(ediff-fine-diff-A    ((,c (:background ,dark-grey :bold ,bold))))
   `(ediff-fine-diff-B    ((,c (:background ,dark-grey :bold ,bold))))
   `(ediff-fine-diff-C    ((,c (:background ,dark-grey :bold ,bold))))
   `(ediff-current-diff-A ((,c (:background ,black))))
   `(ediff-current-diff-B ((,c (:background ,black))))
   `(ediff-current-diff-C ((,c (:background ,black))))
   `(ediff-even-diff-A    ((,c (:inherit hl-line))))
   `(ediff-even-diff-B    ((,c (:inherit hl-line))))
   `(ediff-even-diff-C    ((,c (:inherit hl-line))))
   `(ediff-odd-diff-A     ((,c (:inherit hl-line))))
   `(ediff-odd-diff-B     ((,c (:inherit hl-line))))
   `(ediff-odd-diff-C     ((,c (:inherit hl-line))))

   ;; elscreen
   `(elscreen-tab-background-face     ((,c (:background ,bg-alt))))
   `(elscreen-tab-control-face        ((,c (:background ,bg-alt :foreground ,bg-alt))))
   `(elscreen-tab-current-screen-face ((,c (:background ,bg :foreground ,fg))))
   `(elscreen-tab-other-screen-face   ((,c (:background ,tab-unfocused-bg :foreground ,tab-unfocused-fg))))

   ;; evil
   `(evil-ex-substitute-matches                   ((,c (:background ,black :foreground ,red :strike-through t :bold ,bold))))
   `(evil-ex-substitute-replacement               ((,c (:background ,black :foreground ,green :bold ,bold))))
   `(evil-search-highlight-persist-highlight-face ((,c (:inherit isearch-lazy-highlight-face))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((,c (:foreground ,search-bg :background ,dark-blue))))
   `(evil-snipe-matches-face     ((,c (:foreground ,search-bg :underline t :bold ,bold))))

   ;; flycheck
   `(flycheck-error     ((,c (:underline (:style wave :color ,red)))))
   `(flycheck-warning   ((,c (:underline (:style wave :color ,yellow)))))
   `(flycheck-info      ((,c (:underline (:style wave :color ,green)))))
   `(flyspell-incorrect ((,c (:underline (:style wave :color ,red) :inherit unspecified))))

   ;; git-gutter
   `(git-gutter:modified         ((,c (:foreground ,vc-modified))))
   `(git-gutter:added            ((,c (:foreground ,vc-added))))
   `(git-gutter:deleted          ((,c (:foreground ,vc-deleted))))

   ;; git-gutter+
   `(git-gutter+-modified        ((,c (:foreground ,vc-modified))))
   `(git-gutter+-added           ((,c (:foreground ,vc-added))))
   `(git-gutter+-deleted         ((,c (:foreground ,vc-deleted))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified      ((,c (:foreground ,vc-modified))))
   `(git-gutter-fr:added         ((,c (:foreground ,vc-added))))
   `(git-gutter-fr:deleted       ((,c (:foreground ,vc-deleted))))

   ;; helm
   `(helm-selection              ((,c (:background ,selection))))
   `(helm-match                  ((,c (:foreground ,blue :underline t))))
   `(helm-source-header          ((,c (:background ,current-line :foreground ,light-grey))))
   `(helm-swoop-target-line-face ((,c (:foreground ,highlight :inverse-video t))))
   `(helm-ff-file                ((,c (:foreground ,fg))))
   `(helm-ff-prefix              ((,c (:foreground ,magenta))))
   `(helm-ff-dotted-directory    ((,c (:foreground ,grey))))
   `(helm-ff-directory           ((,c (:foreground ,orange))))
   `(helm-ff-executable          ((,c (:foreground ,white :slant italic))))

   ;; indent-guide
   `(indent-guide-face                         ((,c (:foreground ,(doom-lighten bg 0.1)))))

   ;; highlight-indentation-mode
   `(highlight-indentation-face                ((,c (:background ,(doom-darken bg 0.1)))))
   `(highlight-indentation-current-column-face ((,c (:background ,(doom-darken bg 0.1)))))
   `(highlight-indentation-guides-odd-face     ((,c (:background ,bg))))
   `(highlight-indentation-guides-even-face    ((,c (:background ,(doom-darken bg 0.1)))))

   ;; highlight-quoted-mode
   `(highlight-quoted-symbol                   ((,c (:foreground ,type))))
   `(highlight-quoted-quote                    ((,c (:foreground ,operators))))

   ;; highlight-numbers-mode
   `(highlight-numbers-number                  ((,c (:foreground ,numbers))))

   ;; iedit
   `(iedit-occurrence            ((,c (:foreground ,magenta :bold ,bold :inverse-video t))))
   `(iedit-read-only-occurrence  ((,c (:inherit region))))

   ;; ivy
   `(ivy-current-match           ((,c (:background ,dark-blue))))
   `(ivy-minibuffer-match-face-1 ((,c (:background ,black :foreground ,(doom-lighten grey 0.1)))))
   `(ivy-minibuffer-match-face-2 ((,c (:background ,black :foreground ,magenta :bold ,bold))))
   `(ivy-minibuffer-match-face-3 ((,c (:background ,black :foreground ,green   :bold ,bold))))
   `(ivy-minibuffer-match-face-4 ((,c (:background ,black :foreground ,yellow  :bold ,bold))))
   `(ivy-virtual                 ((,c (:foreground ,fg))))

   ;; jabber
   `(jabber-activity-face          ((,c (:foreground ,red   :bold ,bold))))
   `(jabber-activity-personal-face ((,c (:foreground ,blue  :bold ,bold))))
   `(jabber-chat-error             ((,c (:foreground ,red   :bold ,bold))))
   `(jabber-chat-prompt-foreign    ((,c (:foreground ,red   :bold ,bold))))
   `(jabber-chat-prompt-local      ((,c (:foreground ,blue  :bold ,bold))))
   `(jabber-chat-prompt-system     ((,c (:foreground ,green :bold ,bold))))
   `(jabber-chat-text-foreign      ((,c (:foreground ,fg))))
   `(jabber-chat-text-local        ((,c (:foreground ,fg))))
   `(jabber-rare-time-face         ((,c (:foreground ,green))))
   `(jabber-roster-user-away       ((,c (:foreground ,yellow))))
   `(jabber-roster-user-chatty     ((,c (:foreground ,green :bold ,bold))))
   `(jabber-roster-user-dnd        ((,c (:foreground ,red))))
   `(jabber-roster-user-error      ((,c (:foreground ,red))))
   `(jabber-roster-user-offline    ((,c (:foreground ,fg))))
   `(jabber-roster-user-online     ((,c (:foreground ,green :bold ,bold))))
   `(jabber-roster-user-xa         ((,c (:foreground ,cyan))))

   ;; multiple cursors
   `(mc/cursor-face              ((,c (:inherit cursor))))

   ;; neotree
   `(neo-root-dir-face           ((,c (:foreground ,green :background ,bg :box (:line-width 4 :color ,bg)))))
   `(neo-file-link-face          ((,c (:foreground ,fg))))
   `(neo-dir-link-face           ((,c (:foreground ,blue))))
   `(neo-expand-btn-face         ((,c (:foreground ,blue))))

   ;; popup
   `(popup-face                  ((,c (:inherit tooltip))))
   `(popup-selection-face        ((,c (:background ,selection))))

   ;; pos-tip
   `(popup                       ((,c (:inherit tooltip))))
   `(popup-tip-face              ((,c (:inherit tooltip))))

   ;; powerline
   `(powerline-active1           ((,c (:inherit mode-line))))
   `(powerline-active2           ((,c (:inherit mode-line))))
   `(powerline-inactive1         ((,c (:inherit mode-line-inactive))))
   `(powerline-inactive2         ((,c (:inherit mode-line-inactive))))

   ;; spaceline
   `(spaceline-highlight-face    ((,c (:foreground ,blue))))

   ;; tabbar
   `(tabbar-default              ((,c (:foreground ,bg-alt :background ,bg-alt :height 0.9))))
   `(tabbar-modified             ((,c (:inherit tabbar-default :foreground ,red :bold ,bold))))
   `(tabbar-unselected           ((,c (:inherit tabbar-default :foreground ,grey))))
   `(tabbar-selected             ((,c (:inherit tabbar-default :foreground ,fg :background ,bg :bold ,bold))))
   `(tabbar-selected-modified    ((,c (:inherit tabbar-selected :foreground ,green))))
   `(tabbar-highlight            ((,c (:foreground ,fg :background ,bg-alt :inverse-video t))))
   `(tabbar-button
     ((((type graphic)) :background ,bg-alt    :foreground ,fg)
      (t                :background ,dark-grey :foreground ,fg)))
   `(tabbar-button-highlight     ((,c (:inherit tabbar-button :inverse-video t))))

   ;; smartparens
   `(sp-pair-overlay-face        ((,c (:background ,region))))

   ;; swiper
   `(swiper-line-face            ((,c (:background ,blue    :foreground ,black))))
   `(swiper-match-face-1         ((,c (:background ,black   :foreground ,light-grey))))
   `(swiper-match-face-2         ((,c (:background ,orange  :foreground ,black :bold ,bold))))
   `(swiper-match-face-3         ((,c (:background ,magenta :foreground ,black :bold ,bold))))
   `(swiper-match-face-4         ((,c (:background ,green   :foreground ,black :bold ,bold))))

   ;; stripe-buffer
   `(stripe-highlight            ((,c (:background ,bg-alt))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face   ((,c (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face   ((,c (:foreground ,magenta))))
   `(rainbow-delimiters-depth-3-face   ((,c (:foreground ,green))))
   `(rainbow-delimiters-depth-4-face   ((,c (:foreground ,orange))))
   `(rainbow-delimiters-depth-5-face   ((,c (:foreground ,violet))))
   `(rainbow-delimiters-unmatched-face ((,c (:foreground ,red :bold ,bold :inverse-video t))))

   ;; re-builder
   `(reb-match-0 ((,c (:foreground ,orange   :inverse-video t))))
   `(reb-match-1 ((,c (:foreground ,magenta  :inverse-video t))))
   `(reb-match-2 ((,c (:foreground ,green    :inverse-video t))))
   `(reb-match-3 ((,c (:foreground ,yellow   :inverse-video t))))

   ;; wgrep
   `(wgrep-face ((,c (:background ,dark-grey))))

   ;; which-func
   `(which-func ((,c (:foreground ,blue))))

   ;; which-key
   `(which-key-key-face                   ((,c (:foreground ,green))))
   `(which-key-group-description-face     ((,c (:foreground ,violet))))
   `(which-key-command-description-face   ((,c (:foreground ,blue))))
   `(which-key-local-map-description-face ((,c (:foreground ,magenta))))

   ;; whitespace
   `(whitespace-tab              ((,c (:foreground ,light-grey))))
   `(whitespace-newline          ((,c (:foreground ,light-grey))))
   `(whitespace-trailing         ((,c (:inherit trailing-whitespace))))
   `(whitespace-line             ((,c (:background ,black :foreground ,red :bold ,bold))))

   ;; workgroups2
   `(wg-current-workgroup-face   ((,c (:foreground ,black  :background ,blue))))
   `(wg-other-workgroup-face     ((,c (:foreground ,light-grey :background ,current-line))))
   `(wg-divider-face             ((,c (:foreground ,grey))))
   `(wg-brace-face               ((,c (:foreground ,blue))))

   ;; volatile-highlights
   `(vhl/default-face            ((,c (:background ,grey))))


   ;; --- major-mode faces -------------------
   ;; (css|scss)-mode
   `(css-proprietary-property ((,c (:foreground ,orange))))
   `(css-property             ((,c (:foreground ,green))))
   `(css-selector             ((,c (:foreground ,blue))))

   ;; js2-mode
   `(js2-function-param  ((,c (:foreground ,variables))))
   `(js2-function-call   ((,c (:foreground ,functions))))
   `(js2-object-property ((,c (:foreground ,violet))))
   `(js2-jsdoc-tag       ((,c (:foreground ,comments))))

   ;; makefile-*-mode
   `(makefile-targets     ((,c (:foreground ,blue))))

   ;; markdown-mode
   `(markdown-header-face           ((,c (:foreground ,red))))
   `(markdown-header-delimiter-face ((,c (:inherit markdown-header-face))))
   `(markdown-metadata-key-face     ((,c (:foreground ,red))))
   ;;`(markdown-blockquote-face ((,c (:foreground ,violet))))
   `(markdown-markup-face     ((,c (:foreground ,light-grey))))
   ;;`(markdown-markup-face     ((,c (:foreground ,operators))))
   `(markdown-pre-face        ((,c (:foreground ,green))))
   `(markdown-inline-face     ((,c (:foreground ,cyan))))
   `(markdown-list-face       ((,c (:foreground ,red))))
   `(markdown-link-face       ((,c (:foreground ,blue :bold nil))))
   `(markdown-url-face        ((,c (:foreground ,magenta :bold nil))))
   `(markdown-header-face-1   ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-2   ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-3   ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-4   ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-5   ((,c (:inherit markdown-header-face))))
   `(markdown-header-face-6   ((,c (:inherit markdown-header-face))))
   ;;`(markdown-header-rule-face       ((,c (:inherit shadow))))
   `(markdown-italic-face            ((,c (:inherit italic :foreground ,violet))))
   `(markdown-bold-face              ((,c (:inherit bold :foreground ,orange))))
   ;;`(markdown-link-face              ((,c (:inherit shadow))))
   ;;`(markdown-link-title-face        ((,c (:inherit link))))
   ;;`(markdown-url-face               ((,c (:inherit link))))

   ;; org-mode
   `(org-tag                   ((,c (:foreground ,green :bold nil))))
   `(org-priority              ((,c (:foreground ,red))))
   `(org-ellipsis              ((,c (:background ,dark-grey :foreground ,light-grey :underline nil))))
   `(org-hide                  ((,c (:foreground ,bg))))
   `(org-table                 ((,c (:foreground ,violet))))
   `(org-quote                 ((,c (:inherit italic :background ,dark-grey))))
   `(org-document-info         ((,c (:foreground ,magenta))))
   `(org-document-title        ((,c (:foreground ,magenta :bold ,bold))))
   `(org-default               ((,c (:inherit variable-pitch))))
   ;; `(org-document-info-keyword ((,c (:foreground ,grey))))
   `(org-meta-line             ((,c (:foreground ,doc-comments))))
   `(org-block-begin-line      ((,c (:background ,current-line :foreground ,comments))))
   `(org-block-end-line        ((,c (:inherit org-block-begin-line))))
   `(org-block-background      ((,c (:background ,dark-grey))))
   `(org-block                 ((,c (:background ,dark-grey))))
   `(org-archived              ((,c (:foreground ,light-grey))))
   `(org-level-1               ((,c (:background ,dark-grey :foreground ,blue   :bold ,bold :height 1.2))))
   `(org-level-2               ((,c (:background ,dark-grey :foreground ,violet :bold ,bold))))
   `(org-level-3               ((,c (                       :foreground ,white  :bold ,bold))))
   `(org-level-4               ((,c (                       :foreground ,white  :bold ,bold))))
   `(org-level-5               ((,c (                       :foreground ,white  :bold ,bold))))
   `(org-level-6               ((,c (                       :foreground ,white  :bold ,bold))))
   `(org-code                  ((,c (:foreground ,orange))))
   `(org-verbatim              ((,c (:foreground ,green))))
   `(org-formula               ((,c (:foreground ,cyan))))
   `(org-list-dt               ((,c (:foreground ,blue))))
   `(org-footnote              ((,c (:foreground ,orange))))
   `(org-link                  ((,c (:foreground ,green :underline t :bold inherit))))
   `(org-date                  ((,c (:foreground ,violet))))
   `(org-todo                  ((,c (:foreground ,blue :bold inherit))))
   `(org-done                  ((,c (:foreground ,light-grey :bold inherit))))
   `(org-headline-done         ((,c (:foreground ,light-grey :bold nil))))
   `(org-special-keyword       ((,c (:foreground ,magenta))))
   `(org-checkbox              ((,c (:inherit org-todo))))
   `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
   `(org-checkbox-statistics-done ((,c (:inherit org-done))))
   `(message-header-name       ((,c (:foreground ,green)))) ; custom

   ;; rpm-spec-mode
   `(rpm-spec-macro-face        ((,c (:foreground ,yellow))))
   `(rpm-spec-var-face          ((,c (:foreground ,violet))))
   `(rpm-spec-tag-face          ((,c (:foreground ,blue))))
   `(rpm-spec-obsolete-tag-face ((,c (:foreground ,red))))
   `(rpm-spec-package-face      ((,c (:foreground ,orange))))
   `(rpm-spec-dir-face          ((,c (:foreground ,green))))
   `(rpm-spec-doc-face          ((,c (:foreground ,orange))))
   `(rpm-spec-ghost-face        ((,c (:foreground ,(doom-lighten grey 0.2)))))
   `(rpm-spec-section-face      ((,c (:foreground ,magenta))))

   ;; typescript-mode
   `(ts-object-property         ((,c (:inherit js2-object-property))))

   ;; web-mode
   `(web-mode-doctype-face           ((,c (:foreground ,comments))))
   `(web-mode-html-tag-face          ((,c (:foreground ,methods))))
   `(web-mode-html-tag-bracket-face  ((,c (:foreground ,methods))))
   `(web-mode-html-attr-name-face    ((,c (:foreground ,type))))
   `(web-mode-html-entity-face       ((,c (:foreground ,cyan :italic ,italic))))
   `(web-mode-block-control-face     ((,c (:foreground ,orange))))
   ;;`(web-mode-html-tag-bracket-face  ((,c (:foreground ,operators))))))
   )

  ;; --- variables --------------------------
  (`(vc-annotate-color-map
     '((20 .  ,green)
       (40 .  ,(doom-blend yellow green (/ 1.0 3)))
       (60 .  ,(doom-blend yellow green (/ 2.0 3)))
       (80 .  ,yellow)
       (100 . ,(doom-blend orange yellow (/ 1.0 3)))
       (120 . ,(doom-blend orange yellow (/ 2.0 3)))
       (140 . ,orange)
       (160 . ,(doom-blend magenta orange (/ 1.0 3)))
       (180 . ,(doom-blend magenta orange (/ 2.0 3)))
       (200 . ,magenta)
       (220 . ,(doom-blend red magenta (/ 1.0 3)))
       (240 . ,(doom-blend red magenta (/ 2.0 3)))
       (260 . ,red)
       (280 . ,(doom-blend grey red (/ 1.0 4)))
       (300 . ,(doom-blend grey red (/ 2.0 4)))
       (320 . ,(doom-blend grey red (/ 3.0 4)))
       (340 . ,light-grey)
       (360 . ,light-grey)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background ,black)

   `(ansi-color-names-vector
     [,black ,red ,green ,yellow ,blue ,magenta ,cyan ,white])
   `(ansi-term-color-vector
     [unspecified ,black ,red ,green ,yellow ,blue ,magenta ,cyan ,white])))

;;; doom-one-theme.el ends here
