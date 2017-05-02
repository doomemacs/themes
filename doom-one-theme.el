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

  ;; name      gui       term (256)
  ((bg         "#282c34" nil      )
   (bg-alt     "#21242b" nil      )
   (fg         "#bbc2cf" "#bfbfbf")
   (fg-alt     "#BBBBBB" "#bbbbbb")
   (black      "#1B2229" "black"  )
   (light-grey "#5B6268" "#525252")
   (grey       "#3B3F46" "#3d3d3d")
   (dark-grey  "#23272e" "#262626")
   (white      "#DFDFDF" "#dfdfdf")
   (red        "#ff6c6b" "#ff6655")
   (orange     "#da8548" "#dd8844")
   (green      "#98be65" "#99bb66")
   (teal       "#4db5bd" "#44b9b1")
   (yellow     "#ECBE7B"          )
   (blue       "#51afef"          )
   (dark-blue  "#1f5572"          )
   (magenta    "#c678dd"          )
   (violet     "#a9a1e1"          )
   (cyan       "#46D9FF"          )
   (dark-cyan  "#5699AF"          )

   ;; face categories
   (highlight      blue)
   (vertical-bar   black)
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
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red))


  ;;
  ;; doom-one faces
  ;;

  (;; --- custom faces -----------------------
   (doom-default                    :inherit 'default :background bg)
   (doom-hl-line                    :background (if gui current-line "black"))
   (doom-linum                      :inherit 'linum :background bg)
   (doom-minibuffer-active          :background bg)
   (doom-nlinum-highlight           :foreground (doom-darken white 0.25) :bold nil)
   (doom-modeline-buffer-path       :foreground (if bold white cyan) :bold bold)
   (doom-modeline-buffer-project    :foreground fg)
   (doom-modeline-buffer-modified   :foreground red)
   (doom-modeline-buffer-major-mode :foreground (if bold white blue) :bold bold)
   (doom-modeline-highlight         :foreground blue)
   (doom-modeline-panel             :foreground black :background blue)
   (doom-modeline-bar               :background blue)
   (doom-modeline-eldoc-bar         :background yellow)
   (doom-neotree-dir-face           :foreground blue)
   (doom-neotree-file-face          :foreground fg)
   (doom-neotree-hidden-file-face   :foreground light-grey)
   (doom-neotree-text-file-face     :foreground white)
   (doom-neotree-media-file-face    :foreground light-grey)
   (doom-neotree-data-file-face     :foreground violet)

   ;; --- base faces -------------------------
   (default              :background bg-alt     :foreground fg)
   (fringe               :inherit 'default      :foreground comments)
   (region               :background region)
   (highlight            :background blue       :foreground black)
   (hl-line              :background black)
   (cursor               :background blue)
   (shadow               :foreground light-grey)
   (minibuffer-prompt    :foreground blue)
   (tooltip              :inherit 'doom-default :background bg-alt)
   (secondary-selection  :background blue       :foreground black)
   (lazy-highlight       :background dark-blue  :foreground white)
   (match                :foreground green      :background black :bold bold)
   (trailing-whitespace  :background red)
   (vertical-border      :foreground black :background black)

   (bold        :weight (if bold 'bold 'normal) :color white)
   (italic      :slant  (if italic 'italic 'normal))
   (bold-italic :inherit '(bold italic))

   (error       :foreground red)
   (warning     :foreground yellow)
   (success     :foreground green)

   (font-lock-builtin-face                :foreground builtin)
   (font-lock-comment-face                :foreground comments)
   (font-lock-comment-delimiter-face      :foreground comments)
   (font-lock-doc-face                    :foreground doc-comments)
   (font-lock-doc-string-face             :foreground doc-comments)
   (font-lock-constant-face               :foreground constants)
   (font-lock-function-name-face          :foreground functions)
   (font-lock-keyword-face                :foreground keywords)
   (font-lock-string-face                 :foreground strings)
   (font-lock-type-face                   :foreground type)
   (font-lock-variable-name-face          :foreground variables)
   (font-lock-warning-face                :inherit 'warning)
   (font-lock-negation-char-face          :inherit 'bold :foreground operators)
   (font-lock-preprocessor-face           :inherit 'bold :foreground operators)
   (font-lock-preprocessor-char-face      :inherit 'bold :foreground operators)
   (font-lock-regexp-grouping-backslash   :inherit 'bold :foreground operators)
   (font-lock-regexp-grouping-construct   :inherit 'bold :foreground operators)

   (mode-line :background (if gui (if doom-one-brighter-modeline bg bg-alt) "brightblack"))
   (mode-line-inactive :foreground (if gui light-grey grey)
                       :background (if gui (if doom-one-brighter-modeline bg dark-grey) "black"))
   (header-line :inherit 'mode-line)

   (linum :foreground (if gui (doom-lighten grey 0.05) light-grey)
          :background bg-alt
          :bold nil)

   ;; dired
   (dired-directory :foreground orange)
   (dired-ignored   :foreground light-grey)

   ;; Search
   (isearch                     :background search-bg :foreground black :bold bold)
   (isearch-lazy-highlight-face :background dark-grey)
   (yas-field-highlight-face    :inherit 'match)

   ;; Terminal colors
   (term-color-black   :background black   :foreground black)
   (term-color-red     :background red     :foreground red)
   (term-color-green   :background green   :foreground green)
   (term-color-yellow  :background yellow  :foreground yellow)
   (term-color-blue    :background blue    :foreground blue)
   (term-color-magenta :background magenta :foreground magenta)
   (term-color-cyan    :background cyan    :foreground cyan)
   (term-color-white   :background white   :foreground white)

   ;; `window-divider'
   (window-divider             :foreground vertical-bar)
   (window-divider-first-pixel :foreground vertical-bar)
   (window-divider-last-pixel  :foreground vertical-bar)


   ;; --- plugin faces -----------------------
   ;; avy
   (avy-lead-face-0 :background search-bg :foreground search-fg)
   (avy-lead-face-1 :background search-bg :foreground search-fg)
   (avy-lead-face-2 :background search-bg :foreground search-fg)
   (avy-lead-face   :background search-bg :foreground search-fg)

   ;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-common                           :foreground blue)
   (company-tooltip-search     :background highlight :foreground search-fg)
   (company-tooltip-selection  :background selection)
   (company-tooltip-mouse      :background magenta   :foreground bg)
   (company-tooltip-annotation                       :foreground violet)
   (company-scrollbar-bg       :inherit 'tooltip)
   (company-scrollbar-fg       :background blue)
   (company-preview                                  :foreground blue)
   (company-preview-common     :background dark-grey :foreground magenta)
   (company-preview-search     :inherit 'company-tooltip-search)

   ;; diff-hl
   (diff-hl-change :foreground vc-modified)
   (diff-hl-delete :foreground vc-deleted)
   (diff-hl-insert :foreground vc-added)

   ;; dired+
   (diredp-file-name              :foreground white)
   (diredp-dir-name               :foreground white :inherit 'bold)
   (diredp-ignored-file-name      :foreground light-grey)
   (diredp-compressed-file-suffix :foreground light-grey)
   (diredp-symlink                :foreground violet)
   (diredp-dir-heading            :foreground blue  :inherit 'bold)
   (diredp-file-suffix            :foreground violet)
   (diredp-read-priv              :foreground magenta)
   (diredp-write-priv             :foreground green)
   (diredp-exec-priv              :foreground yellow)
   (diredp-rare-priv              :foreground red   :inherit 'bold)
   (diredp-dir-priv               :foreground blue  :inherit 'bold)
   (diredp-no-priv                :foreground light-grey)
   (diredp-number                 :foreground magenta)
   (diredp-date-time              :foreground blue)

   ;; dired-k
   (dired-k-directory :foreground blue)

   ;; ediff
   (ediff-fine-diff-A    :background dark-grey :inherit 'bold)
   (ediff-fine-diff-B    :background dark-grey :inherit 'bold)
   (ediff-fine-diff-C    :background dark-grey :inherit 'bold)
   (ediff-current-diff-A :background black)
   (ediff-current-diff-B :background black)
   (ediff-current-diff-C :background black)
   (ediff-even-diff-A    :inherit 'hl-line)
   (ediff-even-diff-B    :inherit 'hl-line)
   (ediff-even-diff-C    :inherit 'hl-line)
   (ediff-odd-diff-A     :inherit 'hl-line)
   (ediff-odd-diff-B     :inherit 'hl-line)
   (ediff-odd-diff-C     :inherit 'hl-line)

   ;; elscreen
   (elscreen-tab-background-face     :background bg-alt)
   (elscreen-tab-control-face        :background bg-alt           :foreground bg-alt)
   (elscreen-tab-current-screen-face :background bg               :foreground fg)
   (elscreen-tab-other-screen-face   :background tab-unfocused-bg :foreground tab-unfocused-fg) ; FIXME

   ;; evil
   (evil-ex-substitute-matches     :background black :foreground red   :strike-through t :bold bold)
   (evil-ex-substitute-replacement :background black :foreground green :bold bold)
   (evil-search-highlight-persist-highlight-face :inherit 'isearch-lazy-highlight-face)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground search-bg :background dark-blue)
   (evil-snipe-matches-face     :foreground search-bg :underline t :bold bold)

   ;; flycheck
   (flycheck-error     :underline `(:style wave :color ,red))
   (flycheck-warning   :underline `(:style wave :color ,yellow))
   (flycheck-info      :underline `(:style wave :color ,green))
   (flyspell-incorrect :underline `(:style wave :color ,red) :inherit 'unspecified)

   ;; git-gutter
   (git-gutter:modified :foreground vc-modified)
   (git-gutter:added    :foreground vc-added)
   (git-gutter:deleted  :foreground vc-deleted)

   ;; git-gutter+
   (git-gutter+-modified :foreground vc-modified)
   (git-gutter+-added    :foreground vc-added)
   (git-gutter+-deleted  :foreground vc-deleted)

   ;; git-gutter-fringe
   (git-gutter-fr:modified :foreground vc-modified)
   (git-gutter-fr:added    :foreground vc-added)
   (git-gutter-fr:deleted  :foreground vc-deleted)

   ;; gnus
   (gnus-group-mail-1           :inherit 'gnus-group-mail-1-empty   :bold bold)
   (gnus-group-mail-2           :inherit 'gnus-group-mail-2-empty   :bold bold)
   (gnus-group-mail-3           :inherit 'gnus-group-mail-3-empty   :bold bold)
   (gnus-group-mail-low         :inherit 'gnus-group-mail-low-empty :bold bold)
   (gnus-group-mail-1-empty     :inherit 'gnus-group-news-1-empty)
   (gnus-group-mail-2-empty     :inherit 'gnus-group-news-2-empty)
   (gnus-group-mail-3-empty     :inherit 'gnus-group-news-3-empty)
   (gnus-group-mail-low-empty   :inherit 'gnus-group-news-low-empty)
   (gnus-group-news-1           :inherit 'gnus-group-news-1-empty   :bold bold)
   (gnus-group-news-2           :inherit 'gnus-group-news-2-empty   :bold bold)
   (gnus-group-news-3           :inherit 'gnus-group-news-3-empty   :bold bold)
   (gnus-group-news-4           :inherit 'gnus-group-news-4-empty   :bold bold)
   (gnus-group-news-5           :inherit 'gnus-group-news-5-empty   :bold bold)
   (gnus-group-news-6           :inherit 'gnus-group-news-6-empty   :bold bold)
   (gnus-group-news-low         :inherit 'gnus-group-news-low-empty :bold bold)
   (gnus-header-content         :inherit 'message-header-other)
   (gnus-header-from            :inherit 'message-header-other)
   (gnus-header-name            :inherit 'message-header-name)
   (gnus-header-newsgroups      :inherit 'message-header-other)
   (gnus-header-subject         :inherit 'message-header-subject)
   (gnus-summary-cancelled      :foreground orange)
   (gnus-summary-high-ancient   :foreground blue :bold bold)
   (gnus-summary-high-read      :foreground green :bold bold)
   (gnus-summary-high-ticked    :foreground orange :bold bold)
   (gnus-summary-high-unread    :foreground light-grey :bold bold)
   (gnus-summary-low-ancient    :foreground blue)
   (gnus-summary-low-read       :foreground green)
   (gnus-summary-low-ticked     :foreground orange)
   (gnus-summary-low-unread     :foreground light-grey)
   (gnus-summary-normal-ancient :foreground blue)
   (gnus-summary-normal-read    :foreground green)
   (gnus-summary-normal-ticked  :foreground orange)
   (gnus-summary-normal-unread  :foreground light-grey)
   (gnus-summary-selected       :foreground yellow :bold bold)
   (gnus-cite-1                 :foreground blue)
   (gnus-cite-2                 :foreground blue)
   (gnus-cite-3                 :foreground blue)
   (gnus-cite-4                 :foreground green)
   (gnus-cite-5                 :foreground green)
   (gnus-cite-6                 :foreground green)
   (gnus-cite-7                 :foreground red)
   (gnus-cite-8                 :foreground red)
   (gnus-cite-9                 :foreground red)
   (gnus-cite-10                :foreground yellow)
   (gnus-cite-11                :foreground yellow)
   (gnus-group-news-1-empty     :foreground yellow)
   (gnus-group-news-2-empty     :foreground green)
   (gnus-group-news-3-empty     :foreground green)
   (gnus-group-news-4-empty     :foreground blue)
   (gnus-group-news-5-empty     :foreground blue)
   (gnus-group-news-6-empty     :foreground blue)
   (gnus-group-news-low-empty   :foreground light-grey)
   (gnus-signature              :foreground yellow)
   (gnus-x-face                 :background light-grey :foreground fg)

   ;; helm
   (helm-selection              :background selection)
   (helm-match                  :foreground blue :underline t)
   (helm-source-header          :background current-line :foreground light-grey)
   (helm-swoop-target-line-face :foreground highlight :inverse-video t)
   (helm-ff-file                :foreground fg)
   (helm-ff-prefix              :foreground magenta)
   (helm-ff-dotted-directory    :foreground grey)
   (helm-ff-directory           :foreground orange)
   (helm-ff-executable          :foreground white :inherit 'italic)

   ;; indent-guide
   (indent-guide-face :foreground (doom-lighten bg 0.1))

   ;; highlight-indentation-mode
   (highlight-indentation-face                :background (doom-darken bg 0.1))
   (highlight-indentation-current-column-face :background (doom-darken bg 0.1))
   (highlight-indentation-guides-odd-face     :background bg)
   (highlight-indentation-guides-even-face    :background (doom-darken bg 0.1))

   ;; highlight-quoted-mode
   (highlight-quoted-symbol :foreground type)
   (highlight-quoted-quote  :foreground operators)

   ;; highlight-numbers-mode
   (highlight-numbers-number :foreground numbers)

   ;; iedit
   (iedit-occurrence :foreground magenta :bold bold :inverse-video t)
   (iedit-read-only-occurrence :inherit 'region)

   ;; ivy
   (ivy-current-match :background (if gui dark-blue "brightblack"))
   (ivy-minibuffer-match-face-1
    :background black
    :foreground (if gui (doom-lighten grey 0.1) "brightblack")
    :bold bold)
   (ivy-minibuffer-match-face-2 :inherit 'ivy-minibuffer-match-face-1 :foreground magenta)
   (ivy-minibuffer-match-face-3 :inherit 'ivy-minibuffer-match-face-1 :foreground green)
   (ivy-minibuffer-match-face-4 :inherit 'ivy-minibuffer-match-face-1 :foreground yellow)
   (ivy-virtual :foreground fg)

   ;; jabber
   (jabber-activity-face          :foreground red   :bold bold)
   (jabber-activity-personal-face :foreground blue  :bold bold)
   (jabber-chat-error             :foreground red   :bold bold)
   (jabber-chat-prompt-foreign    :foreground red   :bold bold)
   (jabber-chat-prompt-local      :foreground blue  :bold bold)
   (jabber-chat-prompt-system     :foreground green :bold bold)
   (jabber-chat-text-foreign      :foreground fg)
   (jabber-chat-text-local        :foreground fg)
   (jabber-rare-time-face         :foreground green)
   (jabber-roster-user-away       :foreground yellow)
   (jabber-roster-user-chatty     :foreground green :bold bold)
   (jabber-roster-user-dnd        :foreground red)
   (jabber-roster-user-error      :foreground red)
   (jabber-roster-user-offline    :foreground fg)
   (jabber-roster-user-online     :foreground green :bold bold)
   (jabber-roster-user-xa         :foreground cyan)

   ;; multiple cursors
   (mc/cursor-face :inherit 'cursor)

   ;; neotree
   (neo-root-dir-face   :foreground green :background bg :box `(:line-width 4 :color ,bg))
   (neo-file-link-face  :foreground fg)
   (neo-dir-link-face   :foreground blue)
   (neo-expand-btn-face :foreground blue)

   ;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :background selection)

   ;; pos-tip
   (popup          :inherit 'tooltip)
   (popup-tip-face :inherit 'tooltip)

   ;; powerline
   (powerline-active1   :inherit 'mode-line)
   (powerline-active2   :inherit 'mode-line)
   (powerline-inactive1 :inherit 'mode-line-inactive)
   (powerline-inactive2 :inherit 'mode-line-inactive)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face    :foreground blue)
   (rainbow-delimiters-depth-2-face    :foreground magenta)
   (rainbow-delimiters-depth-3-face    :foreground green)
   (rainbow-delimiters-depth-4-face    :foreground orange)
   (rainbow-delimiters-depth-5-face    :foreground violet)
   (rainbow-delimiters-depth-6-face    :foreground yellow)
   (rainbow-delimiters-depth-7-face    :foreground teal)
   (rainbow-delimiters-unmatched-face  :foreground red :bold bold :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)

   ;; re-builder
   (reb-match-0 :foreground orange  :inverse-video t)
   (reb-match-1 :foreground magenta :inverse-video t)
   (reb-match-2 :foreground green   :inverse-video t)
   (reb-match-3 :foreground yellow  :inverse-video t)

   ;; smartparens
   (sp-pair-overlay-face :background region)

   ;; show-paren
   (show-paren-match    :foreground red   :background black :bold bold)
   (show-paren-mismatch :foreground black :background red   :bold bold)

   ;; spaceline
   (spaceline-highlight-face :foreground blue)

   ;; stripe-buffer
   (stripe-highlight :background bg-alt)

   ;; swiper
   (swiper-line-face    :background blue    :foreground black)
   (swiper-match-face-1 :background black   :foreground light-grey)
   (swiper-match-face-2 :background orange  :foreground black :bold bold)
   (swiper-match-face-3 :background magenta :foreground black :bold bold)
   (swiper-match-face-4 :background green   :foreground black :bold bold)

   ;; ;; tabbar
   (tabbar-default           :foreground bg-alt :background bg-alt :height 0.9)
   (tabbar-modified          :inherit 'tabbar-default  :foreground red :bold bold)
   (tabbar-unselected        :inherit 'tabbar-default  :foreground grey)
   (tabbar-selected          :inherit 'tabbar-default  :foreground fg :background bg :bold bold)
   (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)
   (tabbar-highlight         :foreground fg :background bg-alt :inverse-video t)
   (tabbar-button            :background (if gui bg-alt dark-grey) :foreground fg)
   (tabbar-button-highlight  :inherit 'tabbar-button :inverse-video t)

   ;; volatile-highlights
   (vhl/default-face :background grey)

   ;; wgrep
   (wgrep-face :background dark-grey)

   ;; which-func
   (which-func :foreground blue)

   ;; which-key
   (which-key-key-face                   :foreground green)
   (which-key-group-description-face     :foreground violet)
   (which-key-command-description-face   :foreground blue)
   (which-key-local-map-description-face :foreground magenta)

   ;; whitespace
   (whitespace-tab      :foreground light-grey)
   (whitespace-newline  :foreground light-grey)
   (whitespace-trailing :inherit 'trailing-whitespace)
   (whitespace-line     :background black :foreground red :bold bold)

   ;; workgroups2
   (wg-current-workgroup-face :foreground black      :background blue)
   (wg-other-workgroup-face   :foreground light-grey :background current-line)
   (wg-divider-face           :foreground grey)
   (wg-brace-face             :foreground blue)


   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; js2-mode
   (js2-function-param  :foreground variables)
   (js2-function-call   :foreground functions)
   (js2-object-property :foreground violet)
   (js2-jsdoc-tag       :foreground comments)

   ;; makefile-*-mode
   (makefile-targets :foreground blue)

   ;; markdown-mode
   (markdown-header-face           :foreground red)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground red)
   (markdown-markup-face           :foreground light-grey)
   (markdown-pre-face              :foreground green)
   (markdown-inline-face           :foreground cyan)
   (markdown-list-face             :foreground red)
   (markdown-link-face             :foreground blue    :bold nil)
   (markdown-url-face              :foreground magenta :bold nil)
   (markdown-header-face-1         :inherit 'markdown-header-face)
   (markdown-header-face-2         :inherit 'markdown-header-face)
   (markdown-header-face-3         :inherit 'markdown-header-face)
   (markdown-header-face-4         :inherit 'markdown-header-face)
   (markdown-header-face-5         :inherit 'markdown-header-face)
   (markdown-header-face-6         :inherit 'markdown-header-face)
   (markdown-italic-face           :inherit 'italic :foreground violet)
   (markdown-bold-face             :inherit 'bold   :foreground orange)
   ;; (markdown-header-rule-face   :inherit 'shadow)
   ;; (markdown-markup-face        :foreground operators)
   ;; (markdown-link-face          :inherit 'shadow)
   ;; (markdown-link-title-face    :inherit 'link)
   ;; (markdown-url-face           :inherit 'link)
   ;; (markdown-blockquote-face :foreground violet)

   ;; org-mode
   (org-tag                   :foreground green :bold nil)
   (org-priority              :foreground red)
   (org-ellipsis              :foreground light-grey :background dark-grey :underline nil)
   (org-hide                  :foreground bg)
   (org-table                 :foreground violet)
   (org-quote                 :inherit 'italic :background dark-grey)
   (org-document-info         :foreground magenta)
   (org-document-title        :foreground magenta :bold bold)
   (org-default               :inherit 'variable-pitch)
   (org-meta-line             :foreground doc-comments)
   (org-block-begin-line      :foreground comments :background current-line)
   (org-block-end-line        :inherit 'org-block-begin-line)
   (org-block-background      :background dark-grey)
   (org-block                 :background dark-grey)
   (org-archived              :foreground light-grey)
   (org-level-1               :foreground blue   :bold bold :background dark-grey :height 1.2)
   (org-level-2               :foreground violet :bold bold :background dark-grey)
   (org-level-3               :foreground white  :bold bold)
   (org-level-4               :foreground white  :bold bold)
   (org-level-5               :foreground white  :bold bold)
   (org-level-6               :foreground white  :bold bold)
   (org-code                  :foreground orange)
   (org-verbatim              :foreground green)
   (org-formula               :foreground cyan)
   (org-list-dt               :foreground blue)
   (org-footnote              :foreground orange)
   (org-link                  :foreground green :underline t :bold 'inherit)
   (org-date                  :foreground violet)
   (org-todo                  :foreground blue       :bold 'inherit)
   (org-done                  :foreground light-grey :bold 'inherit)
   (org-headline-done         :foreground light-grey :bold nil)
   (org-special-keyword       :foreground magenta)
   (org-checkbox              :inherit 'org-todo)
   (org-checkbox-statistics-todo :inherit 'org-todo)
   (org-checkbox-statistics-done :inherit 'org-done)
   (message-header-name :foreground green)
   ;; (org-document-info-keyword :foreground grey)

   ;; typescript-mode
   (ts-object-property :inherit 'js2-object-property)

   ;; rpm-spec-mode
   (rpm-spec-macro-face        :foreground yellow)
   (rpm-spec-var-face          :foreground violet)
   (rpm-spec-tag-face          :foreground blue)
   (rpm-spec-obsolete-tag-face :foreground red)
   (rpm-spec-package-face      :foreground orange)
   (rpm-spec-dir-face          :foreground green)
   (rpm-spec-doc-face          :foreground orange)
   (rpm-spec-ghost-face        :foreground (doom-lighten grey 0.2))
   (rpm-spec-section-face      :foreground magenta)

   ;; web-mode
   (web-mode-doctype-face           :foreground comments)
   (web-mode-html-tag-face          :foreground methods)
   (web-mode-html-tag-bracket-face  :foreground methods)
   (web-mode-html-attr-name-face    :foreground type)
   (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
   (web-mode-block-control-face     :foreground orange)
   ;; (web-mode-html-tag-bracket-face :foreground operators)))
   )


  ;;
  ;; Settings
  ;;

  ((vc-annotate-color-map
    `((20  . ,green)
      (40  . ,(doom-blend yellow green (/ 1.0 3)))
      (60  . ,(doom-blend yellow green (/ 2.0 3)))
      (80  . ,yellow)
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
   (vc-annotate-very-old-color nil)
   (vc-annotate-background black)

   (ansi-color-names-vector
    (vector black red green yellow blue magenta cyan white))
   (ansi-term-color-vector
    (vector 'unspecified black red green yellow blue magenta cyan white))))

;;; doom-one-theme.el ends here
