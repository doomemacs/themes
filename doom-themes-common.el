;;; doom-themes-common.el -*- lexical-binding: t; -*-

(defconst doom-themes-common-faces
  '(;; --- custom faces -----------------------
    (doom-modeline-error
     :background (doom-darken red 0.25)
     :foreground base0
     :distant-foreground base0)

    ;; --- base faces -------------------------
    (bold        :weight (if bold 'bold 'normal) :foreground (unless bold base8))
    (italic      :slant  (if italic 'italic 'normal))
    (bold-italic :inherit '(bold italic))

    (default :background bg :foreground fg)
    (fringe :inherit 'default :foreground base4)
    (region               :background region     :foreground nil   :distant-foreground (doom-darken fg 0.2))
    (highlight            :background highlight  :foreground base0 :distant-foreground base8)
    (cursor               :background highlight)
    (shadow               :foreground base5)
    (minibuffer-prompt    :foreground highlight)
    (tooltip              :background bg-alt :foreground fg)
    (secondary-selection  :background grey)
    (lazy-highlight       :background dark-blue  :foreground base8 :distant-foreground base0 :bold bold)
    (match                :foreground green      :background base0 :bold bold)
    (trailing-whitespace  :background red)
    (vertical-border      :background vertical-bar :foreground vertical-bar)
    (link                 :foreground highlight :underline t :bold 'inherit)

    (error   :foreground error)
    (warning :foreground warning)
    (success :foreground success)

    (font-lock-builtin-face              :foreground builtin)
    (font-lock-comment-face              :foreground comments)
    (font-lock-comment-delimiter-face    :inherit 'font-lock-comment-face)
    (font-lock-doc-face                  :inherit 'font-lock-comment-face :foreground doc-comments)
    (font-lock-constant-face             :foreground constants)
    (font-lock-function-name-face        :foreground functions)
    (font-lock-keyword-face              :foreground keywords)
    (font-lock-string-face               :foreground strings)
    (font-lock-type-face                 :foreground type)
    (font-lock-variable-name-face        :foreground variables)
    (font-lock-warning-face              :inherit 'warning)
    (font-lock-negation-char-face        :inherit 'bold :foreground operators)
    (font-lock-preprocessor-face         :inherit 'bold :foreground operators)
    (font-lock-preprocessor-char-face    :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-backslash :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-construct :inherit 'bold :foreground operators)

    ;; mode-line / header-line
    (mode-line           :background bg     :foreground fg     :distant-foreground bg)
    (mode-line-inactive  :background bg-alt :foreground fg-alt :distant-foreground bg-alt)
    (mode-line-emphasis  :foreground highlight :distant-foreground bg)
    (mode-line-highlight :inherit 'highlight :distant-foreground bg)
    (mode-line-buffer-id :foreground fg :bold bold :distant-foreground bg)
    (header-line :inherit 'mode-line :distant-foreground bg)

    ;; 1. Line number faces must explicitly disable its text style attributes
    ;;    because nearby faces may "bleed" into the line numbers otherwise.
    ;; 2. All other line number plugin faces should &inherit from these.
    (line-number
     :inherit 'default
     :foreground base5 :distant-foreground nil
     :bold nil :italic nil :underline nil :strike-through nil)
    (line-number-current-line
     :inherit 'hl-line
     :foreground fg :distant-foreground nil
     :bold nil :italic nil :underline nil :strike-through nil)


    ;; --- built-in plugin faces --------------
    ;; dired
    (dired-directory :foreground builtin)
    (dired-ignored   :foreground comments)

    ;; ediff
    (ediff-fine-diff-A    :background base3 :inherit 'bold)
    (ediff-fine-diff-B    :background base3 :inherit 'bold)
    (ediff-fine-diff-C    :background base3 :inherit 'bold)
    (ediff-current-diff-A :background base0)
    (ediff-current-diff-B :background base0)
    (ediff-current-diff-C :background base0)
    (ediff-even-diff-A    :inherit 'hl-line)
    (ediff-even-diff-B    :inherit 'hl-line)
    (ediff-even-diff-C    :inherit 'hl-line)
    (ediff-odd-diff-A     :inherit 'hl-line)
    (ediff-odd-diff-B     :inherit 'hl-line)
    (ediff-odd-diff-C     :inherit 'hl-line)

    ;; elfeed
    (elfeed-log-debug-level-face :foreground comments)
    (elfeed-log-error-level-face :inherit 'error)
    (elfeed-log-info-level-face  :inherit 'success)
    (elfeed-log-warn-level-face  :inherit 'warning)
    (elfeed-search-date-face     :foreground violet)
    (elfeed-search-feed-face     :foreground blue)
    (elfeed-search-tag-face      :foreground comments)
    (elfeed-search-title-face    :foreground comments)
    (elfeed-search-filter-face   :foreground violet)
    (elfeed-search-unread-count-face :foreground yellow)
    (elfeed-search-unread-title-face :foreground fg :bold bold)

    ;; eshell
    (eshell-prompt        :foreground base7)
    (eshell-ls-archive    :foreground magenta)
    (eshell-ls-backup     :foreground yellow)
    (eshell-ls-clutter    :foreground red)
    (eshell-ls-directory  :foreground blue)
    (eshell-ls-executable :foreground green)
    (eshell-ls-missing    :foreground red)
    (eshell-ls-product    :foreground orange)
    (eshell-ls-readonly   :foreground orange)
    (eshell-ls-special    :foreground violet)
    (eshell-ls-symlink    :foreground cyan)
    (eshell-ls-unreadable :foreground base5)

    ;; flx-ido
    (flx-highlight-face :inherit 'bold :foreground yellow :underline nil)

    ;; hl-line
    (hl-line :background bg-alt)

    ;; ido
    (ido-first-match :foreground orange)
    (ido-indicator   :foreground red :background bg)
    (ido-only-match  :foreground green)
    (ido-subdir      :foreground violet)
    (ido-virtual     :foreground comments)

    ;; isearch
    (isearch :background highlight :foreground base0 :bold bold)

    ;; linum
    ((linum &inherit line-number))

    ;; term
    (term               :inherit 'default)
    (term-bold          :inherit 'bold)
    (term-color-black   :background base0   :foreground base0)
    (term-color-red     :background red     :foreground red)
    (term-color-green   :background green   :foreground green)
    (term-color-yellow  :background yellow  :foreground yellow)
    (term-color-blue    :background blue    :foreground blue)
    (term-color-magenta :background magenta :foreground magenta)
    (term-color-cyan    :background cyan    :foreground cyan)
    (term-color-white   :background base8   :foreground base8)

    ;; window-divider
    (window-divider :inherit 'vertical-border)
    (window-divider-first-pixel :inherit 'window-divider)
    (window-divider-last-pixel  :inherit 'window-divider)


    ;; --- plugin faces -----------------------
    ;; avy
    (avy-lead-face :background highlight :foreground base0 :distant-foreground base8)
    (avy-lead-face-0 :inherit 'avy-lead-face)
    (avy-lead-face-1 :inherit 'avy-lead-face)
    (avy-lead-face-2 :inherit 'avy-lead-face)

    ;; bookmark+
    (bmkp-*-mark :foreground bg :background yellow)
    (bmkp->-mark :foreground yellow)
    (bmkp-D-mark :foreground bg :background red)
    (bmkp-X-mark :foreground red)
    (bmkp-a-mark :background red)
    (bmkp-bad-bookmark :foreground bg :background yellow)
    (bmkp-bookmark-file :foreground violet :background bg-alt)
    (bmkp-bookmark-list :background bg-alt)
    (bmkp-buffer :foreground blue)
    (bmkp-desktop :foreground bg :background violet)
    (bmkp-file-handler :background red)
    (bmkp-function :foreground green)
    (bmkp-gnus :foreground orange)
    (bmkp-heading :foreground yellow)
    (bmkp-info :foreground cyan)
    (bmkp-light-autonamed :foreground bg-alt :background cyan)
    (bmkp-light-autonamed-region :foreground bg-alt :background red)
    (bmkp-light-fringe-autonamed :foreground bg-alt :background violet)
    (bmkp-light-fringe-non-autonamed :foreground bg-alt :background green)
    (bmkp-light-mark :foreground bg :background cyan)
    (bmkp-light-non-autonamed :foreground bg :background violet)
    (bmkp-light-non-autonamed-region :foreground bg :background red)
    (bmkp-local-directory :foreground bg :background violet)
    (bmkp-local-file-with-region :foreground yellow)
    (bmkp-local-file-without-region :foreground comments)
    (bmkp-man :foreground violet)
    (bmkp-no-jump :foreground comments)
    (bmkp-no-local :foreground yellow)
    (bmkp-non-file :foreground green)
    (bmkp-remote-file :foreground orange)
    (bmkp-sequence :foreground blue)
    (bmkp-su-or-sudo :foreground red)
    (bmkp-t-mark :foreground violet)
    (bmkp-url :foreground blue :underline t)
    (bmkp-variable-list :foreground green)

    ;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg)
    (company-tooltip-selection  :background selection)
    (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
    (company-tooltip-annotation                       :foreground violet)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview                                  :foreground highlight)
    (company-preview-common     :background base3 :foreground magenta)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)

    ;; circe
    (circe-fool :foreground doc-comments)
    (circe-highlight-nick-face :inherit 'bold :foreground constants)
    (circe-prompt-face :inherit 'bold :foreground highlight)
    (circe-server-face :foreground comments)
    (circe-my-message-face :inherit 'bold)

    ;; diff-hl
    (diff-hl-change :foreground vc-modified)
    (diff-hl-delete :foreground vc-deleted)
    (diff-hl-insert :foreground vc-added)

    ;; diff-mode
    (diff-added   :inherit 'hl-line :foreground green)
    (diff-changed :foreground violet)
    (diff-removed :foreground red :background base3)
    (diff-header  :foreground cyan :background nil)
    (diff-file-header :foreground blue :background nil)
    (diff-hunk-header :foreground violet)
    (diff-refine-added   :inherit 'diff-added :inverse-video t)
    (diff-refine-changed :inherit 'diff-changed :inverse-video t)
    (diff-refine-removed :inherit 'diff-removed :inverse-video t)

    ;; dired+
    (diredp-file-name              :foreground base8)
    (diredp-dir-name               :foreground base8 :inherit 'bold)
    (diredp-ignored-file-name      :foreground base5)
    (diredp-compressed-file-suffix :foreground base5)
    (diredp-symlink                :foreground violet)
    (diredp-dir-heading            :foreground blue  :inherit 'bold)
    (diredp-file-suffix            :foreground violet)
    (diredp-read-priv              :foreground magenta)
    (diredp-write-priv             :foreground green)
    (diredp-exec-priv              :foreground yellow)
    (diredp-rare-priv              :foreground red   :inherit 'bold)
    (diredp-dir-priv               :foreground blue  :inherit 'bold)
    (diredp-no-priv                :foreground base5)
    (diredp-number                 :foreground magenta)
    (diredp-date-time              :foreground blue)

    ;; dired-k
    (dired-k-directory :foreground blue)

    ;; elscreen
    (elscreen-tab-background-face     :background bg)
    (elscreen-tab-control-face        :background bg     :foreground bg)
    (elscreen-tab-current-screen-face :background bg-alt :foreground fg)
    (elscreen-tab-other-screen-face   :background bg     :foreground fg-alt)

    ;; evil
    (evil-ex-substitute-matches     :background base0 :foreground red   :strike-through t :bold bold)
    (evil-ex-substitute-replacement :background base0 :foreground green :bold bold)
    (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight)

    ;; evil-mc
    (evil-mc-cursor-default-face :background magenta :foreground base0 :inverse-video nil)
    (evil-mc-region-face :inherit 'region)
    (evil-mc-cursor-bar-face :height 1 :background magenta :foreground base0)
    (evil-mc-cursor-hbar-face :underline `(:color ,highlight))

    ;; evil-snipe
    (evil-snipe-first-match-face :foreground highlight :background dark-blue :bold bold)
    (evil-snipe-matches-face     :foreground highlight :underline t :bold bold)

    ;; flycheck
    (flycheck-error     :underline `(:style wave :color ,red))
    (flycheck-warning   :underline `(:style wave :color ,yellow))
    (flycheck-info      :underline `(:style wave :color ,green))

    ;; flymake
    (flymake-warnline :background bg :underline `(:style wave :color ,orange))
    (flymake-errline  :background bg :underline `(:style wave :color ,red))

    ;; flyspell
    (flyspell-incorrect :underline `(:style wave :color ,error) :inherit 'unspecified)

    ;; git-gutter
    (git-gutter:modified :foreground vc-modified)
    (git-gutter:added    :foreground vc-added)
    (git-gutter:deleted  :foreground vc-deleted)

    ;; git-gutter+
    (git-gutter+-modified :foreground vc-modified :background nil)
    (git-gutter+-added    :foreground vc-added :background nil)
    (git-gutter+-deleted  :foreground vc-deleted :background nil)

    ;; git-gutter-fringe
    ((git-gutter-fr:modified &inherit git-gutter:modified))
    ((git-gutter-fr:added    &inherit git-gutter:added))
    ((git-gutter-fr:deleted  &inherit git-gutter:deleted))

    ;; gnus
    (gnus-group-mail-1           :bold bold :foreground fg)
    (gnus-group-mail-2           :inherit 'gnus-group-mail-1)
    (gnus-group-mail-3           :inherit 'gnus-group-mail-1)
    (gnus-group-mail-1-empty     :foreground base5)
    (gnus-group-mail-2-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-mail-3-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-1           :inherit 'gnus-group-mail-1)
    (gnus-group-news-2           :inherit 'gnus-group-news-1)
    (gnus-group-news-3           :inherit 'gnus-group-news-1)
    (gnus-group-news-4           :inherit 'gnus-group-news-1)
    (gnus-group-news-5           :inherit 'gnus-group-news-1)
    (gnus-group-news-6           :inherit 'gnus-group-news-1)
    (gnus-group-news-1-empty     :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-2-empty     :inherit 'gnus-groupnews-1-empty)
    (gnus-group-news-3-empty     :inherit 'gnus-groupnews-1-empty)
    (gnus-group-news-4-empty     :inherit 'gnus-groupnews-1-empty)
    (gnus-group-news-5-empty     :inherit 'gnus-groupnews-1-empty)
    (gnus-group-news-6-empty     :inherit 'gnus-groupnews-1-empty)
    (gnus-group-mail-low         :inherit 'gnus-group-mail-1 :bold nil)
    (gnus-group-mail-low-empty   :inherit 'gnus-group-mail-1-empty)
    (gnus-group-news-low         :inherit 'gnus-group-mail-1 :foreground base5)
    (gnus-group-news-low-empty   :inherit 'gnus-group-news-low :bold nil)
    (gnus-header-content         :inherit 'message-header-other)
    (gnus-header-from            :inherit 'message-header-other)
    (gnus-header-name            :inherit 'message-header-name)
    (gnus-header-newsgroups      :inherit 'message-header-other)
    (gnus-header-subject         :inherit 'message-header-subject)
    (gnus-summary-cancelled      :foreground red :strike-through t)
    (gnus-summary-high-ancient   :foreground (doom-lighten base5 0.2) :inherit 'italic)
    (gnus-summary-high-read      :foreground (doom-lighten fg 0.2))
    (gnus-summary-high-ticked    :foreground (doom-lighten magenta 0.2))
    (gnus-summary-high-unread    :foreground (doom-lighten green 0.2))
    (gnus-summary-low-ancient    :foreground (doom-darken base5 0.2) :inherit 'italic)
    (gnus-summary-low-read       :foreground (doom-darken fg 0.2))
    (gnus-summary-low-ticked     :foreground (doom-darken magenta 0.2))
    (gnus-summary-low-unread     :foreground (doom-darken green 0.2))
    (gnus-summary-normal-ancient :foreground base5 :inherit 'italic)
    (gnus-summary-normal-read    :foreground fg)
    (gnus-summary-normal-ticked  :foreground magenta)
    (gnus-summary-normal-unread  :foreground green :inherit 'bold)
    (gnus-summary-selected       :foreground blue :bold bold)
    (gnus-cite-1                 :foreground violet)
    (gnus-cite-2                 :foreground violet)
    (gnus-cite-3                 :foreground violet)
    (gnus-cite-4                 :foreground green)
    (gnus-cite-5                 :foreground green)
    (gnus-cite-6                 :foreground green)
    (gnus-cite-7                 :foreground magenta)
    (gnus-cite-8                 :foreground magenta)
    (gnus-cite-9                 :foreground magenta)
    (gnus-cite-10                :foreground yellow)
    (gnus-cite-11                :foreground yellow)
    (gnus-signature              :foreground yellow)
    (gnus-x-face                 :background base5 :foreground fg)

    ;; helm
    (helm-selection
     (&all :inherit 'bold :background selection)
     (&dark  :distant-foreground highlight)
     (&light :distant-foreground base0))
    (helm-match :foreground highlight :distant-foreground base8 :underline t)
    (helm-source-header          :background base2 :foreground base5)
    (helm-swoop-target-line-face :foreground highlight :inverse-video t)
    (helm-visible-mark           :inherit '(bold highlight))
    (helm-ff-file                :foreground fg)
    (helm-ff-prefix              :foreground keywords)
    (helm-ff-dotted-directory    :foreground grey)
    (helm-ff-directory           :foreground variables)
    (helm-ff-executable          :foreground base8 :inherit 'italic)
    (helm-grep-match             :foreground highlight :distant-foreground red)
    (helm-grep-file              :foreground methods)
    (helm-grep-lineno            :foreground base5)
    (helm-grep-finish            :foreground green)
    (helm-swoop-target-line-face       :foreground highlight :inverse-video t)
    (helm-swoop-target-line-block-face :foreground yellow)
    (helm-swoop-target-word-face       :foreground green :inherit 'bold)
    (helm-swoop-target-number-face     :foreground base5)

    ;; highlight-indentation-mode
    (highlight-indentation-face                :inherit 'hl-line)
    (highlight-indentation-current-column-face :background base1)
    (highlight-indentation-guides-odd-face     :inherit 'highlight-indentation-face)
    (highlight-indentation-guides-even-face    :inherit 'highlight-indentation-face)

    ;; highlight-quoted-mode
    (highlight-quoted-symbol :foreground type)
    (highlight-quoted-quote  :foreground operators)

    ;; highlight-numbers-mode
    (highlight-numbers-number :inherit 'bold :foreground numbers)

    ;; hlinum
    (linum-highlight-face :foreground fg :distant-foreground nil :bold nil)

    ;; hl-todo
    (hl-todo :foreground red :bold bold)

    ;; hydra
    (hydra-face-red      :foreground red     :bold bold)
    (hydra-face-blue     :foreground blue    :bold bold)
    (hydra-face-amaranth :foreground magenta :bold bold)
    (hydra-face-pink     :foreground violet  :bold bold)
    (hydra-face-teal     :foreground teal    :bold bold)

    ;; iedit
    (iedit-occurrence :foreground magenta :bold bold :inverse-video t)
    (iedit-read-only-occurrence :inherit 'region)

    ;; indent-guide
    ((indent-guide-face &inherit highlight-indentation-face))

    ;; ivy
    (ivy-current-match :background dark-blue :distant-foreground base0 :bold bold)
    (ivy-minibuffer-match-face-1
     :background base0
     :foreground (doom-lighten grey 0.1)
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

    ;; linum-relative
    ((linum-relative-current-face &inherit line-number-current-line))

    ;; lui
    (lui-time-stamp-face :foreground violet)
    (lui-highlight-face :foreground highlight)
    (lui-button-face :foreground builtin :underline t)

    ;; multiple cursors
    (mc/cursor-face :inherit 'cursor)

    ;; nav-flash
    (nav-flash-face :background selection :foreground base8 :bold bold)

    ;; neotree
    (neo-root-dir-face   :foreground strings :background bg :box `(:line-width 4 :color ,bg))
    (neo-file-link-face  :foreground fg)
    (neo-dir-link-face   :foreground highlight)
    (neo-expand-btn-face :foreground highlight)
    (neo-vc-edited-face  :foreground yellow)
    (neo-vc-added-face   :foreground green)
    (neo-vc-removed-face :foreground red :strike-through t)
    (neo-vc-conflict-face :foreground magenta :bold bold)
    (neo-vc-ignored-face  :foreground comments)
    (doom-neotree-dir-face :foreground highlight)
    (doom-neotree-file-face :foreground base8)
    (doom-neotree-hidden-file-face :foreground comments)
    (doom-neotree-text-file-face :foreground fg)
    (doom-neotree-data-file-face :foreground violet)
    (doom-neotree-media-file-face :inherit 'doom-neotree-hidden-file-face)

    ;; nlinum
    ((nlinum-current-line &inherit line-number-current-line))

    ;; nlinum-hl
    ((nlinum-hl-face &inherit line-number-current-line))

    ;; nlinum-relative
    ((nlinum-relative-current-face &inherit line-number-current-line))

    ;; lsp
    ;; TODO Add light versions
    (lsp-face-highlight-textual :background dark-blue :foreground base8 :distant-foreground base0 :bold bold)
    (lsp-face-highlight-read    :background dark-blue :foreground base8 :distant-foreground base0 :bold bold)
    (lsp-face-highlight-write   :background dark-blue :foreground base8 :distant-foreground base0 :bold bold)

    ;; magit
    (magit-bisect-bad        :foreground red)
    (magit-bisect-good       :foreground green)
    (magit-bisect-skip       :foreground orange)
    (magit-blame-date        :foreground red)
    (magit-blame-heading     :foreground orange :background base3)
    (magit-branch-current    :foreground blue)
    (magit-branch-local      :foreground cyan)
    (magit-branch-remote     :foreground green)
    (magit-cherry-equivalent :foreground violet)
    (magit-cherry-unmatched  :foreground cyan)
    (magit-diff-added             :foreground (doom-darken green 0.2)  :background (doom-blend green bg 0.1))
    (magit-diff-added-highlight   :foreground green                    :background (doom-blend green bg 0.2) :bold bold)
    (magit-diff-base              :foreground (doom-darken orange 0.2) :background (doom-blend orange bg 0.1))
    (magit-diff-base-highlight    :foreground orange                   :background (doom-blend orange bg 0.2) :bold bold)
    (magit-diff-context           :foreground (doom-darken fg 0.4) :background bg)
    (magit-diff-context-highlight :foreground fg                   :background bg-alt)
    (magit-diff-file-heading           :foreground fg :bold bold)
    (magit-diff-file-heading-selection :foreground magenta               :background dark-blue :bold bold)
    (magit-diff-hunk-heading           :foreground bg                    :background (doom-blend violet bg 0.3))
    (magit-diff-hunk-heading-highlight :foreground bg                    :background violet :bold bold)
    (magit-diff-removed                :foreground (doom-darken red 0.3) :background (doom-blend red base3 0.05))
    (magit-diff-removed-highlight      :foreground red                   :background (doom-blend red base3 0.1) :bold bold)
    (magit-diff-lines-heading          :foreground yellow     :background red)
    (magit-diffstat-added              :foreground green)
    (magit-diffstat-removed            :foreground red)
    (magit-dimmed :foreground comments)
    (magit-hash :foreground comments)
    (magit-header-line :background dark-blue :foreground base8 :bold bold
                       :box `(:line-width 3 :color ,dark-blue))
    (magit-log-author :foreground orange)
    (magit-log-date :foreground blue)
    (magit-log-graph :foreground comments)
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
    (magit-section-heading           :foreground blue :bold bold)
    (magit-section-heading-selection :foreground orange :bold bold)
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
    (magit-signature-untrusted :foreground cyan)
    (magit-tag :foreground yellow)
    (magit-filename :foreground violet)
    (magit-section-secondary-heading :foreground violet :bold bold)

    ;; mic-paren
    (paren-face-match    :foreground red   :background base0 :bold bold)
    (paren-face-mismatch :foreground base0 :background red   :bold bold)
    (paren-face-no-match :inherit 'paren-face-mismatch)

    ;; parenface
    (paren-face :foreground comments)

    ;; perspective
    (persp-selected-face :foreground blue :bold bold)

    ;; popup
    (popup-face :inherit 'tooltip)
    (popup-selection-face :background selection)

    ;; pos-tip
    (popup          :inherit 'tooltip)
    (popup-tip-face :inherit 'tooltip)

    ;; rainbow-delimiters
    (rainbow-delimiters-depth-1-face :foreground blue)
    (rainbow-delimiters-depth-2-face :foreground magenta)
    (rainbow-delimiters-depth-3-face :foreground green)
    (rainbow-delimiters-depth-4-face :foreground orange)
    (rainbow-delimiters-depth-5-face :foreground violet)
    (rainbow-delimiters-depth-6-face :foreground yellow)
    (rainbow-delimiters-depth-7-face :foreground teal)
    (rainbow-delimiters-unmatched-face  :foreground red :bold bold :inverse-video t)
    (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)

    ;; re-builder
    (reb-match-0 :foreground orange  :inverse-video t)
    (reb-match-1 :foreground magenta :inverse-video t)
    (reb-match-2 :foreground green   :inverse-video t)
    (reb-match-3 :foreground yellow  :inverse-video t)

    ;; show-paren
    ((show-paren-match &inherit paren-face-match))
    ((show-paren-mismatch &inherit paren-face-mismatch))

    ;; smartparens
    (sp-pair-overlay-face :background region)

    ;; smartparens
    ((sp-show-pair-match-face    &inherit show-paren-match))
    ((sp-show-pair-mismatch-face &inherit show-paren-mismatch))

    ;; solaire-mode
    (solaire-default-face  :inherit 'default :background bg-alt)
    (solaire-hl-line-face  :inherit 'hl-line :background bg)
    (solaire-org-hide-face :foreground bg-alt)

    ;; stripe-buffer
    (stripe-highlight
     (&light :background base5)
     (&dark  :background base3))

    ;; swiper
    (swiper-line-face    :background blue    :foreground base0)
    (swiper-match-face-1 :background base0   :foreground base5)
    (swiper-match-face-2 :background orange  :foreground base0 :bold bold)
    (swiper-match-face-3 :background magenta :foreground base0 :bold bold)
    (swiper-match-face-4 :background green   :foreground base0 :bold bold)

    ;; tabbar
    (tabbar-default             :foreground bg :background bg :height 1.0)
    (tabbar-highlight           :foreground fg :background selection :distant-foreground bg)
    (tabbar-button              :foreground fg :background bg)
    (tabbar-button-highlight    :inherit 'tabbar-button :inverse-video t)
    (tabbar-modified            :inherit 'tabbar-default :foreground red :bold bold)
    (tabbar-unselected          :inherit 'tabbar-default :foreground base5)
    (tabbar-unselected-modified :inherit 'tabbar-modified)
    (tabbar-selected
     :inherit 'tabbar-default :bold bold
     :foreground fg :background bg-alt)
    (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)

    ;; undo-tree
    (undo-tree-visualizer-default-face :foreground base5)
    (undo-tree-visualizer-current-face :foreground green :bold bold)
    (undo-tree-visualizer-unmodified-face :foreground base5)
    (undo-tree-visualizer-active-branch-face :foreground blue)
    (undo-tree-visualizer-register-face :foreground yellow)

    ;; vimish-fold
    (vimish-fold-overlay :inherit 'font-lock-comment-face :background base0)
    (vimish-fold-fringe  :foreground magenta)

    ;; volatile-highlights
    (vhl/default-face :background grey)

    ;; wgrep
    (wgrep-face :bold bold :foreground green :background base5)
    (wgrep-delete-face :foreground base3 :background red)
    (wgrep-done-face   :foreground blue)
    (wgrep-file-face   :foreground comments)
    (wgrep-reject-face :foreground red :bold bold)

    ;; which-func
    (which-func :foreground blue)

    ;; which-key
    (which-key-key-face                   :foreground green)
    (which-key-group-description-face     :foreground violet)
    (which-key-command-description-face   :foreground blue)
    (which-key-local-map-description-face :foreground magenta)

    ;; whitespace
    (whitespace-empty    :background base3)
    (whitespace-space    :foreground base4)
    (whitespace-tab      :foreground base4 :background (unless indent-tabs-mode base3))
    (whitespace-newline  :foreground base4)
    (whitespace-indentation :foreground red :background yellow)
    (whitespace-trailing :inherit 'trailing-whitespace)
    (whitespace-line     :background base0 :foreground red :bold bold)

    ;; workgroups2
    (wg-current-workgroup-face :foreground base0 :background highlight)
    (wg-other-workgroup-face   :foreground base5)
    (wg-divider-face           :foreground grey)
    (wg-brace-face             :foreground highlight)

    ;; yasnippet
    (yas-field-highlight-face :inherit 'match)


    ;; --- major-mode faces -------------------
    ;; auctex (latex-mode)
    (font-latex-bold-face         :inherit 'bold)
    (font-latex-italic-face       :inherit 'italic)
    (font-latex-math-face         :foreground blue)
    (font-latex-sectioning-0-face :inherit 'org-level-1)
    (font-latex-sectioning-1-face :inherit 'org-level-2)
    (font-latex-sectioning-2-face :inherit 'org-level-3)
    (font-latex-sectioning-3-face :inherit 'font-latex-sectioning-2-face)
    (font-latex-sectioning-4-face :inherit 'font-latex-sectioning-2-face)
    (font-latex-sectioning-5-face :inherit 'font-latex-sectioning-2-face)
    (font-latex-script-char-face  :foreground dark-blue)
    (font-latex-string-face       :inherit 'font-lock-string-face)
    (font-latex-warning-face      :inherit 'font-lock-warning-face)

    ;; jdee-mode
    (jdee-font-lock-number-face :foreground numbers)
    (jdee-font-lock-operator-face :foreground operators)
    (jdee-font-lock-constant-face :inherit 'font-lock-constant-face)
    (jdee-font-lock-constructor-face :foreground methods)
    (jdee-font-lock-public-face :inherit 'font-lock-keyword-face)
    (jdee-font-lock-protected-face :inherit 'font-lock-keyword-face)
    (jdee-font-lock-private-face :inherit 'font-lock-keyword-face)
    (jdee-font-lock-modifier-face :inherit 'font-lock-type-face)
    (jdee-font-lock-doc-tag-face :foreground violet)
    (jdee-font-lock-italic-face :inherit 'italic)
    (jdee-font-lock-bold-face :inherit 'bold)
    (jdee-font-lock-link-face :foreground blue :italic nil :underline t)

    ;; js2-mode
    (js2-function-param  :foreground variables)
    (js2-function-call   :foreground functions)
    (js2-object-property :foreground violet)
    (js2-jsdoc-tag       :foreground comments)

    ;; ledger-mode
    (ledger-font-posting-date-face :foreground blue)
    (ledger-font-posting-amount-face :foreground yellow)
    (ledger-font-posting-account-face :foreground base8)
    (ledger-font-payee-cleared-face :foreground violet :bold t :height 1.2)
    (ledger-font-payee-uncleared-face :foreground base5 :bold t :height 1.2)
    (ledger-font-xact-highlight-face :background base0)

    ;; makefile-*-mode
    (makefile-targets :foreground blue)

    ;; markdown-mode
    (markdown-header-face           :inherit 'bold :foreground highlight)
    (markdown-header-delimiter-face :inherit 'markdown-header-face)
    (markdown-metadata-key-face     :foreground red)
    (markdown-list-face             :foreground red)
    (markdown-link-face             :inherit 'bold :foreground blue)
    (markdown-url-face              :foreground magenta :bold nil)
    (markdown-header-face-1         :inherit 'markdown-header-face)
    (markdown-header-face-2         :inherit 'markdown-header-face)
    (markdown-header-face-3         :inherit 'markdown-header-face)
    (markdown-header-face-4         :inherit 'markdown-header-face)
    (markdown-header-face-5         :inherit 'markdown-header-face)
    (markdown-header-face-6         :inherit 'markdown-header-face)
    (markdown-italic-face           :inherit 'italic :foreground violet)
    (markdown-bold-face             :inherit 'bold   :foreground orange)
    (markdown-markup-face           :foreground operators)
    (markdown-blockquote-face       :inherit 'italic :foreground doc-comments)
    (markdown-pre-face              :foreground strings)
    (markdown-code-face :background base3)
    (markdown-inline-code-face :inherit '(markdown-code-face markdown-pre-face))

    ;; org-agenda
    (org-agenda-structure :foreground blue)
    (org-agenda-date      :foreground violet)
    (org-agenda-done      :inherit 'org-done)
    (org-agenda-dimmed-todo-face :foreground comments)

    ;; org-mode
    (org-level-1 :foreground blue   :background base3 :bold bold :height 1.2)
    (org-level-2 :foreground violet :background base3 :bold bold)
    (org-level-3 :foreground base8  :bold bold)
    (org-level-4 :inherit 'org-level-3)
    (org-level-5 :inherit 'org-level-3)
    (org-level-6 :inherit 'org-level-3)
    (org-tag :foreground green :bold nil)
    (org-priority :foreground red)
    (org-ellipsis :underline nil :background base3 :foreground violet)
    (org-hide :foreground bg)
    (org-table :foreground violet)
    (org-quote :inherit 'italic :background base3)
    (org-document-info         :foreground builtin)
    (org-document-title        :foreground builtin :bold bold)
    (org-default               :inherit 'variable-pitch)
    (org-meta-line             :foreground doc-comments)
    (org-block-begin-line      :foreground comments :background base3)
    (org-block-end-line        :inherit 'org-block-begin-line)
    (org-block-background      :background base3)
    (org-block                 :background base3)
    (org-archived              :foreground doc-comments)
    (org-code                  :foreground orange)
    (org-verbatim              :foreground green)
    (org-formula               :foreground cyan)
    (org-list-dt               :foreground highlight)
    (org-footnote              :foreground orange)
    (org-date                  :foreground yellow)
    (org-headline-done         :foreground base5)
    (org-todo                  :bold 'inherit :foreground highlight)
    (org-done                  :inherit 'org-headline-done :bold 'inherit)
    (org-special-keyword       :foreground magenta)
    (org-checkbox :inherit 'org-todo)
    (org-checkbox-statistics-todo :inherit 'org-todo)
    (org-checkbox-statistics-done :inherit 'org-done)
    (message-header-name :foreground green) ; FIXME move this

    ;; rpm-spec-mode
    (rpm-spec-macro-face        :foreground yellow)
    (rpm-spec-var-face          :foreground violet)
    (rpm-spec-tag-face          :foreground blue)
    (rpm-spec-obsolete-tag-face :foreground red)
    (rpm-spec-package-face      :foreground orange)
    (rpm-spec-dir-face          :foreground green)
    (rpm-spec-doc-face          :foreground orange)
    (rpm-spec-ghost-face        :foreground comments)
    (rpm-spec-section-face      :foreground magenta)

    ;; typescript-mode
    ((ts-object-property &inherit js2-object-property))

    ;; sh-mode
    (sh-heredoc :inherit 'font-lock-string-face :weight 'normal)
    (sh-quoted-exec :inherit 'font-lock-preprocessor-face)

    ;; web-mode
    (web-mode-doctype-face           :foreground comments)
    (web-mode-html-tag-face          :foreground methods)
    (web-mode-html-tag-bracket-face  :foreground methods)
    (web-mode-html-attr-name-face    :foreground type)
    (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
    (web-mode-block-control-face     :foreground orange)
    (web-mode-html-tag-bracket-face  :foreground operators))
  "TODO")

(defconst doom-themes-common-vars
  '((ansi-color-names-vector
     (vconcat (mapcar #'doom-color '(base0 red green yellow blue magenta cyan base8))))

    (fci-rule-color (doom-color 'base5))

    (jdee-db-spec-breakpoint-face-colors `(cons ,(doom-color 'base0) ,(doom-color 'grey)))
    (jdee-db-requested-breakpoint-face-colors `(cons ,(doom-color 'base0) ,(doom-color 'green)))
    (jdee-db-active-breakpoint-face-colors `(cons ,(doom-color 'base0) ,(doom-color 'highlight)))

    (org-fontify-whole-heading-line t)
    (org-fontify-done-headline t)
    (org-fontify-quote-and-verse-blocks t)

    (vc-annotate-color-map
     `(list (cons 20  ,(doom-color 'green))
            (cons 40  ,(doom-blend (doom-color 'yellow) (doom-color 'green) (/ 1.0 3)))
            (cons 60  ,(doom-blend (doom-color 'yellow) (doom-color 'green) (/ 2.0 3)))
            (cons 80  ,(doom-color 'yellow))
            (cons 100 ,(doom-blend (doom-color 'orange) (doom-color 'yellow) (/ 1.0 3)))
            (cons 120 ,(doom-blend (doom-color 'orange) (doom-color 'yellow) (/ 2.0 3)))
            (cons 140 ,(doom-color 'orange))
            (cons 160 ,(doom-blend (doom-color 'magenta) (doom-color 'orange) (/ 1.0 3)))
            (cons 180 ,(doom-blend (doom-color 'magenta) (doom-color 'orange) (/ 2.0 3)))
            (cons 200 ,(doom-color 'magenta))
            (cons 220 ,(doom-blend (doom-color 'red) (doom-color 'magenta) (/ 1.0 3)))
            (cons 240 ,(doom-blend (doom-color 'red) (doom-color 'magenta) (/ 2.0 3)))
            (cons 260 ,(doom-color 'red))
            (cons 280 ,(doom-blend (doom-color 'grey) (doom-color 'red) (/ 1.0 4)))
            (cons 300 ,(doom-blend (doom-color 'grey) (doom-color 'red) (/ 2.0 4)))
            (cons 320 ,(doom-blend (doom-color 'grey) (doom-color 'red) (/ 3.0 4)))
            (cons 340 ,(doom-color 'base5))
            (cons 360 ,(doom-color 'base5))))
    (vc-annotate-very-old-color nil)
    (vc-annotate-background (doom-color 'base0)))
  "TODO")


;; Library
(defvar doom-themes--colors)
(defvar doom--min-colors '(257 256 16))
(defvar doom--quoted-p nil)

(defvar doom-themes--faces nil)
(defvar doom-themes--vars nil)

(defun doom-themes--colors-p (item)
  "TODO"
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote doom-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((doom--quoted-p t))
                      (doom-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (doom--quoted-p)
                      (doom-themes--colors-p (cdr item))))

                   (t
                    (or (doom-themes--colors-p car)
                        (doom-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not doom--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item doom-themes--colors))))))

(defun doom-themes--colorize (item type)
  "TODO"
  (when item
    (let ((doom--quoted-p doom--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote doom-color))
                    item)
                   ((eq (car item) 'doom-ref)
                    (doom-themes--colorize
                     (apply #'doom-ref (cdr item)) type))
                   (t
                    (let* ((item (append item nil))
                           (car (car item))
                           (doom--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t doom--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (doom-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not doom--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item doom-themes--colors))
             `(doom-color ',item ',type))

            (t item)))))

(defun doom-themes--get-face (face &optional merge noerror)
  "TODO"
  (let ((face-body
         (or (cdr (assq face doom-themes--faces))
             (unless noerror
               (error "Couldn't find the '%s' face" face))))
        arg)
    (while (setq arg (pop merge))
      (if (keywordp arg)
          (plist-put face-body arg (pop merge))
        (push arg face-body)))
    face-body))

(defun doom-themes--add-face (face)
  "TODO"
  (let ((face-name (car face))
        (face-body (cdr face)))
    (when (listp face-name)
      (setq face-body
            (pcase (cadr face-name)
              (`&override
               (prog1 (doom-themes--get-face (car face-name) face-body t)
                 (setq doom-themes--faces (assq-delete-all face-name doom-themes--faces))))
              (`&inherit
               (doom-themes--get-face (car (cdr (cdr face-name))) face-body))
              (_
               (error "Malformed face spec for %s" (car face-name))))
            face-name (car face-name)))
    (when (assq face-name doom-themes--faces)
      (setq doom-theme--faces (assq-delete-all face-name doom-themes--faces)))
    (push `(,face-name ,@face-body) doom-themes--faces)))

(defun doom-themes--build-face (face)
  (let ((face-name (car face))
        (face-body (cdr face)))
    `(list
      ',face-name
      ,(cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                (cond ((doom-themes--colors-p real-attrs)
                       (dolist (cl doom--min-colors `(list ,@(nreverse defs)))
                         (push `(list '((class color) (min-colors ,cl))
                                      (list ,@(doom-themes--colorize real-attrs cl)))
                               defs)))

                      (t
                       `(list (list 't (list ,@real-attrs)))))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))

             (t
              (let (all-attrs defs)
                (dolist (attrs face-body `(list ,@(nreverse defs)))
                  (cond ((eq (car attrs) '&all)
                         (setq all-attrs (append all-attrs (cdr attrs))))

                        ((memq (car attrs) '(&dark &light))
                         (let ((bg (if (eq (car attrs) '&dark) 'dark 'light))
                               (real-attrs (append all-attrs (cdr attrs) '())))
                           (cond ((doom-themes--colors-p real-attrs)
                                  (dolist (cl doom--min-colors)
                                    (push `(list '((class color) (min-colors ,cl) (background ,bg))
                                                 (list ,@(doom-themes--colorize real-attrs cl)))
                                          defs)))

                                 (t
                                  (push `(list '((background ,bg)) (list ,@real-attrs))
                                        defs)))))))))))))

(defun doom-themes--build-var (var)
  "TODO"
  `(list ',(car var) ,(cadr var)))

(defun doom-themes-common-faces (&optional extra-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (setq doom-themes--faces nil)
  (mapc #'doom-themes--add-face (append doom-themes-common-faces extra-faces))
  (reverse (mapcar #'doom-themes--build-face doom-themes--faces)))

(defun doom-themes-common-variables (&optional extra-vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (setq doom-themes--vars nil)
  (mapcar #'doom-themes--build-var (append doom-themes-common-vars extra-vars)))

(provide 'doom-themes-common)
;;; doom-themes-common.el ends here
