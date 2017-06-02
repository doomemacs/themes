;;; doom-themes-common.el

(defun doom-themes--build-face (spec)
  `(list ',(car spec)
         ,(if (listp (cadr spec))
              (cadr spec)
            `(list (list '((class color) (min-colors 89)) (list ,@(cdr spec)))))))

(defun doom-themes--build-var (var)
  `(list ',(car var) ,(cadr var)))

;;
(defun doom-themes-common-faces (&optional extra-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (mapcar
   #'doom-themes--build-face
   (cl-delete-duplicates
    (append
     '(;; --- custom faces -----------------------
       (doom-default :inherit 'default :background bg-alt)

       ;; --- base faces -------------------------
       (bold        :weight (if bold 'bold 'normal) :color white)
       (italic      :slant  (if italic 'italic 'normal))
       (bold-italic :inherit '(bold italic))

       (default :background bg :foreground fg)
       (fringe
        `((((background dark))  (:inherit 'default :foreground ,grey))
          (((background light)) (:inherit 'default :foreground ,light-grey))))
       (region               :background region     :foreground nil :distant-foreground (doom-darken fg 0.3))
       (highlight            :background highlight  :foreground black :distant-foreground white)
       (cursor               :background highlight)
       (shadow               :foreground light-grey)
       (minibuffer-prompt    :foreground highlight)
       (tooltip              :inherit 'doom-default :background bg :foreground fg)
       (secondary-selection  :background grey)
       (lazy-highlight       :background dark-blue  :foreground white :distant-foreground black :bold bold)
       (match                :foreground green      :background black :bold bold)
       (trailing-whitespace  :background red)
       (vertical-border :background vertical-bar :foreground vertical-bar)
       (link                 :foreground blue :underline t)

       (error   :foreground error)
       (warning :foreground warning)
       (success :foreground success)

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

       ;; mode-line / header-line
       (mode-line           :background bg-alt :foreground fg)
       (mode-line-inactive  :background bg     :foreground fg-alt)
       (mode-line-emphasis  :foreground highlight)
       (mode-line-highlight :inherit 'highlight)
       (mode-line-buffer-id :foreground fg :bold bold)
       (header-line :inherit 'mode-line)


       ;; --- built-in plugin faces --------------
       ;; dired
       (dired-directory :foreground builtin)
       (dired-ignored   :foreground comments)

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

       ;; flx-ido
       (flx-highlight-face :inherit 'bold :foreground yellow :underline nil)

       ;; hl-line
       (hl-line :background bg-alt)
       (doom-hl-line :background current-line)

       ;; ido
       (ido-first-match :foreground orange)
       (ido-indicator   :foreground red :background bg)
       (ido-only-match  :foreground green)
       (ido-subdir      :foreground violet)
       (ido-virtual     :foreground comments)

       ;; isearch
       (isearch :background highlight :foreground black :bold bold)
       (isearch-lazy-highlight-face :background dark-grey)

       ;; linum
       (linum :foreground fg-alt :background bg :bold nil :distant-foreground nil)
       (doom-linum :inherit 'linum :background bg-alt)
       (doom-linum-highlight :foreground fg :background current-line :bold nil :distant-foreground nil)

       ;; term
       (term-color-black   :background black   :foreground black)
       (term-color-red     :background red     :foreground red)
       (term-color-green   :background green   :foreground green)
       (term-color-yellow  :background yellow  :foreground yellow)
       (term-color-blue    :background blue    :foreground blue)
       (term-color-magenta :background magenta :foreground magenta)
       (term-color-cyan    :background cyan    :foreground cyan)
       (term-color-white   :background white   :foreground white)

       ;; window-divider
       (window-divider :inherit 'vertical-border)
       (window-divider-first-pixel  :inherit 'window-divider)
       (window-divider-last-pixel   :inherit 'window-divider)


       ;; --- plugin faces -----------------------
       ;; avy
       (avy-lead-face
        `((((background dark))  (:background ,highlight :foreground ,black :distant-foreground ,white))
          (((background light)) (:background ,highlight :foreground ,white :distant-foreground ,black))))
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
       (company-preview-common     :background dark-grey :foreground magenta)
       (company-preview-search     :inherit 'company-tooltip-search)

       ;; diff-hl
       (diff-hl-change :foreground vc-modified)
       (diff-hl-delete :foreground vc-deleted)
       (diff-hl-insert :foreground vc-added)

       ;; diff-mode
       (diff-added   :inherit 'hl-line :foreground green)
       (diff-changed :foreground violet)
       (diff-removed :foreground red :background dark-grey)
       (diff-header  :foreground cyan :background nil)
       (diff-file-header :foreground blue :background nil)
       (diff-hunk-header :foreground violet)
       (diff-refine-added   :inherit 'diff-added :inverse-video t)
       (diff-refine-changed :inherit 'diff-changed :inverse-video t)
       (diff-refine-removed :inherit 'diff-removed :inverse-video t)

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

       ;; elscreen
       (elscreen-tab-background-face     :background bg)
       (elscreen-tab-control-face        :background bg     :foreground bg)
       (elscreen-tab-current-screen-face :background bg-alt :foreground fg)
       (elscreen-tab-other-screen-face   :background bg     :foreground fg-alt)

       ;; evil
       (evil-ex-substitute-matches     :background black :foreground red   :strike-through t :bold bold)
       (evil-ex-substitute-replacement :background black :foreground green :bold bold)
       (evil-search-highlight-persist-highlight-face :inherit 'isearch-lazy-highlight-face)

       ;; evil-mc
       (evil-mc-cursor-default-face :background magenta :foreground black :inverse-video nil)
       (evil-mc-region-face :inherit 'region)
       (evil-mc-cursor-bar-face :height 1 :background magenta :foreground black)
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
       (git-gutter-fr:modified :foreground vc-modified)
       (git-gutter-fr:added    :foreground vc-added)
       (git-gutter-fr:deleted  :foreground vc-deleted)

       ;; gnus
       (gnus-group-mail-1           :bold bold :foreground fg)
       (gnus-group-mail-2           :inherit 'gnus-group-mail-1)
       (gnus-group-mail-3           :inherit 'gnus-group-mail-1)
       (gnus-group-mail-1-empty     :foreground light-grey)
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
       (gnus-group-news-low         :inherit 'gnus-group-mail-1 :foreground light-grey)
       (gnus-group-news-low-empty   :inherit 'gnus-group-news-low :bold nil)
       (gnus-header-content         :inherit 'message-header-other)
       (gnus-header-from            :inherit 'message-header-other)
       (gnus-header-name            :inherit 'message-header-name)
       (gnus-header-newsgroups      :inherit 'message-header-other)
       (gnus-header-subject         :inherit 'message-header-subject)
       (gnus-summary-cancelled      :foreground red :strike-through t)
       (gnus-summary-high-ancient   :foreground (doom-lighten light-grey 0.2) :inherit 'italic)
       (gnus-summary-high-read      :foreground (doom-lighten fg 0.2))
       (gnus-summary-high-ticked    :foreground (doom-lighten magenta 0.2))
       (gnus-summary-high-unread    :foreground (doom-lighten green 0.2))
       (gnus-summary-low-ancient    :foreground (doom-darken light-grey 0.2) :inherit 'italic)
       (gnus-summary-low-read       :foreground (doom-darken fg 0.2))
       (gnus-summary-low-ticked     :foreground (doom-darken magenta 0.2))
       (gnus-summary-low-unread     :foreground (doom-darken green 0.2))
       (gnus-summary-normal-ancient :foreground light-grey :inherit 'italic)
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

       ;; hlinum
       (linum-highlight-face :inherit 'doom-linum-highlight)

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
       (indent-guide-face :foreground (doom-lighten bg 0.1))

       ;; ivy
       (ivy-current-match :background (if gui dark-blue "brightblack") :distant-foreground black :bold bold)
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

       ;; linum-relative
       (linum-relative-current-face :inherit 'doom-linum-highlight)

       ;; multiple cursors
       (mc/cursor-face :inherit 'cursor)

       ;; nav-flash
       (nav-flash-face :background dark-blue :foreground white :bold bold)

       ;; neotree
       (neo-root-dir-face   :foreground green :background bg :box `(:line-width 4 :color ,bg))
       (neo-file-link-face  :foreground fg)
       (neo-dir-link-face   :foreground blue)
       (neo-expand-btn-face :foreground blue)
       (doom-neotree-dir-face :foreground highlight)
       (doom-neotree-file-face
        `((((background dark))  (:foreground ,white))
          (((background light)) (:foreground ,black))))
       (doom-neotree-hidden-file-face
        `((((background dark))  (:foreground ,light-grey))
          (((background light)) (:foreground ,dark-grey))))
       (doom-neotree-text-file-face :foreground fg)
       (doom-neotree-data-file-face :foreground violet)
       (doom-neotree-media-file-face :inherit 'doom-neotree-hidden-file-face)

       ;; nlinum-relative
       (nlinum-relative-current-face :inherit 'doom-linum-highlight)

       ;; lsp
       (lsp-face-highlight-textual :background dark-blue :foreground white :distant-foreground black :bold bold)
       (lsp-face-highlight-read    :background dark-blue :foreground white :distant-foreground black :bold bold)
       (lsp-face-highlight-write   :background dark-blue :foreground white :distant-foreground black :bold bold)

       ;; magit
       (magit-bisect-bad        :foreground red)
       (magit-bisect-good       :foreground green)
       (magit-bisect-skip       :foreground orange)
       (magit-blame-date        :foreground red)
       (magit-blame-heading     :foreground orange :background highlight)
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
       (magit-diff-removed                :foreground (doom-darken red 0.3) :background (doom-blend red dark-grey 0.05))
       (magit-diff-removed-highlight      :foreground red                   :background (doom-blend red dark-grey 0.1) :bold bold)
       (magit-diff-lines-heading          :foreground yellow     :background red)
       (magit-diffstat-added              :foreground green)
       (magit-diffstat-removed            :foreground red)
       (magit-dimmed :foreground comments)
       (magit-hash :foreground comments)
       (magit-header-line :background dark-blue :foreground white :bold bold
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
       (paren-face-match    :inherit 'show-paren-match)
       (paren-face-mismatch :inherit 'show-paren-mismatch)
       (paren-face-no-match :inherit 'show-paren-mismatch)

       ;; parenface
       (paren-face :foreground comments)

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

       ;; smartparens
       (sp-pair-overlay-face :background region)

       ;; show-paren
       (show-paren-match    :foreground red   :background black :bold bold)
       (show-paren-mismatch :foreground black :background red   :bold bold)

       ;; smartparens
       (sp-show-pair-match-face :inherit 'show-paren-match)
       (sp-show-pair-mismatch-face :inherit 'show-paren-mismatch)

       ;; spaceline
       (spaceline-highlight-face :foreground blue)

       ;; stripe-buffer
       (stripe-highlight
        `((((background dark))  (:background ,dark-grey))
          (((background light)) (:background ,light-grey))))

       ;; swiper
       (swiper-line-face    :background blue    :foreground black)
       (swiper-match-face-1 :background black   :foreground light-grey)
       (swiper-match-face-2 :background orange  :foreground black :bold bold)
       (swiper-match-face-3 :background magenta :foreground black :bold bold)
       (swiper-match-face-4 :background green   :foreground black :bold bold)

       ;; tabbar
       (tabbar-default           :foreground bg :background bg :height 0.9)
       (tabbar-modified          :inherit 'tabbar-default  :foreground red :bold bold)
       (tabbar-unselected        :inherit 'tabbar-default  :foreground grey)
       (tabbar-selected          :inherit 'tabbar-default  :foreground fg :background bg :bold bold)
       (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)
       (tabbar-highlight         :foreground fg :background bg :inverse-video t)
       (tabbar-button            :background (if gui bg dark-grey) :foreground fg)
       (tabbar-button-highlight  :inherit 'tabbar-button :inverse-video t)

       ;; undo-tree
       (undo-tree-visualizer-default-face :foreground light-grey)
       (undo-tree-visualizer-current-face :foreground green :bold bold)
       (undo-tree-visualizer-unmodified-face :foreground light-grey)
       (undo-tree-visualizer-active-branch-face :foreground blue)
       (undo-tree-visualizer-register-face :foreground yellow)

       ;; vimish-fold
       (vimish-fold-overlay :inherit 'font-lock-comment-face :background black)
       (vimish-fold-fringe  :foreground magenta)

       ;; volatile-highlights
       (vhl/default-face :background grey)

       ;; wgrep
       (wgrep-face
        `((((background dark))  (:background ,dark-grey  :bold ,bold :foreground ,green))
          (((background light)) (:background ,light-grey :bold ,bold :foreground ,green))))
       (wgrep-delete-face :foreground dark-grey :background red)
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
       (whitespace-tab      :foreground light-grey)
       (whitespace-newline  :foreground light-grey)
       (whitespace-trailing :inherit 'trailing-whitespace)
       (whitespace-line     :background black :foreground red :bold bold)

       ;; workgroups2
       (wg-current-workgroup-face :foreground black      :background blue)
       (wg-other-workgroup-face   :foreground light-grey :background current-line)
       (wg-divider-face           :foreground grey)
       (wg-brace-face             :foreground blue)

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
       (ledger-font-posting-account-face :foreground white)
       (ledger-font-payee-cleared-face :foreground violet :bold t :height 1.2)
       (ledger-font-payee-uncleared-face :foreground light-grey :bold t :height 1.2)
       (ledger-font-xact-highlight-face
        `((((background dark))  (:background ,dark-grey))
          (((background light)) (:background ,light-grey))))

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
       ;; (markdown-header-rule-face      :inherit 'shadow)
       ;; (markdown-markup-face           :foreground operators)
       ;; (markdown-link-face             :inherit 'shadow)
       ;; (markdown-link-title-face       :inherit 'link)
       ;; (markdown-url-face              :inherit 'link)
       ;; (markdown-blockquote-face       :foreground violet)

       ;; org-agenda
       (org-agenda-structure :foreground blue)
       (org-agenda-date      :foreground violet)
       (org-agenda-done      :inherit 'org-done)
       (org-agenda-dimmed-todo-face :foreground comments)

       ;; org-mode
       (org-level-1
        `((((background dark))  (:background ,dark-grey  :foreground ,blue :bold ,bold :height 1.2))
          (((background light)) (:background ,light-grey :foreground ,blue :bold ,bold :height 1.2))))
       (org-level-2 :inherit 'org-level-1 :foreground violet :height 1.0)
       (org-level-3
        `((((background dark))  (:foreground ,white :bold ,bold))
          (((background light)) (:foreground ,black :bold ,bold))))
       (org-level-4 :inherit 'org-level-3)
       (org-level-5 :inherit 'org-level-3)
       (org-level-6 :inherit 'org-level-3)
       (org-tag :foreground green :bold nil)
       (org-priority :foreground red)
       (org-ellipsis
        `((((background dark))
           (:background ,dark-grey :foreground ,violet :underline nil))
          (((background light))
           (:background ,light-grey :foreground ,dark-grey :underline nil))))
       (org-hide :foreground bg)
       (doom-org-hide :foreground bg-alt)
       (org-table :foreground violet)
       (org-quote
        `((((background dark)) (:inherit 'italic :background ,dark-grey))
          (((background light)) (:inherit 'italic :background ,light-grey))))
       (org-document-info         :foreground builtin)
       (org-document-title        :foreground builtin :bold bold)
       (org-default               :inherit 'variable-pitch)
       (org-meta-line             :foreground doc-comments)
       (org-block-begin-line      :foreground comments :background current-line)
       (org-block-end-line        :inherit 'org-block-begin-line)
       (org-block-background      :background current-line)
       (org-block                 :background current-line)
       (org-archived              :foreground light-grey)
       (org-code                  :foreground orange)
       (org-verbatim              :foreground green)
       (org-formula               :foreground cyan)
       (org-list-dt               :foreground blue)
       (org-footnote              :foreground orange)
       (org-link                  :foreground green :underline t :bold 'inherit)
       (org-date                  :foreground violet)
       (org-todo                  :foreground blue :bold 'inherit)
       (org-done
        `((((background dark))  (:bold inherit :foreground ,light-grey))
          (((background light)) (:bold inherit :foreground ,dark-grey))))
       (org-headline-done         :foreground light-grey)
       (org-special-keyword       :foreground magenta)
       (org-checkbox :inherit 'org-todo)
       (org-checkbox-statistics-todo :inherit 'org-todo)
       (org-checkbox-statistics-done :inherit 'org-done)
       (message-header-name :foreground green)

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
       (ts-object-property :inherit 'js2-object-property)

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
       ;; (web-mode-html-tag-bracket-face :foreground operators)))
       )
     extra-faces)
    :key #'car)))

(defun doom-themes-common-variables (&optional extra-vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (mapcar
   #'doom-themes--build-var
   (cl-delete-duplicates
    (append
     '((ansi-color-names-vector
        (vector black red green yellow blue magenta cyan white))

       (fci-rule-color light-grey)

       (jdee-db-spec-breakpoint-face-colors `(cons ,black ,grey))
       (jdee-db-requested-breakpoint-face-colors `(cons ,black ,green))
       (jdee-db-active-breakpoint-face-colors `(cons ,black ,highlight))

       (org-ellipsis "  ")
       (org-fontify-whole-heading-line t)
       (org-fontify-done-headline t)
       (org-fontify-quote-and-verse-blocks t)

       (vc-annotate-color-map
        `(list (cons 20  ,green)
               (cons 40  ,(doom-blend yellow green (/ 1.0 3)))
               (cons 60  ,(doom-blend yellow green (/ 2.0 3)))
               (cons 80  ,yellow)
               (cons 100 ,(doom-blend orange yellow (/ 1.0 3)))
               (cons 120 ,(doom-blend orange yellow (/ 2.0 3)))
               (cons 140 ,orange)
               (cons 160 ,(doom-blend magenta orange (/ 1.0 3)))
               (cons 180 ,(doom-blend magenta orange (/ 2.0 3)))
               (cons 200 ,magenta)
               (cons 220 ,(doom-blend red magenta (/ 1.0 3)))
               (cons 240 ,(doom-blend red magenta (/ 2.0 3)))
               (cons 260 ,red)
               (cons 280 ,(doom-blend grey red (/ 1.0 4)))
               (cons 300 ,(doom-blend grey red (/ 2.0 4)))
               (cons 320 ,(doom-blend grey red (/ 3.0 4)))
               (cons 340 ,light-grey)
               (cons 360 ,light-grey)))
       (vc-annotate-very-old-color nil)
       (vc-annotate-background black))
     extra-vars)
    :key #'car)))

(provide 'doom-themes-common)
;;; doom-themes-common.el ends here
