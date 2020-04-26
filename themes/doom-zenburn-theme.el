;;; doom-zenburn-theme.el ---  -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-zenburn-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-zenburn-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-zenburn-theme
  :type 'boolean)

(defcustom doom-zenburn-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-zenburn-theme
  :type 'boolean)

(defcustom doom-zenburn-comment-bg doom-zenburn-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-zenburn-theme
  :type 'boolean)

(defcustom doom-zenburn-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-zenburn-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-zenburn
  ""

  ;; name        default   256       16
  ((bg         '("#3F3F3F" nil       nil            )) ;; zenburn-bg
   (bg-alt     '("#383838" nil       nil            )) ;; zenburn-bg-05
   (base0      '("#000000" "black"   "black"        )) ;; zenburn-bg-2
   (base1      '("#2B2B2B" "#1e1e1e" "brightblack"  )) ;; zenburn-bg-1
   (base2      '("#303030" "#2e2e2e" "brightblack"  )) ;; zenburn-bg-08
   (base3      '("#383838" "#262626" "brightblack"  )) ;; zenburn-bg-05
   (base4      '("#494949" "#3f3f3f" "brightblack"  )) ;; zenburn-bg+05
   (base5      '("#4F4F4F" "#525252" "brightblack"  )) ;; zenburn-bg+1
   (base6      '("#5F5F5F" "#6b6b6b" "brightblack"  )) ;; zenburn-bg+2
   (base7      '("#6F6F6F" "#979797" "brightblack"  )) ;; zenburn-bg+3
   (base8      '("#FFFFEF" "#dfdfdf" "white"        )) ;; zenburn-fg+1
   (fg         '("#DCDCDC" "#bfbfbf" "brightwhite"  )) ;; zenburn-fg
   (fg-alt     '("#989890" "#2d2d2d" "white"        )) ;; zenburn-fg-05

   (grey       base4)
   (red        '("#CC9393" "#ff6655" "red"          )) ;; zenburn-red
   (orange     '("#DFAF8F" "#dd8844" "brightred"    )) ;; zenburn-orange
   (green      '("#7F9F7F" "#99bb66" "green"        )) ;; zenburn-green
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  )) ;; zenburn-??
   (yellow     '("#F0DFAF" "#ECBE7B" "yellow"       )) ;; zenburn-yellow
   (blue       '("#8CD0D3" "#51afef" "brightblue"   )) ;; zenburn-blue
   (dark-blue  '("#2257A0" "#2257A0" "blue"         )) ;; zenburn-??
   (magenta    '("#DC8CC3" "#c678dd" "brightmagenta")) ;; zenburn-magenta
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      )) ;; zendurn-??
   (cyan       '("#93E0E3" "#46D9FF" "brightcyan"   )) ;; zenburn-cyan
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         )) ;; zenburn-??

   ;; Extra zenburn colors
   (fg-1       '("#656555"))
   (fg+2       '("#FFFFFD"))
   (red-6      '("#6C3333"))
   (red-5      '("#7C4343"))
   (red-4      '("#8C5353"))
   (red-3      '("#9C6363"))
   (red-2      '("#AC7373"))
   (red-1      '("#BC8383"))
   (red+1      '("#DCA3A3"))
   (red+2      '("#ECB3B3"))
   (yellow-2   '("#D0BF8F"))
   (yellow-1   '("#E0CF9F"))
   (green-5    '("#2F4F2F"))
   (green-4    '("#3F5F3F"))
   (green-3    '("#4F6F4F"))
   (green-2    '("#5F7F5F"))
   (green-1    '("#6F8F6F"))
   (green+1    '("#8FB28F"))
   (green+2    '("#9FC59F"))
   (green+3    '("#AFD8AF"))
   (green+4    '("#BFEBBF"))
   (blue+3     '("#BDE0F3"))
   (blue+2     '("#ACE0E3"))
   (blue+1     '("#94BFF3"))
   (blue-1     '("#7CB8BB"))
   (blue-2     '("#6CA0A3"))
   (blue-3     '("#5C888B"))
   (blue-4     '("#4C7073"))
   (blue-5     '("#366060"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        fg)
   (comments       green)
   (doc-comments   green+2)
   (constants      green+4)
   (functions      cyan)
   (keywords       yellow)
   (methods        cyan)
   (operators      blue)
   (type           blue-1)
   (strings        red)
   (variables      orange)
   (numbers        fg)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-zenburn-brighter-modeline)
   (-modeline-pad
    (when doom-zenburn-padded-modeline
      (if (integerp doom-zenburn-padded-modeline) doom-zenburn-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-zenburn-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (link :foreground yellow :underline t :weight 'bold)
   (link :foreground yellow-2 :underline t :weight 'bold)
   (cursor :foreground fg :background base8)
   (widget-field :foreground fg :background base7)
   (escape-glyph :foreground yellow :weight 'bold)
   (fringe :foreground fg :background base5)
   (header-line :foreground yellow :background base1)
   (highlight :background base4)
   (success :foreground green :weight 'bold)
   (warning :foreground orange :weight 'bold)
   (tooltip :foreground fg :background base5)

   (font-lock-builtin-face :foreground fg :weight 'bold)
   (font-lock-comment-delimiter-face :foreground green-2)
   (font-lock-constant-face :foreground green+4)
   (font-lock-doc-face :foreground green+2)
   (font-lock-type-face :foreground blue-1)
   (font-lock-warning-face :foreground yellow-1 :weigth 'bold)

   ;; compilation
   (compilation-error-face :inherit compilation-error-face :underline t)
   (compilation-info :foreground blue)
   (compilation-line-number :foreground yellow)
   (compilation-warning-face :foreground yellow)
   (compilation-mode-line-exit :foreground green+2 :weight 'bold)

   ;; Completions
   (completion-annotations :foreground fg-1)

   ;; Customize
   (custom-variable-tag :foreground blue :weight 'bold)
   (custom-group-tag :foreground blue :weight 'bold)
   (custom-state :foreground green+4)

   ;;fill-column
   (fill-column-indicator :foreground base4 :weight 'semilight)

   ;; hi-lock
   (hi-green :background green+4 :background base1)
   (hi-green-b :foreground green+2 :weight 'bold)


   ;; isearch
   (isearch :freground yellow-2 :weight 'bold :background base6)
   (isearch-fail :foreground fg :background red-4)
   (lazy-highlight :foreground yellow-2 :weight 'bold :background base3)

   (minibuffer-prompt :foreground yellow)

   (mode-line :foreground green+1 :background base1)
   (mode-line-inactive :foreground green-2)

   (vertical-border :foreground fg-1) ;; different

   ;; line numbers
   (line-number :inherit 'default :foreground base7 :background base3)
   (line-number-current-line :inherit 'line-number :foreground yellow-2)

   ;; woman
   (woman :inherit 'font-lock-keyword-face)
   (woman :inherit 'font-lock-string-face italic)

   ;; anzu
   (anzu-mode-line :foreground cyan :weight 'bold)
   (anzu-mode-line-no-match :foreground red :weight 'bold)
   (anzu-match-1 :foreground bg :background green)
   (anzu-match-2 :foreground bg :background orange)
   (anzu-match-3 :foreground bg :background blue)
   (anzu-replace-to :inherit 'anzu-replace-highlight :foreground yellow)

   ;; autex
   (font-latex-italic :foreground cyan :slant 'italic)

   ;; avy
   (avy-background-face :foreground fg-1 :background bg)
   (avy-lead-face-0 :foreground green+3 :background bg :weight 'bold)
   (avy-lead-face-1 :foreground yellow :background bg :weight 'bold)
   (avy-lead-face-2 :foreground red+1 :background bg :weight 'bold)
   (avy-lead-face :foreground cyan :background bg :weight 'bold)

   ;; auto-complete
   (popup-tip-face :background yellow-2 :foreground base0)
   (popup-menu-mouse-face :background yellow-2 :foreground base0)
   (popup-summary-face :background base7 :foreground base0)
   (popup-scroll-bar-foreground-face :background blue-5)
   (popup-scroll-bar-background-face :background base1)
   (popup-isearch-match :background bg :foreground fg)

   ;; centaur-tabs
   (centaur-tabs-selected :background bg :foreground fg+2)
   (centaur-tabs-unselected :background base1 :foreground fg-alt)
   (centaur-tabs-selected-modified :background bg :foreground orange)
   (centaur-tabs-unselected-modified :background base1 :foreground orange)
   (centaur-tabs-active-bar-face :background yellow)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected-modified :foreground yellow)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected-modified :foreground yellow)
   
   ;; company
   (company-tooltip-annotation :foreground orange :background base5)
   (company-tooltip-annotation-selection :foreground orange :background base1)
   (company-tooltip-selection :foreground fg :background base1)
   (company-tooltop-mouse :foreground base1)
   (company-tooltip-common :foreground green+2)
   (company-tooltip-common-selection :foreground green+2)
   (company-scrollbar-fg :background base1)
   (company-preview :background green+2)
   (company-preview-common :foreground green+2 :background base1)

   ;; calfw
   (cfw:face-default-content :foreground green)
   (cfw:face-disable :foreground fg-1)
   (cfw:face :inherit 'shadow)
   (cfw:face :inherit 'font-lock-keyword-face)
   (cfw:face-sunday :foreground red :weight 'bold)
   (cfw:face :inherit 'cfw:face-sunday)
   (cfw:face-periods :foreground cyan)
   (cfw:face-select :background blue-5)
   (cfw:face-saturday :foreground blue :weight 'bold)
   (cfw:face-select :background blue-5)
   (cfw:face-title :height 2.0 :inherit '(variable-pitch font-lock-keyword-face))
   (cfw:face-today :foreground cyan :weight 'bold)
   (cfw:face-toolbar-button-off :underline nil :inherit 'link)
   (cfw:face-toolbar-button-on :underline nil :inherit 'link-visited)

   ;; circe
   (circe-highlight-nick-face :foreground cyan)
   (circe-my-message-face :foreground fg)
   (circe-fool-face :foreground red+1)
   (circe-topic-diff-removed-face :foreground red :weight 'bold)
   (circe-originator-face :foreground fg)
   (circe-server-face :foreground green)
   (circe-topic-diff-new-face :foreground orange :weight 'bold)
   (circe-prompt-face :foreground orange :background bg :weight 'bold)

   ;; diredfl
   (diredfl-date-time :foreground magenta)
   (diredfl-deletion :foreground red)
   (diredfl-dir-priv :foreground cyan)
   (diredfl-executable-tag :foreground green+1)
   (diredfl-exec-priv :foreground red)
   (diredfl-file-name :foreground blue)
   (diredfl-file-suffix :foreground green)
   (diredfl-flat-mark-line :foreground orange)
   (diredfl-ignore-file-name :foreground red)
   (diredfl-link-priv :foreground yellow)
   (diredfl-number :foreground green+1)
   (diredfl-other-priv :foreground yellow-1)
   (diredfl-rare-priv :foreground red-1)
   (diredfl-read-priv :foreground green-1)
   (diredfl-symlink :foreground yellow)
   (diredfl-write-priv :foreground magenta)

   ;; diff
   (diff-added          :background "#335533" :foreground green)
   (diff-changed        :background "#555511" :foreground yellow-1)
   (diff-removed        :background "#553333" :foreground red-2)
   (diff-refine-added   :background "#338833" :foreground green+4)
   (diff-refine-changed :background "#888811" :foreground yellow)
   (diff-refine-removed :background "#883333" :foreground red)

   ;; diff-hl
   (diff-hl-change :foreground blue :background blue-2)
   (diff-hl-delete :foreground red+1 :background red-1)
   (diff-hl-insert :foreground green+1 :background green-2)

   ;; Doom modeline
   (doom-modeline-bar :background yellow)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; elfeed
   (elfeed-log-info-level-face :foreground blue)
   (elfeed-search-date-face :foreground yellow-1 :underline t :weight 'bold)
   (elfeed-search-tag-face :foreground green)
   (elfeed-search-feed-face :foreground cyan)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; ediff
   (ediff-current-diff-A :foreground fg :background red-4)
   (ediff-current-diff-Ancestor :foreground fg :background red-4)
   (ediff-current-diff-B :foreground fg :background green-2)
   (ediff-current-diff-C :foreground fg :background blue-5)
   (ediff-even-diff-A :background base5)
   (ediff-even-diff-B :inherit 'ediff-even-diff-A)
   (ediff-even-diff-C :inherit 'ediff-even-diff-A)
   (ediff-fine-diff-A :foreground fg :background red-2 :weight bold)
   (ediff-fine-diff-B :inherit 'ediff-fine-diff-A)
   (ediff-fine-diff-C :inherit 'ediff-fine-diff-A)
   (ediff-odd-diff-A :background base6)
   (ediff-odd-diff-B :inherit 'ediff-odd-diff-A)
   (ediff-odd-diff-C :inherit 'ediff-odd-diff-A)

   ;; eshell
   (eshell-prompt        :foreground yellow :weight 'bold)
   (eshell-ls-archive    :foreground red-1 :weight 'bold)
   (eshell-ls-backup     :inherit 'font-lock-comment-face)
   (eshell-ls-clutter    :inherit 'font-lock-comment-face)
   (eshell-ls-directory  :foreground blue+1 :weight 'bold)
   (eshell-ls-executable :foreground red+1 :weight 'bold)
   (eshell-ls-product    :foreground green+2)
   (eshell-ls-readonly   :foreground orange)
   (eshell-ls-special    :foreground yellow :weight 'bold)
   (eshell-ls-symlink    :foreground cyan :weight 'bold)
   (eshell-ls-unreadable :foreground fg)

   ;; erc
   (erc-current-nick-face :foreground blue :weight 'bold)
   (erc-direct-msg-face :inherit 'erc-default-face)
   (erc-input-face :foreground yellow)
   (erc-keyword-face :foreground blue :weight 'bold)
   (erc-nick-default-face :foreground yellow :weight 'bold)
   (erc-my-nick-face :foreground red :weight 'bold)
   (erc-nick-msg-face :inherit 'erc-default-face)
   (erc-notice-face :foreground green)
   (erc-pal-face :foreground orange)
   (erc-prompt-face :foreground orange)
   (erc-timestamp-face :foreground green+4)

   ;; flycheck
   (flycheck-error :underline `(:style wave :color ,red-1) :weight 'bold)
   (flycheck-warning :underline `(:style wave :color ,yellow) :weight 'bold)
   (flycheck-info :underline `(:style wave :color ,cyan) :weight 'bold)

   ;; git-commit
   (git-commit-comment-action :foreground green+1 :weight 'bold)
   (git-commit-comment-branch :foreground blue+1 :weight 'bold)
   (git-commit-comment-branch-local :foreground blue+1 :weight 'bold)

   ;; git-gutter
   (git-gutter:added :foreground green :weight 'bold)
   (git-gutter:deleted :foreground red :weight 'bold)
   (git-gutter:modified :foreground magenta :weight 'bold)

   ;; gnus
   (gnus-server-opened :foreground green+2 :weight 'bold)
   (gnus-server-denied :foreground red+1 :weight 'bold)
   (gnus-server-closed :foreground blue :slant 'italic)
   (gnus-server-offline :foreground yellow :weight 'bold)
   (gnus-server-agent :foreground blue :weight 'bold)
   (gnus-summary-cancelled :foreground orange)
   (gnus-summary-high-ancient :foreground blue)
   (gnus-summary-high-read :foreground green :weight 'bold)
   (gnus-summary-high-ticked :foreground orange :weight 'bold)
   (gnus-summary-high-unread :foreground fg :weight 'bold)
   (gnus-summary-low-ancient :foreground blue)
   (gnus-summary-low-read :foreground green)
   (gnus-summary-low-ticked :foreground orange :weight 'bold)
   (gnus-summary-low-unread :foreground fg)
   (gnus-summary-normal-ancient :foreground blue)
   (gnus-summary-normal-read :foreground green)
   (gnus-summary-normal-ticked :foreground orange :weight 'bold)
   (gnus-summary-normal-unread :foreground fg)
   (gnus-summary-selected :foreground yellow :weight 'bold)
   (gnus-cite-1 :foreground blue)
   (gnus-cite-10 :foreground yellow-1)
   (gnus-cite-11 :foreground yellow)
   (gnus-cite-2 :foreground blue-1)
   (gnus-cite-3 :foreground blue-2)
   (gnus-cite-4 :foreground green+2)
   (gnus-cite-5 :foreground green+1)
   ;; (gnus-cite-6 :foreground green)
   (gnus-cite-7 :foreground red)
   (gnus-cite-8 :foreground red-1)
   (gnus-cite-9 :foreground red-2)
   (gnus-group-news-1-empty     :foreground yellow)
   (gnus-group-news-2-empty     :foreground green+3)
   (gnus-group-news-3-empty     :foreground green+1)
   (gnus-group-news-4-empty     :foreground blue-2)
   (gnus-group-news-5-empty     :foreground blue-3)
   (gnus-group-news-6-empty     :foreground base6)
   (gnus-x :background fg :foreground bg)

   ;; ivy
   (ivy-current-match :foreground yellow :weight 'bold)
   (ivy-cursor :foreground fg :background bg)
   (ivy-minibuffer-match-face-1 :foreground base5)
   (ivy-minibuffer-match-face-2 :foreground green-2)
   (ivy-minibuffer-match-face-3 :foreground green)
   (ivy-minibuffer-match-face-4 :foreground green+1)

   ;; helm
   (helm-header :foreground yellow :background base1 :weight 'bold :extend t)
   (helm-source-header :foreground yellow :background base1 :weight 'bold :extend t)
   (helm-selection :background base5)
   (helm-selection-line :background base5)
   (helm-visible-mark :foreground bg :background yellow-2)
   (helm-candidate-number :foreground green+4 :background base1)
   (helm-separator :foreground red :background bg)
   (helm-time-zone-current :foreground green+2 :background bg)
   (helm-time-zone-home :foreground red :background bg)
   (helm-bookmark-addressbook :foreground orange :background bg)
   (helm-bookmark-directory :foreground nil :background nil :inherit 'helm-ff-directory)
   (helm-bookmark-file :foreground nil :background nil :inherit 'helm-ff-file)
   (helm-bookmark-gnus :foreground magenta :background bg)
   (helm-bookmark-info :foreground green+2 :background bg)
   (helm-bookmark-man :foreground yellow :background bg)
   (helm-bookmark-w3m :foreground magenta :background bg)
   (helm-buffer-not-saved :foreground red :background bg)
   (helm-buffer-process :foreground cyan :background bg)
   (helm-buffer-saved-out :foreground fg :background bg)
   (helm-buffer-size :foreground fg-1 :background bg)
   (helm-ff-directory :foreground cyan :weight 'bold)
   (helm-ff-executable :foreground green+2 :background bg :weight 'normal)
   (helm-ff-invalid-symlink :foreground red :background bg :weight 'bold)
   (helm-ff-symlink :foreground yellow :background bg :weight 'bold)
   (helm-ff-prefix :foreground bg :background yellow :weight 'normal)
   (helm-grep-cmd-line :foreground cyan :background bg)
   (helm-grep-file :foreground fg :background bg)
   (helm-grep-finish :foreground green+2 :background bg)
   (helm-grep-lineno :foreground fg-1 :background bg)
   (helm-grep-match :foreground 'nil :background 'nil :inherit 'helm-match)
   (helm-grep-running :foreground red :background bg)
   (helm-match :foreground orange :background base1 :weight bold)
   (helm-swoop-target-line-face :foreground fg :background base6)
   (helm-swoop-target-word-face :foreground yellow :background base6 :weight bold)

   ;; hl-line
   (hl-line :background base3 :weight 'bold)

   ;; hydra
   (hydra-face-red :foreground red-1 :background bg)
   (hydra-face-amaranth :foreground red-3 :background bg)
   (hydra-face-blue :foreground blue :background bg)
   (hydra-face-pink :foreground magenta :background bg)
   (hydra-face-teal :foreground cyan :background bg)

   ;; ido
   (ido-first-match :foreground yellow :weight 'bold)
   (ido-only-match :foreground orange :weight 'bold)
   (ido-subdir :foreground yellow)
   (ido-indicator :foreground yellow :background red-4)

   ;; jabber
   (jabber-roster-user-away :foreground green+2)
   (jabber-roster-user-online :foreground blue-1)
   (jabber-roster-user-dnd :foreground red+1)
   (jabber-roster-xa :foreground cyan)
   (jabber-roster-chatty :foreground orange)
   (jabber-roster-error :foreground red+1)
   (jabber-rare-time-face :foreground blue-1)
   (jabber-chat-prompt-local :foreground blue-1)
   (jabber-chat-prompt-foreign :foreground red+1)
   (jabber-chat-prompt-system :foreground green+3)
   (jabber-activity-face :foreground red+1)
   (jabber-activity-personal-face :foreground blue+1)

   ;; js2-mode
   (js2-jsdoc-tag :foreground green-2)
   (js2-jsdoc-type :foreground green+2)
   (js2-jsdoc-value :foreground green+3)
   (js2-exernal-variable :foreground orange)
   (js2-instance-member :foreground green-2)
   (js2-jsdoc-html-tag-delimiter :foreground orange)
   (js2-jsdoc-html-tag-name :foreground red-1)
   (js2-object-property :foreground blue+1)
   (js2-magic-paren :foreground blue-5)
   (js2-private-function-call :foreground cyan)
   (js2-function-call :foreground cyan)
   (js2-private-member :foreground blue-1)
   (js2-keywords :foreground magenta)

   ;; lui
   (lui-time-stampe-face :foreground blue-1)
   (lui-hilight-face :foreground green+2 :background bg)
   (lui-button-face :inherit 'hover-highlight)

   ;; magit
   (magit-section-highlight :background base4)
   (magit-section-heading :foreground yellow :weight 'bold)
   (magit-diff-file-heading-selection :foreground base4 :weight 'bold)
   (magit-diff-added :background green-2)
   (magit-diff-removed :background red-4)
   (magit-diff-removed-highlight :background red-3)
   (magit-diff-hunk-heading :background base5)
   (magit-diff-hunk-heading-highlight :background base6)
   (magit-diff-hunk-heading-selection :background base6 :foreground orange)
   (magit-diff-lines-heading :background orange :foreground base6)
   (magit-diff-context-highlight :background base4 :foreground "grey70")
   (magit-diffstat-added :foreground green+4)
   (magit-log-date :foreground fg-1)
   (magit-log-graph :foreground base8)
   (magit-sequence-part :forground yellow)
   (magit-bisect-skip :foreground yellow)
   (magit-blame-heading :background base1 :foreground blue-2)
   (magit-blame-hash    :background base1 :foreground blue-2)
   (magit-blame-name    :background base1 :foreground orange)
   (magit-blame-date    :background base1 :foreground orange)
   (magit-blame-summary :background base1 :foreground blue-2 :weight 'bold)
   (magit-dimmed        :foreground base7)
   (magit-hash          :foreground base7)
   (magit-tag           :foreground orange)
   (magit-branch-local  :foreground blue :weight 'bold)
   (magit-refname       :background base6 :foreground fg :weight 'bold)
   (magit-signature-expired :foreground red)
   (magit-cherry-equivalent :foreground magenta)

   ;; message
   (message-cited-text :inherit 'font-lock-comment-face)
   (message-header-name :foreground green+1)
   (message-header-other :foreground green)
   (message_header-to :foreground yellow :weight 'bold)
   (message-header-cc :foreground yellow :weight 'bold)
   (message-mml :foreground yellow :weight 'bold)

   ;; mic-paren
   (paren-face-match    :foreground cyan :background bg :weight 'bold)
   (paren-face-mismatch :foreground bg :background magenta :weight 'bold)
   (paren-face-no-match :foreground bg :background red :weight 'bold)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; mu4e
   (mu4e-cited-1-face :foreground blue    :slant italic)
   (mu4e-cited-2-face :foreground green+2 :slant italic)
   (mu4e-cited-3-face :foreground blue-2  :slant italic)
   (mu4e-cited-4-face :foreground green   :slant italic)
   (mu4e-cited-5-face :foreground blue-4  :slant italic)
   (mu4e-cited-6-face :foreground green-2 :slant italic)
   (mu4e-cited-7-face :foreground blue    :slant italic)
   (mu4e-replied-face :foreground base6)
   (mu4e-trashed-face :foreground base6 :strike-through t)

   ;; neo-tree
   (neo-banner-face :foreground blue+1 :weight bold)
   (neo-header-face :foreground fg)
   (neo-root-dir-face :foreground blue+1 :weight bold)
   (neo-dir-link-face :foreground blue)
   (neo-file-link-face :foreground fg)
   (neo-expand-btn-face :foreground blue)
   (neo-vc-default-face :foreground base8)
   (neo-vc-user-face :foreground red :slant italic)
   (neo-vc-up-to-date-face :foreground fg)
   (neo-vc-edited-face :foreground magenta)
   (neo-vc-needs-merge-face :foreground red+1)
   (neo-vc-unlocked-changes-face :foreground red :background blue-5)
   (neo-vc-added-face :foreground green+1)
   (neo-vc-conflict-face :foreground red+1)
   (neo-vc-missing-face :foreground red+1)
   (neo-vc-ignored-face :foreground fg-1)

   ;; org-mode
   (org-hide :foreground bg)
   (solaire-org-hide-face :foreground hidden)
   (org-agenda-date-today :foreground base8 :slant 'italic :weight 'bold)
   (org-agenda-structure :inherit 'font-lock-comment-face)
   (org-archived :foreground fg :weight 'bold)
   (org-block :foreground base4)
   (org-checkbox :background base6 :foreground base8)
   (org-date :foreground blue :underline t)
   (org-forumla :foreground yellow-2)
   (org-headline-done :foreground green+3)
   (org-level-1 :foreground orange)
   (org-level-2 :foreground green+4)
   (org-level-3 :foreground blue-1)
   (org-level-4 :foreground yellow-2)
   (org-level-5 :foreground cyan)
   (org-level-6 :foreground green+2)
   (org-level-7 :foreground red-4)
   (org-level-8 :foreground blue-4)
   (org-scheduled :foreground green+4)
   (org-scheduled-previously :foreground red)
   (org-scheduled-today :foreground blue+1)
   (org-sexp-date :foreground blue+1 :underline t)
   (org-table :foreground green+2)
   (org-tag :weight 'bold)
   (org-time-grid :foreground orange)
   (org-todo :foreground red :weight 'bold)
   (org-upcoming-deadline :inherit font-lock-keyword-face)
   (org-ellipsis :foreground yellow-1 :underline t)
   (org-footnote :foreground cyan :underline t)
   (org-document-title :foreground blue :weight 'bold)
   (org-document-info :foreground blue)
   (org-habit-ready-face :background green)
   (org-habit-alert-face :background yellow-1 :foreground bg)
   (org-habit-clear-face :background blue-3)
   (org-habit-overdue-face :background red-3)
   (org-habit-clear-future-face :background blue-4)
   (org-habit-ready-future-face :background green-2)
   (org-habit-alert-future-face :background yellow-2 :foreground bg)
   (org-habit-overdue-future-face :background red-4)

   ;; outline
   (outline-1 :foreground orange)
   (outline-2 :foreground green+4)
   (outline-3 :foreground blue-1)
   (outline-4 :foreground yellow-2)
   (outline-5 :foreground cyan)
   (outline-6 :foreground green+2)
   (outline-7 :foreground red-4)
   (outline-8 :foreground blue-4)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground fg)
   (rainbow-delimiters-depth-2-face :foreground green+4)
   (rainbow-delimiters-depth-3-face :foreground yellow-2)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground green+2)
   (rainbow-delimiters-depth-6-face :foreground blue+1)
   (rainbow-delimiters-depth-7-face :foreground yellow-1)
   (rainbow-delimiters-depth-8-face :foreground green+1)
   (rainbow-delimiters-depth-9-face :foreground blue-2)
   (rainbow-delimiters-depth-10-face :foreground orange)
   (rainbow-delimiters-depth-11-face :foreground green)
   (rainbow-delimiters-depth-12-face :foreground blue-5)

   ;; re-builder
   (reb-match-0 :foreground bg :background magenta)
   (reb-match-1 :foreground bg :background blue)
   (reb-match-2 :foreground bg :background orange)
   (reb-match-3 :foreground bg :background red)

   ;; rpm-model
   (rpm-doc-face :foreground green)
   (rpm-ghost-face :foreground red)
   (rpm-package-face :foreground red)
   (rpm-package-section-face :foreground yellow)
   
   ;; rst-mode
   (rst-level-1-face :foreground orange)
   (rst-level-2-face :foreground green+1)
   (rst-level-3-face :foreground blue-1)
   (rst-level-4-face :foreground yellow-2)
   (rst-level-5-face :foreground cyan)
   (rst-level-6-face :foreground green-2)

   ;; solaire
   (solaire-default-face :inherit 'default :background base2)
   (solaire-minibuffer-face :inherit 'default :background base2)
   (solaire-hl-line-face :inherit 'hl-line :background bg)
   (solaire-org-hide-face :inherit 'org-hide :background bg)

   ;; speedbar
   (speedbar-button-face :foreground green+2)
   (speedbar-directory-face :foreground cyan)
   (speedbar-file-face :foreground fg)
   (speedbar-highlight-face :foreground bg :background green+2)
   (speedbar-selected-face :foreground red)
   (speedbar-separator-face :foreground bg :background blue-1)
   (speedbar-tag-face :foreground yellow)
   
   ;; swiper
   (swiper-line-face :underline t)

   ;; term
   (term-color-black :foreground bg :background base1)
   (term-color-red :foreground red-2 :background red-4)
   (term-color-green :foreground green :background green+2)
   (term-color-yellow :foreground orange :background yellow)
   (term-color-blue :foreground blue-1 :background blue-4)
   (term-color-magenta :foreground magenta :background red)
   (term-color-cyan :foreground cyan :background blue)
   (term-color-white :foreground fg :background fg-1)
   (term-default-fg-color :inherit 'term-color-white)
   (term-default-bg-color :inherit 'term-color-black)

   ;; undo-tree
   (undo-tree-visualizer-default-face :foreground fg)
   (undo-tree-visualizer-current-face :foreground red-1 :weight 'bold)
   (undo-tree-visualizer-unmodified-face :foreground cyan)
   (undo-tree-visualizer-active-branch-face :foreground base8 :weight 'bold)

   ;; volatile-highlights
   (vhl/default-face :background base3)

   ;; web-mode
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-css-pseudo-class-face :foreground green+3 :weight 'bold)
   (web-mode-css-at-rule-face :foreground orange )
   (web-mode-function-name-face :foreground blue)
   (web-mode-html-attr-value-face :inherit 'font-lock-string-face)
   (web-mode-whitespaces-face :background red)

   ;; whitespace
   (whitespace-space :background base5 :foreground base5)
   (whitespace-hspace :background base5 :foreground base5)
   (whitespace-tab :background red-1)
   (whitespace-newline :foreground base5)
   (whitespace-line :background bg :foreground magenta)
   (whitespace-indentation :background yellow :foreground red)
   (whitespace-empty :background yellow)

   ;; which-func-mode
   (which-func :foreground green+4)
   ;; which-key
   (which-key-key-face                   :foreground yellow-2)
   (which-key-group-description-face     :foreground green-3)
   (which-key-command-description-face   :foreground green+1)
   (which-key-local-map-description-face :foreground blue)
   )

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-zenburn-theme.el ends here
