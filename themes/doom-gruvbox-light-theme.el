;;; doom-gruvbox-light-theme.el --- inspired by morhetz's Gruvbox (light) -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 6, 2020 (#446)
;; Author: jsoa <https://github.com/jsoa>
;; Maintainer:
;; Source: https://github.com/morhetz/gruvbox
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-gruvbox-light-theme nil
  "Options for the `doom-gruvbox-light' theme."
  :group 'doom-themes)

(defcustom doom-gruvbox-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-gruvbox-light-theme
  :type 'boolean)

(defcustom doom-gruvbox-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gruvbox-light-theme
  :type 'boolean)

(defcustom doom-gruvbox-light-comment-bg doom-gruvbox-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-gruvbox-light-theme
  :type 'boolean)

(defcustom doom-gruvbox-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gruvbox-light-theme
  :type '(choice integer boolean))

(defcustom doom-gruvbox-light-variant nil
  "A choice of \"hard\" or \"soft\" can be used to change the
background contrast. All other values default to \"medium\"."
  :group 'doom-gruvbox-light-theme
  :type  'string)


;;
;;; Theme definition

(def-doom-theme doom-gruvbox-light
  "gruvbox light theme"

  ;; name        default   256       16
  ((bg
    (cond ((equal doom-gruvbox-light-variant "hard") '("#f9f5d7" "#ffffd7" nil))   ; bg0_h
          ((equal doom-gruvbox-light-variant "soft") '("#f2e5bc" "#ffffd7" nil))   ; bg0_s
          (t                                         '("#fbf1c7" "#ffffd7" nil)))) ; bg0
   (bg-alt
    (cond ((equal doom-gruvbox-light-variant "hard") '("#fbf1c7" "#ffffd7" nil))
          ((equal doom-gruvbox-light-variant "soft") '("#ebdbb2" "#ffffaf" nil))
          (t                                         '("#f2e5bc" "#ffffd7" nil))))
   (base0      '("#f0f0f0" "#f0f0f0" "white"           )) ;;
   (base1      '("#ebdbb2" "#ffffaf" "brightblack"     )) ;; gruvbox-dark1
   (base2      '("#d5c4a1" "#d7d6af" "brightblack"     )) ;; gruvbox-dark2
   (base3      '("#bdae93" "#afaf87" "brightblack"     )) ;; gruvbox-dark3
   (base4      '("#a89984" "#afafaf" "brightblack"     )) ;; gruvbox-dark4
   (base5      '("#504945" "#4e4e4e" "brightblack"     )) ;; gruvbox-light2
   (base6      '("#3c3836" "#3a3a3a" "brightblack"     )) ;; gruvbox-light1
   (base7      '("#282828" "#262626" "brightblack"     )) ;; gruvbox-light0
   (base8      '("#1d2021" "#1c1c1c"  "black"          )) ;; gruvbox-light0_hard
   (fg         '("#282828" "#262626" "black"           )) ;; gruvbox-light0
   (fg-alt     '("#1c1c1c" "#1c1c1c" "brightblack"     )) ;;


   (grey       '("#928374" "#8a8a8a" "grey"            )) ;; gruvbox-gray
   (red        '("#9d0006" "#870000" "red"             )) ;; gruvbox-bright_red
   (orange     '("#af3a03" "#af5f00" "brightred"       )) ;; gruvbox-bright_orange
   (green      '("#79740e" "#878700" "green"           )) ;; gruvbox-bright_green
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"     )) ;; gruvbox-
   (yellow     '("#b57614" "#af8700" "yellow"          )) ;; gruvbox-bright_yellow
   (blue       '("#076678" "#005f87" "brightblue"      )) ;; gruvbox-bright_blue
   (dark-blue  '("#2b3c44" "#000087" "blue"            )) ;; gruvbox-dark_blue
   (magenta    '("#b16286" "#d75f87" "magenta"         )) ;; gruvbox-faded_purple
   (violet     '("#8f3f71" "#875f87" "brightmagenta"   )) ;; gruvbox-bright_purple
   (cyan       '("#427b58" "#5f8787" "brightcyan"      )) ;; gruvbox-bright_aqua
   (dark-cyan  '("#36473a" "#005f5f" "cyan"            )) ;; gruvbox-dark_aqua

   ;; Extra
   (delimiter-3       '("#8ec07c" "#87af87"                 ))
   (light3            '("#665c54" "#626262" "grey"          ))
   (light4            '("#7c6f64" "#767676" "grey"          ))
   (faded-red         '("#cc241d" "#d75f5f" "red"           ))
   (faded-green       '("#98971a" "#afaf00" "green"         ))
   (faded-yellow      '("#d79921" "#ffaf00" "yellow"        ))
   (faded-blue        '("#458588" "#87afaf" "blue"          ))
   (faded-orange      '("#d65d0e" "#ff8700" "brightorange"  ))
   (faded-aqua        '("#689d6a" "#87af87" "brightcyan"    ))
   (dark-red          '("#421E1E" "#5f0000"                 ))
   (dark-blue         '("#2B3C44" "#000087"                 ))
   (dark-aqua         '("#36473A" "#005f5f"                 ))
   (sienna            '("#dd6f48" "d7875f"                  ))
   (lightblue4        '("#66999D" "#5fafaf" "brightblue"    ))
   (burlywood4        '("#BBAA97" "#aafaf87"                ))
   (aquamarine4       '("#83af98" "#87af87"                 ))
   (turquoise4        '("#61ACBB" "#5fafaf" "brightblue"    ))

   ;; face categories -- required for all themes
   (highlight      base4)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      base3)
   (builtin        orange)
   (comments       (if doom-gruvbox-light-brighter-comments base5 base4))
   (doc-comments   green)
   (constants      violet)
   (functions      yellow)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           violet)
   (strings        green)
   (variables      blue)
   (numbers        violet)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-gruvbox-light-brighter-modeline)
   (-modeline-pad
    (when doom-gruvbox-light-padded-modeline
      (if (integerp doom-gruvbox-light-padded-modeline) doom-gruvbox-light-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

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
  ((cursor :background base4)
   ((font-lock-comment-face &override) :background (if doom-gruvbox-light-comment-bg base0 'unspecified))
   ((font-lock-doc-face &override) :slant 'italic)
   (isearch           :foreground "black" :background orange)
   (isearch-fail      :foreground fg :background red)
   (lazy-highlight    :background base2  :foreground base8 :distant-foreground base0 :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   (tooltip :background base1 :foreground base6)

   ;;;; anzu
   (anzu-mode-line         :foreground yellow :weight 'bold)
   (anzu-match-1           :background green)
   (anzu-match-2           :background faded-yellow)
   (anzu-match-3           :background aquamarine4)
   (anzu-replace-to        :foreground yellow)
   (anzu-replace-highlight :inherit 'isearch)
   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; company
   (company-scrollbar-bg                      :background base1)
   (company-scrollbar-fg                      :background bg-alt)
   (company-tooltip                           :background bg-alt)
   (company-tooltip-annotation                :foreground green)
   (company-tooltip-annotation-selection      :inherit 'company-tooltip-annotation)
   (company-tooltip-selection                 :foreground violet :background base2)
   (company-tooltip-common                    :foreground blue :underline t)
   (company-tooltip-common-selection          :foreground blue :underline t)
   (company-preview-common                    :foreground base7)
   (company-preview                           :background lightblue4)
   (company-preview-search                    :background turquoise4)
   (company-template-field                    :foreground "black" :background yellow)
   (company-echo-common                       :foreground faded-red)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; diredfl
   (diredfl-autofile-name :foreground base5)
   (diredfl-compressed-file-name :foreground base5)
   (diredfl-compressed-file-suffix :foreground faded-blue)
   (diredfl-dir-priv :foreground blue :backgtround dark-blue)
   (diredfl-exec-priv :foreground blue :backgrond dark-blue)
   (diredfl-file-name :foreground base5)
   (diredfl-file-suffix :foreground light4)
   (diredfl-link-priv :foreground magenta)
   (diredfl-no-priv :foreground base5)
   (diredfl-number :foreground yellow)
   (diredfl-other-priv :foreground violet)
   (diredfl-rare-priv :foreground base5)
   ;;;; diredp
   (diredp-file-name :foreground base5)
   (diredp-file-suffix :foreground light4)
   (diredp-compressed-file-suffix :foreground faded-blue)
   (diredp-dir-name :foreground faded-blue)
   (diredp-symlink :foreground orange)
   (diredp-date-time :foreground light3)
   (diredp-number :foreground faded-blue)
   (diredp-no-priv :foreground base4)
   (diredp-other-priv :foreground base2)
   (diredp-rare-priv :foreground base4)
   (diredp-ignored-file-name :foreground base5)
   (diredp-dir-priv :foreground faded-blue :background dark-blue)
   ((diredp-dir-exec-priv &inherit diredp-dir-priv))
   (diredp-link-priv :foreground faded-aqua)
   ;;;; doom-emacs
   (doom-dashboard-banner      :foreground (doom-darken base4 0.3))
   (doom-dashboard-menu-title  :foreground green)
   (doom-dashboard-menu-desc   :foreground green)
   (doom-dashboard-footer-icon :foreground (doom-darken yellow 0.4))
   (doom-dashboard-loaded      :foreground yellow)
   ;;;; diff-mode
   (diff-changed                   :foreground base6)
   (diff-removed                   :foreground red)
   (diff-indicator-changed         :inherit 'diff-changed)
   (diff-indicator-added           :inherit 'diff-added)
   (diff-indicator-removed         :inherit 'diff-removed)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; elfeed
   (elfeed-search-title-face :foreground grey)
   (elfeed-search-date-face :inherit 'font-lock-builtin-face :underline t)
   (elfeed-search-tag-face :inherit 'font-lock-keyword-face)
   (elfeed-search-unread-count-face :inherit 'font-lock-comment-face)
   (elfeed-search-filter-face :inherit 'font-lock-string-face)
   ;;;; flycheck
   (flycheck-info :underline `(:style wave :color ,blue))
   ;;;; git-gutter
   (git-gutter:modified :foreground faded-blue :background blue)
   (git-gutter:added    :foreground faded-green :background green)
   (git-gutter:deleted  :foreground faded-red :backgrond red)
   ;;;; git-gutter+
   (git-gutter+-modified :foreground faded-blue :background blue)
   (git-gutter+-added    :foreground faded-green :background green)
   (git-gutter+-deleted  :foreground faded-red :backgrond red)
   ;;;; helm
   (helm-candidate-number       :background blue :foreground bg)
   (helm-M-x-key                :foreground orange)
   (helm-action                 :foreground base8 :underline t)
   (helm-bookmark-addressbook   :foreground red)
   (helm-bookmark-directory     :foreground violet)
   (helm-bookmark-file          :foreground faded-blue)
   (helm-bookmark-gnus          :foreground magenta)
   (helm-bookmark-info          :foreground turquoise4)
   (helm-bookmark-man           :foreground sienna)
   (helm-bookmark-w3m           :foreground yellow)
   (helm-buffer-directory       :foreground "white" :background blue)
   (helm-buffer-not-saved       :foreground red)
   (helm-buffer-process         :foreground burlywood4)
   (helm-buffer-saved-out       :foreground red)
   (helm-buffer-size            :foreground violet)
   (helm-candidate-number       :foreground green)
   (helm-ff-directory           :foreground violet)
   (helm-ff-executable          :foreground turquoise4)
   (helm-ff-file                :foreground sienna)
   (helm-ff-invalid-symlink     :foreground "white" :background red)
   (helm-ff-prefix              :foreground "black" :background yellow)
   (helm-ff-symlink             :foreground orange)
   (helm-grep-cmd-line          :foreground green)
   (helm-grep-file              :foreground magenta)
   (helm-grep-finish            :foreground turquoise4)
   (helm-grep-lineno            :foreground orange)
   (helm-grep-match             :foreground yellow)
   (helm-grep-running           :foreground red)
   (helm-header                 :foreground aquamarine4)
   (helm-helper                 :foreground aquamarine4)
   (helm-history-deleted        :foreground "black" :background red)
   (helm-history-remote         :foreground faded-red)
   (helm-lisp-completion-info   :foreground faded-orange)
   (helm-lisp-show-completion   :foreground red)
   (helm-locate-finish          :foreground "white" :background aquamarine4)
   (helm-match                  :foreground orange)
   (helm-moccur-buffer          :foreground cyan :underline t)
   (helm-prefarg                :foreground turquoise4)
   (helm-selection              :foreground "white" :background base2)
   (helm-selection-line         :foreground "white" :background base2)
   (helm-separator              :foreground faded-red)
   (helm-source-header          :foreground base5)
   (helm-visible-mark           :foreground "black" :background light3)
   ;;;; ivy
   (ivy-minibuffer-match-face-1     :foreground orange)
   (ivy-minibuffer-match-face-2     :foreground yellow)
   (ivy-minibuffer-match-face-3     :foreground faded-orange)
   (ivy-minibuffer-match-face-4     :foreground faded-yellow)
   ;;;; ivy-posframe
   (ivy-posframe               :background bg-alt)
   ;;;; js2-mode
   (js2-warning                    :underline `(:style wave :color ,yellow))
   (js2-error                      :underline `(:style wave :color ,red))
   (js2-external-variable          :underline `(:style wave :color ,cyan))
   (js2-jsdoc-tag                  :foreground grey)
   (js2-jsdoc-type                 :foreground light4)
   (js2-jsdoc-value                :foreground light3)
   (js2-function-param             :foreground cyan)
   (js2-function-call              :foreground blue)
   (js2-instance-member            :foreground orange)
   (js2-private-member             :foreground yellow)
   (js2-private-function-call      :foreground faded-aqua)
   (js2-jsdoc-html-tag-name        :foreground light4)
   (js2-jsdoc-html-tag-delimiter   :foreground light3)
   ;;;; lsp-mode
   (lsp-face-highlight-textual     :background (doom-blend bg orange 0.9) :foreground base0 :distant-foreground base8)
   ;;;; lsp-ui
   (lsp-ui-doc-background          :background base2)
   ;;;; magit
   (magit-bisect-bad                      :foreground faded-red)
   (magit-bisect-good                     :foreground faded-green)
   (magit-bisect-skip                     :foreground faded-yellow)
   (magit-blame-heading                   :foreground base7 :background base2)
   (magit-branch-local                    :foreground blue)
   (magit-branch-current                  :underline blue :inherit 'magit-branch-local)
   (magit-branch-remote                   :foreground green)
   (magit-cherry-equivalent               :foreground violet)
   (magit-cherry-unmatched                :foreground cyan)
   (magit-diff-added                      :foreground green)
   (magit-diff-added-highlight            :foreground green :inherit 'magit-diff-context-highlight)
   (magit-diff-base                       :background faded-yellow :foreground base5)
   (magit-diff-base-highlight             :background faded-yellow :foreground base7)
   (magit-diff-context                    :foreground base1  :foreground base6)
   (magit-diff-context-highlight          :background base1 :foreground base7)
   (magit-diff-hunk-heading               :background base3 :foreground base5)
   (magit-diff-hunk-heading-highlight     :background base2 :foreground base7)
   (magit-diff-hunk-heading-selection     :background base2 :foreground orange)
   (magit-diff-lines-heading              :background faded-orange :foreground base7)
   (magit-diff-removed                    :foreground red)
   (magit-diff-removed-highlight          :foreground red :inherit 'magit-diff-context-highlight)
   (magit-diffstat-added                  :foreground faded-green)
   (magit-diffstat-removed                :foreground faded-red)
   (magit-dimmed                          :foreground base4)
   (magit-hash                            :foreground blue)
   (magit-log-author                      :foreground red)
   (magit-log-date                        :foreground cyan)
   (magit-log-graph                       :foreground base4)
   (magit-process-ng                      :foreground red :weight 'bold)
   (magit-process-ok                      :foreground green :weight 'bold)
   (magit-reflog-amend                    :foreground violet)
   (magit-reflog-checkout                 :foreground blue)
   (magit-reflog-cherry-pick              :foreground green)
   (magit-reflog-commit                   :foreground green)
   (magit-reflog-merge                    :foreground green)
   (magit-reflog-other                    :foreground cyan)
   (magit-reflog-rebase                   :foreground violet)
   (magit-reflog-remote                   :foreground blue)
   (magit-reflog-reset                    :foreground red)
   (magit-refname                         :foreground light4)
   (magit-section-heading                 :foreground yellow :weight 'bold)
   (magit-section-heading-selection       :foreground faded-yellow)
   (magit-section-highlight               :background base1)
   (magit-sequence-drop                   :foreground faded-yellow)
   (magit-sequence-head                   :foreground cyan)
   (magit-sequence-part                   :foreground yellow)
   (magit-sequence-stop                   :foreground green)
   (magit-signature-bad                   :foreground red :weight 'bold)
   (magit-signature-error                 :foreground red)
   (magit-signature-expired               :foreground orange)
   (magit-signature-good                  :foreground green)
   (magit-signature-revoked               :foreground violet)
   (magit-signature-untrusted             :foreground blue)
   (magit-tag                             :foreground yellow)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)
   (markdown-header-face-1          :foreground blue)
   (markdown-header-face-2          :foreground yellow)
   (markdown-header-face-3          :foreground violet)
   (markdown-header-face-4          :foreground red)
   (markdown-header-face-5          :foreground green)
   (markdown-header-face-6          :foreground dark-cyan)
   ;;;; message <built-in>
   (message-header-cc :inherit 'font-lock-variable-name-face)
   (message-header-subject :foreground orange :weight 'bold)
   (message-header-other :inherit 'font-lock-variable-name-face)
   (message-header-name :inherit 'font-lock-keyword-face)
   (message-cited-text :inherit 'font-lock-comment-face)
   (message-mml :foregrond faded-green :weight 'bold)
   ;;;; mu4e
   (mu4e-highlight-face :foreground green)
   (mu4e-unread-face :foreground blue :weight 'bold)
   (mu4e-header-key-face :foreground green :weight 'bold)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground red)
   ((outline-2 &override) :foreground orange)
   ;;;; org <built-in>
   (org-agenda-date-today       :foreground base7 :weight 'bold :italic t)
   (org-agenda-done             :foreground cyan)
   (org-agenda-structure        :inherit 'font-lock-comment-face)
   (org-archived                :foreground base7 :weight 'bold)
   (org-block                   :background base1 :extend t)
   (org-block-begin-line        :background base2 :extend t)
   (org-block-end-line          :background base2 :extend t)
   (org-date                    :foreground blue :underline t)
   (org-deadline-announce       :foreground faded-red)
   (org-document-info           :foreground faded-blue)
   (org-document-title          :foreground faded-blue)
   (org-done                    :foreground cyan :weight 'bold :bold t)
   (org-drawer                  :inherit 'font-lock-function-name-face)
   (org-ellipsis                :foreground light4)
   (org-footnote                :foreground cyan :underline t)
   (org-formula                 :foreground yellow)
   (org-headline-done           :foreground cyan)
   (org-latex-and-related       :foreground blue)
   (org-level-1                 :foreground blue)
   (org-level-2                 :foreground yellow)
   (org-level-3                 :foreground violet)
   (org-level-4                 :foreground red)
   (org-level-5                 :foreground green)
   (org-level-6                 :foreground cyan)
   (org-level-7                 :foreground faded-blue)
   (org-level-8                 :foreground orange)
   (org-link                    :foreground faded-aqua :underline t)
   (org-scheduled               :foreground yellow)
   (org-scheduled-previously    :foreground faded-red)
   (org-scheduled-today         :foreground blue)
   (org-sexp-date               :foreground faded-blue :underline t)
   (org-table                   :foreground blue)
   (org-tag                     :bold t :weight 'bold)
   (org-time-grid               :foreground faded-orange)
   (org-todo                    :foreground red :weight 'bold :bold t)
   (org-upcoming-deadline       :inherit 'font-lock-keyword-face)
   (org-warning                 :foreground red :weight 'bold :bold t)
   ;;;; org-habit
   (org-habit-clear-face          :background faded-blue)
   (org-habit-clear-future-face   :background blue)
   (org-habit-ready-face          :background faded-green)
   (org-habit-ready-future-face   :background green)
   (org-habit-alert-face          :background faded-yellow)
   (org-habit-alert-future-face   :background yellow)
   (org-habit-overdue-face        :background faded-red)
   (org-habit-overdue-future-face :background red)
   ;;;; popup
   (popup-face :foreground base6  :background base1)
   (popup-menu-selection-face :foreground fg :background faded-green)
   (popup-menu-mouse-face :foreground fg :background faded-green)
   (popup-tip-face :foreground base5 :background base2)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-3-face :foreground delimiter-3)
   (rainbow-delimiters-depth-4-face :foreground faded-orange)
   (rainbow-delimiters-depth-7-face :foreground delimiter-3)
   (rainbox-delimiters-depth-8-face :foreground faded-orange)
   (rainbow-delimiters-depth-11-face :foreground delimiter-3)
   (rainbox-delimiters-depth-12-face :foreground faded-orange)
   (rainbow-delimiters-unmatched-face: :foreground fg)
   ;;;; swiper
   (swiper-line-face    :background base3 :foreground base0)
   (swiper-match-face-1 :inherit 'unspecified :background base1  :foreground base5)
   (swiper-match-face-2 :inherit 'unspecified :background orange :foreground base0 :weight 'bold)
   (swiper-match-face-3 :inherit 'unspecified :background violet :foreground base1 :weight 'bold)
   (swiper-match-face-4 :inherit 'unspecified :background green  :foreground base2 :weight 'bold)
   (swiper-background-match-face-1 :inherit 'unspecified :background base2)
   (swiper-background-match-face-2 :inherit 'unspecified :background base3)
   (swiper-background-match-face-3 :inherit 'unspecified :background base4)
   (swiper-background-match-face-4 :inherit 'unspecified :background base5)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; vterm
   (vterm-color-black   :background grey    :foreground base1)
   (vterm-color-red     :background red     :foreground faded-red)
   (vterm-color-green   :background green   :foreground faded-green)
   (vterm-color-yellow  :background yellow  :foreground faded-yellow)
   (vterm-color-blue    :background blue    :foreground faded-blue)
   (vterm-color-magenta :background violet  :foreground magenta)
   (vterm-color-cyan    :background cyan    :foreground faded-aqua)
   (vterm-color-white   :background base7   :foreground light4)
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; whitespace <built-in>
   (whitespace-trailing :foreground red :background base1)
   (whitespace-line :foreground red :background base1)
   (whitespace-indentation :foreground base4 :background bg)
   (whitespace-empty))

  ;;;; Base theme variable overrides-
  ())

;;; doom-gruvbox-light-theme.el ends here
