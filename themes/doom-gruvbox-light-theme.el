;;; doom-gruvbox-light-theme.el --- gruvbox light soft -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-gruvbox-light-theme nil
  "Options for doom-themes"
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

(defcustom doom-gruvbox-light-variant "soft"
  "If non-nil, choice of hard, medium or soft can be use to change the
variant. Defaults to soft."
  :group 'doom-gruvbox-light-theme
  :type  'string)

;;
(def-doom-theme doom-gruvbox-light
  "gruvbox light theme"

  ;; name        default   256       16
  ((bg
    (cond ((equal doom-gruvbox-light-variant "hard") '("#f9f5d7" "#ffffd7" nil))
          ((equal doom-gruvbox-light-variant "medium") '("#fbf1c7" "#ffffd7" nil))
          (t '("#f2e5bc" "#ffffd7" nil))))
   (bg-alt
    (cond ((equal doom-gruvbox-light-variant "hard")    '("#fbf1c7" "#ffffd7" nil))
          ((equal doom-gruvbox-light-variant "medium")  '("#f2e5bc" "#ffffd7" nil))
          (t '("#ebdbb2" "#ffffaf" nil))))
   (base0      '("#f0f0f0" "#f0f0f0" "white"        ))
   (base1      '("#ebdbb2" "#e7e7e7" "brightblack"  ))
   (base2      '("#d5c4a1" "#dfdfdf" "brightblack"  ))
   (base3      '("#bdae93" "#c6c7c7" "brightblack"  ))
   (base4      '("#a89984" "#9ca0a4" "brightblack"  ))
   (base5      '("#504945" "#424242" "brightblack"  ))
   (base6      '("#3c3836" "#2e2e2e" "brightblack"  ))
   (base7      '("#282828" "#1e1e1e" "brightblack"  ))
   (base8      '("#1d2021" "black"   "black"        ))
   (fg         '("#383a42" "#424242" "black"        ))
   (fg-alt     '("#000000" "#c7c7c7" "brightblack"  ))

   (grey       '("#928374" "#8a8a8a"))
   (red        '("#9d0006" "#e45649" "red"          ))
   (orange     '("#af3a03" "#dd8844" "brightred"    ))
   (green      '("#79740e" "#50a14f" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#b57614" "#986801" "yellow"       ))
   (blue       '("#076678" "#4078f2" "brightblue"   ))
   (dark-blue  '("#2b3c44" "#a0bcf8" "blue"         ))
   (magenta    '("#b16286" "#a626a4" "magenta"      ))
   (violet     '("#8f3f71" "#b751b6" "brightmagenta"))
   (cyan       '("#427b58" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#36473a" "#005478" "cyan"         ))

   ;; Extra
   (faded-orange '("#d65d0e"))
   (faded-yellow '("#d79921"))
   (faded-blue   '("#458588" "#87afaf"))
   (faded-cyan   '("#689d6a" "#87af87"))
   (faded-red    '("#cc241d" "#d75f5f"))
   (faded-green  '("#98971a" "#afaf00"))
   (light3       '("#665c54" "#626262"))
   (light4       '("#7c6f64" "#767676"))
   (lightblue4   '("#66999D" "#5fafaf"))
   (turquoise4   '("#61ACBB" "#5fafaf"))

   ;; face categories -- required for all themes
   (highlight      "black")
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
   (numbers        orange)
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

   (modeline-fg     nil)
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

  ;; --- extra faces ------------------------
  ((centaur-tabs-unselected :background bg-alt :foreground base4)
   (font-lock-comment-face
    :foreground comments
    :background (if doom-gruvbox-light-comment-bg base0))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments
    :slant 'italic)

   ((line-number &override) :foreground base4 :background base1)
   ((line-number-current-line &override) :foreground orange :background (doom-lighten bg 0.1))
   (linum     :foreground base4 :background base2)
   (linum-highlight-face :foreground orange :background base2)
   (linum-relative-current-face :foreground orange :background base2)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (lazy-highlight       :background base2  :foreground base8 :distant-foreground base0 :weight 'bold)

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

   ;; swiper
   (swiper-line-face    :background base3 :foreground base0)
   (swiper-match-face-1 :inherit 'unspecified :background base1   :foreground base5)
   (swiper-match-face-2 :inherit 'unspecified :background orange  :foreground base0 :weight 'bold)
   (swiper-match-face-3 :inherit 'unspecified :background violet :foreground base1 :weight 'bold)
   (swiper-match-face-4 :inherit 'unspecified :background green   :foreground base2 :weight 'bold)
   (swiper-background-match-face-1 :inherit 'unspecified :background base2)
   (swiper-background-match-face-2 :inherit 'unspecified :background base3)
   (swiper-background-match-face-3 :inherit 'unspecified :background base4)
   (swiper-background-match-face-4 :inherit 'unspecified :background base5)

   ;; magit
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

   ;; ivy
   (ivy-current-match               :foreground base8 :weight 'bold :underline t)
   (ivy-minibuffer-match-face-1     :foreground orange)
   (ivy-minibuffer-match-face-2     :foreground yellow)
   (ivy-minibuffer-match-face-3     :foreground faded-orange)
   (ivy-minibuffer-match-face-4     :foreground faded-yellow)

   ;; markdown-model
   (markdown-header-face-1          :foreground blue)
   (markdown-header-face-2          :foreground yellow)
   (markdown-header-face-3          :foreground violet)
   (markdown-header-face-4          :foreground red)
   (markdown-header-face-5          :foreground green)
   (markdown-header-face-6          :foreground dark-cyan)
   
   ;; major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)

   ;; org-mode
   ((outline-1 &override) :foreground red)
   ((outline-2 &override) :foreground orange)

   (org-level-1                 :foreground blue)
   (org-level-2                 :foreground yellow)
   (org-level-3                 :foreground violet)
   (org-level-4                 :foreground red)
   (org-level-5                 :foreground green)
   (org-level-6                 :foreground cyan)
   (org-level-7                 :foreground faded-blue)
   (org-level-8                 :foreground orange)
   (org-drawer                  :inherit 'font-lock-function-name-face)
   ;; (org-column   :background )
   (org-warning                 :foreground red :weight 'bold :bold t)
   (org-archived                :foreground base7 :weight 'bold)
   (org-link                    :foreground faded-cyan :underline t)
   (org-footnote                :foreground cyan :underline t)
   (org-ellipsis                :foreground light4)
   (org-date                    :foreground blue :underline t)
   (org-sexp-date               :foreground faded-blue :underline t)
   (org-tag                     :bold t :weight 'bold)
   (org-todo                    :foreground red :weight 'bold :bold t)
   (org-done                    :foreground cyan :weight 'bold :bold t)
   (org-agenda-done             :foreground cyan)
   (org-headline-done           :foreground cyan)
   (org-table                   :foreground blue)
   (org-block                   :background bg)
   (org-block-begin-line        :foreground base3)
   (org-block-end-line          :foreground base3)
   (org-formula                 :foreground yellow)
   (org-document-title          :foreground faded-blue)
   (org-document-info           :foreground faded-blue)
   (org-agenda-structure        :inherit 'font-lock-comment-face)
   (org-agenda-date-today       :foreground base7 :weight 'bold :italic t)
   (org-scheduled               :foreground yellow)
   (org-scheduled-today         :foreground blue)
   (org-scheduled-previously    :foreground faded-red)
   (org-upcoming-deadline       :inherit 'font-lock-keyword-face)
   (org-deadline-announce       :foreground faded-red)
   (org-time-grid               :foreground faded-orange)
   (org-latex-and-related       :foreground blue)

   ;; company-mode
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

   ;; helm
   (helm-candidate-number :background blue :foreground bg)

   ;; mu4e
   (mu4e-highlight-face :foreground green)
   (mu4e-unread-face :foreground blue :weight 'bold)
   (mu4e-header-key-face :foreground green :weight 'bold)
   
   ;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)

   ;; wgrep
   (wgrep-face :background base1)

   ;; ediff
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))

   ;; tooltip
   (tooltip :background base1 :foreground base6)

   ;; posframe
   (ivy-posframe               :background base0)

   ;; lsp
   (lsp-ui-doc-background      :background base2)
   (lsp-face-highlight-read    :background (doom-blend bg orange 0.5))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)

   ;; js2
   (js2-warning                    :underline `(:style wave :color ,yellow))
   (js2-error                      :underline `(:style wave :color ,red))
   (js2-external-variable          :underline `(:style wave :color ,cyan))
   (js2-jsdoc-tag                  :background nil :foreground grey  )
   (js2-jsdoc-type                 :background nil :foreground light4)
   (js2-jsdoc-value                :background nil :foreground light3)
   (js2-function-param             :background nil :foreground cyan)
   (js2-function-call              :background nil :foreground blue)
   (js2-instance-member            :background nil :foreground orange)
   (js2-private-member             :background nil :foreground yellow)
   (js2-private-function-call      :background nil :foreground faded-cyan)
   (js2-jsdoc-html-tag-name        :background nil :foreground light4)
   (js2-jsdoc-html-tag-delimiter   :background nil :foreground light3)

   ;; whitespace-mode
   (whitespace-space               :background bg :foreground base4)
   (whitespace-hspace              :background bg :foreground base4)
   (whitespace-tab                 :background bg :foreground base4)
   (whitespace-newline             :background bg :foreground base4)
   (whitespace-trailing            :background base1 :foreground red)
   (whitespace-line                :background base1 :foreground red)
   (whitespace-space-before-tab    :background bg :foreground base4)
   (whitespace-indentation         :background bg :foreground base4)
   (whitespace-empty               :background nil :foreground nil)
   (whitespace-space-after-tab     :background bg :foreground base4)

   ;; Diffs
   (diff-changed                   :background nil :foreground base6)
   (diff-added                     :background nil :foreground green)
   (diff-removed                   :background nil :foreground red)
   (diff-indicator-changed         :inherit 'diff-changed)
   (diff-indicator-added           :inherit 'diff-added)
   (diff-indicator-removed         :inherit 'diff-removed)
   )

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-gruvbox-light-theme.el ends here
