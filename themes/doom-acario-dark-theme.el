;;; doom-acario-dark-theme.el --- Acario dark theme -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;; Variables
(defgroup doom-acario-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-acario-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-acario-dark-theme
  :type 'boolean)

(defcustom doom-acario-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-acario-dark-theme
  :type 'boolean)

(defcustom doom-acario-dark-comment-bg doom-acario-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-acario-dark-theme
  :type 'boolean)

(defcustom doom-acario-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-acario-dark-theme
  :type '(or integer boolean))

;;; Theme definition
(def-doom-theme doom-acario-dark
  "A dark theme inspired by Acario"

;;;; Colors
  ;; name        default   256         16
  ((bg         '("#0D0E16" "color-233" "black"        ))
   (bg-alt     '("#040408" "color-232" "brightblack"  ))
   (base0      '("#0F1019" "color-234" "black"        ))
   (base1      '("#121212" "color-233" "brightblack"  ))
   (base2      '("#1E1E33" "color-236" "brightblack"  ))
   (base3      '("#464A56" "color-240" "brightblack"  ))
   (base4      '("#585C6C" "color-60"  "brightblack"  ))
   (base5      '("#767676" "color-243" "brightblack"  ))
   (base6      '("#959EA5" "color-109" "white"        ))
   (base7      '("#B2B2B2" "color-249" "white"        ))
   (base8      '("#D0D0D0" "color-252" "brightwhite"  ))
   (fg         '("#CEDBE5" "color-152" "brightwhite"  ))
   (fg-alt     '("#E5F4FF" "color-195" "brightwhite"  ))

   (grey       base5)

   (red        '("#D83441" "color-167" "red"          ))
   (green      '("#79D836" "color-113" "green"        ))
   (yellow     '("#D8B941" "color-179" "yellow"       ))
   (blue       '("#3679D8" "color-68"  "blue"         ))
   (magenta    '("#8041D8" "color-98"  "magenta"      ))
   (cyan       '("#36D8BD" "color-79"  "cyan"         ))

   (orange     '("#D85F00" "color-166"   "brightred"    ))
   (teal       '("#2D9574" "color-29"  "brightcyan"   ))
   (violet     '("#AB11D8" "color-128" "brightmagenta"))

   (bg-blue    '("#0C213E" "color-17"  "brightblack"  ))
   (dark-blue  bg-blue)
   (bg-cyan    '("#092D27" "color-23"   "brightblack"  ))
   (dark-cyan  bg-cyan)

;;;; face categories -- required for all themes
   (highlight      orange)
   (vertical-bar   base0)
   (selection      bg-blue)
   (builtin        blue)
   (comments       (if doom-acario-dark-brighter-comments bg-cyan grey))
   (doc-comments   (doom-lighten (if doom-acario-dark-brighter-comments bg-cyan green) 0.25))
   (constants      magenta)
   (functions      yellow)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           blue)
   (strings        green)
   (variables      (doom-lighten cyan 0.4))
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

;;;; custom categories
   (hidden bg)
   (-modeline-bright doom-acario-dark-brighter-modeline)
   (-modeline-pad
    (when doom-acario-dark-padded-modeline
      (if (integerp doom-acario-dark-padded-modeline) doom-acario-dark-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base7)

   (modeline-bg
    (if -modeline-bright
        (doom-blend blue bg-alt 0.35)
      `(,(car base2) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        modeline-bg
      `(,(car base3) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.20))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))

;;;; --- extra faces ------------------------
  (((all-the-icons-dblue &override) :foreground teal)
   (elscreen-tab-other-screen-face :background bg-blue :foreground fg-alt)

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

;;;;; hl-fill-column-face
   (hl-fill-column-face :background bg-alt :foreground fg-alt)

;;;;; line-number
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :bold bold)

;;;;; comments and doc
   (font-lock-comment-face
    :inherit 'fixed-pitch-serif
    :slant 'italic
    :foreground comments
    :background (if doom-acario-dark-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

;;;;; Flycheck
   (flycheck-popup-tip-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-info-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-warning-face :inherit 'warning)
   (flycheck-posframe-error-face :inherit 'error)

;;;;; Magit
   (magit-blame-culprit :foreground yellow)
   (magit-blame-header :foreground green)
   (magit-blame-sha1 :foreground yellow)
   (magit-blame-subject :foreground yellow)
   (magit-blame-time :foreground green)
   (magit-blame-name :foreground yellow)
   (magit-blame-heading :foreground green)
   (magit-blame-hash :foreground yellow)
   (magit-blame-summary :foreground yellow)
   (magit-blame-date :foreground green)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :foreground red)
   (magit-branch :foreground magenta :weight 'bold)
   (magit-branch-current :foreground blue :weight 'bold :box t)
   (magit-branch-local :foreground blue :weight 'bold)
   (magit-branch-remote :foreground orange :weight 'bold)
   (magit-diff-file-header :foreground yellow)
   (magit-diff-file-heading :foreground blue :weight 'light)
   (magit-diff-file-heading-highlight :foreground blue :weight 'bold)
   (magit-diff-file-heading-selection :foreground blue :weight 'bold :background base1)
   (magit-diff-hunk-heading :foreground yellow :weight 'light)
   (magit-diff-hunk-heading-highlight :foreground yellow :weight 'bold)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-added :foreground green :weight 'light)
   (magit-diff-removed :foreground red :weight 'light)
   (magit-diff-context :foreground fg :weight 'light)
   (magit-diff-added-highlight :foreground green :weight 'bold)
   (magit-diff-removed-highlight :foreground red :weight 'bold)
   (magit-diff-context-highlight :foreground fg :weight 'bold)
   (magit-diff-base :foreground fg :weight 'light)
   (magit-diff-base-highlight :foreground fg :weight 'bold)
   (magit-diff-lines-boundary :background fg :foreground base2)
   (magit-diff-lines-heading :background fg :foreground base2)
   (magit-hash :foreground yellow)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground yellow)
   (magit-log-head-label-head :background yellow :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background red :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background magenta :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background cyan :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground green)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground yellow :weight 'bold)
   (magit-section-heading :foreground red)
   (magit-section-highlight :weight 'bold)
   (section-heading-selection :foreground red :weight 'bold)
   (magit-section-title :background bg-alt :foreground red :weight 'bold)
   (magit-cherry-equivalent :foreground magenta)
   (magit-cherry-unmatched :foreground cyan)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-bisect-bad :foreground red)
   (magit-bisect-good :foreground green)
   (magit-bisect-skip :foreground fg)
   (magit-diff-conflict-heading :foreground fg)
   (magit-dimmed :foreground base8)
   (magithub-ci-no-status :foreground grey)
   (magithub-issue-number :foreground fg)
   (magithub-notification-reason :foreground fg)

;;;;; Modeline, Solaire modeline and Doom modeline
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

;;;;; whitespace
   (whitespace-indentation :inherit 'default)
   (whitespace-big-indent :inherit 'default)

;;;;; ivy-mode
   (ivy-current-match :background bg-blue :distant-foreground base0 :weight 'normal)
   (ivy-posframe :background base1 :foreground fg)
   (internal-border :background base7)

;;;;; lsp-mode and lsp-ui-mode
   (lsp-ui-peek-highlight :foreground yellow)
   (lsp-ui-sideline-symbol-info :foreground (doom-blend comments bg 0.85)
                                :background bg-alt)

;;;; --- major-mode faces -------------------
;;;;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

;;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

;;;;; org-mode
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg :foreground comments :slant 'italic)
   ((org-quote &override) :background base1)

   (org-hide :foreground hidden))


  ;;;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-acario-dark-theme.el ends here
