;;; doom-acario-light-theme.el --- Acario light theme
(require 'doom-themes)

(defgroup doom-acario-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-acario-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-acario-light-theme
  :type 'boolean)

(defcustom doom-acario-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-acario-light-theme
  :type 'boolean)

(defcustom doom-acario-light-comment-bg doom-acario-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-acario-light-theme
  :type 'boolean)

(defcustom doom-acario-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-acario-light-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-acario-light
  "A light theme inspired by Acario light"

  ;; name        default   256         16
  ((bg         '("#EAF1FB" "color-255" "black"        ))
   (bg-alt     '("#EFFAFF" "color-253" nil            ))
   (base0      '("#DDDDDD" "color-253" "black"        ))
   (base1      '("#D0D0D0" "color-252" "brightblack"  ))
   (base2      '("#C0CCD0" "color-152" "brightblack"  ))
   (base3      '("#9EA6B0" "color-249" "brightblack"  ))
   (base4      '("#585C6C" "color-246" "brightblack"  ))
   (base5      '("#4E4E4E" "color-243" "brightblack"  ))
   (base6      '("#3A3A3A" "color-240" "white"        ))
   (base7      '("#303030" "color-237" "white"        ))
   (base8      '("#1E1E33" "color-234" "brightwhite"  ))
   (fg         '("#0F1019" "color-233" "brightwhite"  ))
   (fg-alt     '("#0D0E16" "color-232" "brightwhite"  ))

   (grey       base5)

   (red        '("#D70000" "color-160" "red"          ))
   (green      '("#005F00" "color-22"  "green"        ))
   (yellow     '("#AF8700" "color-136" "yellow"       ))
   (blue       '("#1F55A0" "color-20"  "blue"         ))
   (magenta    '("#AF005F" "color-125" "magenta"      ))
   (cyan       '("#007687" "color-24"  "cyan"         ))

   (orange     '("#D75F00" "color-166" "brightred"    ))
   (teal       '("#00876C" "color-29"  "brightgreen"  ))
   (violet     '("#8700AF" "color-91"  "brightmagenta"))

   (bg-blue    '("#B9D1F1" "#B9D1F1"   "blue"         ))
   (dark-blue  bg-blue)
   (bg-cyan    '("#D5FAFF" "#D5FAFF"   "cyan"         ))
   (dark-cyan  bg-cyan)

   ;; face categories -- required for all themes
   (highlight      teal)
   (vertical-bar   base0)
   (selection      bg-blue)
   (builtin        blue)
   (comments       (if doom-acario-light-brighter-comments bg-cyan grey))
   (doc-comments   (doom-darken (if doom-acario-light-brighter-comments bg-cyan green) 0.25))
   (constants      magenta)
   (functions      yellow)
   (keywords       red)
   (methods        cyan)
   (operators      blue)
   (type           blue)
   (strings        green)
   (variables      (doom-darken cyan 0.4))
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-dark doom-acario-light-brighter-modeline)
   (-modeline-bright -modeline-dark)
   (-modeline-pad
    (when doom-acario-light-padded-modeline
      (if (integerp doom-acario-light-padded-modeline) doom-acario-light-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-dark
        (doom-lighten blue 0.475)
      `(,(car base2) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-dark
        modeline-bg
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.20))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background bg-blue :foreground fg-alt)

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ;;; hl-fill-column-face
   (hl-fill-column-face :background bg-alt :foreground fg-alt)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange)

   (font-lock-comment-face
    :inherit 'fixed-pitch-serif-face
    :slant 'italic
    :foreground comments
    :background (if doom-acario-light-comment-bg (doom-darken bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   ;; Flycheck
   (flycheck-popup-tip-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-info-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-warning-face :inherit 'warning)
   (flycheck-posframe-error-face :inherit 'error)

   ;; Magit
   (magit-blame-culprit :foreground cyan)
   (magit-blame-header :foreground green)
   (magit-blame-sha1 :foreground cyan)
   (magit-blame-subject :foreground cyan)
   (magit-blame-time :foreground green)
   (magit-blame-name :foreground cyan)
   (magit-blame-heading :foreground green)
   (magit-blame-hash :foreground cyan)
   (magit-blame-summary :foreground cyan)
   (magit-blame-date :foreground green)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-other :foreground yellow)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground yellow)
   (magit-reflog-reset :foreground red)
   (magit-branch :foreground magenta :weight 'bold)
   (magit-branch-current :background bg :foreground blue :weight 'bold :box t)
   (magit-branch-local :background bg :foreground blue :weight 'bold)
   (magit-branch-remote :background bg :foreground orange :weight 'bold)
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
   (magit-hash :foreground cyan)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground cyan)
   (magit-log-head-label-head :background cyan :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background red :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background magenta :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background yellow :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground green)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground cyan :weight 'bold)
   (magit-section-heading :foreground red)
   (magit-section-highlight :weight 'bold)
   (section-heading-selection :foreground red :weight 'bold)
   (magit-section-title :background bg-alt :foreground red :weight 'bold)
   (magit-cherry-equivalent :foreground magenta)
   (magit-cherry-unmatched :foreground orange)
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

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-dark base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; whitespace
   (whitespace-indentation :inherit 'default)
   (whitespace-big-indent :inherit 'default)

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-dark modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background bg-blue :distant-foreground base0 :weight 'normal)
   (ivy-posframe :background base1 :foreground fg)
   (internal-border :background base7)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-block :background bg-alt)
   (org-block-begin-line :background bg :slant 'italic :fg comments)
   (org-quote :background base1)

   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-acario-light-theme.el ends here

