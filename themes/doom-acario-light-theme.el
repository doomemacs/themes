;;; doom-acario-light-theme.el --- an original light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: August 12, 2019 (#319)
;; Author: gagbo <https://github.com/gagbo>
;; Maintainer: gagbo <https://github.com/gagbo>
;; Source: original
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-acario-light-theme nil
  "Options for the `doom-acario-light' theme."
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
;;; Theme definition

(def-doom-theme doom-acario-light
  "A light theme inspired by Acario light"
  :family 'doom-acario
  :background-mode 'light

;;;; Colors
  ;; name        default   256         16
  ((bg         '("#F5F5F9" "color-255" "black"        ))
   (bg-alt     '("#E9E9F2" "color-254" "brightblack"  ))
   (base0      '("#D0D0E3" "color-188" "black"        ))
   (base1      '("#D0D0E3" "color-188" "brightblack"  ))
   (base2      '("#C0CCD0" "color-152" "brightblack"  ))
   (base3      '("#9EA6B0" "color-103" "brightblack"  ))
   (base4      '("#585C6C" "color-60"  "brightblack"  ))
   (base5      '("#4E4E4E" "color-239" "brightblack"  ))
   (base6      '("#3A3A3A" "color-237" "white"        ))
   (base7      '("#303030" "color-236" "white"        ))
   (base8      '("#1E1E33" "color-236" "brightwhite"  ))
   (fg         '("#0F1019" "color-234" "brightwhite"  ))
   (fg-alt     '("#0D0E16" "color-233" "brightwhite"  ))

   (grey       base5)

   (red        '("#D70000" "color-160" "red"          ))
   (green      '("#005F00" "color-22"  "green"        ))
   (yellow     '("#AF8700" "color-136" "yellow"       ))
   (blue       '("#1F55A0" "color-25"  "blue"         ))
   (magenta    '("#AF005F" "color-125" "magenta"      ))
   (cyan       '("#007687" "color-30"  "cyan"         ))

   (orange     '("#D75F00" "color-166" "brightred"    ))
   (teal       '("#009B7C" "color-36"  "brightgreen"  ))
   (violet     '("#8700AF" "color-91"  "brightmagenta"))

   (bg-blue    '("#DEEAF8" "color-189"   "blue"         ))
   (dark-blue  bg-blue)
   (bg-cyan    '("#D5FAFF" "color-195"   "cyan"         ))
   (dark-cyan  bg-cyan)

;;;; face categories -- required for all themes
   (highlight      teal)
   (vertical-bar   base0)
   (selection      bg-blue)
   (builtin        blue)
   (comments       (if doom-acario-light-brighter-comments cyan grey))
   (doc-comments   (doom-darken (if doom-acario-light-brighter-comments cyan green) 0.25))
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
   (hidden bg)
   (-modeline-dark doom-acario-light-brighter-modeline)
   (-modeline-bright -modeline-dark)
   (-modeline-pad
    (when doom-acario-light-padded-modeline
      (if (integerp doom-acario-light-padded-modeline) doom-acario-light-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-dark
        (doom-blend blue bg 0.35)
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-dark
        (doom-blend blue bg-alt 0.35)
      `(,(car base2) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0)))
   (modeline-bg-inactive-l (doom-darken bg 0.20)))

  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :slant 'italic
    :background (if doom-acario-light-comment-bg (doom-darken bg 0.05) 'unspecified))
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-dark base8 highlight))

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background bg-blue :foreground fg-alt)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-dark modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; doom-themes
   (doom-themes-treemacs-file-face :foreground comments)
   ;;;; comments and doc
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; Flycheck
   (flycheck-popup-tip-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-info-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-warning-face :inherit 'warning)
   (flycheck-posframe-error-face :inherit 'error)
   ;;;; Magit
   (magit-bisect-skip :foreground fg)
   (magit-blame-date :foreground green)
   (magit-blame-header :foreground green)
   (magit-blame-heading :foreground green)
   (magit-blame-name :foreground cyan)
   (magit-blame-sha1 :foreground cyan)
   (magit-blame-subject :foreground cyan)
   (magit-blame-summary :foreground cyan)
   (magit-blame-time :foreground green)
   (magit-branch :foreground magenta :weight 'bold)
   ((magit-branch-current &override) :weight 'bold :box t)
   (magit-branch-local :foreground blue :weight 'bold)
   (magit-branch-remote :foreground orange :weight 'bold)
   (magit-cherry-equivalent :foreground magenta)
   (magit-cherry-unmatched :foreground orange)
   (magit-diff-added :foreground green :weight 'light)
   (magit-diff-added-highlight :foreground green :weight 'bold)
   (magit-diff-base :foreground fg :weight 'light)
   (magit-diff-base-highlight :foreground fg :weight 'bold)
   (magit-diff-conflict-heading :foreground fg)
   (magit-diff-context :foreground fg :weight 'light)
   (magit-diff-context-highlight :foreground fg :weight 'bold)
   (magit-diff-file-header :foreground yellow)
   (magit-diff-file-heading :foreground blue :weight 'light)
   (magit-diff-file-heading-highlight :foreground blue :weight 'bold)
   (magit-diff-file-heading-selection :foreground blue :weight 'bold :background base1)
   (magit-diff-hunk-heading :foreground yellow :weight 'light)
   (magit-diff-hunk-heading-highlight :foreground yellow :weight 'bold)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-lines-boundary :background fg :foreground base2)
   (magit-diff-lines-heading :background fg :foreground base2)
   (magit-diff-removed :foreground red :weight 'light)
   (magit-diff-removed-highlight :foreground red :weight 'bold)
   (magit-dimmed :foreground base8)
   (magit-hash :foreground cyan)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground cyan)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-log-head-label-head :background cyan :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background red :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background magenta :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background yellow :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground green)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground cyan :weight 'bold)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-reflog-other :foreground yellow)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground yellow)
   (magit-reflog-reset :foreground red)
   (magit-section-heading :foreground red)
   (magit-section-highlight :weight 'bold)
   (magit-section-title :background bg-alt :foreground red :weight 'bold)
   (magit-section-heading-selection :foreground red :weight 'bold)
   ;;;; magithub
   (magithub-ci-no-status :foreground grey)
   (magithub-issue-number :foreground fg)
   (magithub-notification-reason :foreground fg)
   ;;;; hl-fill-column-face
   (hl-fill-column-face :background bg-alt :foreground fg-alt)
   ;;;; ivy
   (ivy-current-match :background bg-blue :distant-foreground base0 :weight 'normal)
   (ivy-posframe :background base1 :foreground fg)
   (internal-border :background base7)
   ;;;; lsp-mode and lsp-ui-mode
   (lsp-ui-peek-highlight :foreground yellow)
   (lsp-ui-sideline-symbol-info :foreground (doom-blend comments bg 0.85)
                                :background bg-alt)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg :slant 'italic)
   ((org-quote &override) :background base1)
   (org-hide :foreground hidden)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; treemacs
   (treemacs-root-face :foreground strings :weight 'bold :height 1.2)
   ;;;; whitespace <built-in>
   (whitespace-indentation :inherit 'default)
   (whitespace-big-indent :inherit 'default))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-acario-light-theme.el ends here
