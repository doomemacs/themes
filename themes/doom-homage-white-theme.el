;;; doom-homage-white-theme.el --- minimal white theme inspired by editors from 2000s -*- no-byte-compile: t; -*-
;;;
;;; Commentary:
;;;
;;; Theme is using palette inspired by various editors from 2000s, with a lot of
;;; inspiration from eziam theme (thblt/eziam-theme-emacs) and tao themes
;;; (11111000000/tao-theme-emacs).

(require 'doom-themes)

;;
(defgroup doom-homage-white-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-homage-white-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-homage-white-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-homage-white
  "A light theme inspired by Atom One"

  ;; name        default   256       16
  ((bg         '("#fafafa" nil       nil            ))
   (bg-alt     '("#f0f0f0" nil       nil            ))
   (base0      '("#f0f0f0" "#f0f0f0" "white"        ))
   (base1      '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#9ca0a4" "#9ca0a4" "brightblack"  ))
   (base5      '("#383a42" "#424242" "brightblack"  ))
   (base6      '("#202328" "#2e2e2e" "brightblack"  ))
   (base7      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base8      '("#1b2229" "black"   "black"        ))
   (fg         '("#383a42" "#424242" "black"        ))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"  ))

   (grey       base8)
   (red        '("#e45649" "#e45649" "red"          ))
   (orange     '("#8a3b3c" "#dd8844" "brightred"    ))
   (green      '("#556b2f" "#556b2f" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#986801" "#986801" "yellow"       ))
   (yellow-alt '("#fafadd" "#fafadd" "yellow"       ))
   (blue       '("#014980" "#014980" "brightblue"   ))
   (dark-blue  '("#030f64" "#030f64" "blue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#b751b6" "#b751b6" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      base3)
   (builtin        fg)
   (comments       green)
   (doc-comments   (doom-darken comments 0.15))
   (constants      fg)
   (functions      blue)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        orange)
   (variables      fg)
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright t)
   (-modeline-pad
    (when doom-homage-white-padded-modeline
      (if (integerp doom-homage-white-padded-modeline) doom-homage-white-padded-modeline 4)))

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
    :foreground comments)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments
    :slant 'italic)

   ((secondary-selection &override) :background grey :foreground bg :extend t)

   ((line-number &override) :foreground (doom-lighten base4 0.15))
   ((line-number-current-line &override) :foreground base8)

   ;; Apply bold value for different things
   (font-lock-builtin-face       :inherit 'bold)
   (font-lock-function-name-face :inherit 'bold)
   (font-lock-keyword-face       :inherit 'bold)
   (font-lock-type-face          :inherit 'bold)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

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

   ;; magit
   ((magit-diff-hunk-heading           &override) :foreground base4 :background bg :bold bold)
   ((magit-diff-hunk-heading-highlight &override) :foreground fg    :background bg :bold bold)
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   ;; --- major-mode faces -------------------
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
   ((outline-1 &override) :foreground fg :inherit 'bold)
   ((outline-2 &override) :foreground fg :inherit 'bold)
   ((outline-3 &override) :foreground fg :inherit 'bold)
   ((outline-4 &override) :foreground fg :inherit 'bold)
   ((outline-5 &override) :foreground fg :inherit 'bold)
   ((outline-6 &override) :foreground fg :inherit 'bold)
   ((outline-7 &override) :foreground fg :inherit 'bold)
   ((outline-8 &override) :foreground fg :inherit 'bold)

   ((org-todo &override)  :foreground red   :bold 'inherit)
   ((org-done &override)  :foreground green :bold 'inherit)
   ((org-tag &override)   :foreground fg :background yellow-alt
    :box '(:line-width -1 :color "#333333" :style 'released-button))
   ((org-date &override)  :foreground fg :background base1
    :box '(:line-width -1 :color "#333333" :style 'released-button))
   ((org-special-keyword &override)  :foreground base5)
   ((org-drawer          &override)  :foreground base5)

   ((org-document-title &override) :height 2.0)
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   (org-ellipsis :underline nil :box nil :background bg-alt :foreground fg :inherit 'bold)
   ((org-quote &override) :background base1)
   ((org-table &override) :foreground fg)

   ;; Deadlines
   ((org-upcoming-deadline &override)         :foreground fg)
   ((org-upcoming-distant-deadline &override) :foreground base5)

   ;; Scheduled things
   ((org-scheduled &override)            :foreground fg)
   ((org-scheduled-today &override)      :foreground fg)
   ((org-scheduled-previously &override) :foreground base5)

   ;; Indent guides character face
   (highlight-indent-guides-character-face :foreground base2)

   ;; helm
   (helm-candidate-number :background blue :foreground bg)

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
   (tooltip :background base1 :foreground fg)

   ;; posframe
   (ivy-posframe               :background base0)

   ;; lsp
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)
   )

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-homage-white-theme.el ends here
