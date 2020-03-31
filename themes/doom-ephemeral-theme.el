;;; doom-ephemeral-theme.el --- ephemeral -*- no-byte-compile: t; -*-
;;; Commentary:
;;; A doom theme inspired by https://github.com/elenapan/dotfiles

;;; Code:
(require 'doom-themes)

;;
(defgroup doom-ephemeral-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ephemeral-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ephemeral-theme
  :type 'boolean)

(defcustom doom-ephemeral-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ephemeral-theme
  :type 'boolean)

(defcustom doom-ephemeral-comment-bg doom-ephemeral-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'doom-ephemeral-theme
  :type 'boolean)

(defcustom doom-ephemeral-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-ephemeral-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-ephemeral-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-ephemeral-theme
    :type 'symbol))

;;
(def-doom-theme doom-ephemeral
  "A dark theme inspired by Nord."

  ;; name        default   256       16
  ((bg         '("#323f4e" nil       nil            ))
   (bg-alt     '("#28323e" nil       nil            ))
   (base0      '("#181e26" "black"   "black"        ))
   (base1      '("#1e262d" "#1e262f" "brightblack"  ))
   (base2      '("#242d39" "#242d39" "brightblack"  ))
   (base3      '("#2a3542" "#2a3542" "brightblack"  ))
   (base4      '("#323f4e" "#323f4e" "brightblack"  ))
   (base5      '("#364455" "#364455" "brightblack"  ))
   (base6      '("#505d6f" "#505d6f" "brightblack"  ))
   (base7      '("#77818f" "#77818f" "brightblack"  ))
   (base8      '("#ebedef" "#ebedef" "white"        ))
   (fg         '("#f8f8f2" "#f8f8f2" "white"        ))
   (fg-alt     '("#fdfdfd" "#fdfdfd" "brightwhite"  ))

   (grey       '("#3d4c5f" "#3d4c5f" "grey"         ))
   (red        '("#f48fb1" "#f48fb1" "red"          ))
   (orange     '("#f2a272" "#f2a272" "brightred"    ))
   (green      '("#53e2ae" "#53e2ae" "green"        ))
   (teal       '("#a1efd3" "#a1efd3" "brightgreen"  ))
   (yellow     '("#f1fa8c" "#f1fa8c" "yellow"       ))
   (blue       '("#92b6f4" "#92b6f4" "brightblue"   ))
   (dark-blue  '("#9f92f4" "#9f92f4" "blue"         ))
   (magenta    '("#BD99FF" "#c574dd" "magenta"      ))
   (violet     '("#8897f4" "#8897f4" "brightmagenta"))
   (dark-violet     '("#985EFF" "#8897f4" "brightmagenta"))
   (cyan       '("#79e6f3" "#87dfeb" "brightcyan"   ))
   (dark-cyan  '("#24d1e7" "#24d1e7" "cyan"         ))

   ;; ephemeral colours
   (pink       '("#c574dd" "#c574dd" "grey"         ))
   (light-pink (doom-lighten pink 0.6))
   (dark-grey  (doom-darken grey 0.3)                )
   (light-grey '("#56687e" "#56687e" "grey"         ))
   (alt-blue   '("#87DFEB" "#87dfeb" "brightblue"   ))

   ;; face categories -- required for all themes
   (highlight      alt-blue)
   (vertical-bar   bg-alt)
   (selection      blue)
   (builtin        yellow)
   (comments       light-grey)
   (doc-comments   light-grey)
   (constants      dark-violet)
   (functions      alt-blue)
   (keywords       yellow)
   (methods        teal)
   (operators      blue)
   (type           cyan)
   (strings        red)
   (variables      light-pink)
   (numbers        green)
   (region         base1)
   (error          orange)
   (warning        yellow)
   (success        green)
   (vc-modified    teal)
   (vc-added       blue)
   (vc-deleted     orange)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-ephemeral-brighter-modeline)
   (-modeline-pad
    (when doom-ephemeral-padded-modeline
      (if (integerp doom-ephemeral-padded-modeline) doom-ephemeral-padded-modeline 4)))

   (region-fg
    (when (memq doom-ephemeral-region-highlight '(frost snowstorm))
      bg-alt))

    (modeline-fg            fg)
    (modeline-fg-alt        light-grey)
    (modeline-bg            bg)
    (modeline-bg-l          base2)
    (modeline-bg-inactive   base3)
    (modeline-bg-inactive-l `(,(car base3), (cdr base6))))


  ;; --- extra faces ------------------------
  (((region &override) :foreground region-fg)
   ((line-number &override) :foreground grey)
   ((line-number-current-line &override) :foreground blue)
   ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal)
   (shadow               :foreground base6)

   (font-lock-comment-face :inherit 'bold :foreground comments :background (if doom-ephemeral-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face :inherit 'font-lock-comment-face :foreground doc-comments)
   (font-lock-builtin-face :inherit 'italic :foreground builtin)
   (font-lock-keyword-face :inherit 'bold :foreground keywords)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   ;; modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-highlight :foreground (doom-lighten base2 0.3))
   (doom-modeline-project-dir :foreground teal :inherit 'bold)
   (doom-modeline-buffer-path :foreground red)
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-major-mode :foreground teal :weight 'bold)
   (doom-modeline-buffer-modified :foreground violet)
   (doom-modeline-panel :background base0)
   (doom-modeline-urgent :foreground modeline-fg)
   (doom-modeline-info :inherit 'bold :foreground cyan)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ediff
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))

   ;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; --- major-mode faces -------------------
   ;; NOTE: there are lots of tweaks here to mimic the VSCode theme

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground violet)
   (css-property             :foreground fg)
   (css-selector             :foreground red)

   ;; markdown-mode
   (markdown-markup-face           :foreground red)
   (markdown-link-face             :foreground teal)
   (markdown-link-title-face       :foreground alt-blue)
   (markdown-header-face           :foreground red :inherit 'bold)
   (markdown-header-delimiter-face :foreground red :inherit 'bold)
   (markdown-language-keyword-face :foreground pink :inherit 'italic)
   (markdown-markup-face           :foreground blue)
   (markdown-bold-face             :foreground blue)
   (markdown-table-face            :foreground fg :background bg)
   ((markdown-code-face &override) :foreground teal :background base1)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue :background nil)

   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :inherit 'bold :background base2 :foreground light-grey)
   (org-hide :foreground hidden :background bg-alt)
   (org-link :inherit 'underline :foreground pink)
   (org-document-info-keyword :foreground comments)
   (org-agenda-done :foreground teal)
   (org-todo :foreground red)
   (org-headline-done :foreground red)
   (solaire-org-hide-face :foreground hidden)
   (header-line :background base2 :foreground fg)
   (org-level-1 :foreground alt-blue)
   (org-level-2 :foreground violet)
   (org-level-3 :foreground blue)
   (org-level-4 :foreground red)
   (org-level-5 :foreground pink)
   (org-level-6 :foreground light-grey)
   (org-level-7 :foreground yellow)
   (org-level-8 :foreground cyan)
   (org-list-dt :foreground light-grey)

   ;; tooltip
   (tooltip              :background base1 :foreground fg)

   ;; haskell
   (haskell-type-face :inherit 'bold :foreground violet)
   (haskell-constructor-face :inherit 'bold :foreground alt-blue)
   (haskell-keyword-face :inherit 'italic :foreground blue)
   (haskell-operator-face :foreground light-pink)
   (haskell-literate-comment-face :foreground doc-comments)
   (haskell-definition-face :inherit 'bold :foreground functions)

   ;; magit
   (magit-diff-hunk-heading           :foreground bg                    :background (doom-blend magenta bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground bg                    :background magenta :weight 'bold :extend t)
   (magit-section-heading :foreground red)
   (magit-branch-remote   :foreground orange)

   ;; --- extra variables ---------------------
   ;; basics
   (link :foreground (doom-lighten light-grey 0.3) :inherit 'underline)
   (fringe :background bg-alt :foreground bg-alt)

   ;; evil
   (evil-ex-search              :background base1  :foreground fg)
   (evil-ex-lazy-highlight      :background base1  :foreground fg)
   (evil-snipe-first-match-face :background base1  :foreground orange)

   ;; ivy
   (ivy-current-match           :background base0      :distant-foreground nil)
   (ivy-posframe-cursor         :background alt-blue   :foreground base0)
   (ivy-minibuffer-match-face-2 :foreground red        :weight 'bold)

   ;; company
   (company-box-background      :background base0  :foreground fg )
   (company-tooltip-common      :foreground violet                )
   (company-tooltip-selection   :background base0  :foreground red)
  ))

;;; doom-ephemeral-theme.el ends here
