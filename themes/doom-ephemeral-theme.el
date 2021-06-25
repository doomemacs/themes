;;; doom-ephemeral-theme.el --- ephemeral -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Aloysuis <aloysuis@users.noreply.github.com>
;; Created: January 27, 2020
;; Version: 2.0.0
;; Keywords: custom themes, faces
;; Homepage: https://github.com/hlissner/emacs-doom-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Inspired by https://github.com/elenapan/dotfiles
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-ephemeral-theme nil
  "Options for the `doom-ephemeral' theme."
  :group 'doom-themes)

(defcustom doom-ephemeral-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ephemeral-theme
  :type 'boolean)

(defcustom doom-ephemeral-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ephemeral-theme
  :type 'boolean)

(defcustom doom-ephemeral-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-ephemeral-theme
  :type '(choice integer boolean))

(defcustom doom-ephemeral-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-ephemeral-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-ephemeral
  "A dark theme inspired by Nord."

  ;; name        default   256       16
  ((bg          '("#323f4e" "#000000" "black"        ))
   (fg          '("#f8f8f2" "#f8f8f2" "white"        ))

   (bg-alt      '("#28323e" "#000000" "black"        ))
   (fg-alt      '("#fdfdfd" "#fdfdfd" "brightwhite"  ))

   (base0       '("#181e26" "black"   "black"        ))
   (base1       '("#1e262d" "#1e262f" "brightblack"  ))
   (base2       '("#242d39" "#242d39" "brightblack"  ))
   (base3       '("#2a3542" "#2a3542" "brightblack"  ))
   (base4       '("#323f4e" "#323f4e" "brightblack"  ))
   (base5       '("#364455" "#364455" "brightblack"  ))
   (base6       '("#505d6f" "#505d6f" "brightblack"  ))
   (base7       '("#77818f" "#77818f" "brightblack"  ))
   (base8       '("#ebedef" "#ebedef" "white"        ))

   (grey        '("#3d4c5f" "#3d4c5f" "grey"         ))
   (red         '("#f48fb1" "#f48fb1" "red"          ))
   (orange      '("#f2a272" "#f2a272" "brightred"    ))
   (green       '("#53e2ae" "#53e2ae" "green"        ))
   (teal        '("#a1efd3" "#a1efd3" "brightgreen"  ))
   (yellow      '("#f1fa8c" "#f1fa8c" "yellow"       ))
   (blue        '("#92b6f4" "#92b6f4" "brightblue"   ))
   (dark-blue   '("#9f92f4" "#9f92f4" "blue"         ))
   (magenta     '("#BD99FF" "#c574dd" "magenta"      ))
   (violet      '("#8897f4" "#8897f4" "brightmagenta"))
   (dark-violet '("#985EFF" "#8897f4" "brightmagenta"))
   (cyan        '("#79e6f3" "#87dfeb" "brightcyan"   ))
   (dark-cyan   '("#24d1e7" "#24d1e7" "cyan"         ))

   ;; ephemeral colours
   (pink        '("#c574dd" "#c574dd" "grey"         ))
   (light-pink  (doom-lighten pink 0.6))
   (dark-grey   (doom-darken grey 0.3)                )
   (light-grey  '("#56687e" "#56687e" "grey"         ))
   (alt-blue    '("#87DFEB" "#87dfeb" "brightblue"   ))

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


  ;;;; Base theme face overrides
  (((line-number &override) :foreground grey)
   ((line-number-current-line &override) :foreground blue)
   (link :foreground (doom-lighten light-grey 0.3) :inherit 'underline)
   ((font-lock-comment-face &override)
    :inherit 'bold :background (if doom-ephemeral-brighter-comments (doom-lighten bg 0.05)))
   ((font-lock-builtin-face &override) :inherit 'italic)
   ((font-lock-keyword-face &override) :inherit 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))
   ((region &override) :foreground region-fg)
   (shadow :foreground base6)
   ((tooltip &override) :background base1)

   ;;;; company
   (company-tooltip-common :foreground violet)
   (company-tooltip-selection :background base0 :foreground red)
   ;;;; company-box
   (company-box-background :background base0 :foreground fg)
   ;;;; css-mode <built-in> / scss-mode
   (css-property             :foreground fg)
   (css-proprietary-property :foreground violet)
   (css-selector             :foreground red)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-major-mode :foreground teal :weight 'bold)
   (doom-modeline-buffer-modified :foreground violet)
   (doom-modeline-buffer-path :foreground red)
   (doom-modeline-highlight :foreground (doom-lighten base2 0.3))
   (doom-modeline-info :inherit 'bold :foreground cyan)
   (doom-modeline-panel :background base0)
   (doom-modeline-project-dir :foreground teal :inherit 'bold)
   (doom-modeline-urgent :foreground modeline-fg)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; evil
   (evil-ex-lazy-highlight      :background base1  :foreground fg)
   (evil-ex-search              :background base1  :foreground fg)
   (evil-snipe-first-match-face :background base1  :foreground orange)
   ;;;; haskell-mode
   (haskell-constructor-face :inherit 'bold :foreground alt-blue)
   (haskell-definition-face :inherit 'bold :foreground functions)
   (haskell-keyword-face :inherit 'italic :foreground blue)
   (haskell-literate-comment-face :foreground doc-comments)
   (haskell-operator-face :foreground light-pink)
   (haskell-type-face :inherit 'bold :foreground violet)
   ;;;; highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)
   ;;;; ivy
   (ivy-current-match           :background base0      :distant-foreground nil)
   (ivy-posframe-cursor         :background alt-blue   :foreground base0)
   (ivy-minibuffer-match-face-2 :foreground red        :weight 'bold)
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground functions :weight 'bold)
   ;;;; magit
   (magit-diff-hunk-heading           :foreground bg :background (doom-blend magenta bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground bg :background magenta :weight 'bold :extend t)
   (magit-section-heading :foreground red)
   (magit-branch-remote   :foreground orange)
   ;;;; markdown-mode
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
   ;;;; notmuch
   (notmuch-wash-cited-text :foreground base6)
   ;;;; outline <built-in>
   (outline-1 :foreground alt-blue)
   (outline-2 :foreground violet)
   (outline-3 :foreground blue)
   (outline-4 :foreground red)
   (outline-5 :foreground pink)
   (outline-6 :foreground light-grey)
   (outline-7 :foreground yellow)
   (outline-8 :foreground cyan)
   ;;;; org <built-in>
   (org-agenda-done :foreground teal)
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :inherit 'bold :background base2 :foreground light-grey)
   (org-document-info-keyword :foreground comments)
   (org-headline-done :foreground red)
   (org-link :inherit 'underline :foreground pink)
   (org-list-dt :foreground light-grey)
   (org-todo :foreground red)
   ;;;; mic-paren
   ((paren-face-match &override) :background base3)
   ((paren-face-mismatch &override) :foreground base3)
   ;;;; rjsx-mode
   (rjsx-tag :foreground magenta)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :background base3)
   ((vimish-fold-fringe &override)  :foreground teal)))

;;; doom-ephemeral-theme.el ends here
