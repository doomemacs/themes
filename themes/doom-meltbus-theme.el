;;; doom-meltbus-theme.el --- a dark (mostly) monochromatic theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: November 20, 2021 (#683)
;; Author: spacefrogg <https://github.com/spacefrogg>
;; Maintainer:
;; Source: https://github.com/equinusocio/vsc-material-theme
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-meltbus-theme nil
  "Options for the `meltbus' theme"
  :group 'doom-themes)

(defcustom doom-meltbus-hl-line t
  "If non-nil, highlight the current line with an underline."
  :group 'doom-meltbus-theme
  :type 'boolean)

(defcustom doom-meltbus-uniform-font-size nil
  "If non-nil, all faces use the basic font size."
  :group 'doom-meltbus-theme
  :type 'boolean)


;;
;;; Theme definition

(def-doom-theme doom-meltbus
  "A dark mostly monochromatic theme inspired by the eltbus theme.

Colours are only used to distinguish states and priorities, and to
highlight interactive elements."

  ;;     default   256       16
  ((bg '("black"    "black"  "black"))
   (fg '("#dddddd" "#dddddd" "white"))

   (bg-alt '("black"   "black"   "black"))
   (fg-alt '("#dddddd" "#dddddd" "white"))

   (base0 '("black"   "black"   "black"))
   (base1 '("#111111" "#111111" "brightblack"))
   (base2 '("#242424" "#222222" "brightblack"))
   (base3 '("#464646" "#444444" "brightblack"))
   (base4 '("#686868" "#666666" "brightblack"))
   (base5 '("#8a8a8a" "#888888" "brightblack"))
   (base6 '("#acacac" "#aaaaaa" "brightblack"))
   (base7 '("#cecece" "#cccccc" "brightblack"))
   (base8 '("#efefef" "#eeeeee" "white"))

   (grey base4)
   (red       '("#f8b0b0" "#ffaaaa" "red"))
   (dark-red  '("#b22222" "#bb2222" "red"))
   (orange    '("#da8548" "#dd8844" "brightred"))
   (green     '("#448844" "#448844" "green"))
   (teal      '("#c0f860" "#aaffaa" "brightgreen"))
   (yellow    '("#cdad00" "#ccaa00" "yellow"))
   (blue      '("#87afff" "#88aaff" "brightblue"))
   (dark-blue '("#7070aa" "#6666aa" "blue"))
   (magenta   '("#db7093" "#ee6688" "brightmagenta"))
   (violet    '("#a9a1e1" "#a9a1e1" "magenta"))
   (cyan      '("#46D9FF" "#46D9FF" "brightcyan"))
   (dark-cyan '("#5699AF" "#5699AF" "cyan"))
   (white     '("#ffffff" "#ffffff" "white"))

   ;; "universal syntax classes"; *mandatory*
   (highlight blue)
   (vertical-bar base1)
   (selection base3)
   (builtin base5)
   (comments base4)
   (doc-comments base5)
   (constants base5)
   (functions fg)
   (keywords fg)
   (methods fg)
   (operators base5)
   (type base6)
   (strings base6)
   (variables fg)
   (numbers fg)
   (region base2)
   (error       red)
   (warning     orange)
   (success     green)
   (vc-modified orange)
   (vc-added    green)
   (vc-deleted  red)
   (vc-conflict magenta)

   ;; theme-local variables
   (almost-invisible base3))

  ;; Base theme face overrides
  (((cursor &override) :background base7)
   (region :inverse-video t)
   (hl-line :underline doom-meltbus-hl-line)
   ((link &override) :weight 'normal :underline nil :foreground highlight)
   (link-visited :inherit 'link)
   (minibuffer-prompt :foreground base5 :background base2 :weight 'bold)
   (mode-line-emphasis :foreground fg :weight 'bold :distant-foreground bg)
   (mode-line-highlight :foreground fg :weight 'bold :distant-foreground bg)
   (mode-line-inactive :foreground base5)

   ;;;; compilation <built-in>
   (compilation-line-number :foreground fg :weight 'bold)
   ;;;; custom <built-in>
   ((custom-button &override) :foreground highlight)
   ((custom-button-unraised &override) :foreground comments)
   ((custom-button-pressed-unraised &override) :background comments)
   ((custom-button-pressed &override) :background highlight)
   ((custom-button-mouse &override) :background highlight)
   ((custom-variable-button &override) :foreground fg)
   (custom-saved :inherit 'custom-state)
   (custom-modified :inherit 'custom-changed)
   (custom-variable-tag :foreground fg)
   ((custom-visibility &override) :foreground fg)
   (custom-group-tag :foreground base8 :weight 'bold)
   (custom-group-tag-1 :foreground fg)
   (custom-group-subtitle :foreground fg)
   ((custom-invalid &override) :foreground error)
   (custom-state :foreground green)
   (custom-changed :foreground orange)
   ;;;; diff <built-in>
   (diff-added :foreground vc-added)
   (diff-changed :foreground vc-modified)
   (diff-header :foreground fg :weight 'bold)
   (diff-hunk-header :foreground base7 :background base3)
   (diff-file-header :foreground fg :background base3 :weight 'bold)
   (diff-refine-added :foreground bg :background vc-added :distant-foreground fg)
   (diff-refine-changed :foreground bg :background vc-modified :distant-foreground fg)
   (diff-refine-removed :foreground bg :background vc-deleted :distant-foreground fg)
   (diff-removed :foreground vc-deleted)
   ;;;; dired <built-in>
   (dired-header :foreground doc-comments :weight 'bold)
   (dired-mark :foreground builtin)
   (dired-marked :foreground orange :weight 'bold)
   (dired-symlink :foreground doc-comments :weight 'bold)
   ;;;; diredfl
   (diredfl-read-priv :foreground fg)
   (diredfl-symlink :foreground cyan :weight 'bold)
   ;;;; doom-modeline
   (doom-modeline-bar :background fg :foreground bg)
   (doom-modeline-bar-inactive :background base4 :foreground bg)
   (doom-modeline-info :foreground fg :weight 'bold)
   (doom-modeline-lsp-running :inherit 'doom-modeline-info)
   (doom-modeline-evil-insert-state :foreground green)
   (doom-modeline-evil-emacs-state :foreground cyan)
   (doom-modeline-evil-normal-state :foreground base5)
   (doom-modeline-evil-visual-state :foreground white)
   (doom-modeline-evil-operator-state :inherit 'doom-modeline-evil-visual-state)
   ;;;; evil
   ((evil-ex-substitute-replacement &override) :foreground cyan)
   ;;;; evil-snipe
   ((evil-snipe-first-match-face &override) :background bg)
   ;;;; flycheck
   (flycheck-error :underline `(:style wave :color ,error))
   (flycheck-info :underline `(:style wave :color ,success))
   (flycheck-warning :underline `(:style wave :color ,warning))
   ;;;; flymake
   (flymake-error :underline `(:style wave :color ,error))
   (flymake-note :underline `(:style wave :color ,success))
   (flymake-warning :underline `(:style wave :color ,warning))
   ;;;; flx-ido
   ((flx-highlight-face &override) :foreground highlight)
   ;;;; git-commit
   ((git-commit-keyword &override) :foreground keywords)
   (git-commit-comment-branch-local :inherit 'magit-branch-local)
   (git-commit-comment-branch-remote :inherit 'magit-branch-remote)
   (git-commit-comment-detached :foreground warning)
   (git-commit-comment-file :foreground doc-comments)
   ;;;; magit
   ;; TODO reflog colours
   (magit-blame-hash :foreground fg)
   (magit-blame-date :foreground base6)
   (magit-blame-heading :inherit 'magit-log-author :background base3 :extend t)
   (magit-branch-current :foreground fg :weight 'bold :underline t)
   (magit-branch-local :foreground bg :background fg :weight 'bold :distant-foreground fg)
   (magit-branch-remote :foreground fg :weight 'bold)
   (magit-diff-added :inherit 'diff-added)
   (magit-diff-added-highlight :inherit 'magit-diff-context-highlight :foreground vc-added)
   (magit-diff-base :foreground (doom-darken orange 0.2))
   (magit-diff-base-highlight :foreground orange)
   (magit-diff-context-highlight :foreground base7 :background base1)
   (magit-diff-hunk-heading :inherit 'diff-hunk-header)
   (magit-diff-hunk-heading-highlight :inherit 'diff-file-header)
   (magit-diff-removed :inherit 'diff-removed)
   (magit-diff-removed-highlight :inherit 'magit-diff-context-highlight :foreground vc-deleted)
   (magit-diff-whitespace-warning :foreground bg :background vc-deleted)
   (magit-diffstat-added :inherit 'diff-added)
   (magit-diffstat-removed :inherit 'diff-removed)
   (magit-header-line :background base2 :foreground base6 :weight 'bold :box `(:line-width 3 :color ,base2))
   (magit-filename :foreground fg)
   (magit-item-highlight :weight 'bold)
   (magit-log-author :foreground fg)
   (magit-log-date :foreground fg)
   (magit-section-heading :weight 'bold :extend t)
   (magit-section-highlight :background base1)
   (magit-tag :foreground vc-added)
   ;;;; marginalia
   ;; TODO (uses many colours)
   ;;;; markdown <modes:markdown-mode,gfm-mode>
   (markdown-header-face :inherit 'bold :foreground fg)
   (markdown-metadata-key-face :foreground builtin)
   (markdown-list-face :foreground builtin)
   (markdown-link-face :foreground fg)
   (markdown-url-face :inherit 'link)
   (markdown-italic-face :inherit 'italic)
   (markdown-bold-face :inherit 'bold)
   (markdown-blockquote-face :inherit 'org-quote)
   (markdown-code-face :inherit 'org-code)
   ;;;; message <built-in>
   (message-cited-text-1  :foreground base7 :background base2)
   (message-cited-text-2 :foreground base5 :background base2)
   (message-cited-text-3 :inherit 'message-cited-text-1)
   (message-cited-text-4 :inherit 'message-cited-text-2)
   (message-header-cc :foreground fg :background bg :weight 'bold)
   (message-header-name :foreground base5)
   (message-header-newsgroups :inherit 'message-cited-text)
   (message-header-other :foreground almost-invisible)
   (message-header-subject :inherit 'message-cited-text)
   (message-header-to :inherit 'message-header-cc)
   (message-header-xheader :inherit 'message-header-other)
   (message-header-mml :inherit 'message-header-other)
   (message-header-separator :foreground highlight)
   ;;;; neotree
   (neo-vc-added-face :inherit 'treemacs-git-added-face)
   (neo-vc-conflict-face :inherit 'treemacs-git-conflict-face)
   (neo-vc-modified-face :inherit 'treemacs-git-modified-face)
   (doom-neotree-data-file-face :foreground comments)
   ;;;; notmuch
   (notmuch-crypto-signature-bad :foreground error)
   (notmuch-crypto-signature-good :foreground almost-invisible)
   (notmuch-crypto-signature-unknown :foreground warning)
   (notmuch-crypto-signature-good-key :foreground error)
   (notmuch-crypto-decryption :foreground error)
   (notmuch-message-summary-face :background base2)
   (notmuch-search-matching-authros :foreground highlight)
   (notmuch-tag-face :inherit 'message-header-separator)
   (notmuch-tree-match-tag-face :inherit 'notmuch-tag-face)
   (notmuch-tree-match-author-face :inherit 'notmuch-tree-match-tag-face)
   ;;;; lsp-ui
   (lsp-ui-peek-header :foreground base8 :background base3 :bold bold)
   (lsp-ui-peek-highlight :inherit 'lsp-ui-peek-header :background base4 :foreground fg)
   (lsp-ui-peek-list :background base2)
   (lsp-ui-peek-peek :background bg)
   (lsp-ui-peek-line-number :foreground fg)
   (lsp-ui-peek-selection :foreground bg :background highlight :bold bold)
   (lsp-ui-sideline-code-action :foreground base8)
   (lsp-ui-sideline-current-symbol :inherit 'lsp-ui-sideline-code-action)
   ;;;; org <built-in> <modes:org-mode>
   (org-code :foreground doc-comments)
   (org-date :inherit 'link)
   (org-date-selected :inherit 'lazy-highlight)
   (org-document-title :height (if doom-meltbus-uniform-font-size 1.0 1.7)
                       :foreground (if doom-meltbus-uniform-font-size base8 doc-comments)
                       :weight 'bold :slant 'normal)
   (org-done :foreground success :weight 'bold)
   (org-done-keyword-face :foreground success)
   (org-drawer :foreground comments)
   (org-block :foreground fg :background nil :weight 'normal :slant 'normal :underline nil :inverse-video nil)
   (org-block-begin-line :foreground base5 :weight 'bold)
   (org-block-end-line :inherit 'org-block-begin-line)
   (org-footnote :foreground doc-comments)
   (org-formula :foreground doc-comments)
   (org-list-dt :foreground base8 :weight 'bold)
   ;; org-level-N inherits from outline-N
   (org-property-value :foreground fg)
   (org-table :foreground fg)
   (org-todo :foreground error :weight 'bold)
   (org-todo-keyword-face :foreground error)
   (org-verbatim :inherit 'org-code)
   ;;;; org-agenda <built-in>
   (org-agenda-clocking :background base3)
   ; (org-agenda-date :foreground fg)
   ; (org-agenda-date-today :inherit 'default :bold bold :underline t)
   ; (org-agenda-date-weekend :inherit 'default :foreground (doom-darken (face-foreground 'org-agenda-date) 0.3))
   (org-scheduled-previously :foreground base6 :bold bold)
   ;;;; org-journal <modes:org-journal-mode>
   (org-journal-calendar-entry-face :foreground fg :slant 'italic :underline t)
   (org-journal-calendar-scheduled-face :foreground bg :background fg :slant 'italic )
   ;;;; org-ref
   (org-ref-acronym-face :foreground red)
   (org-ref-cite-face :foreground green)
   (org-ref-glossary-face :foreground dark-red)
   (org-ref-label-face :foreground blue)
   (org-ref-ref-face :foreground teal)
   ;;;; outline
   (outline-1 :height (if doom-meltbus-uniform-font-size 1.0 1.5)
              :background base1 :weight 'bold)
   (outline-2 :height (if doom-meltbus-uniform-font-size 1.0 1.4)
              :foreground (if doom-meltbus-uniform-font-size (doom-darken fg 0.2))
              :background base1 :weight 'bold)
   (outline-3 :height (if doom-meltbus-uniform-font-size 1.0 1.3)
              :foreground (if doom-meltbus-uniform-font-size (doom-darken fg 0.3))
              :background base1 :weight 'bold)
   (outline-4 :height (if doom-meltbus-uniform-font-size 1.0 1.2)
              :foreground (if doom-meltbus-uniform-font-size (doom-darken fg 0.4))
              :background base1 :weight 'bold)
   (outline-5 :height (if doom-meltbus-uniform-font-size 1.0 1.1)
              :background base1
              :weight (if doom-meltbus-uniform-font-size 'normal 'bold))
   (outline-6 :height (if doom-meltbus-uniform-font-size 1.0 1.0)
              :foreground (if doom-meltbus-uniform-font-size (doom-darken fg 0.2))
              :background base1
              :weight (if doom-meltbus-uniform-font-size 'normal 'bold))
   (outline-7 :height (if doom-meltbus-uniform-font-size 1.0 1.0)
              :foreground (if doom-meltbus-uniform-font-size (doom-darken fg 0.3))
              :background base1 :weight 'normal)
   (outline-8 :height (if doom-meltbus-uniform-font-size 1.0 1.0)
              :foreground (if doom-meltbus-uniform-font-size (doom-darken fg 0.4))
              :background base1 :foreground base5 :weight 'normal)
   ;;;; pkgbuild-mode <modes:pkgbuild-mode>
   (pkgbuild-error-face :underline `(:style wave :color ,error))
   ;;;; popup
   (popup-tip-face :inherit 'popup-face :foreground fg :background bg :bold bold :underline t)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground green)
   (rainbow-delimiters-depth-2-face :foreground dark-red)
   (rainbow-delimiters-depth-3-face :foreground red)
   (rainbow-delimiters-depth-4-face :inherit 'rainbow-delimiters-depth-1-face)
   (rainbow-delimiters-depth-5-face :inherit 'rainbow-delimiters-depth-2-face)
   (rainbow-delimiters-depth-6-face :inherit 'rainbow-delimiters-depth-3-face)
   (rainbow-delimiters-depth-7-face :inherit 'rainbow-delimiters-depth-1-face)
   (rainbow-delimiters-depth-8-face :inherit 'rainbow-delimiters-depth-2-face)
   (rainbow-delimiters-depth-9-face :inherit 'rainbow-delimiters-depth-3-face)
   (rainbow-delimiters-base-face :inherit 'default)
   ;;;; smerge-tool
   (smerge-markers :inherit 'magit-diff-conflict-heading)
   (smerge-lower :inherit 'diff-added)
   (smerge-upper :inherit 'diff-removed)
   (smerge-base :inherit 'diff-changed)
   (smerge-refined-added :inherit 'diff-refine-added)
   (smerge-refined-removed :inherit 'diff-refine-removed)
   ;;;; treemacs
   (treemacs-git-conflict-face :foreground vc-conflict)
   (treemacs-git-modified-face :foreground vc-modified)
   ;;;; vterm
   (vterm-color-black :inherit 'term-color-black)
   (vterm-color-red :inherit 'term-color-red)
   (vterm-color-blue :inherit 'term-color-blue)
   (vterm-color-green :inherit 'term-color-green)
   (vterm-color-yellow :inherit 'term-color-yellow)
   (vterm-color-magenta :inherit 'term-color-magenta)
   (vterm-color-cyan :inherit 'term-color-cyan)
   (vterm-color-white :inherit 'term-color-white)
   ;;;; which-key
   (which-key-key-face :foreground base5)
   (which-key-group-description-face :foreground base5)
   (which-key-command-description-face :foreground fg :weight 'bold)
   (which-key-local-map-description-face :foreground fg))

  ;; Base theme variable overrides
  ())

;;; doom-meltbus-theme.el ends here
