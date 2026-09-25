;;; doom-miramare-theme.el --- a port of Franbach's Miramare theme; a variant of Grubox -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: July 11, 2020 (#492)
;; Author: sagittaros <https://github.com/sagittaros>
;; Maintainer:
;; Source: https://github.com/franbach/miramare
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)


;;
;;; Variables

(defgroup doom-miramare-theme nil
  "Options for doom-miramare."
  :group 'doom-themes)

(defcustom doom-miramare-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-miramare-theme
  :type 'boolean)

(defcustom doom-miramare-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-miramare-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-miramare
  "A gruvbox variant with comfortable and pleasant colors."

  ;; name        gui       256       16
  ((bg         '("#2a2426" "#2a2426" nil          )) ; bg1
   (bg-alt     '("#242021" "#242021" nil          )) ; bg1
   (bg-alt2    '("#504945" "#504945" "brown"      )) ; bg2 (for region, selection etc.)

   (base0      '("#0d1011" "black"   "black"      )) ; (self-defined)
   (base1      '("#1d2021" "#1d1d1d" "brightblack")) ; bg0_h
   (base2      '("#282828" "#282828" "brightblack")) ; bg0
   (base3      '("#3c3836" "#383838" "brightblack")) ; bg1
   (base4      '("#5b5b5b" "#5c5c5c" "brightblack")) ; bg3
   (base5      '("#7c6f64" "#6f6f6f" "brightblack")) ; bg4
   (base6      '("#928374" "#909090" "brightblack")) ; gray
   (base7      '("#d5c4a1" "#cccccc" "brightblack")) ; fg2
   (base8      '("#fbf1c7" "#fbfbfb" "brightwhite")) ; fg0
   (fg         '("#e6d6ac" "#e6d6ac" "brightwhite")) ; fg/fg1
   (fg-alt     '("#d8caac" "#d8caac" "brightwhite")) ; fg2

   (grey       '("#5b5b5b" "#5b5b5b" "brightblack"))   ; gray
   (red        '("#e68183" "#e68183" "red"))           ; bright-red
   (magenta    '("#e68183" "#e68183" "magenta"))       ; red
   (violet     '("#d3a0bc" "#d3a0bc" "brightmagenta")) ; bright-purple
   (orange     '("#e39b7b" "#e39b7b" "orange"))        ; bright-orange
   (yellow     '("#d9bb80" "#d9bb80" "yellow"))        ; bright-yellow
   (teal       '("#87af87" "#87af87" "green"))         ; bright-aqua
   (green      '("#87af87" "#87af87" "green"))         ; bright-green
   (dark-green '("#678f67" "#678f67" "green"))         ; green
   (blue       '("#89beba" "#89beba" "brightblue"))    ; bright-blue
   (dark-blue  '("#458588" "#458588" "blue"))          ; blue
   (cyan       '("#87c095" "#87c095" "brightcyan"))    ; bright-aqua
   (dark-cyan  '("#67a075" "#67a075" "cyan"))          ; aqua

   ;; face categories
   (highlight      yellow)
   (vertical-bar   grey)
   (selection      bg-alt2)
   (builtin        orange)
   (comments       (if doom-miramare-brighter-comments magenta grey))
   (doc-comments   (if doom-miramare-brighter-comments (doom-lighten magenta 0.2) (doom-lighten fg-alt 0.25)))
   (constants      violet)
   (functions      cyan)
   (keywords       red)
   (methods        cyan)
   (operators      cyan)
   (type           yellow)
   (strings        green)
   (variables      cyan)
   (numbers        violet)
   (region         bg-alt2)
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    (doom-darken blue 0.15))
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     (doom-darken red 0.15))

   ;; custom categories
   (-modeline-pad
    (when doom-miramare-padded-modeline
      (if (integerp doom-miramare-padded-modeline)
          doom-miramare-padded-modeline
        4)))

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;;;; Base theme face overrides
  ((button :foreground cyan :underline t :bold t)
   (cursor :background "white")
   (font-lock-variable-name-face :foreground cyan :italic t)
   (hl-line :background bg-alt)
   (isearch :foreground base0 :background orange)
   (lazy-highlight
    :background yellow :foreground base0 :distant-foreground base0
    :weight 'bold)
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :background bg-alt2 :foreground fg :bold t)
   (minibuffer-prompt :foreground cyan)
   (mode-line
    :background bg-alt2 :foreground (doom-lighten fg-alt 0.25)
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,bg-alt2)))
   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,bg)))

   ;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background bg-alt2 :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground "white" :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)
   ;;;; company
   (company-preview-common :foreground cyan)
   (company-tooltip-common :foreground cyan)
   (company-tooltip-common-selection :foreground cyan)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background bg-alt)
   (company-scrollbar-fg :background cyan)
   (company-tooltip-selection :background bg-alt2)
   (company-tooltip-mouse :background bg-alt2 :foreground nil)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground keywords)
   ;;;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)
   ;;;; doom-emacs
   (+workspace-tab-selected-face :background dark-green :foreground "white")
   ;;;; doom-modeline
   (doom-modeline-bar :background dark-green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-error :background bg)
   (doom-modeline-info :bold t :foreground cyan)
   (doom-modeline-panel :background dark-green :foreground fg)
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-warning :foreground red :bold t)
   ;;;; doom-themes
   (doom-themes-neotree-file-face :foreground fg)
   (doom-themes-neotree-hidden-file-face :foreground (doom-lighten fg-alt 0.25))
   (doom-themes-neotree-media-file-face :foreground (doom-lighten fg-alt 0.25))
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (doom-blend red bg 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.2))
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background yellow)
   (evil-ex-substitute-replacement :foreground cyan :inherit 'evil-ex-substitute-matches)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground "white" :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)
   ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,cyan)  :background base3)
   ;;;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)
   ;;;; highlight-quoted
   (highlight-quoted-symbol :foreground dark-cyan)
   ;;;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base3 0.03) :distant-foreground fg-alt)
   ;;;; highlight-thing
   (highlight-thing :background (doom-lighten base3 0.03) :distant-foreground fg-alt)
   ;;;; ivy
   (ivy-current-match :background bg-alt2)
   (ivy-subdir :background nil :foreground cyan)
   (ivy-action :background nil :foreground cyan)
   (ivy-grep-line-number :background nil :foreground cyan)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow :bold t)
   (ivy-minibuffer-match-face-2 :background nil :foreground red :bold t)
   (ivy-minibuffer-match-highlight :foreground cyan)
   (counsel-key-binding :foreground cyan)
   ;;;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground dark-cyan)
   ;;;; magit
   (magit-section-heading             :foreground yellow :weight 'bold)
   (magit-branch-current              :underline cyan :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base3 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background bg-alt2 :foreground fg)
   (magit-diff-context                :foreground bg-alt :foreground fg-alt)
   ;;;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground cyan)
   (markdown-list-face :foreground red)
   (markdown-url-face :foreground red)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground cyan)
   ((markdown-code-face &override) :background (doom-lighten base2 0.045))
   ;;;; mu4e-view
   (mu4e-header-key-face :foreground red)
   ;;;; neotree
   (neo-root-dir-face   :foreground cyan)
   (doom-neotree-dir-face :foreground cyan)
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground yellow)
   ((outline-2 &override) :foreground cyan)
   ((outline-3 &override) :foreground cyan)
   ;;;; org <built-in>
   (org-ellipsis :underline nil :foreground orange)
   (org-tag :foreground yellow :bold nil)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow)
   ;;;; show-paren
   ((show-paren-match &override) :foreground nil :background base5 :bold t)
   ((show-paren-mismatch &override) :foreground nil :background "red")
   ;;;; which-func
   (which-func :foreground cyan)
   ;;;; which-key
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground (doom-lighten fg-alt 0.25))
   (which-key-local-map-description-face :foreground cyan)
   ;;;; undo-tree
   (undo-tree-visualizer-active-branch-face :foreground cyan)
   (undo-tree-visualizer-current-face :foreground yellow)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground cyan)
   (rainbow-delimiters-depth-4-face :foreground red)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground cyan)
   (rainbow-delimiters-depth-7-face :foreground red)
   ;;;; rjsx-mode
   (rjsx-tag :foreground cyan :weight 'semi-bold)
   (rjsx-text :foreground fg)
   (rjsx-attr :foreground violet)
   ;;;; solaire-mode
   (solaire-hl-line-face :background bg-alt2)
   ;;;; swiper
   (swiper-line-face :background bg-alt2)
   ;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground blue)
   (web-mode-html-tag-face         :foreground cyan :weight 'semi-bold)
   (web-mode-html-attr-name-face   :foreground violet)
   (web-mode-json-key-face         :foreground green)
   (web-mode-json-context-face     :foreground cyan))

  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-miramare-theme.el ends here
