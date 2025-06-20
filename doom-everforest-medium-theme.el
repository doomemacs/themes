;;; doom-everforest-hard-theme.el --- A port of Sainnhe's everforest-hard theme; -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Echinoidea <https://github.com/echinoidea>
;; Maintainer:
;; Source: https://github.com/sainnhe/everforest
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)


;;
;;; Variables

(defgroup doom-everforest-medium-theme nil
  "Options for doom-everforest-medium."
  :group 'doom-themes)

(defcustom doom-everforest-medium-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-everforest-medium-theme
  :type 'boolean)

(defcustom doom-everforest-medium-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-everforest-medium-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-everforest-medium
    "Green based color scheme; designed to be warm and soft."

  ;; name        gui       256       16
  ((bg         '("#232a2e" "#232a2e" nil          )) ; bg1
   (bg-alt     '("#2d353b" "#2d353b" nil          )) ; bg1
   (bg-alt2    '("#343f44" "#343f44" "brown"      )) ; bg2 (for region, selection etc.)

   (base0      '("#232a2e" "black"   "black"      )) ; (self-defined)
   (base1      '("#2d353b" "#2d353b" "brightblack")) ; bg0_h
   (base2      '("#343f44" "#343f44" "brightblack")) ; bg0
   (base3      '("#3d484d" "#3d484d" "brightblack")) ; bg1
   (base4      '("#475258" "#475258" "brightblack")) ; bg3
   (base5      '("#4f585e" "#4f585e" "brightblack")) ; bg4
   (base6      '("#56635f" "#56635f" "brightblack")) ; gray
   (base7      '("#9da9a0" "#9da9a0" "brightblack")) ; fg2
   (base8      '("#d3c6aa" "#d3c6aa" "brightwhite")) ; fg0
   (fg         '("#d3c6aa" "#d3c6aa" "brightwhite")) ; fg/fg1
   (fg-alt     '("#d6c9ad" "#d6c9ad" "brightwhite")) ; fg2

   (grey       '("#7a8478" "#7a8478" "brightblack"))   ; gray
   (red        '("#e67e80" "#e67e80" "red"))           ; bright-red
   (magenta    '("#d699b6" "#d699b6" "magenta"))       ; red
   (violet     '("#cc9bc7" "#cc9bc7" "brightmagenta")) ; bright-purple
   (orange     '("#e69876" "#e69876" "orange"))        ; bright-orange
   (yellow     '("#dbbc7f" "#dbbc7f" "yellow"))        ; bright-yellow
   (teal       '("#83c092" "#83c092" "green"))         ; bright-aqua
   (green      '("#a7c080" "#a7c080" "green"))         ; bright-green
   (dark-green '("#3c4841" "#3c4841" "green"))         ; green
   (blue       '("#7fbbb3" "#7fbbb3" "brightblue"))    ; bright-blue
   (dark-blue  '("#384b55" "#384b55" "blue"))          ; blue
   (cyan       '("#7fbbb3" "#7fbbb3" "brightcyan"))    ; bright-aqua
   (dark-cyan  '("#384b55" "#384b55" "cyan"))          ; aqua

   ;; face categories
   (highlight      yellow)
   (vertical-bar   grey)
   (selection      bg-alt2)
   (builtin        orange)
   (comments       (if doom-everforest-medium-brighter-comments magenta grey))
   (doc-comments   (if doom-everforest-medium-brighter-comments (doom-lighten magenta 0.2) (doom-lighten fg-alt 0.25)))
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
    (when doom-everforest-medium-padded-modeline
      (if (integerp doom-everforest-medium-padded-modeline)
          doom-everforest-medium-padded-modeline
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
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base3)))
   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base2)))

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

;;; doom-everforest-medium-theme.el ends here
