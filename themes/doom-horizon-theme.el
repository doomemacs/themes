;;; doom-horizon-theme.el --- Inspired by VSCode Horizon -*- no-byte-compile: t; -*-

;;; Commentary:
;;; This theme was inspired by the port of Horizon to Emacs
;;; see: https://github.com/aodhneine/horizon-theme.el

(require 'doom-themes)

;;; Code:
(defgroup doom-horizon-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-horizon-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-horizon-theme
  :type 'boolean)

(defcustom doom-horizon-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-horizon-theme
  :type 'boolean)

(defcustom doom-horizon-comment-bg doom-horizon-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their legibility."
  :group 'doom-horizon-theme
  :type 'boolean)

(defcustom doom-horizon-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-horizon-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-horizon
  "A port of the port of the Visual Studio Code theme Horizon"

  ;; name        default   256       16
  ( (bg         '("#1c1e26" "#232530" nil            ))
    (bg-alt     '("#232530" "#1c1e26" nil            ))
    (base0      '("#16161c" "#16161c" "black"        ))
    (base1      '("#1a1c23" "#1a1c23" "brightblack"  ))
    (base2      '("#1d1f27" "#1c1e26" "brightblack"  ))
    (base3      '("#232530" "#232530" "brightblack"  ))
    (base4      '("#6a6a6a" "#6a6a6a" "brightblack"  ))
    (base5      '("#f9cec3" "#f9cec3" "brightblack"  ))
    (base6      '("#f9cbbe" "#f9cbbe" "brightblack"  ))
    (base7      '("#fadad1" "#fadad1" "brightblack"  ))
    (base8      '("#fdf0ed" "#fdf0ed" "white"        ))
    (fg-alt     '("#fdf0ed" "#fdf0ed" "brightwhite"  ))
    (fg         '("#c7c9cb" "#c7c9cb" "white"        ))

    (grey       base4)
    (red        '("#e95678" "#e95678" "red"          ))
    (orange     '("#f09383" "#f09383" "brightred"    ))
    (green      '("#09f7a0" "#09f7a0" "green"        ))
    (teal       '("#21bfc2" "#21bfc2" "brightgreen"  ))
    (yellow     '("#fab795" "#fab795" "yellow"       ))
    (blue       '("#21bfc2" "#21bfc2" "brightblue"   ))
    (dark-blue  '("#25b2bc" "#25b2bc" "blue"         ))
    (magenta    '("#6c6f93" "#6c6f93" "magenta"      ))
    (violet     '("#b877db" "#b877db" "brightmagenta"))
    (cyan       '("#59e3e3" "#59e3e3" "brightcyan"   ))
    (dark-cyan  '("#27d797" "#27d797" "cyan"   ))

    ;; additional highlighting colours for horizon
    (hor-highlight  (doom-lighten base3 0.05))
    (hor-highlight-selected (doom-lighten base3 0.1))
    (hor-highlight-bright (doom-lighten base3 0.2))

    ;; face categories -- required for all themes
    (highlight      red)
    (vertical-bar   base0)
    (selection      violet)
    (builtin        violet)
    (comments       hor-highlight-bright)
    (doc-comments   yellow)
    (constants      orange)
    (functions      teal)
    (keywords       violet)
    (methods        magenta)
    (operators      teal)
    (type           teal)
    (strings        yellow)
    (variables      red)
    (numbers        orange)
    (region         hor-highlight)
    (error          red)
    (warning        dark-cyan)
    (success        green)
    (vc-modified    orange)
    (vc-added       green)
    (vc-deleted     red)


    ;; custom categories
    (hidden     `(,(car bg) "black" "black"))
    (-modeline-bright doom-horizon-brighter-modeline)
    (-modeline-pad
      (when doom-horizon-padded-modeline
        (if (integerp doom-horizon-padded-modeline) doom-horizon-padded-modeline 4)))

    (modeline-fg     (doom-darken fg 0.2))
    (modeline-fg-alt (doom-lighten bg 0.2))

    (modeline-bg
      (if -modeline-bright
          base4
        `(,(car base1), (cdr fg-alt))))
    (modeline-bg-l
      (if -modeline-bright
          base4
        `(,(car base1), (cdr fg))))
    (modeline-bg-inactive   base1)
    (modeline-bg-inactive-l base1))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")


    ;; ((line-number &override) :foreground (doom-lighten bg 0.05))
    ((line-number &override) :foreground hor-highlight-selected)
    ((line-number-current-line &override) :foreground hor-highlight-bright)

    (font-lock-comment-face
      :inherit 'italic
      :foreground comments
      :background (if doom-horizon-comment-bg (doom-lighten bg 0.05)))
    (font-lock-doc-face
      :inherit 'font-lock-comment-face
      :foreground doc-comments)

    (mode-line
      :background modeline-bg :foreground modeline-fg
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
    (mode-line-inactive
      :background modeline-bg-inactive :foreground modeline-fg-alt
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
    (mode-line-emphasis
      :foreground (if -modeline-bright base8 highlight))
    (header-line :inherit 'mode-line :background fg-alt)
    (mode-line-highlight :background base1 :foreground fg)

    ;; modeline
    (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    (doom-modeline-highlight :foreground (doom-lighten bg 0.3))
    (doom-modeline-project-dir :foreground red :inherit 'bold )
    (doom-modeline-buffer-path :foreground red)
    (doom-modeline-buffer-file :foreground fg)
    (doom-modeline-buffer-modified :foreground violet)
    (doom-modeline-panel :background base1)
    (doom-modeline-urgent :foreground modeline-fg)
    (doom-modeline-info :foreground cyan)

    (solaire-mode-line-face
      :inherit 'mode-line
      :background modeline-bg-l
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
    (solaire-mode-line-inactive-face
      :inherit 'mode-line-inactive
      :background modeline-bg-inactive-l
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

    ;; --- major-mode faces -------------------
    ;; NOTE: there are lots of tweaks here to mimic the VSCode theme

    ;; css-mode / scss-mode
    (css-proprietary-property :foreground violet)
    (css-property             :foreground fg)
    (css-selector             :foreground red)

    ;; mic-paren
    (paren-face-match    :foreground green   :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground yellow :background base0   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)

    ;; markdown-mode
    (markdown-markup-face           :foreground cyan)
    (markdown-link-face             :foreground orange)
    (markdown-link-title-face       :foreground yellow)
    (markdown-header-face           :foreground red :inherit 'bold)
    (markdown-header-delimiter-face :foreground red :inherit 'bold)
    (markdown-language-keyword-face :foreground orange)
    (markdown-markup-face           :foreground fg)
    (markdown-bold-face             :foreground violet)
    (markdown-table-face            :foreground fg :background base1)
    ((markdown-code-face &override) :foreground orange :background base1)

    ;; outline (affects org-mode)
    ((outline-1 &override) :foreground blue :background nil)

    ;; org-mode
    ((org-block &override) :background base1)
    ((org-block-begin-line &override) :background base1 :foreground comments)
    (org-hide :foreground hidden)
    (org-link :inherit 'underline :foreground yellow)
    (org-agenda-done :foreground cyan)
    (solaire-org-hide-face :foreground hidden)
    (solaire-header-line-face :background bg-alt :foreground fg)
    (header-line :background base2 :foreground fg)

    ;; tooltip
    (tooltip              :background base0 :foreground fg)

    ;; haskell
    (haskell-type-face :foreground violet)
    (haskell-constructor-face :foreground yellow)
    (haskell-operator-face :foreground fg)
    (haskell-literate-comment-face :foreground hor-highlight-selected)

    ;; magit
    (magit-section-heading :foreground red)
    (magit-branch-remote   :foreground orange)

    ;; --- extra variables ---------------------
    ;; basics
    (link :foreground yellow :inherit 'underline)
    (fringe :background bg)

    ;; evil
    (evil-ex-search          :background hor-highlight-selected :foreground fg)
    (evil-ex-lazy-highlight  :background hor-highlight :foreground fg)

    ;; ivy
    (ivy-current-match       :background hor-highlight :distant-foreground nil)
    (ivy-posframe-cursor     :background red :foreground base0)
    (ivy-minibuffer-match-face-2 :foreground red :weight 'bold)

    ;; company
    (company-box-background    :background base0 :foreground fg)
    (company-tooltip-common    :foreground red :weight 'bold)
    (company-tooltip-selection :background hor-highlight :foreground fg)
    
   ;; treemacs
   (treemacs-root-face :foreground fg :weight 'bold :height 1.2)
   (doom-themes-treemacs-root-face :foreground fg :weight 'ultra-bold :height 1.2)
   (doom-themes-treemacs-file-face :foreground fg)
   (treemacs-directory-face :foreground fg)
   (treemacs-git-modified-face :foreground green)

   ;; js2-mode
   (js2-object-property        :foreground red)
   
   ;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-tag-bracket-face :foreground red)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)
    ))


;;; doom-horizon-theme.el ends here
