;;; doom-nord-aurora-theme.el --- a light variant of Nord -*- no-byte-compile: t; -*-
;;
;; Added: January 27, 2022 (#703)
;; Author: MoskitoHero <https://github.com/MoskitoHero>
;; Maintainer:
;; Source: https://www.nordtheme.com
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-nord-aurora-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-nord-aurora-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-nord-aurora-theme
  :type 'boolean)

(defcustom doom-nord-aurora-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-nord-aurora-theme
  :type 'boolean)

(defcustom doom-nord-aurora-comment-bg doom-nord-aurora-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-nord-aurora-theme
  :type 'boolean)

(defcustom doom-nord-aurora-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-nord-aurora-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-nord-aurora-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-nord-aurora-theme
    :type 'symbol))


;;
;;; Theme definition

(def-doom-theme doom-nord-aurora
  "A dark theme inspired by Nord. Aurora flavour."

  ;; name        default   256       16
  ((bg         '("#2E3440" nil       nil            ))
   (bg-alt     '("#272C36" nil       nil            ))
   (base0      '("#191C25" "black"   "black"        ))
   (base1      '("#242832" "#1e1e1e" "brightblack"  ))
   (base2      '("#2C333F" "#2e2e2e" "brightblack"  ))
   (base3      '("#373E4C" "#262626" "brightblack"  ))
   (base4      '("#434C5E" "#3f3f3f" "brightblack"  ))
   (base5      '("#4C566A" "#525252" "brightblack"  ))
   (base6      '("#9099AB" "#6b6b6b" "brightblack"  ))
   (base7      '("#D8DEE9" "#979797" "brightblack"  ))
   (base8      '("#F0F4FC" "#dfdfdf" "white"        ))
   (fg         '("#ECEFF4" "#ECECEC" "white"        ))
   (fg-alt     '("#E5E9F0" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#BF616A" "#ff6655" "red"          )) ;; Nord11
   (orange     '("#D08770" "#dd8844" "brightred"    )) ;; Nord12
   (green      '("#A3BE8C" "#99bb66" "green"        )) ;; Nord14
   (teal       '("#8FBCBB" "#44b9b1" "brightgreen"  )) ;; Nord7
   (yellow     '("#EBCB8B" "#ECBE7B" "yellow"       )) ;; Nord13
   (blue       '("#81A1C1" "#51afef" "brightblue"   )) ;; Nord9
   (dark-blue  '("#5E81AC" "#2257A0" "blue"         )) ;; Nord10
   (magenta    '("#B48EAD" "#c678dd" "magenta"      )) ;; Nord15
   (violet     '("#5D80AE" "#a9a1e1" "brightmagenta")) ;; ??
   (cyan       '("#88C0D0" "#46D9FF" "brightcyan"   )) ;; Nord8
   (dark-cyan  '("#507681" "#5699AF" "cyan"         )) ;; ??

   ;; face categories -- required for all themes
   (highlight      base6)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base6)
   (doc-comments   base7)
   (constants      blue)
   (functions      yellow)
   (keywords       blue)
   (methods        yellow)
   (operators      cyan)
   (type           green)
   (strings        teal)
   (variables      cyan)
   (numbers        orange)
   (region         (pcase doom-nord-aurora-region-highlight
                     (`frost teal)
                     (`snowstorm base7)
                     (_ base4)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-nord-aurora-brighter-modeline)
   (-modeline-pad
    (when doom-nord-aurora-padded-modeline
      (if (integerp doom-nord-aurora-padded-modeline) doom-nord-aurora-padded-modeline 4)))

   (region-fg
    (when (memq doom-nord-aurora-region-highlight '(frost snowstorm))
      base0))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-blend bg base5 0.2)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-blend bg base5 0.2)
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  (((region &override) :foreground region-fg)

   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-nord-aurora-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (doom-modeline-project-root-dir :foreground base6)
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

   ;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base4 0.1) :distant-foreground fg-alt)

   ;; highlight-thing
   (highlight-thing :background (doom-lighten base4 0.1) :distant-foreground fg-alt)

   ;; ivy
   ((ivy-current-match &override) :foreground region-fg :weight 'semi-bold)


   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-nord-aurora-theme.el ends here
