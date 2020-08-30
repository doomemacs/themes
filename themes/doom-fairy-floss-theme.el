;;; doom-fairy-floss-theme.el --- inspired by sailorhg Fairy Floss -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-fairy-floss-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-fairy-floss-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-fairy-floss-theme
  :type 'boolean)

(defcustom doom-fairy-floss-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-fairy-floss-theme
  :type 'boolean)

(defcustom doom-fairy-floss-comment-bg doom-fairy-floss-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-fairy-floss-theme
  :type 'boolean)

(defcustom doom-fairy-floss-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-fairy-floss-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-fairy-floss
  "A candy colored theme inspired by Sublime's Fairy Floss"

  ;; name        default   256       16
  ((bg         '("#5a5475" nil       nil           ))
   (bg-alt     '("#343145" nil       nil           ))
   (base0      '("#464258" "black"   "black"       ))
   (base1      '("#514C66" "#1e1e1e" "brightblack" ))
   (base2      '("#6A6483" "#2e2e2e" "brightblack" ))
   (base3      '("#9673D3" "#262626" "brightblack" ))
   (base4      '("#A0A0C0" "#3f3f3f" "brightblack" ))
   (base5      '("#B8A2CE" "#525252" "brightblack" ))
   (base6      '("#726C8A" "#6b6b6b" "brightblack" ))
   (base7      '("#5B576C" "#979797" "brightblack" ))
   (base8      '("#716799" "#dfdfdf" "white"       ))
   (fg-alt     '("#B5B2Bd" "#2d2d2d" "white"       ))
   (fg         '("#F8F8F0" "#bfbfbf" "brightwhite" ))

   (grey       '("#656565" "#515154" "brightblack"  ))
   (red        '("#CC6666" "#CC6666" "red"          ))
   (orange     '("#E6C000" "#E6C000" "brightred"    ))
   (green      '("#C2FFDF" "#C2FFDF" "green"        ))
   (yellow     '("#FFEA00" "#FFEA00" "yellow"       ))
   (blue       '("#55b3cc" "#55b3cc" "brightblue"   ))
   (teal       '("#8295D6" "#8295D6" "brightgreen"  ))
   (dark-blue  '("#167be2" "#3F88AD" "blue"         ))
   (magenta    '("#FFB8D1" "#FFB8D1" "magenta"      ))
   (violet     '("#C5A3FF" "#C5A3FF" "brightmagenta"))
   (cyan       '("#96CBFE" "#C2FFDF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#204052" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-fairy-floss-brighter-comments cyan orange))
   (doc-comments   violet)
   (constants      violet)
   (functions      green)
   (keywords       cyan)
   (methods        green)
   (operators      orange)
   (type           cyan)
   (strings        yellow)
   (variables      magenta)
   (numbers        violet)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-fairy-floss-brighter-modeline)
   (-modeline-pad
    (when doom-fairy-floss-padded-modeline
      (if (integerp doom-fairy-floss-padded-modeline) doom-fairy-floss-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  (
   (company-tooltip-selection     :background base3)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-fairy-floss-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-keyword-face
    :slant 'italic
    :foreground keywords)

   ;; Centaur tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
					  :foreground blue)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
					    :foreground blue)
   ;; Doom modeline
   (doom-modeline-bar :background blue)

   ;; highlight-thing highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)

   ;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)

   ;; hl-line
   (hl-line :background base2)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright cyan highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (tooltip              :background bg-alt :foreground fg)
   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; magit
   (magit-diff-removed                :foreground (doom-darken red 0.2) :background (doom-blend red base5 0.1))
   (magit-diff-removed-highlight      :foreground red                   :background (doom-blend red base5 0.2) :weight 'bold)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground orange)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken orange 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken orange 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :background base0)
   (org-scheduled         :foreground green)
   (org-scheduled-previously :foreground yellow)
   (org-scheduled-today   :foreground orange)
   (org-hide              :foreground hidden))

  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-fairy-floss-theme.el ends here
