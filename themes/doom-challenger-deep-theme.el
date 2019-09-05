;;; doom-challenger-deep-theme.el --- inspired by VIM Challenger Deep
(require 'doom-themes)

;;
(defgroup doom-challenger-deep-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-challenger-deep-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-challenger-deep-theme
  :type 'boolean)

(defcustom doom-challenger-deep-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-challenger-deep-theme
  :type 'boolean)

(defcustom doom-challenger-deep-comment-bg doom-challenger-deep-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-challenger-deep-theme
  :type 'boolean)

(defcustom doom-challenger-deep-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-challenger-deep-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-challenger-deep
  "A dark theme inspired by VIM Challenger Deep"

  ;; name        default   256       16
  ((bg         '("#1b182c" "#1b182c" nil            ))
   (bg-alt     '("#12111E" "#12111E" nil            ))
   (base0      '("#100e23" "#100e23" "black"        ))
   (base1      '("#292F37" "#292F37" "brightblack"  ))
   (base2      '("#3d4551" "#3d4551" "brightblack"  ))
   (base3      '("#4C4B68" "#4C4B68" "brightblack"  ))
   (base4      '("#565575" "#565575" "brightblack"  ))
   (base5      '("#858FA5" "#858FA5" "brightblack"  ))
   (base6      '("#9BA7BF" "#9BA7BF" "brightblack"  ))
   (base7      '("#B0BED8" "#B0BED8" "brightblack"  ))
   (base8      '("#BAC9E4" "#BAC9E4" "white"        ))
   (fg-alt     '("#b2b2b2" "#b2b2b2" "brightwhite"  ))
   (fg         '("#cbe3e7" "#cbe3e7" "white"        ))

   (grey       base4)
   (red        '("#ff8080" "#ff8080" "red"          ))
   (orange     '("#ffb378" "#ffb378" "brightred"    ))
   (green      '("#95ffa4" "#95ffa4" "green"        ))
   (teal       '("#63f2f1" "#63f2f1" "brightgreen"  ))
   (yellow     '("#ffe9aa" "#ffe9aa" "yellow"       ))
   (blue       '("#91ddff" "#91ddff" "brightblue"   ))
   (dark-blue  '("#65b2ff" "#65b2ff" "blue"         ))
   (magenta    '("#c991e1" "#c991e1" "magenta"      ))
   (violet     '("#906cff" "#906cff" "brightmagenta"))
   (cyan       '("#aaffe4" "#aaffe4" "brightcyan"   ))
   (dark-cyan  '("#62d196" "#62d196" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar base1)
   (selection      violet)
   (builtin        magenta)
   (comments       (if doom-challenger-deep-brighter-comments dark-blue base4))
   (doc-comments   (if doom-challenger-deep-brighter-comments (doom-darken dark-cyan 0.3) base5) )
   (constants      cyan)
   (functions      magenta)
   (keywords       red)
   (methods        magenta)
   (operators      teal)
   (type           blue)
   (strings        yellow)
   (variables      yellow)
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-challenger-deep-brighter-modeline)
   (-modeline-pad
    (when doom-challenger-deep-padded-modeline
      (if (integerp doom-challenger-deep-padded-modeline) doom-challenger-deep-padded-modeline 4)))

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
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-challenger-deep-comment-bg (doom-lighten bg 0.05)))
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

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue :background nil)

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base1)
   (org-block-begin-line :background base1 :foreground comments)
   (solaire-org-hide-face :foreground hidden)

   ;; tooltip
   (tooltip              :background base0 :foreground fg))

  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-challenger-deep-theme.el ends here
