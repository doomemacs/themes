;;; doom-manilla-theme.el --- inspired by Solarized Light -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-manilla-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-manilla-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-manilla-theme
  :type 'boolean)

(defcustom doom-manilla-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-manilla-theme
  :type 'boolean)

(defcustom doom-manilla-comment-bg doom-manilla-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-manilla-theme
  :type 'boolean)

(defcustom doom-manilla-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-manilla-theme
  :type '(choice integer boolean))

(defcustom doom-manilla-italic-constants t
  "If nil, constants will be rendered in bold face, otherwise italic."
  :group 'doom-manilla-theme
  :type 'boolean)

;;
(def-doom-theme doom-manilla
  "A light theme inspired by Solarized Light, with more contrast"

  ;; name        default   256       16
  ((bg         '("#fdf9e9" nil        nil            ))
   (bg-alt     '("#fffbe1" nil        nil            ))
   (base0      '("#fffcf3" "#434f4f"  "white"        ))
   (base1      '("#fcf8ed" "#fcf8ed"  "brightwhite"  ))
   (base2      '("#fcf7e8" "#fcf7e8"  "white"        ))
   (base3      '("#f2e6ce" "#f2e6ce"  "white"        ))
   (base4      '("#c4beb0" "#c4beb0"  "white"        ))
   (base5      '("#a6a6a6" "#a6a6a6"  "white"        ))
   (base6      '("#6e8082" "#6e8082"  "white"        ))
   (base7      '("#566262" "#566262"  "brightblack"  ))
   (base8      '("#444e4e" "#444e4e"  "brightblack"  ))
   (fg         '("#061212" "#061212"  "black"        ))
   (fg-alt     '("#434f4f" "#434f4f"  "brightblack"  ))

   (grey       base6)
   (red        '("#dc322f" "#204052"  "red"          ))
   (orange     '("#cb4b16" "#cb4b16"  "brightred"    ))
   (green      '("#7b8e00" "#7b8e00"  "green"        ))
   (teal       '("#35a69c" "#35a69c"  "brightgreen"  ))
   (yellow     '("#906f0c" "#906f0c"  "yellow"       ))
   (blue       '("#268bd2" "#268bd2"  "brightblue"   ))
   (dark-blue  '("#3f88ad" "#3f88ad"  "blue"         ))
   (magenta    '("#bc2971" "#bc2971"  "magenta"      ))
   (violet     '("#6c71c4" "#6c71c4"  "brightmagenta"))
   (cyan       '("#268c86" "#268c86"  "brightcyan"   ))
   (dark-cyan  '("#204052" "#204052"  "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-manilla-brighter-comments
                       (doom-lighten teal 0.25)
                     base6))
   (doc-comments   teal)
   (constants      violet)
   (functions      magenta)
   (keywords       green)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.1)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-manilla-brighter-modeline)
   (-modeline-pad
    (when doom-manilla-padded-modeline
      (if (integerp doom-manilla-padded-modeline) doom-manilla-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-lighten base3 0.2)))
   (modeline-bg-l
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-darken bg 0.05)))
   (modeline-bg-inactive   (doom-darken bg 0.02))
   (modeline-bg-inactive-l (doom-darken bg 0.025)))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (hl-line :background base3)

   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg :background region :weight 'bold)

   (flyspell-correct-highlight-face :background highlight :foreground bg)

   (org-block :background (doom-blend yellow bg 0.04))
   (org-block-background :background (doom-blend yellow bg 0.04))
   (org-block-begin-line :background (doom-blend yellow bg 0.08))
   (org-block-end-line :background (doom-blend yellow bg 0.08))
   (org-verbatim :foreground (doom-darken green 0.1) :background (doom-blend base3 base2 0.6))

   (lsp-ui-sideline-code-action :foreground blue)

   (font-lock-comment-face
    :slant 'italic
    :foreground comments
    :extend t
    :background (if doom-manilla-comment-bg (doom-blend base3 base2 0.4)))
   ((font-lock-doc-face &override) :foreground doc-comments)
   ;; ((font-lock-function-name-face &override) :foreground type)

   (font-lock-keyword-face
    :weight (if doom-manilla-italic-constants 'normal 'bold)
    :slant (if doom-manilla-italic-constants 'italic 'normal)
    :foreground keywords)

   (font-lock-constant-face
    :weight (if doom-manilla-italic-constants 'normal 'bold)
    :slant (if doom-manilla-italic-constants 'italic 'normal)
    :foreground constants)


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
   (markdown-markup-face :foreground base6)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-blend base3 base2 0.6))
   (markdown-pre-face :foreground (doom-darken green 0.1))
   ;; ivy-mode
   (ivy-current-match :background (doom-lighten yellow 0.65) :distant-foreground fg)
   (ivy-minibuffer-match-face-1 :foreground blue :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground magenta :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground green   :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground yellow  :background base3 :weight 'bold)
   (ivy-minibuffer-match-highlight :foreground violet :weight 'bold)
   (swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
   (swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
   (swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
   (swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)

   ;; posframe
   (ivy-posframe :background modeline-bg-l)
   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; helm
   (helm-selection :foreground base0 :weight 'bold :background blue)
   (helm-match :foreground violet)

   ;; company
   (company-tooltip-selection :background blue
                              :foreground base3)

   ;; widget
   (widget-field :foreground fg :background base3)
   (widget-single-line-field :foreground fg :background base3)

   ;; latex
   (font-latex-sedate-face :foreground base6)

   ;; notmuch
   (notmuch-message-summary-face :foreground teal)
   (notmuch-wash-cited-text :foreground base6)

   )


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-manilla-theme.El Ends Here
