;;; doom-oksolar-light-theme.el --- an OKLab variant of Solarized light -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added:
;; Author: logc <https://github.com/logc>
;; Maintainer:
;; Source: https://meat.io/oksolar.json
;; Source: https://meat.io/oksolar
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-oksolar-light-theme nil
  "Options for the `doom-oksolar-light' theme."
  :group 'doom-themes)

(defcustom doom-oksolar-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-oksolar-light-theme
  :type 'boolean)

(defcustom doom-oksolar-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-oksolar-light-theme
  :type 'boolean)

(defcustom doom-oksolar-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-oksolar-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-oksolar-light
  "A light theme inspired by OKSolar."

  ;; name        default   256       16
  ((bg         '("#FBF7EF" "#FBF7EF" "white"        ))
   (fg         '("#657377" "#657377" "black"        ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#F1E9D2" "#F1E9D2" "white"        ))
   (fg-alt     '("#5B7279" "#5B7279" "brightwhite"  ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#FBF7EF" "#FBF7EF" "white"        ))
   (base1      '("#F1E9D2" "#F1E9D2" "white"        ))
   ;; NOTE: base 2 never used, left as-is in Solarized
   (base2      '("#8FAAAB" "#8FAAAB" "brightblack"  ))
   (base3      '("#FCF7E8" "#FCF7E8" "brightblack"  ))
   (base4      '("#8FAAAB" "#8FAAAB" "brightblack"  ))
   (base5      '("#98A8A8" "#98A8A8" "brightblack"  ))
   (base6      '("#657377" "#657377" "brightblack"  ))
   (base7      '("#5B7279" "#5B7279" "brightblack"  ))
   (base8      '("#657377" "#657377" "brightblack"  ))

   (grey       base4)
   (red        '("#F23749" "#ff6655" "red"          ))
   (orange     '("#D56500" "#dd8844" "brightred"    ))
   (green      '("#819500" "#99bb66" "green"        ))
   (teal       '("#35A69C" "#33aa99" "brightgreen"  ))
   (yellow     '("#AC8300" "#ECBE7B" "yellow"       ))
   (blue       '("#2B90D8" "#51afef" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#DD459D" "#c678dd" "magenta"      ))
   (violet     '("#7D80D1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#259D94" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-oksolar-light-brighter-comments
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
   (-modeline-bright doom-oksolar-light-brighter-modeline)
   (-modeline-pad
    (when doom-oksolar-light-padded-modeline
      (if (integerp doom-oksolar-light-padded-modeline) doom-oksolar-light-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-darken bg 0.05)))
   (modeline-bg-alt
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-lighten base3 0.2)))
   (modeline-bg-inactive     (doom-darken bg 0.025))
   (modeline-bg-inactive-alt (doom-darken bg 0.02)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :slant 'italic
    :background (if doom-oksolar-light-brighter-comments
                    (doom-blend teal base0 0.07)
                  'unspecified))
   ((font-lock-type-face &override) :slant 'italic)
   ((font-lock-builtin-face &override) :slant 'italic)
   ((font-lock-function-name-face &override) :foreground type)
   ((font-lock-keyword-face &override) :weight 'bold)
   ((font-lock-constant-face &override) :weight 'bold)
   (hl-line :background base1)
   ((line-number &override) :foreground base6)
   ((line-number-current-line &override) :foreground fg :background region :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-evil-emacs-state  :foreground magenta)
   (doom-modeline-evil-insert-state :foreground blue)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; lsp-ui
   (lsp-ui-sideline-code-action :foreground blue)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-darken base3 0.05))
   ;;;; ivy
   (ivy-current-match :background (doom-lighten yellow 0.65) :distant-foreground fg)
   (ivy-minibuffer-match-face-1 :foreground blue :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground magenta :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground green   :background base3 :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground yellow  :background base3 :weight 'bold)
   (ivy-minibuffer-match-highlight :foreground violet :weight 'bold)
   ;;;; ivy-posframe
   (ivy-posframe :background modeline-bg-alt)
   ;;;; swiper
   (swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
   (swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
   (swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
   (swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)
   ;;;; helm
   (helm-selection :foreground base0 :weight 'bold :background blue)
   ;;;; company
   (company-tooltip-selection :background blue :foreground base3)
   ;;;; org <built-in>
   (org-block :background (doom-blend yellow bg 0.04) :extend t)
   (org-block-background :background (doom-blend yellow bg 0.04))
   (org-block-begin-line :background (doom-blend yellow bg 0.08) :extend t)
   (org-block-end-line :background (doom-blend yellow bg 0.08) :extend t)
   ;;;; widget
   (widget-field :foreground fg :background base3)
   (widget-single-line-field :foreground fg :background base3)
   ;;;; latex
   (font-latex-sedate-face :foreground base6)
   ;;;; notmuch
   (notmuch-message-summary-face :foreground teal)
   (notmuch-wash-cited-text :foreground base6))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-oksolar-light-theme.el ends here
