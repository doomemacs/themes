;;; doom-peacock-theme.el --- inspired by daylerees' Peacock -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: November 9, 2017 (#112)
;; Author: teesloane <https://github.com/teesloane>
;; Maintainer:
;; Source: https://github.com/daylerees/colour-schemes/blob/master/emacs/peacock-theme.el
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-peacock-theme nil
  "Options for the `doom-peacock' theme."
  :group 'doom-themes)

(defcustom doom-peacock-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-peacock-theme
  :type 'boolean)

(defcustom doom-peacock-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-peacock-theme
  :type 'boolean)

(defcustom doom-peacock-comment-bg doom-peacock-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-peacock-theme
  :type 'boolean)

(defcustom doom-peacock-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-peacock-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-peacock
  "Peacock theme from daylerees themes "

  ;; name        default   256       16
  ((bg         '("#2b2a27" nil       nil            ))
   (bg-alt     '("#1F1E1D" nil       nil            ))
   (base0      '("#2b2a27" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#ede0ce" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"        ))

   (grey       base4)
   (white      '("#f8f8f0" "base4"   "base4"        ))
   (red        '("#ff5d38" "#ff6655" "red"          )) ;; peacock todo 16
   (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#26a6a6" "#44b9b1" "brightgreen"  )) ;; peacock
   (yellow     '("#bcd42a" "#ECBE7B" "yellow"       )) ;; peacock, todo 16
   (blue       '("#51afef" "#51afef" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "magenta"      ))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))
   (coral-popup  '("#a60033" "#f6bfbc" "coral-popup"         ))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      coral-popup)
   (builtin        red)
   (comments       (if doom-peacock-brighter-comments dark-cyan base5)) ;; TODO
   (doc-comments   (doom-lighten (if doom-peacock-brighter-comments dark-cyan base5) 0.25)) ;; TODO
   (constants      red)        ;; done
   (functions      yellow)     ;; done
   (keywords       teal)       ;; done
   (methods        yellow)     ;; not sure how to test this.
   (operators      red)        ;; not showing up on `=` etc.
   (type           white)      ;;
   (strings        yellow)
   (variables      white)      ;; done
   (numbers        red)        ;; done

   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-peacock-brighter-modeline)
   (-modeline-pad
    (when doom-peacock-padded-modeline
      (if (integerp doom-peacock-padded-modeline) doom-peacock-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken bg 0.475)
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0.1) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  ((font-lock-comment-face
    :foreground comments
    :background (if doom-peacock-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base7)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-common                           :foreground highlight)
   (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg)
   (company-tooltip-selection  :background selection)
   (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
   (company-tooltip-annotation                       :foreground violet)
   (company-scrollbar-bg       :inherit 'tooltip)
   (company-scrollbar-fg       :background highlight)
   (company-preview                                  :foreground highlight)
   (company-preview-common     :background base3 :foreground magenta)
   (company-preview-search     :inherit 'company-tooltip-search)
   (company-template-field     :inherit 'match)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)
   ;;;; pos-tip
   (popup-tip-face :inherit 'tooltip)
   ;;;; rjsx-mode
   (rjsx-tag :foreground teal)
   (rjsx-attr :foreground red)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-peacock-theme.el ends here
