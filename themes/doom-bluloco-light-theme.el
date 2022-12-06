;;; doom-bluloco-light-theme.el --- ported from VSCode's Bluloco Light Theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: RazikSF <https://github.com/RazikSF>
;; Maintainer:
;; Source: https://github.com/uloco/theme-bluloco-light
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-bluloco-light-theme nil
  "Options for the `doom-bluloco' themes."
  :group 'doom-themes)

(defcustom doom-bluloco-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-bluloco-light-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-bluloco-light
  "A light theme inspired by bluloco light theme"

  ;; name        default     256       16
  ((bg           '("#f9f9f9" "white"   "white"        ))
   (fg           '("#383a42" "#424242" "black"        ))

   (bg-alt       '("#e7e7e7" "white"   "white"        ))
   (fg-alt       '("#c6c7c7" "#c7c7c7" "brightblack"  ))

   (base0        '("#f1f1f1" "#f0f0f0" "white"        ))
   (base1        '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2        '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3        '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4        '("#9ca0a4" "#9ca0a4" "brightblack"  ))
   (base5        '("#383a42" "#424242" "brightblack"  ))
   (base6        '("#202328" "#2e2e2e" "brightblack"  ))
   (base7        '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base8        '("#1b2229" "black"   "black"        ))

   (grey         '("#a0a1a7" "#a0a1a7" "brightblack"  ))
   (red          '("#d52753" "#d7005f" "red"          ))
   (orange       '("#df631c" "#d75f00" "brightred"    ))
   (brown        '("#a05a48" "#af5f5f" "brown"        ))
   (green        '("#23974a" "#00875f" "green"        ))
   (teal         '("#40B8C5" "#5fafd7" "brightgreen"  ))
   (yellow       '("#c5a332" "#d7af5f" "yellow"       ))
   (blue         '("#0098dd" "#0087d7" "brightblue"   ))
   (dark-blue    '("#275fe4" "#005fd7" "blue"         ))
   (magenta      '("#ce33c0" "#d75faf" "magenta"      ))
   (violet       '("#823ff1" "#875fff" "brightmagenta"))
   (dark-violet  '("#7a82da" "#8787d7" "brightmagenta"))
   (cyan         '("#33c0ce" "#5fafd7" "brightcyan"   ))
   (dark-cyan    '("#217b84" "#008080" "brightcyan"   ))

   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        green)
   (comments       grey)
   (doc-comments   yellow)
   (constants      violet)
   (functions      green)
   (keywords       blue)
   (methods        green)
   (operators      dark-violet)
   (type           red)
   (strings        yellow)
   (variables      teal)
   (numbers        magenta)
   (region         `(,(doom-darken (car bg-alt) 0.075) ,@(doom-darken (cdr base0) 0.075)))
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories

   (modeline-fg               fg)
   (modeline-fg-alt           (doom-blend violet base4 0.2))
   (modeline-bg               base1)
   (modeline-bg-alt           base2)
   (modeline-bg-inactive      (doom-darken bg 0.1))
   (modeline-bg-alt-inactive  `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1)))

   (-modeline-pad
    (when doom-bluloco-light-padded-modeline
      (if (integerp doom-bluloco-light-padded-modeline) doom-bluloco-light-padded-modeline 4))))

;;;; Base theme face overrides

(((font-lock-operator-face &override) :foreground dark-violet)

   ;; mode-line
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground brown :background base2)
   (css-property             :foreground brown)
   (css-selector             :foreground dark-blue)

   ;; diredfl
   (diredfl-date-time :foreground cyan :weight 'normal)

   ;; posframe
   (ivy-posframe :background base2)

   ;; js2-mode
   (js2-object-property :foreground brown)
   (js2-object-property-access :foreground brown)

   ;; latex
   ((font-latex-math-face &override) :foreground yellow)

   ;; magit
   (magit-blame-heading :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   ;; markdown
   ((markdown-blockquote-face &override) :foreground violet)
   ((markdown-bold-face &override) :foreground red :weight 'bold)
   ((markdown-code-face &override) :foreground brown :background (doom-lighten base1 0.3))
   ((markdown-header-face &override) :inherit 'bold :foreground yellow)
   ((markdown-italic-face &override) :foreground orange :slant 'italic)
   ((markdown-link-face &override) :foreground dark-blue :underline t)
   ((markdown-markup-face &override) :foreground dark-violet)
   ((markdown-metadata-key-face &override) :foreground brown)
   ((markdown-pre-face &override) :foreground brown)
   ((markdown-url-face &override) :foreground blue)
   (markdown-footnote-marker-face :foreground dark-blue)
   (markdown-language-keyword-face :foreground brown)
   (markdown-strike-through-face :foreground grey :strike-through t)

   ;; org-mode
   ((org-block &override) :background (doom-lighten base1 0.3))
   ((org-code &override) :foreground brown :background bg)
   ((org-document-title &override) :inherit 'bold :foreground green :height 1.25)
   ((org-link &override) :foreground dark-blue :weight 'normal)
   ((org-meta-line &override) :foreground yellow)
   ((org-quote &override) :foreground violet :background bg-alt)
   ((org-table &override) :foreground brown)
   ((org-verbatim &override) :foreground brown :background bg)
   (org-ellipsis :underline nil :background bg)
   (org-level-1 :inherit 'bold :foreground blue :height 1.2)
   (org-level-2 :inherit 'bold :foreground magenta :height 1.15)
   (org-level-3 :inherit 'bold :foreground green :height 1.1)
   (org-level-4 :inherit 'bold :foreground (doom-lighten blue 0.25)  :height 1.05)

   ;; org-modern-indent
   (org-modern-indent-line :inherit 'org-block-begin-line)

   ;; rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground operators)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground blue)
   (rainbow-delimiters-depth-4-face :foreground magenta)

   ;; selectrum
   (selectrum-current-candidate :background base2)

   ;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-alt-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt-inactive)))

   ;; tree-sitter
   (tree-sitter-hl-face:constant.builtin :foreground violet)
   (tree-sitter-hl-face:escape :foreground orange)
   (tree-sitter-hl-face:function.call :foreground functions)
   (tree-sitter-hl-face:function.special :foreground operators)
   (tree-sitter-hl-face:label :foreground variables)
   (tree-sitter-hl-face:number :foreground numbers)
   (tree-sitter-hl-face:operator :foreground operators)
   (tree-sitter-hl-face:property :foreground brown :slant 'italic)
   (tree-sitter-hl-face:punctution :foreground operators)
   (tree-sitter-hl-face:tag :foreground keywords)
   (tree-sitter-hl-face:type :foreground type)

   ;;vertico
   (vertico-current :background base2)

   ;; web-mode
   (web-mode-html-entity-face :foreground violet)
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-html-tag-bracket-face :foreground dark-blue)
   (web-mode-html-tag-face :foreground dark-blue))

;;;; Base theme variable overrides-
())

;;; doom-bluloco-light-theme.el ends here
