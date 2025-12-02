;;; doom-bluloco-dark-theme.el --- ported from VSCode's Bluloco Dark Theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: RazikSF <https://github.com/RazikSF>
;; Maintainer:
;; Source: https://github.com/uloco/theme-bluloco-dark
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-bluloco-dark-theme nil
  "Options for the `doom-bluloco' themes."
  :group 'doom-themes)

(defcustom doom-bluloco-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to determine the exact padding."
  :group 'doom-bluloco-dark-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme doom-bluloco-dark
  "A dark theme inspired by bluloco dark theme"
  :family 'doom-bluloco
  :background-mode 'dark

  ;; name        default     256       16
  ((bg           '("#282c34" "#303030" "black"        ))
   (fg           '("#abb2bf" "#b2b2b2" "white"        ))

   (bg-alt       '("#23252A" "#262626" "black"        ))
   (fg-alt       '("#7a808b" "#808080" "white"        ))

   (base0        '("#1e222a" "#262626" "black"        ))
   (base1        '("#2E333C" "#303030" "brightblack"  ))
   (base2        '("#404450" "#3a3a3a" "brightblack"  ))
   (base3        '("#494e57" "#4e4e4e" "brightblack"  ))
   (base4        '("#595e68" "#5f5f5f" "brightblack"  ))
   (base5        '("#7a808b" "#808080" "white"  ))
   (base6        '("#8a919c" "#949494" "white"        ))
   (base7        '("#9ba1ae" "#9e9e9e" "white"        ))
   (base8        '("#b5bbc9" "#bcbcbc" "white"        ))

   (grey         '("#636d83" "#5f5f87" "brightblack"  ))
   (red          '("#ff6480" "#ff5f87" "red"          ))
   (orange       '("#ff936a" "#ff875f" "brightred"    ))
   (brown        '("#ce9887" "#d78787" "brown"        ))
   (green        '("#3fc56b" "#5fd75f" "green"        ))
   (teal         '("#8bcdef" "#87d7ff" "brightgreen"  ))
   (yellow       '("#f9c859" "#ffd75f" "yellow"       ))
   (blue         '("#10b1fe" "#00afff" "brightblue"   ))
   (dark-blue    '("#3691ff" "#5f87ff" "blue"         ))
   (magenta      '("#ff78f8" "#ff87ff" "magenta"      ))
   (violet       '("#9f7efe" "#af87ff" "brightmagenta"))
   (dark-violet  '("#7a82da" "#8787d7" "brightmagenta"))
   (cyan         '("#78F8FF" "#87ffff" "brightcyan"   ))
   (dark-cyan    '("#4d9ea3" "#5fafaf" "brightcyan"   ))

   ;; custom colours
   (rainbow-red       '("#FF6666"))
   (rainbow-yellow    '("#f4ff78"))
   (rainbow-blue      '("#44A5FF"))
   (rainbow-orange    '("#ffa023"))
   (rainbow-green     '("#92f535"))
   (rainbow-violet    '("#ff78ff"))
   (rainbow-cyan      '("#28e4eb"))
   (rainbow-indigo    '("#9F7EFE"))

   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        green)
   (comments       base5)
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
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories

   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (doom-darken bg-alt 0.1))
   (modeline-bg-alt          `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg)))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-bluloco-dark-padded-modeline
      (if (integerp doom-bluloco-dark-padded-modeline) doom-bluloco-dark-padded-modeline 4))))

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
   (ivy-posframe :background base0)

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
   ((markdown-code-face &override) :foreground brown :background bg-alt)
   ((markdown-header-face &override) :inherit 'bold :foreground yellow)
   ((markdown-italic-face &override) :foreground orange :slant 'italic)
   ((markdown-link-face &override) :foreground dark-blue :underline t)
   ((markdown-markup-face &override) :foreground red)
   ((markdown-metadata-key-face &override) :foreground brown)
   ((markdown-pre-face &override) :foreground brown)
   ((markdown-url-face &override) :foreground blue)
   (markdown-footnote-marker-face :foreground dark-blue)
   (markdown-language-keyword-face :foreground brown)
   (markdown-strike-through-face :foreground grey :strike-through t)

   ;; outline
   ((outline-1 &override) :foreground rainbow-red)
   ((outline-2 &override) :foreground rainbow-yellow)
   ((outline-3 &override) :foreground rainbow-blue)
   ((outline-4 &override) :foreground rainbow-orange)
   ((outline-5 &override) :foreground rainbow-green)
   ((outline-6 &override) :foreground rainbow-violet)
   ((outline-7 &override) :foreground rainbow-cyan)
   ((outline-8 &override) :foreground rainbow-indigo)

   ;; org-mode
   ((org-block &override) :background bg-alt)
   ((org-code &override) :foreground brown :background bg)
   ((org-document-title &override) :inherit 'bold :foreground green :height 1.25)
   ((org-link &override) :foreground dark-blue :weight 'normal)
   ((org-meta-line &override) :foreground yellow)
   ((org-quote &override) :foreground violet :background bg-alt)
   ((org-table &override) :foreground brown)
   ((org-verbatim &override) :foreground brown :background bg)
   (org-ellipsis :underline nil :background bg)
   (org-level-1 :inherit 'bold :foreground rainbow-red :height 1.2)
   (org-level-2 :inherit 'bold :foreground rainbow-yellow :height 1.15)
   (org-level-3 :inherit 'bold :foreground rainbow-blue :height 1.1)
   (org-level-4 :inherit 'bold :foreground rainbow-orange :height 1.05)
   (org-level-5 :inherit 'bold :foreground rainbow-green)
   (org-level-6 :inherit 'bold :foreground rainbow-violet)
   (org-level-7 :inherit 'bold :foreground rainbow-cyan)
   (org-level-8 :inherit 'bold :foreground rainbow-indigo)
   (org-macro :foreground dark-violet)

   ;; org-modern-indent
   (org-modern-indent-line :inherit 'org-block-begin-line)

   ;; rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground rainbow-red)
   (rainbow-delimiters-depth-2-face :foreground rainbow-yellow)
   (rainbow-delimiters-depth-3-face :foreground rainbow-blue)
   (rainbow-delimiters-depth-4-face :foreground rainbow-orange)
   (rainbow-delimiters-depth-5-face :foreground rainbow-green)
   (rainbow-delimiters-depth-6-face :foreground rainbow-violet)
   (rainbow-delimiters-depth-7-face :foreground rainbow-cyan)
   (rainbow-delimiters-depth-8-face :foreground rainbow-indigo)

   ;; selectrum
   (selectrum-current-candidate :background base1)

   ;; solaire-mode
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))

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
   (tree-sitter-hl-face:type.builtin :foreground type)

   ;; vertico
   (vertico-current :background base1)

   ;; web-mode
   (web-mode-html-entity-face :foreground violet)
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-html-tag-bracket-face :foreground dark-blue)
   (web-mode-html-tag-face :foreground dark-blue))

;;;; Base theme variable overrides-
())

;;; doom-bluloco-dark-theme.el ends here
