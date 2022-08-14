;;; doom-darcula-theme.el --- Inspired by Intellij Darcula -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Nisal Bandara <https://github.com/nisalb>
;; Maintainer: Nisal Bandara <https://github.com/nisalb>
;; Source: doom-one-theme and Intellij Darcula theme.
;; Version: 1.0
;;
;;; Commentary:
;;
;; Adding darcula to doom themes pack. This theme is written on doom-one-theme.el and provides
;; faces for tree-sitter-hl. It also introduces new tree-sitter queries for C language, and
;; tree-sitter-hl-faces for those captures.
;;
;; This is a WIP. I have tested it with C, seems OK. This essentialy the One Dark theme but
;; with different colors.
;;
;; Much of the work is done by tree-sitter-langs package by matching tree-sitter faces to
;; font-lock faces.
;;
;;; Code:

(require 'doom-themes)
(require 'tree-sitter)


;;
;;; Variables

(defgroup doom-darcula-theme nil
  "Options for the `doom-darcula' theme."
  :group 'doom-themes)

(defcustom doom-darcula-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-darcula-theme
  :type 'boolean)

(defcustom doom-darcula-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-darcula-theme
  :type 'boolean)

(defcustom doom-darcula-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-darcula-theme
  :type '(choice integer boolean))

(defface tree-sitter-hl-face:this
  '((default :inherit tree-sitter-hl-face:keyword))
  "Face for 'this' or 'self' variables.
Although non-standard, they are considered as keywords"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:escape
  '((default :inherit tree-sitter-hl-face:keyword))
  "Face for escape sequences in strings."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.global
  '((default :inherit tree-sitter-hl-face:variable))
  "Face for global variables."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.doc
  '((default :inherit tree-sitter-hl-face:comment))
  "Face for documentation comments."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.block
  '((default :inherit tree-sitter-hl-face:comment))
  "Face for block comments."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.line
  '((default :inherit tree-sitter-hl-face:comment))
  "Face for line comments."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:preproc_directive
  '((default :inherit tree-sitter-hl-face:function.macro))
  "Face for preprocessor directives."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:brace
  '((default :inherit tree-sitter-hl-face:punctuation.bracket))
  "Face for braces."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:bracket
  '((default :inherit tree-sitter-hl-face:punctuation.bracket))
  "Face for brackets."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:dot
  '((default :inherit tree-sitter-hl-face:punctuation.delimiter))
  "Face for dot."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comma
  '((default :inherit tree-sitter-hl-face:punctuation.delimiter))
  "Face for comma."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:parantheses
  '((default :inherit tree-sitter-hl-face:punctuation.delimiter))
  "Face for parantheses."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:semicolon
  '((default :inherit tree-sitter-hl-face:punctuation.delimiter))
  "Face for semicolon."
  :group 'tree-sitter-hl-faces)

;; Additional patterns for C language
(tree-sitter-hl-add-patterns 'c
  [
   ;; 'this' keyword: @this
   ((identifier) @this
    (.match? @this "this|self"))

   ;; escape characters: @escape
   ((escape_sequence) @escape
    (.match? @escape "\[abfnrtw'?\"\\\\]"))

   ;; Keywords: @keyword
   ["break"
    "case"
    "const"
    "continue"
    "default"
    "do"
    "else"
    "enum"
    "extern"
    "for"
    "if"
    "inline"
    "return"
    "sizeof"
    "static"
    "struct"
    "switch"
    "typedef"
    "union"
    "volatile"
    "while"
    "..."] @keyword

    [(storage_class_specifier)
     (type_qualifier)] @keyword

   ;; Variables -----------------------------------------------------

   (translation_unit
    (declaration
     declarator: [
                  (init_declarator (identifier) @variable.global)
                  (identifier) @variable.global]))

   ;; Punctuations --------------------------------------------------

   ["{" "}"] @punctuation.brace
   ["[" "]"] @punctuation.bracket
   "," @punctuation.comma
   "." @punctuation.dot
   ["(" ")"] @punctuation.parantheses
   ";" @punctuation.semicolon

   ;; Comments ------------------------------------------------------
   ((comment) @comment.doc
    (.match? @comment.doc "^\/\\*\\*"))

   ((comment) @comment.block
    (.match? @comment.block "^\/\*"))

   ((comment) @comment.line
    (.match? @comment.line "^\/\/"))

   ;; Properties ----------------------------------------------------

   (field_declaration
    declarator: [(field_identifier) @property.definition
                 (pointer_declarator (field_identifier) @property.definition)
                 (pointer_declarator (pointer_declarator (field_identifier) @property.definition))
                 (pointer_declarator (pointer_declarator (pointer_declarator (field_identifier) @property.definition)))
                 (pointer_declarator (pointer_declarator (pointer_declarator (pointer_declarator (field_identifier) @property.definition))))
                 (pointer_declarator (pointer_declarator (pointer_declarator (pointer_declarator (pointer_declarator (field_identifier) @property.definition)))))])

   (field_identifier) @property

   ;; Functions -----------------------------------------------------

   (preproc_function_def
    name: (identifier) @function.macro)

   ;; Preprocessor --------------------------------------------------

   ["#define"
    "#else"
    "#endif"
    "#if"
    "#ifdef"
    "#ifndef"
    "#include"
    (preproc_directive)] @preproc.directive])
;;
;;; Theme definition

(def-doom-theme doom-darcula
  "A dark theme inspired by Atom One Dark."

  ;; TODO: Enhance colors to have better contrasts.

  ;; name        default   256           16
  ((bg         '("#2b2b2b" "#2b2b2b"     "black"        ))
   (fg         '("#d2dbe9" "#a9b7c6"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#272727" "#1e1e1e"       "black"        ))
   (fg-alt     '("#98a5b2" "#76808b"       "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#272727" "#272727"     "black"        ))
   (base1      '("#2c2c2c" "#2c2c2c"     "brightblack"  ))
   (base2      '("#373737" "#373737"     "brightblack"  ))
   (base3      '("#3a3a3a" "#3a3a3a"     "brightblack"  ))
   (base4      '("#636466" "#636466"     "brightblack"  ))
   (base5      '("#93979e" "#93979e"     "brightblack"  ))
   (base6      '("#a5aab5" "#a5aab5"     "brightblack"  ))
   (base7      '("#acb7c2" "#acb7c2"     "brightblack"  ))
   (base8      '("#b9c7dd" "#b9c7dd"     "white"        ))

   (grey       base4)
   (red        '("#bc3f3c" "#bc3f3c" "red"          ))
   (orange     '("#cc7832" "#cc7832" "brightred"    ))
   (green      '("#88ac73" "#88ac73" "green"        ))
   (teal       '("#0f9795" "#0f9795" "brightgreen"  ))
   (yellow     '("#bbb529" "#bbb529" "yellow"       ))
   (blue       '("#6897bb" "#6897bb" "brightblue"   ))
   (dark-blue  '("#250787" "#250787" "blue"         ))
   (magenta    '("#a771bf" "#a771bf" "brightmagenta"))
   (violet     '("#9876aa" "#9876aa" "magenta"      ))
   (cyan       '("#00e5e5" "#00e5e5" "brightcyan"   ))
   (dark-cyan  '("#00a3a3" "#00a3a3" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-darcula-brighter-comments (doom-lighten grey 0.2) grey))
   (doc-comments   (if doom-darcula-brighter-comments (doom-lighten green 0.2) green))
   (constants      violet)
   (functions      magenta)
   (keywords       (doom-darken orange 0.1))
   (methods        cyan)
   (operators      fg)
   (type           orange)
   (strings        green)
   (variables      fg)
   (numbers        blue)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-darcula-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-darcula-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-darcula-padded-modeline
      (if (integerp doom-darcula-padded-modeline) doom-darcula-padded-modeline 4))))


  ;;;; Base theme face overrides
  ((tree-sitter-hl-face:keyword :foreground orange)
   (tree-sitter-hl-face:constant :weight 'bold :foreground violet)
   (tree-sitter-hl-face:operator :foreground fg)
   (tree-sitter-hl-face:type.builtin :foreground orange)
   (tree-sitter-hl-face:type :foreground fg)
   (tree-sitter-hl-face:function :foreground "#ffc66d")
   (tree-sitter-hl-face:function.call :foreground fg)
   (tree-sitter-hl-face:number :foreground blue)
   (tree-sitter-hl-face:property.definition :slant 'normal :foreground violet)
   (tree-sitter-hl-face:property :slant 'normal :foreground violet)
   (tree-sitter-hl-face:comment.doc :slant 'italic :foreground (doom-darken green 0.1))
   (tree-sitter-hl-face:comment.block :foreground grey)
   (tree-sitter-hl-face:comment.line :foreground grey)
   
   (font-lock-preprocessor-face
      :inherit 'bold
      :foreground yellow)
   (font-lock-preprocessor-char-face
      :inherit 'bold
      :foreground yellow)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-darcula-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-darcula-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-darcula-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-darcula-theme.el ends here
