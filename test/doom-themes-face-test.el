;;; doom-themes-face-test.el

(ert-deftest doom-themes-build-simple-face ()
  "TODO"
  (-with-colors! ((blue "#0000FF" "#2222DD" "blue"))
    ;; literal test
    (should
     (equal (-build-face '(default '((t (:background "blue")))))
                   '(list 'default '((t (:background "blue"))))))
    ;; plain
    (should
     (equal (-build-face '(default :background "blue"))
                   '(list 'default (list (list (quote t) (list :background "blue"))))))))

(ert-deftest doom-themes-build-nested-face ()
  "Test `doom-themes--build-face'; builds a face out of a simple sub-spec that
uses a doom-themes function to modify the resulting color."
  (-with-colors! ((blue "#0000FF" "#2222DD" "blue"))
    (should
     (equal (-build-face '(default :background (doom-darken blue 0.2)))
            '(list 'default
                   (list (list '((class color) (min-colors 257)) (list :background (doom-darken (doom-color 'blue '257) 0.2)))
                         (list '((class color) (min-colors 256)) (list :background (doom-darken (doom-color 'blue '256) 0.2)))
                         (list '((class color) (min-colors 16))  (list :background (doom-darken (doom-color 'blue '16)  0.2)))))))))

(ert-deftest doom-themes-full-build ()
  "Test `doom-themes--build-face'; builds a face out of a list of sub-specs that
use all the available functionality of this API. `doom-themes--colors' is left
blank to prevent expansion of colors."
  (-with-colors! ()
    (-with-faces!
      ((default :background bg :foreground fg)
       ;; (doom-default :inherit 'default :background (doom-lighten bg 0.2))
       (fringe
        (&all   :inherit 'default)
        (&dark  :foreground grey)
        (&light :foreground light-grey))
       (avy-lead-face
        `((((background dark))  (:background ,(car highlight) :foreground ,(car black) :distant-foreground ,(car white)))
          (((background light)) (:background ,(car highlight) :foreground ,(car white) :distant-foreground ,(car black))))))
      (should (equal (doom-themes-common-faces)
                     '((list 'default (list (list 't (list :background bg :foreground fg))))
                       (list 'fringe (list (list '((background dark))  (list :inherit 'default :foreground grey))
                                           (list '((background light)) (list :inherit 'default :foreground light-grey))))
                       (list 'avy-lead-face `((((background dark))  (:background ,(car highlight) :foreground ,(car black) :distant-foreground ,(car white)))
                                              (((background light)) (:background ,(car highlight) :foreground ,(car white) :distant-foreground ,(car black)))))))))))


(ert-deftest doom-themes-build-simple-bg-face ()
  "Test `doom-themes--build-face' with a simple spec that includes &dark/&light sub-specs."
  (-with-colors! ((blue "#0000FF" "#2222DD" "blue"))
    (should
     (equal (-build-face
             '(default
                (&all :background "blue")
                (&dark :foreground "white")
                (&light :foreground "black")))
            '(list 'default
                   (list (list '((background dark))
                               (list :background "blue" :foreground "white"))
                         (list '((background light))
                               (list :background "blue" :foreground "black"))))))))

(ert-deftest doom-themes-build-complex-bg-face ()
  "Test `doom-themes--build-face' with a complex spec that includes &dark/&light sub-specs."
  (-with-colors! ((blue  "#2222DD" "#0000FF" "blue")
                  (red   "#DD2222" "#FF0000" "red")
                  (white "#EEEEEE" "#FFFFFF" "white"))
    (should
     (equal (-build-face
             '(default
                (&all   :background blue)
                (&dark  :foreground white)
                (&light :foreground red)))
            '(list 'default
                   (list (list '((class color) (min-colors 257) (background dark))  (list :background (doom-color 'blue '257) :foreground (doom-color 'white '257)))
                         (list '((class color) (min-colors 256) (background dark))  (list :background (doom-color 'blue '256) :foreground (doom-color 'white '256)))
                         (list '((class color) (min-colors 16)  (background dark))  (list :background (doom-color 'blue '16)  :foreground (doom-color 'white '16)))
                         (list '((class color) (min-colors 257) (background light)) (list :background (doom-color 'blue '257) :foreground (doom-color 'red '257)))
                         (list '((class color) (min-colors 256) (background light)) (list :background (doom-color 'blue '256) :foreground (doom-color 'red '256)))
                         (list '((class color) (min-colors 16)  (background light)) (list :background (doom-color 'blue '16)  :foreground (doom-color 'red '16)))))))))

(ert-deftest doom-themes-build-inherited-face ()
  "TODO"
  (-with-colors! ()
    (-with-faces!
      ((simple :background "black")
       (moderate :background bg :foreground fg)
       (complex (&all   :inherit 'default)
                (&dark  :foreground grey)
                (&light :foreground light-grey))
       ((inherit-simple &inherit simple))
       ((inherit-moderate &inherit moderate))
       ((inherit-complex &inherit complex))
       ((inherit-recursive &inherit inherit-simple)))
      (let ((bg 'bg)
            (fg 'fg)
            (grey 'grey)
            (light-grey 'light-grey))
        (let ((faces (eval `(list ,@(doom-themes-common-faces)))))
          (should (car (cdr-safe (assq 'inherit-simple faces))))
          (should (car (cdr-safe (assq 'inherit-moderate faces))))
          (should (car (cdr-safe (assq 'inherit-complex faces))))
          (should (car (cdr-safe (assq 'inherit-recursive faces))))
          (should (equal (cdr (assq 'inherit-simple faces))
                         (cdr (assq 'simple faces))))
          (should (equal (cdr (assq 'inherit-moderate faces))
                         (cdr (assq 'moderate faces))))
          (should (equal (cdr (assq 'inherit-complex faces))
                         (cdr (assq 'complex faces))))
          (should (equal (cdr (assq 'inherit-recursive faces))
                         (cdr (assq 'simple faces)))))))))

(ert-deftest doom-themes-build-overridden-face ()
  "TODO"
  (-with-colors! ()
    (-with-faces!
      ((simple :background "black")
       (moderate :background bg :foreground fg)
       ;; (complex (&all   :inherit 'default)
       ;;          (&dark  :foreground grey)
       ;;          (&light :foreground light-grey))
       ((simple &override) :foreground "red")
       ((moderate &override) :background "white" :foreground "red")
       ;; ((complex &override))
       )
      (let ((bg 'bg)
            (fg 'fg))
        (let ((faces (eval `(list ,@(doom-themes-common-faces)))))
          (should (equal (cdr (assq 'simple faces))
                         `(((t (:background "black" :foreground "red"))))))
          (should (equal (cdr (assq 'moderate faces))
                         `(((t (:background "white" :foreground "red"))))))
          ;; (should (equal (cdr (assq 'complex faces))
          ;;                ))
          )))))

(ert-deftest doom-themes-face-ref ()
  "TODO"
  (-with-colors! ()
    (-with-faces!
      ((simple :background "black")
       (moderate :background bg :foreground fg)
       (complex (&all   :inherit 'default)
                (&dark  :foreground grey)
                (&light :foreground light-grey))
       (ref-simple :background (doom-ref 'simple :background))
       (ref-moderate :foreground (doom-ref 'moderate :foreground))
       (ref-complex :foreground (doom-ref 'complex :foreground '&dark)))
      (let ((bg 'bg)
            (fg 'fg)
            (grey 'grey)
            (light-grey 'light-grey))
        (let ((faces (eval `(list ,@(doom-themes-common-faces)))))
          (should (equal (cdr (assq 'ref-simple faces))
                         `(((t (:background "black"))))))
          (should (equal (cdr (assq 'ref-moderate faces))
                         `(((t (:foreground fg))))))
          (should (equal (cdr (assq 'ref-complex faces))
                         `(((t (:foreground grey))))))
          )))))

(provide 'doom-themes-face-test)
;;; doom-themes-face-test.el ends here
