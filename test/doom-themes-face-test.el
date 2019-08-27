;;; doom-themes-face-test.el

(ert-deftest doom-themes-build-simple-face ()
  "TODO"
  (-with-colors! ((blue '("#0000FF" "#2222DD" "blue")))
    ;; literal test
    (should
     (equal (-build-face '(default '((t (:background "blue")))))
            '(default ((t (:background "blue"))))))
    ;; plain
    (should
     (equal (-build-face '(default :background "blue"))
            '(default ((t (:background "blue"))))))))

(ert-deftest doom-themes-build-nested-face ()
  "Test `doom-themes--build-face'; builds a face out of a simple sub-spec that
uses a doom-themes function to modify the resulting color."
  (-with-colors! ((blue '("#0000FF" "#2222DD" "blue")))
    (should
     (equal (-build-face '(default :background (doom-darken blue 0.2)))
            `(default
               ((((class color) (min-colors 257))
                 (:background ,(doom-darken (doom-color 'blue '257) 0.2)))
                (((class color) (min-colors 256))
                 (:background ,(doom-darken (doom-color 'blue '256) 0.2)))
                (((class color) (min-colors 16))
                 (:background ,(doom-darken (doom-color 'blue '16)  0.2)))))))))

(ert-deftest doom-themes-build-simple-bg-face ()
  "Test `doom-themes--build-face' with a simple spec that includes &dark/&light sub-specs."
  (should
   (equal (-build-face
           '(default
              (&all :background "blue")
              (&dark :foreground "white")
              (&light :foreground "black")))
          '(default
             ((((background dark))  (:background "blue" :foreground "white"))
              (((background light)) (:background "blue" :foreground "black")))))))

(ert-deftest doom-themes-build-complex-bg-face ()
  "Test `doom-themes--build-face' with a complex spec that includes &dark/&light sub-specs."
  (-with-colors! ((blue  '("#2222DD" "#0000FF" "blue"))
                  (red   '("#DD2222" "#FF0000" "red"))
                  (white '("#EEEEEE" "#FFFFFF" "white")))
    (should
     (equal (-build-face
             '(default
                (&all   :background blue)
                (&dark  :foreground white)
                (&light :foreground red)))
            `(default
               ((((class color) (min-colors 257) (background dark))
                 (:background ,(doom-color 'blue '257) :foreground ,(doom-color 'white '257)))
                (((class color) (min-colors 256) (background dark))
                 (:background ,(doom-color 'blue '256) :foreground ,(doom-color 'white '256)))
                (((class color) (min-colors 16)  (background dark))
                 (:background ,(doom-color 'blue '16)  :foreground ,(doom-color 'white '16)))
                (((class color) (min-colors 257) (background light))
                 (:background ,(doom-color 'blue '257) :foreground ,(doom-color 'red '257)))
                (((class color) (min-colors 256) (background light))
                 (:background ,(doom-color 'blue '256) :foreground ,(doom-color 'red '256)))
                (((class color) (min-colors 16)  (background light))
                 (:background ,(doom-color 'blue '16)  :foreground ,(doom-color 'red '16)))))))))

(ert-deftest doom-themes-build-inherited-face ()
  "TODO"
  (-with-colors! ((bg         '("#000000" "#111111"))
                  (fg         '("#FFFFFF" "#EEEEEE"))
                  (grey       '("#555555" "#5a5a5a"))
                  (light-grey '("#999999" "#9a9a9a")))
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
      (should (equal (cdr (assq 'inherit-simple faces))
                     (cdr (assq 'simple faces))))
      (should (equal (cdr (assq 'inherit-moderate faces))
                     (cdr (assq 'moderate faces))))
      (should (equal (cdr (assq 'inherit-complex faces))
                     (cdr (assq 'complex faces))))
      (should (equal (cdr (assq 'inherit-recursive faces))
                     (cdr (assq 'simple faces)))))))

(ert-deftest doom-themes-build-overridden-face ()
  "TODO"
  (-with-faces!
    ((simple :background "black")
     (moderate :background "blue" :foreground "blue")
     ((simple &override) :foreground "red")
     ((moderate &override) :background "white" :foreground "red"))
    (should (equal (cdr (assq 'simple faces))
                   `(((t (:background "black" :foreground "red"))))))
    (should (equal (cdr (assq 'moderate faces))
                   `(((t (:background "white" :foreground "red"))))))))

(ert-deftest doom-themes-face-ref ()
  "TODO"
  (-with-faces!
    ((simple :background "black")
     (moderate :background "black" :foreground "white")
     (complex (&all   :inherit 'default)
              (&dark  :foreground "grey")
              (&light :foreground "lightgrey"))
     (ref-simple :background (doom-ref 'simple :background))
     (ref-moderate :foreground (doom-ref 'moderate :foreground))
     (ref-complex :foreground (doom-ref 'complex :foreground '&dark)))
    (should (equal (cdr (assq 'ref-simple faces))
                   `(((t (:background "black"))))))
    (should (equal (cdr (assq 'ref-moderate faces))
                   `(((t (:foreground "white"))))))
    (should (equal (cdr (assq 'ref-complex faces))
                   `(((t (:foreground "grey"))))))))

(provide 'doom-themes-face-test)
;;; doom-themes-face-test.el ends here
