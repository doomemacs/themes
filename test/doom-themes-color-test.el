;;; doom-themes-color-test.el

(ert-deftest doom-themes-color ()
  "TODO"
  (-with-colors! ((black "#000000" "#222222" "black"))
    (should-not (doom-color 'red))
    (should (equal (doom-color 'black 't)   "#000000"))
    (should (equal (doom-color 'black '256) "#222222"))
    (should (equal (doom-color 'black '16)  "black"))
    (should (equal (doom-color 'black '9999) (doom-color 'black 't)))
    (should (equal (doom-color 'black) (doom-color 'black 't)))))

(ert-deftest doom-themes-color-alias ()
  (-with-colors! ((black "#000000")
                  (red "#FF0000"))
    (should (equal (doom-darken 'red 0) "#ff0000"))
    (should (equal (doom-darken 'black 0) "#000000"))
    (should (equal (doom-lighten 'red 0) "#ff0000"))
    (should (equal (doom-lighten 'black 0) "#000000"))
    (should (equal (doom-blend 'black 'red 0) "#ff0000"))
    (should (equal (doom-blend 'black 'red 1.0) "#000000"))))

(ert-deftest doom-themes-color-detect ()
  "TODO"
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (should-not (-color-p '(:background "red")))
    (should     (-color-p '(:foreground red)))
    (should-not (-color-p '(:box `(:color red))))
    (should     (-color-p '(:box `(:color ,red))))
    (should-not (-color-p '(:box '(:color red))))
    (should-not (-color-p '(:box '(:color ,red))))

    (should-not (-color-p '(:background "black")))
    (should-not (-color-p '(:foreground black)))
    (should-not (-color-p '(:box `(:color black))))
    (should-not (-color-p '(:box `(:color ,black))))
    (should-not (-color-p '(:box '(:color black))))
    (should-not (-color-p '(:box '(:color ,black))))))

(ert-deftest doom-themes-color-plain-subst ()
  "TODO"
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (-colorize! (:background red)
                (:background (doom-color 'red '256))
                256)

    (-colorize! (:background blue)
                (:background blue)
                256)))

(ert-deftest doom-themes-color-nested-subst ()
  "Should substitute color variables nested with sublists or exprs."
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (-colorize! (:box `(:color ,red))
                (:box `(:color ,(doom-color 'red '256)))
                256)

    (-colorize! (:foreground (doom-darken red 0.2))
                (:foreground (doom-darken (doom-color 'red '256) 0.2))
                256)))

(ert-deftest doom-themes-color-side-effects ()
  "Shouldn't alter the original value."
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (let ((plist '(:background red)))
      (should plist)
      (doom-themes--colorize plist 't)
      (should (equal plist '(:background red))))

    (let ((plist '(:box `(:color ,red))))
      (should plist)
      (doom-themes--colorize plist 't)
      (should (equal plist '(:box `(:color ,red)))))))

(ert-deftest doom-themes-color-no-subst ()
  "TODO"
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (-colorize! (:background "red")
                (:background "red")
                256)))

(ert-deftest doom-themes-color-recurse-subst ()
  "TODO"
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (-colorize! (:foreground red :background "blue")
                (:foreground (doom-color 'red 't) :background "blue")
                t)

    (-colorize! (:background "blue" :foreground red)
                (:background "blue" :foreground (doom-color 'red 't))
                t)))

(ert-deftest doom-themes-color-multi-subst ()
  "TODO"
  (-with-colors! ((red "#FF0000" "#AA4444" "red")
                  (blue "#0000FF" "#4444AA" "blue")
                  (white "#F9F9F9" "#FFFFFF" "white"))
    (-colorize! (:background blue :foreground red)
                (:background (doom-color 'blue 't) :foreground (doom-color 'red 't))
                t)

    (-colorize! (:background blue :distant-foreground "white" :foreground red)
                (:background (doom-color 'blue 't) :distant-foreground "white" :foreground (doom-color 'red 't))
                t)

    (-colorize! (:background "blue" :distant-foreground white :foreground red)
                (:background "blue" :distant-foreground (doom-color 'white 't) :foreground (doom-color 'red 't))
                t)

    (-colorize! (:background blue :distant-foreground white :foreground "red")
                (:background (doom-color 'blue 't) :distant-foreground (doom-color 'white 't) :foreground "red")
                t)))

(ert-deftest doom-themes-color-quoting-subst ()
  "TODO"
  (-with-colors! ((red "#FF0000" "#AA4444" "red"))
    (-colorize! (:box `(:color red)) (:box `(:color red)) t)

    (-colorize! (:box '(:color red)) (:box '(:color red)) t)

    (-colorize! (:box `(:color ,red)) (:box `(:color ,(doom-color 'red 't))) t)

    (-colorize! (:box '(:color ,red)) (:box '(:color ,red)) t)))

(provide 'doom-themes-color-test)
;;; doom-themes-color-test.el ends here
