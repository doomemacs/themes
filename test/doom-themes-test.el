;;; doom-themes-test.el

(defvar doom-themes-dir
  (expand-file-name "../" (file-name-directory load-file-name)))

(ert-deftest doom-themes-load-themes ()
  "TODO"
  (mapc (lambda (file)
          (let ((theme (intern (string-remove-suffix "-theme" (file-name-base file)))))
            (load-theme theme t)
            (should (custom-theme-enabled-p theme))
            (disable-theme theme)))
        (file-expand-wildcards
         (expand-file-name "themes/doom-*-theme.el" doom-themes-dir))))

(provide 'doom-themes-test)
;;; doom-themes-test.el ends here
