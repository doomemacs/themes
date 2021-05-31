;;; doom-themes-test.el

(defvar doom-themes-dir
  (expand-file-name "../" (file-name-directory load-file-name)))

(mapc
 (lambda (file)
   (let ((theme (intern (string-remove-suffix "-theme" (file-name-base file)))))
     (eval `(ert-deftest ,(intern (format "doom-themes-load-%s" theme)) ()
              ,(format "Make sure `%s-theme' loads without issue" theme)
              (load-theme ',theme t)
              (should (custom-theme-enabled-p ',theme)))
           t)))
 (file-expand-wildcards
  (expand-file-name "themes/doom-*-theme.el" doom-themes-dir)))

(provide 'doom-themes-test)
;;; doom-themes-test.el ends here
