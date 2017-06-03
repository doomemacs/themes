;;; test-helper.el

(push (expand-file-name "../" (file-name-directory load-file-name)) load-path)
(push (expand-file-name "../themes" (file-name-directory load-file-name)) load-path)

(require 'doom-themes)
(require 'doom-themes-common)

(defalias '-color-p 'doom-themes--colors-p)
(defalias '-build-face 'doom-themes--build-face)

(defmacro -colorize! (rulesA rulesB type)
  `(should
    (equal (doom-themes--colorize ',rulesA ',type)
           ',rulesB)))

(defmacro -with-colors! (colors &rest body)
  (declare (indent defun))
  `(let ((doom-themes--colors '(,@colors)))
     ,@body))

(defmacro -with-faces! (faces &rest body)
  (declare (indent defun))
  `(let ((doom-themes-common-faces ',faces))
     ,@body))

;;; test-helper.el ends here
