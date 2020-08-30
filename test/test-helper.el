;;; test-helper.el -*- lexical-binding: t; -*-

(push (expand-file-name "../" (file-name-directory load-file-name)) load-path)
(push (expand-file-name "../themes" (file-name-directory load-file-name)) load-path)

(require 'doom-themes)
(require 'doom-themes-base)

(defalias '-color-p 'doom-themes--colors-p)
;; (defalias '-build-face 'doom-themes--build-face)

(defun -build-face (face)
  (eval (doom-themes--build-face face)))

(defmacro -colorize! (rulesA rulesB type)
  `(should
    (equal (doom-themes--colorize ',rulesA ',type)
           ',rulesB)))

(defmacro -with-colors! (colors &rest body)
  (declare (indent defun))
  `(let ((doom-themes--colors ',colors))
     (let* (,@colors)
       (setq doom-themes--colors
             (list ,@(cl-loop for (var val) in colors
                              collect `(cons ',var ,val))))
       ,@body)))

(defmacro -with-faces! (faces &rest body)
  (declare (indent defun))
  (let ((doom-themes--faces (doom-themes--apply-faces nil faces)))
    `(let* ((doom-themes--faces ',doom-themes--faces)
            (faces (list ,@(mapcar #'doom-themes--build-face doom-themes--faces))))
       ,@body
       faces)))

;;; test-helper.el ends here
