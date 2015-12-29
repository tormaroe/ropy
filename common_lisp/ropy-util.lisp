(in-package :cl-user)
(defpackage ropy-util
  (:use cl)
  (:export #:parse-any-value
           #:spacep
           #:string-to-matrix
           #:new-symbol))
(in-package :ropy-util)

(defun parse-any-value (x)
  (with-input-from-string (in x)
    (read in)))

(defun spacep (x)
  (eql x #\space))

(defun list-padder (length pad-value)
  (lambda (list)
    (if (> length (length list))
      (append list (loop for i
                         from (length list) 
                         below length 
                         collect pad-value))
      list)))

(defun string-to-chars (s)
  (loop for c across s collect c))

(defun string-to-matrix (s)
  (let* ((lists (loop for line 
                      in (cl-ppcre:split "\\r?\\n" s)
                      collect (string-to-chars line)))
         (first-dimention (length lists))
         (second-dimention (apply #'max (mapcar #'length lists))))
    (make-array 
      (list first-dimention second-dimention)
      :element-type 'character
      :initial-contents
      (mapcar (list-padder second-dimention #\space)
              lists))))

;; Not working as I hoped
(defun new-symbol (&rest args)
  (intern (format nil "~{~(~a~)~}" args)))