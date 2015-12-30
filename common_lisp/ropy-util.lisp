(in-package :cl-user)
(defpackage ropy-util
  (:documentation "Random utility functions used elsewhere.")
  (:use cl)
  (:export #:parse-any-value
           #:spacep
           #:string-to-matrix))
(in-package :ropy-util)

(defun parse-any-value (x)
  (with-input-from-string (in x)
    (read in)))

(defun spacep (x)
  (eql x #\space))

(defun list-padder (length pad-value)
  "Returns a function which given a list will pad it
   to the specified length with pad-value."
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
  "Accepts a multiline string and transforms it into 
   a type (simple-array character (* *)) containing
   all the characters. When all lines are not of equal
   length, missing characters will be filled in using #\\space."
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
