(in-package :cl-user)
(defpackage ropy-direction
  (:documentation "Helper functions to move around in Ropy 2D space.")
  (:use cl ropy-state)
  (:export :directions
           #:oposite
           #:token-peeker
           #:move))
(in-package :ropy-direction)

(defstruct direction 
  keyword 
  oposite 
  i 
  j
  )

(defvar directions-internal
  (list (make-direction :keyword :east       
                        :oposite :west       
                        :i 0 :j 1)
        (make-direction :keyword :south-east 
                        :oposite :north-west 
                        :i 1 :j 1)
        (make-direction :keyword :south      
                        :oposite :north 
                        :i 1 :j 0)
        (make-direction :keyword :south-west 
                        :oposite :north-east
                        :i 1 :j -1)
        (make-direction :keyword :west       
                        :oposite :east
                        :i 0 :j -1)
        (make-direction :keyword :north-west 
                        :oposite :south-east
                        :i -1 :j -1)
        (make-direction :keyword :north      
                        :oposite :south
                        :i -1 :j 0)
        (make-direction :keyword :north-east 
                        :oposite :south-west
                        :i -1 :j 1)))

(defvar directions 
  (mapcar #'direction-keyword directions-internal))

(defun get-direction (key)
  (find key directions-internal :key #'direction-keyword))

(defun oposite (direction)
  (direction-oposite (get-direction direction)))

(defun coordinates-for-direction (p direction)
  (let ((i (program-i p)) 
        (j (program-j p))
        (offsets (get-direction direction)))
    (values (+ i (direction-i offsets))
            (+ j (direction-j offsets)))))

(defun valid-coordinates-p (p i j)
  (and (not (minusp i))
       (not (minusp j))
       (<= i (max-i p))
       (<= j (max-j p))))

(defun token-peeker (p)
  (lambda (direction)
    (multiple-value-bind (i j)
        (coordinates-for-direction p direction)
      (cond
        ((eq direction (oposite (program-previous-direction p))) #\space)
        ((not (valid-coordinates-p p i j)) #\space)
        (t (token-at p i j))))))

(defun move (p direction)
  (multiple-value-bind (i j)
      (coordinates-for-direction p direction)
    (setf (program-i p) i)
    (setf (program-j p) j)
    (setf (program-previous-direction p) direction)))