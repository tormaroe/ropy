(in-package :cl-user)
(defpackage ropy
  (:use cl ropy-util)
  (:export #:parse
           #:execute
           #:result))
(in-package :ropy)

(defstruct program
  stack 
  memory 
  tokens 
  i 
  j
  done 
  silent 
  previous-direction)

(defun max-i (p)
  (1- (array-dimension (program-tokens p) 0)))

(defun max-j (p)
  (1- (array-dimension (program-tokens p) 1)))

(defun token-at (p i j)
  (aref (program-tokens p) i j))

(defun current-token (p)
  (token-at p (program-i p) (program-j p)))

(defun result (p)
  (car (program-stack p)))

(defvar directions ;; TODO: Make direction a struct
  '(:east :south-east :south :south-west
    :west :north-west :north :north-east))

(defun coordinates-for-direction (p direction) ;; TODO: Add modifiers to directions
  (let ((i (program-i p)) (j (program-j p)))
    (case direction
      (:east       (values     i  (1+ j)))
      (:west       (values     i  (1- j)))
      (:north      (values (1- i)     j ))
      (:south      (values (1+ i)     j ))
      (:north-east (values (1- i) (1+ j)))
      (:south-east (values (1+ i) (1+ j)))
      (:north-west (values (1- i) (1- j)))
      (:south-west (values (1+ i) (1- j))))))

(defun move (p direction)
  (multiple-value-bind (i j)
      (coordinates-for-direction p direction)
    (setf (program-i p) i)
    (setf (program-j p) j)
    (setf (program-previous-direction p) direction)))

(defun oposite (direction) ;; TODO: Add oposite to directions
  (case direction
    (:west :east)
    (:east :west)
    (:north :south)
    (:south :north)
    (:north-west :south-east)
    (:south-east :north-west)
    (:south-west :north-east)
    (:north-east :south-west)))

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

(defun move-next (p)
  (let ((neighbors (mapcar (token-peeker p) directions)))
    (setf (program-done p)
          (null (remove-if #'spacep neighbors)))

    (unless (program-done p)
      (let ((came-from-index (position (oposite (program-previous-direction p)) 
                                       directions)))
        (if (zerop (result p))
          (loop for x 
                from (+ came-from-index 7)
                downto (- came-from-index 1)
                for i = (mod x 8)
                when (not (spacep (nth i neighbors)))
                do (progn 
                     (move p (nth i directions))
                     (return)))
          (loop for x 
                from (+ came-from-index 1)
                upto (+ came-from-index 9)
                for i = (mod x 8)
                when (not (spacep (nth i neighbors)))
                do (progn 
                     (move p (nth i directions))
                     (return)))
          )))
    p))

(defun parse (source &optional silent)
  (labels ((program-seek-token (p)
              (when (and (not (program-done p))
                         (eql (current-token p) #\space))
                (cond
                  ((< (program-j p) (max-j p))
                   (move p :east)
                   (program-seek-token p))
                  ((< (program-i p) (max-i p))
                   (incf (program-i p))
                   (setf (program-j p) 0)
                   (program-seek-token p))))))
    (let ((p (make-program :stack () :memory ()
                           :tokens (string-to-matrix source)
                           :i 0 :j 0
                           :silent silent
                           :previous-direction :east)))
      (program-seek-token p)
      p)))

(defun push-value (p &optional value)
  (push (or value 
            (digit-char-p (current-token p)))
        (program-stack p)))

(defparameter *operations* ())

(defmacro defoperation (name token &body body)
  `(progn 
     (defun ,name ($)
       ,@body)
     (push (cons ,token (function ,name)) *operations*)))

(defoperation op-pop #\?
  (pop (program-stack $)))

(defoperation op-join #\&
  (push-value $
    (parse-any-value 
      (format nil "~a~a" 
              (op-pop $) 
              (op-pop $)))))

(defoperation op-add #\+
  (push-value $ (+ (op-pop $) (op-pop $))))

(defun operationp (token)
  (cdr (assoc token *operations*)))

(defun evaluate (p)
  (let ((token (current-token p)))
    (cond
      ((digit-char-p token) 
       (push-value p))
      ((operationp token) ;; TODO: optimize away second call to operationp
       (funcall (operationp token) p))))
    (move-next p))

(defun execute (p)
  (loop until (program-done p)
        do (evaluate p)
        finally (return p)))
