(in-package :cl-user)
(defpackage ropy
  (:use cl)
  (:export #:parse
           #:execute
           #:result))
(in-package :ropy)

(defun parse-any-value (x)
  (with-input-from-string (in x)
    (read in)))

(defun make-list-padder (length pad-value)
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
      (mapcar (make-list-padder second-dimention #\space)
              lists))))

(defstruct program
  stack memory tokens 
  i j
  done silent previous-direction)

(defun program-max-i (p)
  (1- (array-dimension (program-tokens p) 0)))

(defun program-max-j (p)
  (1- (array-dimension (program-tokens p) 1)))

(defun program-token-at (p i j)
  (aref (program-tokens p) i j))

(defun program-current-token (p)
  (program-token-at p (program-i p)
                      (program-j p)))

(defun result (p)
  (car (program-stack p)))

(defun coordinates-for-direction (p direction)
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

(defun program-move (p direction)
  (multiple-value-bind (i j)
      (coordinates-for-direction p direction)
    (setf (program-i p) i)
    (setf (program-j p) j)
    (setf (program-previous-direction p) direction)))

(defvar directions
  '(:east :south-east :south :south-west
    :west :north-west :north :north-east))

(defun oposite (direction)
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
       (<= i (program-max-i p))
       (<= j (program-max-j p))))

(defun token-peeker (p)
  (lambda (direction)
    (multiple-value-bind (i j)
        (coordinates-for-direction p direction)
      (cond
        ((eq direction (oposite (program-previous-direction p))) #\space)
        ((not (valid-coordinates-p p i j)) #\space)
        (t (program-token-at p i j))))))

(defun spacep (x)
  (eql x #\space))

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
                     (program-move p (nth i directions))
                     (return)))
          (loop for x 
                from (+ came-from-index 1)
                upto (+ came-from-index 9)
                for i = (mod x 8)
                when (not (spacep (nth i neighbors)))
                do (progn 
                     (program-move p (nth i directions))
                     (return)))
          )))))

(defun program-seek-token (p)
  (when (and (not (program-done p))
             (eql (program-current-token p) #\space))
    (cond
      ((< (program-j p) (program-max-j p))
       (program-move p :east)
       (program-seek-token p))
      ((< (program-i p) (program-max-i p))
       (incf (program-i p))
       (setf (program-j p) 0)
       (program-seek-token p))))
  p)

(defun parse (source &optional silent)
  (let ((p (make-program :stack () :memory ()
                         :tokens (string-to-matrix source)
                         :i 0 :j 0
                         :silent silent
                         :previous-direction :east)))
    (program-seek-token p)))

(defun operation-push (p &optional value)
  (push (if value 
           value 
           (digit-char-p (program-current-token p)))
        (program-stack p)))

(defun operation-pop (p)
  (pop (program-stack p)))

(defun operation-join (p)
  (operation-push p
                  (parse-any-value 
                    (format nil "~a~a" 
                            (operation-pop p) 
                            (operation-pop p)))))

(defparameter *operations*
  (list (cons #\& #'operation-join)))

(defun operationp (token)
  (cdr (assoc token *operations*)))

(defun evaluate (p)
  (let ((token (program-current-token p)))
    (cond
      ((digit-char-p token) (operation-push p))
      ((operationp token) (funcall (operationp token) p))))
    (move-next p)
  p)

(defun execute (p)
  (loop until (program-done p)
        do (evaluate p))
  p)
