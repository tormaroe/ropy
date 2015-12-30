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

(defun dbg (p token)
  (format t "~a => [~{~a,~}~%"
          token
          (reverse (program-stack p))))

(defstruct direction keyword oposite i j)

(defvar directions2
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

(defvar directions (mapcar #'direction-keyword directions2))

(defun get-direction (key)
  (find key directions2 :key #'direction-keyword))

(defun coordinates-for-direction (p direction)
  (let ((i (program-i p)) 
        (j (program-j p))
        (offsets (get-direction direction)))
    (values (+ i (direction-i offsets))
            (+ j (direction-j offsets)))))

(defun move (p direction)
  (multiple-value-bind (i j)
      (coordinates-for-direction p direction)
    (setf (program-i p) i)
    (setf (program-j p) j)
    (setf (program-previous-direction p) direction)))

(defun oposite (direction)
  (direction-oposite (get-direction direction)))

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
        (if (eq 0 (result p))
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

(defmacro defop (name token &body body)
  `(progn 
     (defun ,name (%) ,@body)
     (push (cons ,token (function ,name)) *operations*)))

(defmacro defop-push (name token &body body)
  `(defop ,name ,token
     (push-value % (progn ,@body))))

(defmacro defop-binary (name token &body body)
  `(defop-push ,name ,token
     (let ((a (op-pop %))
           (b (op-pop %)))
       ,@body)))

(defop op-pop      
  #\? 
  (pop (program-stack %)))

(defop-binary op-join     
  #\& 
  (parse-any-value (format nil "~a~a" a b)))

(defop-binary op-add      
  #\+ 
  (+ a b))

(defop-binary op-subtract 
  #\- 
  (- a b))

(defop-binary op-multiply 
  #\* 
  (* a b))

(defop-binary op-devide   
  #\/ 
  (/ a b))

(defop-binary op-modulo   
  #\% 
  (mod a b))

(defop-binary op-swap     
  #\< 
  (push-value % a) b)

(defop-push op-not      
  #\! 
  (if (plusp (op-pop %)) 0 1))

(defop op-duplicate 
  #\> 
  (let ((x (op-pop %)))
    (push-value % x)
    (push-value % x)))

(defop-push op-stringify-stack 
  #\" 
  (format nil "~{~a~}" 
          (mapcar #'code-char 
                  (program-stack %))))

(defop op-put
  #\[
  (let ((data (op-pop %))
        (location (op-pop %)))
    (push (cons location data)
          (program-memory %))))

(defop-push op-get
  #\]
  (let ((location (op-pop %)))
    (cdr (assoc location (program-memory %)))))

(defun operationp (token)
  (cdr (assoc token *operations*)))

(defun evaluate (p)
  (let ((token (current-token p)))
    (cond
      ((digit-char-p token) 
       (push-value p))
      ((operationp token) ;; TODO: optimize away second call to operationp
       (funcall (operationp token) p)))
    (unless (program-silent p)
      (dbg p token))
    (move-next p)))

(defun execute (p)
  (loop until (program-done p)
        do (evaluate p)
        finally (return p)))
