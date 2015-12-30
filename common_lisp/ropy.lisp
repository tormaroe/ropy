(in-package :cl-user)
(defpackage ropy
  (:use cl ropy-util ropy-state ropy-direction ropy-operation)
  (:export #:parse
           #:execute
           #:result))
(in-package :ropy)

(defun result (p)
  "Returns the top of the stack, which is the
   result of a program run to it's completion."
  (car (program-stack p)))

(defun dbg (p token)
  (format t "~a => [~{~a,~}~%"
          token
          (reverse (program-stack p))))

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
    (let ((p (make-program :tokens (string-to-matrix source)
                           :silent silent)))
      (program-seek-token p)
      p)))

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
