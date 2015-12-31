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
      (macrolet ((find-next-and-move (from direction to)
                   `(loop for x from ,from ,direction ,to
                          for i = (mod x 8)
                          when (not (spacep (nth i neighbors)))
                          do (progn 
                               (move p (nth i directions))
                               (return)))))
        (let ((came-from-index (position (oposite (program-previous-direction p)) 
                                         directions)))
          (if (eql 0 (result p))
            (find-next-and-move 
              (+ came-from-index 7) downto (- came-from-index 1))
            (find-next-and-move 
              (+ came-from-index 1) upto (+ came-from-index 9))))))
    p))

(defun parse (source &optional silent)
  (labels ((seek-token (p)
              (when (and (not (program-done p))
                         (eql (current-token p) #\space))
                (cond
                  ((< (program-j p) (max-j p))
                   (move p :east)
                   (seek-token p))
                  ((< (program-i p) (max-i p))
                   (incf (program-i p))
                   (setf (program-j p) 0)
                   (seek-token p))))))
    (let ((p (make-program :tokens (string-to-matrix source)
                           :silent silent)))
      (seek-token p)
      p)))

(defun evaluate (p)
  (let* ((token (current-token p))
         (operation (operationp token)))
    (cond
      ((digit-char-p token) 
       (push-value p))
      (operation
       (funcall operation p)))
    (unless (program-silent p)
      (dbg p token))
    (move-next p)))

(defun execute (p)
  (loop until (program-done p)
        do (evaluate p)
        finally (return p)))
