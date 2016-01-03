(in-package :cl-user)
(defpackage ropy-operation
  (:documentation "Implements the user operations of Ropy.")
  (:use cl ropy-util ropy-state)
  (:export #:push-value
           #:operationp))
(in-package :ropy-operation)

(defparameter *operations* ()
  "Will become an association list mapping characters (program tokens)
   to functions that will perform the appropriate program operation.")

;;;
;;; defop Macros
;;;
;;  This is mostly masturbatory I guess - I just wanted to see how it would turn out.
;;

(defmacro defop (name token &body body)
  "Defines an operation function and registers it in *operations*.
   The operation will have access to the program state by the anamorphic
   variable 'program'."
  `(progn 
     (defun ,name (program) ,@body)
     (push (cons ,token (function ,name)) *operations*)))

(defmacro defop-push (name token &body body)
  "Like defop, but pushes the result of the operation onto the program stack."
  `(defop ,name ,token
     (push-value program (progn ,@body))))

(defmacro defop-binary (name token &body body)
  "Like defop-push, but pops out two elements from the stack which are bound
   to the anamorphic variables a and b."
  `(defop-push ,name ,token
     (let ((a (:pop program))
           (b (:pop program)))
       ,@body)))

;;;
;;; Exported API
;;;

(defun operationp (token)
  "If token maps to an operation, that operation is returned.
   If not, nil is returned."
  (cdr (assoc token *operations*)))

(defun push-value (p value)
  "Push a value onto the program stack."
  (push value (program-stack p)))

;;;
;;; The operations
;;;

(defop :pop #\? (pop (program-stack program)))

(defop-binary :join #\& (parse-any-value (format nil "~a~a" a b)))

(defop-binary :add #\+ (+ a b))

(defop-binary :subtract #\- (- a b))

(defop-binary :multiply #\* (* a b))

(defop-binary :devide #\/ (/ a b))

(defop-binary :modulo #\% (mod a b))

(defop-binary :swap #\< (push-value program a) b)

(defop-push :not #\! (if (plusp (:pop program)) 0 1))

(defop :duplicate #\> 
  (let ((x (:pop program)))
    (push-value program x)
    (push-value program x)))

(defop-push :stringify-stack #\" 
  (format nil "~{~a~}" 
          (mapcar #'code-char 
                  (program-stack program))))

(defop :put #\[
  (let ((data (:pop program))
        (location (:pop program)))
    (push (cons location data)
          (program-memory program))))

(defop-push :get #\]
  (let ((location (:pop program)))
    (cdr (assoc location (program-memory program)))))

