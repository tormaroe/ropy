(in-package :cl-user)
(defpackage ropy-state
  (:documentation "
      This package defines the program state datastructure
      and some simple functions to investigate it.
    ")
  (:use cl)
  (:export #:program
           #:make-program
           #:program-stack
           #:program-memory
           #:program-tokens
           #:program-i
           #:program-j
           #:program-done
           #:program-silent
           #:program-previous-direction
           #:max-i
           #:max-j
           #:token-at
           #:current-token))
(in-package :ropy-state)

(defstruct program
  (stack () :type list)
  (memory () :type list) ; RAM, will be an association list of variable keys and values
  (tokens nil :type (simple-array character (* *)))
  (i 0 :type fixnum) ; current program pointer, first dimension into tokens
  (j 0 :type fixnum) ; current program pointer, second dimension into tokens
  done 
  silent 
  (previous-direction :east :type keyword))

(defun max-i (p)
  (1- (array-dimension (program-tokens p) 0)))

(defun max-j (p)
  (1- (array-dimension (program-tokens p) 1)))

(defun token-at (p i j)
  (aref (program-tokens p) i j))

(defun current-token (p)
  (token-at p (program-i p) (program-j p)))
