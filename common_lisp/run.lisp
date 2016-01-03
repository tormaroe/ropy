;;;;
;;;; This file loads Ropy, optionally runs the tests
;;;; and provide some usage information. Depends on Quicklisp.
;;;;

;;; Load external dependencies..
(ql:quickload "cl-ppcre")
(ql:quickload "anaphora")

;;; ropy-util contain some common utility functions
(load "ropy-util")

;;; Defines a struct to keep the state of a ropy program
;;; and some functions to query it
(load "ropy-state")

;;; Functions to work with moving around in the 2D world
;;; of a ropy program
(load "ropy-direction")

;;; Defines the operations you program with in ropy
(load "ropy-operation")

;;; This defines the actual interpreter and the public API
(load "ropy")

;;; Maybe run spec tests
(when (y-or-n-p "Do you want to run the specification test suite?")
  (load "ropy-spec"))

(use-package :ropy)

;;; Some usage info is always nice...
(format t "
  Use #'parse to prepare a program from source.
  Example (parse \" 123++ \") ==> program
  Or (parse-file \"some/path/file.ropy\") ==> program

  Use #'execute to run a program to completion.
  Example (execute *) ==> program

  Use #'result to get the end value.
  Example (result *) ==> 6

  Have fun!
  ")