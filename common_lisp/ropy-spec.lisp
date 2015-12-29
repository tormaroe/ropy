(in-package :cl-user)
(defpackage ropy-spec
  (:use cl ropy))
(in-package :ropy-spec)

(defun example (description &key source expected silent)
  "Tiny test framework"
  (let ((actual (result (execute (parse source silent)))))
    (if (equal actual expected)
      (format t "~&[ PASSED  ]: '~a'~%" description)
      (format t "~&[  FAILED ]: '~a'~%Expected: ~a~%Actual  : ~a~%" 
              description expected actual))))

;;;;
;;;; Ropy Specification
;;;;

(example "Empty program"
         :source "            "
         :expected nil)

(example "Put 1 on the stack"
         :source "     1      "
         :expected 1)

(example "Put 10 on the stack by joining 0 and 1"
         :source "      01&      "
         :expected 10)

(example "Put 9 on the stack by adding 3, 3, and 3"
         :source "      333++      "
         :expected 9)