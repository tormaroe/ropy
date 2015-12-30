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
         :expected nil
         :silent t)

(example "Put 1 on the stack"
         :source "     1      "
         :expected 1
         :silent t)

(example "Put 10 on the stack by joining 0 and 1"
         :source "      01&      "
         :expected 10
         :silent t)

(example "Put 9 on the stack by adding 3, 3, and 3"
         :source "      333++      "
         :expected 9
         :silent t)

(example "Put 5 and 3 in memory and retrieve it"
         :source "   75[63[6]7]  "
         :expected 5
         :silent t)

(example "More arithmetics.."
         :source "   229-2*6+/    "
         :expected 10
         :silent t)

(example "Modulo 1" :source "   35%    " :expected 2 :silent t)
(example "Modulo 2" :source "   36%    " :expected 0 :silent t)

(example "Not 1" :source "   3!   " :expected 0 :silent t)
(example "Not 2" :source "   0!   " :expected 1 :silent t)


(example "Pop an element off the stack"
         :source "      12?      "
         :expected 1
         :silent t)

(example "Swap position of two elements"
         :source "      10<&     "
         :expected 10
         :silent t)

(example "Duplicate an element"
         :source "      3>+      "
         :expected 6
         :silent t)

(example "Create text by joining stack"
         :source " 33&___853**___9<-___>01&___+__\"   "
         :expected "yo!"
         :silent t)

(example "The source can be vertical"
         :source "
                    2
                    3
                    *
         "
         :expected 6
         :silent t)

(example "The source can rope around in any direction"
         :source "
                    2
                     33
                      *
                   +2*
         "
         :expected 20
         :silent t)

(example "Conditions: positive number means select path clockwise"
         :source "
                    1_5+
                    |
                    4
                    +
         "
         :expected 6
         :silent t)

(example "Conditions: zero means select path counter clockwise"
         :source "
                    0_5+
                    |
                    4
                    +
         "
         :expected 4
         :silent t)

(example "Euler 1"
         :source "    
    0_99872***-______   (c) Torbjorn Maro    __+___
                    |                       |      |
 This ROPY program  |             ??        1      <
 calculates the     @1____?___>!_|  |___0___|_?_<__|
 sum of all the        |         |                 |
 multiples of 3 or     0         ?                 ?
 5 below 1000.         -         >                 |
                       <         3          THIS IS THE END
                       1         <
          ______?______|         %
          |                      |
          |___0_>?__MOD_THREE____@__?____>5<%__
          |                     |             |
          |   +------------+    1             |
          |   |            |    ?             |
          |   |            |    |             |
          |   +------------+    |___MOD_FIVE__@
          |                                   |
          |_______________NOT_INCLUDED________|
         "
         :expected 233168
         :silent t)

(example "Another sample (for README)"
         :source "    
                    ?_1     __
                       x   2  |
                        x x   3
                    _++__o____4
                    +     x
                    +      x_5_
                    +         |
                    |_+++9876_|
          "
          :expected 45
          :silent t)

(example "Hello, World!"
         :source "

    001&&_>_8+_>_6+_>_3<-_>_8+__
                               |
    ___+<2*76___________+2*65__|
   |
   |_1>>&&_>_3<->>_7<-_>_3+____\"
         
         "
         :expected "hello, world"
         :silent t)

(example "Hello, World! v2"
         :source "
    0     |()>()|     |()>()|     +()_()6     |()>()|     |()>()|
    0     |     |     |     |     2     7     &     3     |     |
    1     +     6     -     8     *     *     &     <     -     3
    &     8     +     <     +     5     2     >     -     <     +
    &     |     |     3     |     6     <     >     >     7     |
    |()>()|     |()>()|     |()_()|     (+)_(1)     |()>()|     \"
         "
         :expected "hello, world"
         :silent t)

(example "Hello, World! v3"
         :source " 

    |   __+8>____-<3>____+6>__ _________________________
    |  |                      |                         |
    |  |   __>-<3>__&&>>1__   |  _____   ____  _______  |  __
    |  |  |                |  | |  __ \\ / __ \\|  __ \\ \\ | / /
    |  |  |                |  | | |__) | |  | | |__) \\ \\_/ / 
    |  |  |__>7<-__>3+__\"  |  | |  _  /| |  | |  ___/ \\   /  
    |  |                   |  | | | \\ \\| |__| | |      | |   
    |  |__65*2+____67*2<+__|  | |_|  \\_\\\\____/|_|      |_|   
    |                         |                         |
    |____001&&________>8+_____|__+ - * / < > ? % ! & \"__|

         "
         :expected "hello, world"
         :silent t)