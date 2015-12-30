This is a port of the Ropy interpreter from Ruby to Common Lisp. The exact same spec has been used. To load and execute the spec, load the file run.lisp (tested on SBCL). It assumes you have quicklisp, as there is a small dependency to cl-ppcre.

Run.lisp also provides a quick overview of what the different files are.

And there's currently no system definition.

## Excample session

```
* (parse "123++")

#S(ROPY-STATE:PROGRAM
   :STACK NIL
   :MEMORY NIL
   :TOKENS #2A((#\1 #\2 #\3 #\+ #\+))
   :I 0
   :J 0
   :DONE NIL
   :SILENT NIL
   :PREVIOUS-DIRECTION :EAST)

* (execute *)

1 => [1,
2 => [1,2,
3 => [1,2,3,
+ => [1,5,
+ => [6,

#S(ROPY-STATE:PROGRAM
   :STACK (6)
   :MEMORY NIL
   :TOKENS #2A((#\1 #\2 #\3 #\+ #\+))
   :I 0
   :J 4
   :DONE T
   :SILENT NIL
   :PREVIOUS-DIRECTION :EAST)

* (result *)

6
```