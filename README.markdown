**Ropy** is a stack-based, esoteric, 2D programming language. It's mostly works the same way as [Befunge](http://esolangs.org/wiki/Befunge), but the movement strategy of the instruction pointer is, as far as the author knows, unique to Ropy. 

The original interpreter is written in Ruby, and has now been ported to Common Lisp as well.

##Why?##
This programming language is my entry into the [PLT Games](http://www.pltgames.com/) for December 2012.

##Code sample##
This solution to [Project Euler problem #1](http://projecteuler.net/problem=1) is the first real program ever written in Ropy:

       0_99872***-______   (c) Torbjorn Maro    __+___
                       |                       |      |
    This ROPY program  |             ??        1      <  <= REDUCE LOOP
    calculates the     @1____?___>!_|  |___0___|_?_<__|     MAKING SUM
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

##Usage##
In order to run Ropy you need to have Ruby installed. You may then run the interpreter from the command line, giving it a file with Ropy-code to evaluate as the first and only argument.

    PS> .\ropy.rb .\examples\euler1.ropy

The output from this would be:

    Ropy version 0.1
    Executing file .\examples\euler1.ropy
    Result => 233168

You may also provide the v-option for verbose output. This will display the content of the stack after every instruction. An example of this output can be seen further down in the README. 

    PS> .\ropy.rb -v .\examples\euler1.ropy

##How it works##
###Initialization and program flow###
First the source is scanned from left to right, top to bottom, to find the first non-whitespace character; this is the first instruction. The next instruction is then found by moving the instruction pointer (IP) to a non-whitespace character one step in any direction (north, north-east, east, south-east, south, south-west, west, north-west).

The previous instruction will never be selected as the next instruction, leaving only seven possible directions. Of the possibilities that are left, the interpreter will select the first in the clockwise direction from the direction from the previous IP position. Given these "cells":

    +--------------+--------------+--------------+
    |              |              |              |
    |   Previous   |              |              |
    |      IP      |              |      A       |
    |   position   |              |              |
    |              |              |              |
    +--------------+--------------+--------------+
    |              |              |              |
    |              |              |              |
    |              |     _IP_     |      B       |
    |              |              |              |
    |              |              |              |
    +--------------+--------------+--------------+
    |              |              |              |
    |              |              |              |
    |      E       |      D       |      C       |
    |              |              |              |
    |              |              |              |
    +--------------+--------------+--------------+ 

The next instruction would be **A**. This is only the case though if the topmost value on the stack is a number other than zero. If it is zero, the next instruction will be selected counter clockwise - **E** would be the next instruction.

The program runs until there are no more cells for the IP to move to.

###Instruction###

| Character | Instruction                                       |
| --------- | ------------------------------------------------- |
|   0 - 9   | Pushes the number on the stack                    |
|     +     | Adds two topmost numbers and push result to stack |
|     -     | Subtracts two topmost, push to stack              |
|     *     | Multiplies two topmost, push to stack             |
|     /     | Divide two topmost, push to stack                 |
|     <     | Swap two topmost values                           |
|     >     | Duplicate topmost value                           |
|     ?     | Pop and discard topmost value                     |
|     %     | Topmost modulo next value, push result to stack   |
|     !     | NOT-operator, transforms topmost value to 1 if it is 0, and to 0 if it is any other number           |
|     &     | Join two topmost numbers into new number. The program **001&** will result in the number **100** on the stack          |
|     "     | Stringify stack will assume all values on the stack are ASCII-codes. The stack will be left with a signle string |
|     #     | Pops the topmost element off the stack and prints it. |
|     [     | Puts topmost value into memory at location found on next value on stack. |
|     ]     | Gets value stored at the location found on top of stack. Pushes in onto the stack. |
|   Other   | Other characters does not modify the stack, bu are valid placeholders for the IP.        |

##Another example##
If you have understood how Ropy works, you should now be able to understand what the following program does:

    ?_1     __
       x   2  |
        x x   3
    _++__o____4
    +     x
    +      x_5_
    +         |
    |_+++9876_|

Here is a dump of how the instructions modify the stack:

    ? => [
    _ => [
    1 => [1
    x => [1
    x => [1
    o => [1
    x => [1
    2 => [1,2
    _ => [1,2
    _ => [1,2
    | => [1,2
    3 => [1,2,3
    4 => [1,2,3,4
    _ => [1,2,3,4
    _ => [1,2,3,4
    _ => [1,2,3,4
    x => [1,2,3,4
    x => [1,2,3,4
    _ => [1,2,3,4
    5 => [1,2,3,4,5
    _ => [1,2,3,4,5
    | => [1,2,3,4,5
    | => [1,2,3,4,5
    _ => [1,2,3,4,5
    6 => [1,2,3,4,5,6
    7 => [1,2,3,4,5,6,7
    8 => [1,2,3,4,5,6,7,8
    9 => [1,2,3,4,5,6,7,8,9
    + => [1,2,3,4,5,6,7,17
    + => [1,2,3,4,5,6,24
    + => [1,2,3,4,5,30
    _ => [1,2,3,4,5,30
    | => [1,2,3,4,5,30
    + => [1,2,3,4,35
    + => [1,2,3,39
    + => [1,2,42
    _ => [1,2,42
    + => [1,44
    + => [45
    _ => [45
    x => [45
    x => [45
    1 => [45,1
    _ => [45,1
    ? => [45

The result is what's left on the top of the stack. The program calculates the sum of all digits from 1 to 9.

##Hello, World!##
And finally, here is the obligatory *hello, world* program - just as [Kernighan & Ritchi](http://en.wikipedia.org/wiki/The_C_Programming_Language) did in 1978. This one acts as a programming language logo as well.

    |   __+8>____-<3>____+6>__ _________________________
    |  |                      |                         |
    |  |   __>-<3>__&&>>1__   |  _____   ____  _______  |  __
    |  |  |                |  | |  __ \ / __ \|  __ \ \ | / /
    |  |  |                |  | | |__) | |  | | |__) \ \_/ / 
    |  |  |__>7<-__>3+__"  |  | |  _  /| |  | |  ___/ \   /  
    |  |                   |  | | | \ \| |__| | |      | |   
    |  |__65*2+____67*2<+__|  | |_|  \_\\____/|_|      |_|   
    |                         |                         |
    |____001&&________>8+_____|__+ - * / < > ? % ! & "__|

Ropy has a high potential for creating programs that are also ASCII art :)

##License##
Copyright (c) 2012 Torbjørn Marø

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

