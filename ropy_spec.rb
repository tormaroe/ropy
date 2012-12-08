
require './ropy.rb'
### --------------------------------------- TINY TEST FRAMEWORK ---------------------------------------------------
def example spec
  begin
    ropy = Ropy.new spec[:source]
    ropy.silent = spec[:silent]
    ropy.execute
    if ropy.result == spec[:expected_result]
      puts "[ PASSED  ]: '#{spec[:description]}'"
    else
      puts "[  FAILED ]: '#{spec[:description]}'\n\tExpected: #{spec[:expected_result]}\n\tActual  : #{ropy.result}"
    end
  rescue Exception => e
    puts "[EXCEPTION]: '#{spec[:description]}'\n\t#{e}"
  end
end

### --------------------------------------------- TESTS -----------------------------------------------------------

example :description => 'Empty program',
  :source            => %(             ),
  :expected_result   => nil,
  :silent            => true

example :description => 'Put 1 on the stack',
  :source            => %(      1      ),
  :expected_result   => 1,
  :silent            => true

example :description => 'Put 10 on the stack by joining 0 and 1',
  :source            => %(      01&      ),
  :expected_result   => 10,
  :silent            => true

example :description => 'Put 9 on the stack by adding 3, 3, and 3',
  :source            => %(     333++      ),
  :expected_result   => 9,
  :silent            => true

example :description => 'More arithmetics..',
  :source            => %(   229-2*6+/    ),
  :expected_result   => 10,
  :silent            => true

example :description => 'Modulo 1',
  :source            => %(   35%    ),
  :expected_result   => 2,
  :silent            => true

example :description => 'Modulo 2',
  :source            => %(   36%    ),
  :expected_result   => 0,
  :silent            => true

example :description => 'Not 1',
  :source            => %(   3!    ),
  :expected_result   => 0,
  :silent            => true

example :description => 'Not 2',
  :source            => %(   0!    ),
  :expected_result   => 1,
  :silent            => true

example :description => 'Pop an element off the stack',
  :source            => %(   12?   ),
  :expected_result   => 1,
  :silent            => true

example :description => 'Swap position of two elements',
  :source            => %(   10<&   ),
  :expected_result   => 10,
  :silent            => true

example :description => 'Duplicate an element',
  :source            => %(   3>+   ),
  :expected_result   => 6,
  :silent            => true

example :description => 'Create text by joining stack',
  :source            => %( 33&___853**___9<-___>01&___+__"   ),
  :expected_result   => "yo!",
  :silent            => true

example :description => 'The source can be vertical',
  :source            => %(    
                                2
                                3
                                *
                         ),
  :expected_result   => 6,
  :silent            => true

example :description => 'The source can rope around in any direction',
  :source            => %(    
                                2
                                 33
                                  *
                               +2*
                         ),
  :expected_result   => 20,
  :silent            => true


example :description => 'Conditions: positive number means select path clockwise',
  :source            => %(    
                                1_5+
                                |
                                4
                                +
                         ),
  :expected_result   => 6,
  :silent            => true

example :description => 'Conditions: zero means select path counter clockwise',
  :source            => %(    
                                0_5+
                                |
                                4
                                +
                         ),
  :expected_result   => 4,
  :silent            => true

example :description => 'Euler 1',
  :source            => %(    
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
                         ),
  :expected_result   => 233168,
  :silent            => true

example :description => 'Another sample (for README)',
  :source            => %(    
    ?_1     __
       x   2  |
        x x   3
    _++__o____4
    +     x
    +      x_5_
    +         |
    |_+++9876_|
                         ),
  :expected_result   => 45,
  :silent            => true


example :description => 'Hello, World!',
  :source            => %(    
    001&&_>_8+_>_6+_>_3<-_>_8+__
                               |
    ___+<2*76___________+2*65__|
   |
   |_1>>&&_>_3<->>_7<-_>_3+____"
                         ),
  :expected_result   => "hello, world",
  :silent            => true 

example :description => 'Hello, World! v2',
  :source            => %(    
    0     |()>()|     |()>()|     +()_()6     |()>()|     |()>()|
    0     |     |     |     |     2     7     &     3     |     |
    1     +     6     -     8     *     *     &     <     -     3
    &     8     +     <     +     5     2     >     -     <     +
    &     |     |     3     |     6     <     >     >     7     |
    |()>()|     |()>()|     |()_()|     (+)_(1)     |()>()|     "
                         ),
  :expected_result   => "hello, world",
  :silent            => true


example :description => 'Hello, World! v3',
  :source            => %[    

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

],
  :expected_result   => "hello, world",
  :silent            => true

