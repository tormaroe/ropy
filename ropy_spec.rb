
require './ropy.rb'

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

# TODO: empty source

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
