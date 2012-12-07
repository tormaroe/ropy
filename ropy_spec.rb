
require './ropy.rb'

def example spec
  begin
    ropy = Ropy.new spec[:source]
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
  :expected_result   => 1

example :description => 'Put 10 on the stack by joining 1 and 0',
  :source            => %(      10&      ),
  :expected_result   => 10

example :description => 'Put 9 on the stack by adding 3, 3, and 3',
  :source            => %(     333++      ),
  :expected_result   => 9

