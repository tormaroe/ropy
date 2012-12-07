
class Ropy

  def initialize source
    @stack = []
    @tokens = source.split("\n").map{|line| line.split // }
    @i, @j = 0, 0
    @done = false
    
    @operations = {
      '&' => :join,
      '+' => :add
    }

    seek_token while current == ' ' and not @done
  end
  
  def seek_token
    if @j < @tokens[@i].size
      move :east
    elsif @i < @tokens.size
      @i += 1
      @j = 0
    else
      @done = true  
    end
  end

  def current
    @tokens[@i][@j]
  end

  def move_next
    #if @tokens[@i].size < @j + 1 and not (@tokens[@i][@j + 1] == ' ')
      move :east
=begin
      Her må jeg holde track på hva forrige celle var, den kan jeg ikke gå til
      Og så må jeg skjekke alle andre retninger.
        Hvis det bare finnes en mulig retning å gå, så går vi dit.
        Hvis det finnes 2 retninger er det en if-test
        Hvis det finnes mer enn 2 har vi en syntax error
=end
  end

  def move direction
    case direction
    when :east
      @j += 1
    end
  end

  def execute
    evaluate until @done
  end

  def digit? token
    token.ord >= 48 and token.ord <= 57
  end

  def evaluate
    token = current
    if digit? token
      push
    elsif token == ' '
      @done = true
    elsif @operations.include? token
      self.send @operations[token]
    end
    move_next
  end

  def debug txt
    puts "<#{@stack.join(" ")}> #{txt}"
  end

  def push   ; @stack << current.to_i ; end
  def pop    ; @stack.pop ; end
  def add    ; @stack << pop + pop ; end
  def join   ; top = pop ; @stack << "#{pop}#{top}".to_i ; end
  def result ; @stack.last ; end
end
