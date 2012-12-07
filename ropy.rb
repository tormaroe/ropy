
class Ropy
  attr_accessor :silent

  def initialize source
    @stack = []
    @tokens = source.split("\n").map{|line| line.split // }
    @i, @j = 0, 0
    @done = false
    @silent = false
    
    @operations = {
      '&' => :join,
      '+' => :add,
      '-' => :subtract,
      '*' => :multiply,
      '/' => :devide,
      '<' => :swap,
      '>' => :duplicate,
      '"' => :stringify_stack,
      '?' => :pop,
      '%' => :modulo,
      '!' => :not
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
    debug token unless @silent
    move_next
  end

  def debug txt
    puts " #{txt} => [#{@stack.join(",")}"
  end

  def push      ; @stack << current.to_i                      ; end
  def pop       ; @stack.pop                                  ; end
  def add       ; @stack << pop + pop                         ; end
  def multiply  ; @stack << pop * pop                         ; end
  def subtract  ; @stack << pop - pop                         ; end
  def modulo    ; @stack << pop % pop                         ; end
  def devide    ; @stack << pop / pop                         ; end
  def join      ; @stack << "#{pop}#{pop}".to_i               ; end
  def swap      ; a,b = pop,pop ; @stack << a ; @stack << b   ; end
  def duplicate ; x = pop ; @stack << x ; @stack << x         ; end
  def result    ; @stack.last                                 ; end
  def not       ; @stack << (if pop > 0 then 0 else 1 end)    ; end

  def stringify_stack
    tmp = @stack.map{|x| x.chr }.join.reverse
    @stack = []
    @stack << tmp
  end
end
