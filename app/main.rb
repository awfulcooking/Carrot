require 'smaug.rb'
require 'lib/optcarrot-combined.rb'
require 'app/requires.rb'

Maw!

class Fixnum
  def [](index)
    to_s(2)[index].to_i
  end
end

def tick args
  solids << [0,0,1280,720,255,0,0]
  # puts "Hello world"
end
puts "Hello world"
