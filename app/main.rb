require 'smaug.rb'
require 'lib/optcarrot-combined.rb'
require 'app/requires.rb'

class Fixnum
  def [](index)
    to_s(2)[index].to_i
  end
end

class Regexp
  def self.compile str
    log_info "[Regexp.compile] #{str}"
    new str
  end

  attr_accessor :str
  protected :str=

  def initialize str
    @str = str
  end

  def to_s
    "<Regexp #{@str}>"
  end

  def method_missing name, *args, &blk
    log_info "#{self} method_missing: #{name} #{args} #{blk}"
  end
end

class LoadError < StandardError
end
