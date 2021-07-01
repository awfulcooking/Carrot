Maw!

ARGV = ['--headless', '--opt=none', '--load-cpu=golden-cpu.rb', '--load-ppu=golden-ppu.rb', 'lib/optcarrot/examples/Lan_Master.nes']

def reinit!
  $top_level.init
end

init {
  log_info "Carrot is initialising"
  start = Time.now

  $nes = Optcarrot::NES.new(ARGV.dup)
  log_info "Carrot initialised. Took #{start-Time.now}s"
}

tick {
  solids << [0, 0, 1280, 720, 255, 0, 0]
}

puts "Hello from Carrot"
