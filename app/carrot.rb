Maw!

def reinit!
  $top_level.instance_eval { init }
end

init {
  log_info "Carrot is initialising"
  start = Time.now

  $nes = Optcarrot::NES.new(['--video=none', '--audio=none', 'lib/optcarrot/examples/Lan_Master.nes'])
  log_info "Carrot initialised. Took #{start-Time.now}s"
}

tick {
  solids << [0, 0, 1280, 720, 255, 0, 0]
}

puts "Hello from Carrot"
