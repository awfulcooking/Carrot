# optcarrot

# optcarrot/nes

module Optcarrot
  FOREVER_CLOCK = 0xffffffff
  RP2A03_CC = 12

  # NES emulation main
  class NES
    FPS = 60

    def initialize(conf = ARGV)
      @conf = Config.new(conf)

      @video, @audio, @input = Driver.load(@conf)

      @cpu =            CPU.new(@conf)
      @apu = @cpu.apu = APU.new(@conf, @cpu, *@audio.spec)
      @ppu = @cpu.ppu = PPU.new(@conf, @cpu, @video.palette)
      @rom  = ROM.load(@conf, @cpu, @ppu)
      @pads = Pads.new(@conf, @cpu, @apu)

      @frame = 0
      @frame_target = @conf.frames == 0 ? nil : @conf.frames
      @fps_history = [] if save_fps_history?
    end

    def inspect
      "#<#{ self.class }>"
    end

    attr_reader :fps, :video, :audio, :input, :cpu, :ppu, :apu

    def reset
      @cpu.reset
      @apu.reset
      @ppu.reset
      @rom.reset
      @pads.reset
      @cpu.boot
      @rom.load_battery
    end

    def step
      @ppu.setup_frame
      @cpu.run
      @ppu.vsync
      @apu.vsync
      @cpu.vsync
      @rom.vsync

      @input.tick(@frame, @pads)
      @fps = @video.tick(@ppu.output_pixels)
      @fps_history << @fps if save_fps_history?
      @audio.tick(@apu.output)

      @frame += 1
      @conf.info("frame #{ @frame }") if @conf.loglevel >= 2
    end

    def dispose
      if @fps
        @conf.info("fps: %.2f (in the last 10 frames)" % @fps)
        if @conf.print_fps_history
          puts "frame,fps-history"
          @fps_history.each_with_index {|fps, frame| puts "#{ frame },#{ fps }" }
        end
        if @conf.print_p95fps
          puts "p95 fps: #{ @fps_history.sort[(@fps_history.length * 0.05).floor] }"
        end
        puts "fps: #{ @fps }" if @conf.print_fps
      end
      if @conf.print_video_checksum && @video.instance_of?(Video)
        puts "checksum: #{ @ppu.output_pixels.pack("C*").sum }"
      end
      @video.dispose
      @audio.dispose
      @input.dispose
      @rom.save_battery
    end

    def run
      reset

      if @conf.stackprof_mode
        out = @conf.stackprof_output.sub("MODE", @conf.stackprof_mode)
        StackProf.start(mode: @conf.stackprof_mode.to_sym, out: out, raw: true)
      end

      step until @frame == @frame_target

      if @conf.stackprof_mode
        StackProf.stop
        StackProf.results
      end
    ensure
      dispose
    end

    private

    def save_fps_history?
      @conf.print_fps_history || @conf.print_p95fps
    end
  end
end


# optcarrot/rom

module Optcarrot
  # Cartridge class (with NROM mapper implemented)
  class ROM
    MAPPER_DB = { 0x00 => self }

    # # These are optional
    # require_relative "mapper/mmc1"
    # require_relative "mapper/uxrom"
    # require_relative "mapper/cnrom"
    # require_relative "mapper/mmc3"

    def self.zip_extract(filename)
      require "zlib"
      bin = File.binread(filename)
      loop do
        sig, _, flags, comp, _, _, _, data_len, _, fn_len, ext_len = bin.slice!(0, 30).unpack("a4v5V3v2")
        break if sig != "PK\3\4".b
        fn = bin.slice!(0, fn_len)
        bin.slice!(0, ext_len)
        data = bin.slice!(0, data_len)
        next if File.extname(fn).downcase != ".nes"
        next if flags & 0x11 != 0
        next if comp != 0 && comp != 8
        if comp == 8
          zs = Zlib::Inflate.new(-15)
          data = zs.inflate(data)
          zs.finish
          zs.close
        end
        return data
      end
      raise "failed to extract ROM file from `#{ filename }'"
    end

    def self.load(conf, cpu, ppu)
      filename = conf.romfile
      basename = File.basename(filename)

      blob = (File.extname(filename) == ".zip" ? zip_extract(filename) : File.binread(filename)).bytes

      # parse mapper
      mapper = (blob[6] >> 4) | (blob[7] & 0xf0)

      klass = MAPPER_DB[mapper]
      raise NotImplementedError, "Unsupported mapper type 0x%02x" % mapper unless klass
      klass.new(conf, cpu, ppu, basename, blob)
    end

    class InvalidROM < StandardError
    end

    def parse_header(buf)
      raise InvalidROM, "Missing 16-byte header" if buf.size < 16
      raise InvalidROM, "Missing 'NES' constant in header" if buf[0, 4] != "NES\x1a".bytes
      raise NotImplementedError, "trainer not supported" if buf[6][2] == 1
      raise NotImplementedError, "VS cart not supported" if buf[7][0] == 1
      raise NotImplementedError, "PAL not supported" unless buf[9][0] == 0

      prg_banks = buf[4]
      chr_banks = buf[5]
      @mirroring = buf[6][0] == 0 ? :horizontal : :vertical
      @mirroring = :four_screen if buf[6][3] == 1
      @battery = buf[6][1] == 1
      @mapper = (buf[6] >> 4) | (buf[7] & 0xf0)
      ram_banks = [1, buf[8]].max

      return prg_banks, chr_banks, ram_banks
    end

    def initialize(conf, cpu, ppu, basename, buf)
      @conf = conf
      @cpu = cpu
      @ppu = ppu
      @basename = basename

      prg_count, chr_count, wrk_count = parse_header(buf.slice!(0, 16))

      raise InvalidROM, "EOF in ROM bank data" if buf.size < 0x4000 * prg_count
      @prg_banks = (0...prg_count).map { buf.slice!(0, 0x4000) }

      raise InvalidROM, "EOF in CHR bank data" if buf.size < 0x2000 * chr_count
      @chr_banks = (0...chr_count).map { buf.slice!(0, 0x2000) }

      @prg_ref = [nil] * 0x10000
      @prg_ref[0x8000, 0x4000] = @prg_banks.first
      @prg_ref[0xc000, 0x4000] = @prg_banks.last

      @chr_ram = chr_count == 0 # No CHR bank implies CHR-RAM (writable CHR bank)
      @chr_ref = @chr_ram ? [0] * 0x2000 : @chr_banks[0].dup

      @wrk_readable = wrk_count > 0
      @wrk_writable = false
      @wrk = wrk_count > 0 ? (0x6000..0x7fff).map {|addr| addr >> 8 } : nil

      init

      @ppu.nametables = @mirroring
      @ppu.set_chr_mem(@chr_ref, @chr_ram)
    end

    def init
    end

    def reset
      @cpu.add_mappings(0x8000..0xffff, @prg_ref, nil)
    end

    def inspect
      [
        "Mapper: #{ @mapper } (#{ self.class.to_s.split("::").last })",
        "PRG Banks: #{ @prg_banks.size }",
        "CHR Banks: #{ @chr_banks.size }",
        "Mirroring: #{ @mirroring }",
      ].join("\n")
    end

    def peek_6000(addr)
      @wrk_readable ? @wrk[addr - 0x6000] : (addr >> 8)
    end

    def poke_6000(addr, data)
      @wrk[addr - 0x6000] = data if @wrk_writable
    end

    def vsync
    end

    def load_battery
      return unless @battery
      sav = @basename + ".sav"
      return unless File.readable?(sav)
      sav = File.binread(sav)
      @wrk.replace(sav.bytes)
    end

    def save_battery
      return unless @battery
      sav = @basename + ".sav"
      puts "Saving: " + sav
      File.binwrite(sav, @wrk.pack("C*"))
    end
  end
end


# optcarrot/mapper/mmc1

module Optcarrot
  # MMC1 mapper: http://wiki.nesdev.com/w/index.php/MMC1
  class MMC1 < ROM
    MAPPER_DB[0x01] = self

    NMT_MODE = [:first, :second, :vertical, :horizontal]
    PRG_MODE = [:conseq, :conseq, :fix_first, :fix_last]
    CHR_MODE = [:conseq, :noconseq]

    def init
      @nmt_mode = @prg_mode = @chr_mode = nil
      @prg_bank = @chr_bank_0 = @chr_bank_1 = 0
    end

    def reset
      @shift = @shift_count = 0

      @chr_banks = @chr_banks.flatten.each_slice(0x1000).to_a

      @wrk_readable = @wrk_writable = true
      @cpu.add_mappings(0x6000..0x7fff, method(:peek_6000), method(:poke_6000))
      @cpu.add_mappings(0x8000..0xffff, @prg_ref, method(:poke_prg))

      update_nmt(:horizontal)
      update_prg(:fix_last, 0, 0)
      update_chr(:conseq, 0, 0)
    end

    def poke_prg(addr, val)
      if val[7] == 1
        @shift = @shift_count = 0
      else
        @shift |= val[0] << @shift_count
        @shift_count += 1
        if @shift_count == 0x05
          case (addr >> 13) & 0x3
          when 0 # control
            nmt_mode = NMT_MODE[@shift      & 3]
            prg_mode = PRG_MODE[@shift >> 2 & 3]
            chr_mode = CHR_MODE[@shift >> 4 & 1]
            update_nmt(nmt_mode)
            update_prg(prg_mode, @prg_bank, @chr_bank_0)
            update_chr(chr_mode, @chr_bank_0, @chr_bank_1)
          when 1 # change chr_bank_0
            # update_prg might modify @chr_bank_0 and prevent updating chr bank,
            # so keep current value.
            bak_chr_bank_0 = @chr_bank_0
            update_prg(@prg_mode, @prg_bank, @shift)
            @chr_bank_0 = bak_chr_bank_0
            update_chr(@chr_mode, @shift, @chr_bank_1)
          when 2 # change chr_bank_1
            update_chr(@chr_mode, @chr_bank_0, @shift)
          when 3 # change png_bank
            update_prg(@prg_mode, @shift, @chr_bank_0)
          end
          @shift = @shift_count = 0
        end
      end
    end

    def update_nmt(nmt_mode)
      return if @nmt_mode == nmt_mode
      @nmt_mode = nmt_mode
      @ppu.nametables = @nmt_mode
    end

    def update_prg(prg_mode, prg_bank, chr_bank_0)
      return if prg_mode == @prg_mode && prg_bank == @prg_bank && chr_bank_0 == @chr_bank_0
      @prg_mode, @prg_bank, @chr_bank_0 = prg_mode, prg_bank, chr_bank_0

      high_bit = chr_bank_0 & (0x10 & (@prg_banks.size - 1))
      prg_bank_ex = ((@prg_bank & 0x0f) | high_bit) & (@prg_banks.size - 1)
      case @prg_mode
      when :conseq
        lower = prg_bank_ex & ~1
        upper = lower + 1
      when :fix_first
        lower = 0
        upper = prg_bank_ex
      when :fix_last
        lower = prg_bank_ex
        upper = ((@prg_banks.size - 1) & 0x0f) | high_bit
      end
      @prg_ref[0x8000, 0x4000] = @prg_banks[lower]
      @prg_ref[0xc000, 0x4000] = @prg_banks[upper]
    end

    def update_chr(chr_mode, chr_bank_0, chr_bank_1)
      return if chr_mode == @chr_mode && chr_bank_0 == @chr_bank_0 && chr_bank_1 == @chr_bank_1
      @chr_mode, @chr_bank_0, @chr_bank_1 = chr_mode, chr_bank_0, chr_bank_1
      return if @chr_ram

      @ppu.update(0)
      if @chr_mode == :conseq
        lower = @chr_bank_0 & 0x1e
        upper = lower + 1
      else
        lower = @chr_bank_0
        upper = @chr_bank_1
      end
      @chr_ref[0x0000, 0x1000] = @chr_banks[lower]
      @chr_ref[0x1000, 0x1000] = @chr_banks[upper]
    end
  end
end


# optcarrot/mapper/uxrom

module Optcarrot
  # UxROM mapper: http://wiki.nesdev.com/w/index.php/UxROM
  class UxROM < ROM
    MAPPER_DB[0x02] = self

    def reset
      @cpu.add_mappings(0x8000..0xffff, @prg_ref, method(:poke_8000))
    end

    def poke_8000(_addr, data)
      @prg_ref[0x8000, 0x4000] = @prg_banks[data & 7]
    end
  end
end


# optcarrot/mapper/cnrom

module Optcarrot
  # CNROM mapper: http://wiki.nesdev.com/w/index.php/CNROM
  class CNROM < ROM
    MAPPER_DB[0x03] = self

    def reset
      @cpu.add_mappings(0x8000..0xffff, @prg_ref, @chr_ram ? nil : method(:poke_8000))
    end

    def poke_8000(_addr, data)
      @chr_ref.replace(@chr_banks[data & 3])
    end
  end
end


# optcarrot/mapper/mmc3

module Optcarrot
  # MMC3 mapper: http://wiki.nesdev.com/w/index.php/MMC3
  class MMC3 < ROM
    MAPPER_DB[0x04] = self

    def init(rev = :B) # rev = :A or :B or :C
      @persistant = rev != :A

      @prg_banks = @prg_banks.flatten.each_slice(0x2000).to_a
      @prg_bank_swap = false

      @chr_banks = @chr_banks.flatten.each_slice(0x0400).to_a
      @chr_bank_mapping = [nil] * 8
      @chr_bank_swap = false
    end

    def reset
      @wrk_readable = true
      @wrk_writable = false

      poke_a000 = @mirroring != :FourScreen ? method(:poke_a000) : nil
      @cpu.add_mappings(0x6000..0x7fff, method(:peek_6000), method(:poke_6000))
      @cpu.add_mappings(0x8000.step(0x9fff, 2), @prg_ref, method(:poke_8000))
      @cpu.add_mappings(0x8001.step(0x9fff, 2), @prg_ref, method(:poke_8001))
      @cpu.add_mappings(0xa000.step(0xbfff, 2), @prg_ref, poke_a000)
      @cpu.add_mappings(0xa001.step(0xbfff, 2), @prg_ref, method(:poke_a001))
      @cpu.add_mappings(0xc000.step(0xdfff, 2), @prg_ref, method(:poke_c000))
      @cpu.add_mappings(0xc001.step(0xdfff, 2), @prg_ref, method(:poke_c001))
      @cpu.add_mappings(0xe000.step(0xffff, 2), @prg_ref, method(:poke_e000))
      @cpu.add_mappings(0xe001.step(0xffff, 2), @prg_ref, method(:poke_e001))

      update_prg(0x8000, 0)
      update_prg(0xa000, 1)
      update_prg(0xc000, -2)
      update_prg(0xe000, -1)
      8.times {|i| update_chr(i * 0x400, i) }

      @clock = 0
      @hold = PPU::RP2C02_CC * 16
      @ppu.monitor_a12_rising_edge(self)
      @cpu.ppu_sync = true

      @count = 0
      @latch = 0
      @reload = false
      @enabled = false
    end

    # prg_bank_swap = F T
    # 0x8000..0x9fff: 0 2
    # 0xa000..0xbfff: 1 1
    # 0xc000..0xdfff: 2 0
    # 0xe000..0xffff: 3 3
    def update_prg(addr, bank)
      bank %= @prg_banks.size
      addr ^= 0x4000 if @prg_bank_swap && addr[13] == 0
      @prg_ref[addr, 0x2000] = @prg_banks[bank]
    end

    def update_chr(addr, bank)
      return if @chr_ram
      idx = addr / 0x400
      bank %= @chr_banks.size
      return if @chr_bank_mapping[idx] == bank
      addr ^= 0x1000 if @chr_bank_swap
      @ppu.update(0)
      @chr_ref[addr, 0x400] = @chr_banks[bank]
      @chr_bank_mapping[idx] = bank
    end

    def poke_8000(_addr, data)
      @reg_select = data & 7
      prg_bank_swap = data[6] == 1
      chr_bank_swap = data[7] == 1

      if prg_bank_swap != @prg_bank_swap
        @prg_bank_swap = prg_bank_swap
        @prg_ref[0x8000, 0x2000], @prg_ref[0xc000, 0x2000] = @prg_ref[0xc000, 0x2000], @prg_ref[0x8000, 0x2000]
      end

      if chr_bank_swap != @chr_bank_swap
        @chr_bank_swap = chr_bank_swap
        unless @chr_ram
          @ppu.update(0)
          @chr_ref.rotate!(0x1000)
          @chr_bank_mapping.rotate!(4)
        end
      end
    end

    def poke_8001(_addr, data)
      if @reg_select < 6
        if @reg_select < 2
          update_chr(@reg_select * 0x0800, data & 0xfe)
          update_chr(@reg_select * 0x0800 + 0x0400, data | 0x01)
        else
          update_chr((@reg_select - 2) * 0x0400 + 0x1000, data)
        end
      else
        update_prg((@reg_select - 6) * 0x2000 + 0x8000, data & 0x3f)
      end
    end

    def poke_a000(_addr, data)
      @ppu.nametables = data[0] == 1 ? :horizontal : :vertical
    end

    def poke_a001(_addr, data)
      @wrk_readable = data[7] == 1
      @wrk_writable = data[6] == 0 && @wrk_readable
    end

    def poke_c000(_addr, data)
      @ppu.update(0)
      @latch = data
    end

    def poke_c001(_addr, _data)
      @ppu.update(0)
      @reload = true
    end

    def poke_e000(_addr, _data)
      @ppu.update(0)
      @enabled = false
      @cpu.clear_irq(CPU::IRQ_EXT)
    end

    def poke_e001(_addr, _data)
      @ppu.update(0)
      @enabled = true
    end

    def vsync
      @clock = @clock > @cpu.next_frame_clock ? @clock - @cpu.next_frame_clock : 0
    end

    def a12_signaled(cycle)
      clk, @clock = @clock, cycle + @hold
      return if cycle < clk
      flag = @persistant || @count > 0
      if @reload
        @reload = false
        @count = @latch
      elsif @count == 0
        @count = @latch
      else
        @count -= 1
      end
      @cpu.do_irq(CPU::IRQ_EXT, cycle) if flag && @count == 0 && @enabled
    end
  end
end


module Optcarrot
  # Cartridge class (with NROM mapper implemented)
  class ROM
    MAPPER_DB = { 0x00 => self }

    # These are optional

    def self.zip_extract(filename)
      bin = File.binread(filename)
      loop do
        sig, _, flags, comp, _, _, _, data_len, _, fn_len, ext_len = bin.slice!(0, 30).unpack("a4v5V3v2")
        break if sig != "PK\3\4".b
        fn = bin.slice!(0, fn_len)
        bin.slice!(0, ext_len)
        data = bin.slice!(0, data_len)
        next if File.extname(fn).downcase != ".nes"
        next if flags & 0x11 != 0
        next if comp != 0 && comp != 8
        if comp == 8
          zs = Zlib::Inflate.new(-15)
          data = zs.inflate(data)
          zs.finish
          zs.close
        end
        return data
      end
      raise "failed to extract ROM file from `#{ filename }'"
    end

    def self.load(conf, cpu, ppu)
      filename = conf.romfile
      basename = File.basename(filename)

      blob = (File.extname(filename) == ".zip" ? zip_extract(filename) : File.binread(filename)).bytes

      # parse mapper
      mapper = (blob[6] >> 4) | (blob[7] & 0xf0)

      klass = MAPPER_DB[mapper]
      raise NotImplementedError, "Unsupported mapper type 0x%02x" % mapper unless klass
      klass.new(conf, cpu, ppu, basename, blob)
    end

    class InvalidROM < StandardError
    end

    def parse_header(buf)
      raise InvalidROM, "Missing 16-byte header" if buf.size < 16
      raise InvalidROM, "Missing 'NES' constant in header" if buf[0, 4] != "NES\x1a".bytes
      raise NotImplementedError, "trainer not supported" if buf[6][2] == 1
      raise NotImplementedError, "VS cart not supported" if buf[7][0] == 1
      raise NotImplementedError, "PAL not supported" unless buf[9][0] == 0

      prg_banks = buf[4]
      chr_banks = buf[5]
      @mirroring = buf[6][0] == 0 ? :horizontal : :vertical
      @mirroring = :four_screen if buf[6][3] == 1
      @battery = buf[6][1] == 1
      @mapper = (buf[6] >> 4) | (buf[7] & 0xf0)
      ram_banks = [1, buf[8]].max

      return prg_banks, chr_banks, ram_banks
    end

    def initialize(conf, cpu, ppu, basename, buf)
      @conf = conf
      @cpu = cpu
      @ppu = ppu
      @basename = basename

      prg_count, chr_count, wrk_count = parse_header(buf.slice!(0, 16))

      raise InvalidROM, "EOF in ROM bank data" if buf.size < 0x4000 * prg_count
      @prg_banks = (0...prg_count).map { buf.slice!(0, 0x4000) }

      raise InvalidROM, "EOF in CHR bank data" if buf.size < 0x2000 * chr_count
      @chr_banks = (0...chr_count).map { buf.slice!(0, 0x2000) }

      @prg_ref = [nil] * 0x10000
      @prg_ref[0x8000, 0x4000] = @prg_banks.first
      @prg_ref[0xc000, 0x4000] = @prg_banks.last

      @chr_ram = chr_count == 0 # No CHR bank implies CHR-RAM (writable CHR bank)
      @chr_ref = @chr_ram ? [0] * 0x2000 : @chr_banks[0].dup

      @wrk_readable = wrk_count > 0
      @wrk_writable = false
      @wrk = wrk_count > 0 ? (0x6000..0x7fff).map {|addr| addr >> 8 } : nil

      init

      @ppu.nametables = @mirroring
      @ppu.set_chr_mem(@chr_ref, @chr_ram)
    end

    def init
    end

    def reset
      @cpu.add_mappings(0x8000..0xffff, @prg_ref, nil)
    end

    def inspect
      [
        "Mapper: #{ @mapper } (#{ self.class.to_s.split("::").last })",
        "PRG Banks: #{ @prg_banks.size }",
        "CHR Banks: #{ @chr_banks.size }",
        "Mirroring: #{ @mirroring }",
      ].join("\n")
    end

    def peek_6000(addr)
      @wrk_readable ? @wrk[addr - 0x6000] : (addr >> 8)
    end

    def poke_6000(addr, data)
      @wrk[addr - 0x6000] = data if @wrk_writable
    end

    def vsync
    end

    def load_battery
      return unless @battery
      sav = @basename + ".sav"
      return unless File.readable?(sav)
      sav = File.binread(sav)
      @wrk.replace(sav.bytes)
    end

    def save_battery
      return unless @battery
      sav = @basename + ".sav"
      puts "Saving: " + sav
      File.binwrite(sav, @wrk.pack("C*"))
    end
  end
end


# optcarrot/pad

module Optcarrot
  # Pad pair implementation (NES has two built-in game pad.)
  class Pads
    def inspect
      "#<#{ self.class }>"
    end

    ###########################################################################
    # initialization

    def initialize(conf, cpu, apu)
      @conf = conf
      @cpu = cpu
      @apu = apu
      @pads = [Pad.new, Pad.new]
    end

    def reset
      @cpu.add_mappings(0x4016, method(:peek_401x), method(:poke_4016))
      @cpu.add_mappings(0x4017, method(:peek_401x), @apu.method(:poke_4017)) # delegate 4017H to APU
      @pads[0].reset
      @pads[1].reset
    end

    def peek_401x(addr)
      @cpu.update
      @pads[addr - 0x4016].peek | 0x40
    end

    def poke_4016(_addr, data)
      @pads[0].poke(data)
      @pads[1].poke(data)
    end

    ###########################################################################
    # APIs

    def keydown(pad, btn)
      @pads[pad].buttons |= 1 << btn
    end

    def keyup(pad, btn)
      @pads[pad].buttons &= ~(1 << btn)
    end
  end

  ###########################################################################
  # each pad
  class Pad
    A      = 0
    B      = 1
    SELECT = 2
    START  = 3
    UP     = 4
    DOWN   = 5
    LEFT   = 6
    RIGHT  = 7

    def initialize
      reset
    end

    def reset
      @strobe = false
      @buttons = @stream = 0
    end

    def poke(data)
      prev = @strobe
      @strobe = data[0] == 1
      @stream = ((poll_state << 1) ^ -512) if prev && !@strobe
    end

    def peek
      return poll_state & 1 if @strobe
      @stream >>= 1
      return @stream[0]
    end

    def poll_state
      state = @buttons

      # prohibit impossible simultaneous keydown (right and left, up and down)
      state &= 0b11001111 if state & 0b00110000 == 0b00110000
      state &= 0b00111111 if state & 0b11000000 == 0b11000000

      state
    end

    attr_accessor :buttons
  end
end


# optcarrot/cpu

# optcarrot/opt
# if false
module Optcarrot
  # dirty methods manipulating and generating methods...
  module CodeOptimizationHelper
    def initialize(loglevel, enabled_opts)
      return
      @loglevel = loglevel
      options = self.class::OPTIONS
      opts = {}
      # enabled_opts ||= [:all]
      enabled_opts ||= []
      default =
        (enabled_opts == [:all] || enabled_opts != [] && enabled_opts.all? {|opt| opt.to_s.start_with?("-") })
      options.each {|opt| opts[opt] = default }
      (enabled_opts - [:none, :all]).each do |opt|
        val = true
        if opt.to_s.start_with?("-")
          opt = opt.to_s[1..-1].to_sym
          val = false
        end
        raise "unknown optimization: `#{ opt }'" unless options.include?(opt)
        opts[opt] = val
      end
      puts "Opts: #{options}"
      options.each {|opt| instance_variable_set(:"@#{ opt }", opts[opt]) }
    end

    def depends(opt, depended_opt)
      if instance_variable_get(:"@#{ opt }") && !instance_variable_get(:"@#{ depended_opt }")
        raise "`#{ opt }' depends upon `#{ depended_opt }'"
      end
    end

    def gen(*codes)
      codes.map {|code| code.to_s.chomp }.join("\n") + "\n"
    end

    # change indent
    def indent(i, code)
      if i > 0
        code.gsub(/^(.+)$/) { " " * i + $1 }
      elsif i < 0
        code.gsub(/^ {#{ -i }}/, "")
      else
        code
      end
    end

    # generate a branch
    def branch(cond, code1, code2)
      gen(
        "if #{ cond }",
        indent(2, code1),
        "else",
        indent(2, code2),
        "end",
      )
    end

    MethodDef = Struct.new(:params, :body)

    METHOD_DEFINITIONS_RE = nil

    # extract all method definitions
    def parse_method_definitions(file)
      src = File.read(file)
      mdefs = {}
      src.scan(METHOD_DEFINITIONS_RE) do |indent, meth, params, body|
        body = indent(-indent.size - 2, body)

        # noramlize: break `when ... then`
        body = body.gsub(/^( *)when +(.*?) +then +(.*)/) { $1 + "when #{ $2 }\n" + $1 + "  " + $3 }

        # normalize: return unless
        body = "if " + $1 + indent(2, $') + "end\n" if body =~ /\Areturn unless (.*)/

        # normalize: if modifier -> if statement
        nil while body.gsub!(/^( *)((?!#)\S.*) ((?:if|unless) .*\n)/) { indent($1.size, gen($3, "  " + $2, "end")) }

        mdefs[meth.to_sym] = MethodDef[params ? params.split(", ") : nil, body]
      end
      mdefs
    end

    # inline method calls with no arguments
    def expand_methods(code, mdefs, meths = mdefs.keys)
      code.gsub(/^( *)\b(#{ meths * "|" })\b(?:\((.*?)\))?\n/) do
        indent, meth, args = $1, $2, $3
        body = mdefs[meth.to_sym]
        body = body.body if body.is_a?(MethodDef)
        if args
          mdefs[meth.to_sym].params.zip(args.split(", ")) do |param, arg|
            body = replace_var(body, param, arg)
          end
        end
        indent(indent.size, body)
      end
    end

    def expand_inline_methods(code, meth, mdef)
      code.gsub(/\b#{ meth }\b(?:\(((?:@?\w+, )*@?\w+)\))?/) do
        args = $1
        b = "(#{ mdef.body.chomp.gsub(/ *#.*/, "").gsub("\n", "; ") })"
        if args
          mdef.params.zip(args.split(", ")) do |param, arg|
            b = replace_var(b, param, arg)
          end
        end
        b
      end
    end

    def replace_var(code, var, bool)
      re = var.start_with?("@") ? /#{ var }\b/ : /\b#{ var }\b/
      code.gsub(re) { bool }
    end

    def replace_cond_var(code, var, bool)
      code.gsub(/(if|unless)\s#{ var }\b/) { $1 + " " + bool }
    end

    TRIVIAL_BRANCH_RE = nil && /
      ^(\ *)(if|unless)\ (true|false)\n
      ^((?:\1\ +.*\n|\n)*)
       (?:
         \1else\n
         ((?:\1\ +.*\n|\n)*)
       )?
      ^\1end\n
    /x
    # remove "if true" or "if false"
    def remove_trivial_branches(code)
      return
      code = code.dup
      nil while
        code.gsub!(TRIVIAL_BRANCH_RE) do
          if ($2 == "if") == ($3 == "true")
            indent(-2, $4)
          else
            $5 ? indent(-2, $5) : ""
          end
        end
      code
    end

    # replace instance variables with temporal local variables
    # CAUTION: the instance variable must not be accessed out of CPU#run
    def localize_instance_variables(code, ivars = code.scan(/@\w+/).uniq.sort)
      ivars = ivars.map {|ivar| ivar.to_s[1..-1] }

      inits, finals = [], []
      ivars.each do |ivar|
        lvar = "__#{ ivar }__"
        inits << "#{ lvar } = @#{ ivar }"
        finals << "@#{ ivar } = #{ lvar }"
      end

      code = code.gsub(/@(#{ ivars * "|" })\b/) { "__#{ $1 }__" }

      gen(
        "begin",
        indent(2, inits.join("\n")),
        indent(2, code),
        "ensure",
        indent(2, finals.join("\n")),
        "end",
      )
    end
  end
end

# end

module Optcarrot
  # CPU implementation
  class CPU
    NMI_VECTOR   = 0xfffa
    RESET_VECTOR = 0xfffc
    IRQ_VECTOR   = 0xfffe

    IRQ_EXT   = 0x01
    IRQ_FRAME = 0x40
    IRQ_DMC   = 0x80

    CLK_1, CLK_2, CLK_3, CLK_4, CLK_5, CLK_6, CLK_7, CLK_8 = (1..8).map {|i| i * RP2A03_CC }

    def inspect
      "#<#{ self.class }>"
    end

    ###########################################################################
    # initialization

    def initialize(conf)
      @conf = conf

      # load the generated core
      if @conf.load_cpu
        eval(File.read(@conf.load_cpu))
      elsif @conf.opt_cpu and false
        eval(OptimizedCodeBuilder.new(@conf.loglevel, @conf.opt_cpu).build, nil, "(generated CPU core)")
      end

      # main memory
      @fetch = [nil] * 0x10000
      @store = [nil] * 0x10000
      @peeks = {}
      @pokes = {}
      @ram = [0] * 0x800

      # clock management
      @clk = 0                 # the current clock
      @clk_frame = 0           # the next frame clock
      @clk_target = 0          # the goal clock for the current CPU#run
      @clk_nmi = FOREVER_CLOCK # the next NMI clock (FOREVER_CLOCK means "not scheduled")
      @clk_irq = FOREVER_CLOCK # the next IRQ clock
      @clk_total = 0           # the total elapsed clocks

      # interrupt
      @irq_flags = 0
      @jammed = false

      @poke_nop = CPU.method(:poke_nop)

      reset

      # temporary store (valid only during each operation)
      @addr = @data = 0

      @opcode = nil
      @ppu_sync = false
    end

    def reset
      # registers
      @_a = @_x = @_y = 0
      @_sp = 0xfd
      @_pc = 0xfffc

      # P register
      @_p_nz = 1
      @_p_c = 0
      @_p_v = 0
      @_p_i = 0x04
      @_p_d = 0

      # reset clocks
      @clk = @clk_total = 0

      # reset RAM
      @ram.fill(0xff)

      # memory mappings by self
      add_mappings(0x0000..0x07ff, @ram, @ram.method(:[]=))
      add_mappings(0x0800..0x1fff, method(:peek_ram), method(:poke_ram))
      add_mappings(0x2000..0xffff, method(:peek_nop), nil)
      add_mappings(0xfffc, method(:peek_jam_1), nil)
      add_mappings(0xfffd, method(:peek_jam_2), nil)
    end

    def peek_ram(addr)
      @ram[addr % 0x0800]
    end

    def poke_ram(addr, data)
      @ram[addr % 0x0800] = data
    end

    def peek_nop(addr)
      addr >> 8
    end

    def peek_jam_1(_addr)
      @_pc = (@_pc - 1) & 0xffff
      0xfc
    end

    def peek_jam_2(_addr)
      0xff
    end

    ###########################################################################
    # mapped memory API

    def add_mappings(addr, peek, poke)
      # filter the logically equivalent objects
      peek = @peeks[peek] ||= peek
      poke = @pokes[poke] ||= poke

      (addr.is_a?(Integer) ? [addr] : addr).each do |a|
        @fetch[a] = peek
        @store[a] = poke || @poke_nop
      end
    end

    def self.poke_nop(_addr, _data)
    end

    def fetch(addr)
      @fetch[addr][addr]
    end

    def store(addr, value)
      @store[addr][addr, value]
    end

    def peek16(addr)
      @fetch[addr][addr] + (@fetch[addr + 1][addr + 1] << 8)
    end

    ###########################################################################
    # other APIs

    attr_reader :ram
    attr_writer :apu, :ppu, :ppu_sync

    def current_clock
      @clk
    end

    def next_frame_clock
      @clk_frame
    end

    def next_frame_clock=(clk)
      @clk_frame = clk
      @clk_target = clk if clk < @clk_target
    end

    def steal_clocks(clk)
      @clk += clk
    end

    def odd_clock?
      (@clk_total + @clk) % CLK_2 != 0
    end

    def update
      @apu.clock_dma(@clk)
      @clk
    end

    def dmc_dma(addr)
      # This is inaccurate; it must steal *up to* 4 clocks depending upon
      # whether CPU writes in this clock, but this always steals 4 clocks.
      @clk += CLK_3
      dma_buffer = fetch(addr)
      @clk += CLK_1
      dma_buffer
    end

    def sprite_dma(addr, sp_ram)
      256.times {|i| sp_ram[i] = @ram[addr + i] }
      64.times {|i| sp_ram[i * 4 + 2] &= 0xe3 }
    end

    def boot
      @clk = CLK_7
      @_pc = peek16(RESET_VECTOR)
    end

    def vsync
      @ppu.sync(@clk) if @ppu_sync

      @clk -= @clk_frame
      @clk_total += @clk_frame

      @clk_nmi -= @clk_frame if @clk_nmi != FOREVER_CLOCK
      @clk_irq -= @clk_frame if @clk_irq != FOREVER_CLOCK
      @clk_irq = 0 if @clk_irq < 0
    end

    ###########################################################################
    # interrupts

    def clear_irq(line)
      old_irq_flags = @irq_flags & (IRQ_FRAME | IRQ_DMC)
      @irq_flags &= line ^ (IRQ_EXT | IRQ_FRAME | IRQ_DMC)
      @clk_irq = FOREVER_CLOCK if @irq_flags == 0
      old_irq_flags
    end

    def next_interrupt_clock(clk)
      clk += CLK_1 + CLK_1 / 2 # interrupt edge
      @clk_target = clk if @clk_target > clk
      clk
    end

    def do_irq(line, clk)
      @irq_flags |= line
      @clk_irq = next_interrupt_clock(clk) if @clk_irq == FOREVER_CLOCK && @_p_i == 0
    end

    def do_nmi(clk)
      @clk_nmi = next_interrupt_clock(clk) if @clk_nmi == FOREVER_CLOCK
    end

    def do_isr(vector)
      return if @jammed
      push16(@_pc)
      push8(flags_pack)
      @_p_i = 0x04
      @clk += CLK_7
      addr = vector == NMI_VECTOR ? NMI_VECTOR : fetch_irq_isr_vector
      @_pc = peek16(addr)
    end

    def fetch_irq_isr_vector
      fetch(0x3000) if @clk >= @clk_frame
      if @clk_nmi != FOREVER_CLOCK
        if @clk_nmi + CLK_2 <= @clk
          @clk_nmi = FOREVER_CLOCK
          return NMI_VECTOR
        end
        @clk_nmi = @clk + 1
      end
      return IRQ_VECTOR
    end

    ###########################################################################
    # instruction helpers

    ### P regeister ###

    def flags_pack
      # NVssDIZC
      ((@_p_nz | @_p_nz >> 1) & 0x80) | # N: Negative
        (@_p_nz & 0xff != 0 ? 0 : 2) |  # Z: Zero
        @_p_c |                         # C: Carry
        (@_p_v != 0 ? 0x40 : 0) |       # V: Overflow
        @_p_i |                         # I: Inerrupt
        @_p_d |                         # D: Decimal
        0x20
    end

    def flags_unpack(f)
      @_p_nz = (~f & 2) | ((f & 0x80) << 1)
      @_p_c = f & 0x01
      @_p_v = f & 0x40
      @_p_i = f & 0x04
      @_p_d = f & 0x08
    end

    ### branch helper ###
    def branch(cond)
      if cond
        tmp = @_pc + 1
        rel = fetch(@_pc)
        @_pc = (tmp + (rel < 128 ? rel : rel | 0xff00)) & 0xffff
        @clk += tmp[8] == @_pc[8] ? CLK_3 : CLK_4
      else
        @_pc += 1
        @clk += CLK_2
      end
    end

    ### storers ###
    def store_mem
      store(@addr, @data)
      @clk += CLK_1
    end

    def store_zpg
      @ram[@addr] = @data
    end

    ### stack management ###
    def push8(data)
      @ram[0x0100 + @_sp] = data
      @_sp = (@_sp - 1) & 0xff
    end

    def push16(data)
      push8(data >> 8)
      push8(data & 0xff)
    end

    def pull8
      @_sp = (@_sp + 1) & 0xff
      @ram[0x0100 + @_sp]
    end

    def pull16
      pull8 + 256 * pull8
    end

    ###########################################################################
    # addressing modes

    # immediate addressing (read only)
    def imm(_read, _write)
      @data = fetch(@_pc)
      @_pc += 1
      @clk += CLK_2
    end

    # zero-page addressing
    def zpg(read, write)
      @addr = fetch(@_pc)
      @_pc += 1
      @clk += CLK_3
      if read
        @data = @ram[@addr]
        @clk += CLK_2 if write
      end
    end

    # zero-page indexed addressing
    def zpg_reg(indexed, read, write)
      @addr = (indexed + fetch(@_pc)) & 0xff
      @_pc += 1
      @clk += CLK_4
      if read
        @data = @ram[@addr]
        @clk += CLK_2 if write
      end
    end

    def zpg_x(read, write)
      zpg_reg(@_x, read, write)
    end

    def zpg_y(read, write)
      zpg_reg(@_y, read, write)
    end

    # absolute addressing
    def abs(read, write)
      @addr = peek16(@_pc)
      @_pc += 2
      @clk += CLK_3
      read_write(read, write)
    end

    # absolute indexed addressing
    def abs_reg(indexed, read, write)
      addr = @_pc + 1
      i = indexed + fetch(@_pc)
      @addr = ((fetch(addr) << 8) + i) & 0xffff
      if write
        addr = (@addr - (i & 0x100)) & 0xffff
        fetch(addr)
        @clk += CLK_4
      else
        @clk += CLK_3
        if i & 0x100 != 0
          addr = (@addr - 0x100) & 0xffff # for inlining fetch
          fetch(addr)
          @clk += CLK_1
        end
      end
      read_write(read, write)
      @_pc += 2
    end

    def abs_x(read, write)
      abs_reg(@_x, read, write)
    end

    def abs_y(read, write)
      abs_reg(@_y, read, write)
    end

    # indexed indirect addressing
    def ind_x(read, write)
      addr = fetch(@_pc) + @_x
      @_pc += 1
      @clk += CLK_5
      @addr = @ram[addr & 0xff] | @ram[(addr + 1) & 0xff] << 8
      read_write(read, write)
    end

    # indirect indexed addressing
    def ind_y(read, write)
      addr = fetch(@_pc)
      @_pc += 1
      indexed = @ram[addr] + @_y
      @clk += CLK_4
      if write
        @clk += CLK_1
        @addr = (@ram[(addr + 1) & 0xff] << 8) + indexed
        addr = @addr - (indexed & 0x100) # for inlining fetch
        fetch(addr)
      else
        @addr = ((@ram[(addr + 1) & 0xff] << 8) + indexed) & 0xffff
        if indexed & 0x100 != 0
          addr = (@addr - 0x100) & 0xffff # for inlining fetch
          fetch(addr)
          @clk += CLK_1
        end
      end
      read_write(read, write)
    end

    def read_write(read, write)
      if read
        @data = fetch(@addr)
        @clk += CLK_1
        if write
          store(@addr, @data)
          @clk += CLK_1
        end
      end
    end

    ###########################################################################
    # instructions

    # load instructions
    def _lda
      @_p_nz = @_a = @data
    end

    def _ldx
      @_p_nz = @_x = @data
    end

    def _ldy
      @_p_nz = @_y = @data
    end

    # store instructions
    def _sta
      @data = @_a
    end

    def _stx
      @data = @_x
    end

    def _sty
      @data = @_y
    end

    # transfer instructions
    def _tax
      @clk += CLK_2
      @_p_nz = @_x = @_a
    end

    def _tay
      @clk += CLK_2
      @_p_nz = @_y = @_a
    end

    def _txa
      @clk += CLK_2
      @_p_nz = @_a = @_x
    end

    def _tya
      @clk += CLK_2
      @_p_nz = @_a = @_y
    end

    # flow control instructions
    def _jmp_a
      @_pc = peek16(@_pc)
      @clk += CLK_3
    end

    def _jmp_i
      pos = peek16(@_pc)
      low = fetch(pos)
      pos = (pos & 0xff00) | ((pos + 1) & 0x00ff)
      high = fetch(pos)
      @_pc = high * 256 + low
      @clk += CLK_5
    end

    def _jsr
      data = @_pc + 1
      push16(data)
      @_pc = peek16(@_pc)
      @clk += CLK_6
    end

    def _rts
      @_pc = (pull16 + 1) & 0xffff
      @clk += CLK_6
    end

    def _rti
      @clk += CLK_6
      packed = pull8
      @_pc = pull16
      flags_unpack(packed)
      @clk_irq = @irq_flags == 0 || @_p_i != 0 ? FOREVER_CLOCK : @clk_target = 0
    end

    def _bne
      branch(@_p_nz & 0xff != 0)
    end

    def _beq
      branch(@_p_nz & 0xff == 0)
    end

    def _bmi
      branch(@_p_nz & 0x180 != 0)
    end

    def _bpl
      branch(@_p_nz & 0x180 == 0)
    end

    def _bcs
      branch(@_p_c != 0)
    end

    def _bcc
      branch(@_p_c == 0)
    end

    def _bvs
      branch(@_p_v != 0)
    end

    def _bvc
      branch(@_p_v == 0)
    end

    # math operations
    def _adc
      tmp = @_a + @data + @_p_c
      @_p_v = ~(@_a ^ @data) & (@_a ^ tmp) & 0x80
      @_p_nz = @_a = tmp & 0xff
      @_p_c = tmp[8]
    end

    def _sbc
      data = @data ^ 0xff
      tmp = @_a + data + @_p_c
      @_p_v = ~(@_a ^ data) & (@_a ^ tmp) & 0x80
      @_p_nz = @_a = tmp & 0xff
      @_p_c = tmp[8]
    end

    # logical operations
    def _and
      @_p_nz = @_a &= @data
    end

    def _ora
      @_p_nz = @_a |= @data
    end

    def _eor
      @_p_nz = @_a ^= @data
    end

    def _bit
      @_p_nz = ((@data & @_a) != 0 ? 1 : 0) | ((@data & 0x80) << 1)
      @_p_v = @data & 0x40
    end

    def _cmp
      data = @_a - @data
      @_p_nz = data & 0xff
      @_p_c = 1 - data[8]
    end

    def _cpx
      data = @_x - @data
      @_p_nz = data & 0xff
      @_p_c = 1 - data[8]
    end

    def _cpy
      data = @_y - @data
      @_p_nz = data & 0xff
      @_p_c = 1 - data[8]
    end

    # shift operations
    def _asl
      @_p_c = @data >> 7
      @data = @_p_nz = @data << 1 & 0xff
    end

    def _lsr
      @_p_c = @data & 1
      @data = @_p_nz = @data >> 1
    end

    def _rol
      @_p_nz = (@data << 1 & 0xff) | @_p_c
      @_p_c = @data >> 7
      @data = @_p_nz
    end

    def _ror
      @_p_nz = (@data >> 1) | (@_p_c << 7)
      @_p_c = @data & 1
      @data = @_p_nz
    end

    # increment and decrement operations
    def _dec
      @data = @_p_nz = (@data - 1) & 0xff
    end

    def _inc
      @data = @_p_nz = (@data + 1) & 0xff
    end

    def _dex
      @clk += CLK_2
      @data = @_p_nz = @_x = (@_x - 1) & 0xff
    end

    def _dey
      @clk += CLK_2
      @data = @_p_nz = @_y = (@_y - 1) & 0xff
    end

    def _inx
      @clk += CLK_2
      @data = @_p_nz = @_x = (@_x + 1) & 0xff
    end

    def _iny
      @clk += CLK_2
      @data = @_p_nz = @_y = (@_y + 1) & 0xff
    end

    # flags instructions
    def _clc
      @clk += CLK_2
      @_p_c = 0
    end

    def _sec
      @clk += CLK_2
      @_p_c = 1
    end

    def _cld
      @clk += CLK_2
      @_p_d = 0
    end

    def _sed
      @clk += CLK_2
      @_p_d = 8
    end

    def _clv
      @clk += CLK_2
      @_p_v = 0
    end

    def _sei
      @clk += CLK_2
      if @_p_i == 0
        @_p_i = 0x04
        @clk_irq = FOREVER_CLOCK
        do_isr(IRQ_VECTOR) if @irq_flags != 0
      end
    end

    def _cli
      @clk += CLK_2
      if @_p_i != 0
        @_p_i = 0
        if @irq_flags != 0
          clk = @clk_irq = @clk + 1
          @clk_target = clk if @clk_target > clk
        end
      end
    end

    # stack operations
    def _pha
      @clk += CLK_3
      push8(@_a)
    end

    def _php
      @clk += CLK_3
      data = flags_pack | 0x10
      push8(data)
    end

    def _pla
      @clk += CLK_4
      @_p_nz = @_a = pull8
    end

    def _plp
      @clk += CLK_4
      i = @_p_i
      flags_unpack(pull8)
      if @irq_flags != 0
        if i > @_p_i
          clk = @clk_irq = @clk + 1
          @clk_target = clk if @clk_target > clk
        elsif i < @_p_i
          @clk_irq = FOREVER_CLOCK
          do_isr(IRQ_VECTOR)
        end
      end
    end

    def _tsx
      @clk += CLK_2
      @_p_nz = @_x = @_sp
    end

    def _txs
      @clk += CLK_2
      @_sp = @_x
    end

    # undocumented instructions, rarely used
    def _anc
      @_p_nz = @_a &= @data
      @_p_c = @_p_nz >> 7
    end

    def _ane
      @_a = (@_a | 0xee) & @_x & @data
      @_p_nz = @_a
    end

    def _arr
      @_a = ((@data & @_a) >> 1) | (@_p_c << 7)
      @_p_nz = @_a
      @_p_c = @_a[6]
      @_p_v = @_a[6] ^ @_a[5]
    end

    def _asr
      @_p_c = @data & @_a & 0x1
      @_p_nz = @_a = (@data & @_a) >> 1
    end

    def _dcp
      @data = (@data - 1) & 0xff
      _cmp
    end

    def _isb
      @data = (@data + 1) & 0xff
      _sbc
    end

    def _las
      @_sp &= @data
      @_p_nz = @_a = @_x = @_sp
    end

    def _lax
      @_p_nz = @_a = @_x = @data
    end

    def _lxa
      @_p_nz = @_a = @_x = @data
    end

    def _rla
      c = @_p_c
      @_p_c = @data >> 7
      @data = (@data << 1 & 0xff) | c
      @_p_nz = @_a &= @data
    end

    def _rra
      c = @_p_c << 7
      @_p_c = @data & 1
      @data = (@data >> 1) | c
      _adc
    end

    def _sax
      @data = @_a & @_x
    end

    def _sbx
      @data = (@_a & @_x) - @data
      @_p_c = (@data & 0xffff) <= 0xff ? 1 : 0
      @_p_nz = @_x = @data & 0xff
    end

    def _sha
      @data = @_a & @_x & ((@addr >> 8) + 1)
    end

    def _shs
      @_sp = @_a & @_x
      @data = @_sp & ((@addr >> 8) + 1)
    end

    def _shx
      @data = @_x & ((@addr >> 8) + 1)
      @addr = (@data << 8) | (@addr & 0xff)
    end

    def _shy
      @data = @_y & ((@addr >> 8) + 1)
      @addr = (@data << 8) | (@addr & 0xff)
    end

    def _slo
      @_p_c = @data >> 7
      @data = @data << 1 & 0xff
      @_p_nz = @_a |= @data
    end

    def _sre
      @_p_c = @data & 1
      @data >>= 1
      @_p_nz = @_a ^= @data
    end

    # nops
    def _nop
    end

    # interrupts
    def _brk
      data = @_pc + 1
      push16(data)
      data = flags_pack | 0x10
      push8(data)
      @_p_i = 0x04
      @clk_irq = FOREVER_CLOCK
      @clk += CLK_7
      addr = fetch_irq_isr_vector # for inlining peek16
      @_pc = peek16(addr)
    end

    def _jam
      @_pc = (@_pc - 1) & 0xffff
      @clk += CLK_2
      unless @jammed
        @jammed = true
        # interrupt reset
        @clk_nmi = FOREVER_CLOCK
        @clk_irq = FOREVER_CLOCK
        @irq_flags = 0
      end
    end

    ###########################################################################
    # default core

    def r_op(instr, mode)
      send(mode, true, false)
      send(instr)
    end

    def w_op(instr, mode, store)
      send(mode, false, true)
      send(instr)
      send(store)
    end

    def rw_op(instr, mode, store)
      send(mode, true, true)
      send(instr)
      send(store)
    end

    def a_op(instr)
      @clk += CLK_2
      @data = @_a
      send(instr)
      @_a = @data
    end

    def no_op(_instr, ops, ticks)
      @_pc += ops
      @clk += ticks * RP2A03_CC
    end

    def do_clock
      clock = @apu.do_clock

      clock = @clk_frame if clock > @clk_frame

      if @clk < @clk_nmi
        clock = @clk_nmi if clock > @clk_nmi
        if @clk < @clk_irq
          clock = @clk_irq if clock > @clk_irq
        else
          @clk_irq = FOREVER_CLOCK
          do_isr(IRQ_VECTOR)
        end
      else
        @clk_nmi = @clk_irq = FOREVER_CLOCK
        do_isr(NMI_VECTOR)
      end
      @clk_target = clock
    end

    def run
      do_clock
      begin
        begin
          @opcode = fetch(@_pc)

          if @conf.loglevel >= 3
            @conf.debug("PC:%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d : OPCODE:%02X (%d, %d)" % [
              @_pc, @_a, @_x, @_y, flags_pack, @_sp, @clk / 4 % 341, @opcode, @clk, @clk_target
            ])
          end

          @_pc += 1

          send(*DISPATCH[@opcode])

          @ppu.sync(@clk) if @ppu_sync
        end while @clk < @clk_target
        do_clock
      end while @clk < @clk_frame
    end

    ADDRESSING_MODES = {
      ctl: [:imm,   :zpg, :imm, :abs, nil,    :zpg_x, nil,    :abs_x],
      rmw: [:imm,   :zpg, :imm, :abs, nil,    :zpg_y, nil,    :abs_y],
      alu: [:ind_x, :zpg, :imm, :abs, :ind_y, :zpg_x, :abs_y, :abs_x],
      uno: [:ind_x, :zpg, :imm, :abs, :ind_y, :zpg_y, :abs_y, :abs_y],
    }

    DISPATCH = []

    def self.op(opcodes, args)
      opcodes.each do |opcode|
        if args.is_a?(Array) && [:r_op, :w_op, :rw_op].include?(args[0])
          kind, op, mode = args
          mode = ADDRESSING_MODES[mode][opcode >> 2 & 7]
          send_args = [kind, op, mode]
          send_args << (mode.to_s.start_with?("zpg") ? :store_zpg : :store_mem) if kind != :r_op
          DISPATCH[opcode] = send_args
        else
          DISPATCH[opcode] = [*args]
        end
      end
    end

    # load instructions
    op([0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1],       [:r_op, :_lda, :alu])
    op([0xa2, 0xa6, 0xb6, 0xae, 0xbe],                         [:r_op, :_ldx, :rmw])
    op([0xa0, 0xa4, 0xb4, 0xac, 0xbc],                         [:r_op, :_ldy, :ctl])

    # store instructions
    op([0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91],             [:w_op, :_sta, :alu])
    op([0x86, 0x96, 0x8e],                                     [:w_op, :_stx, :rmw])
    op([0x84, 0x94, 0x8c],                                     [:w_op, :_sty, :ctl])

    # transfer instructions
    op([0xaa],                                                 :_tax)
    op([0xa8],                                                 :_tay)
    op([0x8a],                                                 :_txa)
    op([0x98],                                                 :_tya)

    # flow control instructions
    op([0x4c],                                                 :_jmp_a)
    op([0x6c],                                                 :_jmp_i)
    op([0x20],                                                 :_jsr)
    op([0x60],                                                 :_rts)
    op([0x40],                                                 :_rti)
    op([0xd0],                                                 :_bne)
    op([0xf0],                                                 :_beq)
    op([0x30],                                                 :_bmi)
    op([0x10],                                                 :_bpl)
    op([0xb0],                                                 :_bcs)
    op([0x90],                                                 :_bcc)
    op([0x70],                                                 :_bvs)
    op([0x50],                                                 :_bvc)

    # math operations
    op([0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71],       [:r_op, :_adc, :alu])
    op([0xe9, 0xeb, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1], [:r_op, :_sbc, :alu])

    # logical operations
    op([0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31],       [:r_op, :_and, :alu])
    op([0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11],       [:r_op, :_ora, :alu])
    op([0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51],       [:r_op, :_eor, :alu])
    op([0x24, 0x2c],                                           [:r_op, :_bit, :alu])
    op([0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1],       [:r_op, :_cmp, :alu])
    op([0xe0, 0xe4, 0xec],                                     [:r_op, :_cpx, :rmw])
    op([0xc0, 0xc4, 0xcc],                                     [:r_op, :_cpy, :rmw])

    # shift operations
    op([0x0a],                                                 [:a_op, :_asl])
    op([0x06, 0x16, 0x0e, 0x1e],                               [:rw_op, :_asl, :alu])
    op([0x4a],                                                 [:a_op, :_lsr])
    op([0x46, 0x56, 0x4e, 0x5e],                               [:rw_op, :_lsr, :alu])
    op([0x2a],                                                 [:a_op, :_rol])
    op([0x26, 0x36, 0x2e, 0x3e],                               [:rw_op, :_rol, :alu])
    op([0x6a],                                                 [:a_op, :_ror])
    op([0x66, 0x76, 0x6e, 0x7e],                               [:rw_op, :_ror, :alu])

    # increment and decrement operations
    op([0xc6, 0xd6, 0xce, 0xde],                               [:rw_op, :_dec, :alu])
    op([0xe6, 0xf6, 0xee, 0xfe],                               [:rw_op, :_inc, :alu])
    op([0xca],                                                 :_dex)
    op([0x88],                                                 :_dey)
    op([0xe8],                                                 :_inx)
    op([0xc8],                                                 :_iny)

    # flags instructions
    op([0x18],                                                 :_clc)
    op([0x38],                                                 :_sec)
    op([0xd8],                                                 :_cld)
    op([0xf8],                                                 :_sed)
    op([0x58],                                                 :_cli)
    op([0x78],                                                 :_sei)
    op([0xb8],                                                 :_clv)

    # stack operations
    op([0x48],                                                 :_pha)
    op([0x08],                                                 :_php)
    op([0x68],                                                 :_pla)
    op([0x28],                                                 :_plp)
    op([0xba],                                                 :_tsx)
    op([0x9a],                                                 :_txs)

    # undocumented instructions, rarely used
    op([0x0b, 0x2b],                                           [:r_op, :_anc, :uno])
    op([0x8b],                                                 [:r_op, :_ane, :uno])
    op([0x6b],                                                 [:r_op, :_arr, :uno])
    op([0x4b],                                                 [:r_op, :_asr, :uno])
    op([0xc7, 0xd7, 0xc3, 0xd3, 0xcf, 0xdf, 0xdb],             [:rw_op, :_dcp, :alu])
    op([0xe7, 0xf7, 0xef, 0xff, 0xfb, 0xe3, 0xf3],             [:rw_op, :_isb, :alu])
    op([0xbb],                                                 [:r_op, :_las, :uno])
    op([0xa7, 0xb7, 0xaf, 0xbf, 0xa3, 0xb3],                   [:r_op, :_lax, :uno])
    op([0xab],                                                 [:r_op, :_lxa, :uno])
    op([0x27, 0x37, 0x2f, 0x3f, 0x3b, 0x23, 0x33],             [:rw_op, :_rla, :alu])
    op([0x67, 0x77, 0x6f, 0x7f, 0x7b, 0x63, 0x73],             [:rw_op, :_rra, :alu])
    op([0x87, 0x97, 0x8f, 0x83],                               [:w_op, :_sax, :uno])
    op([0xcb],                                                 [:r_op, :_sbx, :uno])
    op([0x9f, 0x93],                                           [:w_op, :_sha, :uno])
    op([0x9b],                                                 [:w_op, :_shs, :uno])
    op([0x9e],                                                 [:w_op, :_shx, :rmw])
    op([0x9c],                                                 [:w_op, :_shy, :ctl])
    op([0x07, 0x17, 0x0f, 0x1f, 0x1b, 0x03, 0x13],             [:rw_op, :_slo, :alu])
    op([0x47, 0x57, 0x4f, 0x5f, 0x5b, 0x43, 0x53],             [:rw_op, :_sre, :alu])

    # nops
    op([0x1a, 0x3a, 0x5a, 0x7a, 0xda, 0xea, 0xfa],             [:no_op, :_nop, 0, 2])
    op([0x80, 0x82, 0x89, 0xc2, 0xe2],                         [:no_op, :_nop, 1, 2])
    op([0x04, 0x44, 0x64],                                     [:no_op, :_nop, 1, 3])
    op([0x14, 0x34, 0x54, 0x74, 0xd4, 0xf4],                   [:no_op, :_nop, 1, 4])
    op([0x0c],                                                 [:no_op, :_nop, 2, 4])
    op([0x1c, 0x3c, 0x5c, 0x7c, 0xdc, 0xfc],                   [:r_op, :_nop, :ctl])

    # interrupts
    op([0x00],                                                 :_brk)
    op([0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xb2, 0xd2, 0xf2], :_jam)

    ###########################################################################
    # optimized core generator
    class OptimizedCodeBuilder
      include CodeOptimizationHelper

      OPTIONS = [:method_inlining, :constant_inlining, :ivar_localization, :trivial_branches]

      LOCALIZE_IVARS = [:@addr, :@data, :@_a, :@_x, :@_y, :@_pc, :@_sp, :@fetch, :@store, :@ram, :@opcode]

      def build
        depends(:ivar_localization, :method_inlining)

        mdefs = parse_method_definitions(__FILE__)
        code = build_loop(mdefs)

        # optimize!
        code = cpu_expand_methods(code, mdefs) if @method_inlining
        code = remove_trivial_branches(code) if @trivial_branches
        code = expand_constants(code) if @constant_inlining
        code = localize_instance_variables(code, LOCALIZE_IVARS) if @ivar_localization

        gen(
          "def self.run",
          indent(2, code),
          "end",
        )
      end

      # generate a main code
      def build_loop(mdefs)
        dispatch = gen(
          "case @opcode",
          *DISPATCH.map.with_index do |args, opcode|
            if args.size > 1
              mhd, instr, = args
              code = expand_inline_methods("#{ mhd }(#{ args.drop(1).join(", ") })", mhd, mdefs[mhd])
              code = code.gsub(/send\((\w+), (.*?)\)/) { "#{ $1 }(#{ $2 })" }
              code = code.gsub(/send\((\w+)\)/) { $1 }
              code = code[1..-2].split("; ")
            else
              instr = code = args[0]
            end
            "when 0x%02x # #{ instr }\n" % opcode + indent(2, gen(*code))
          end,
          "end"
        )
        main = mdefs[:run].body.sub("@conf.loglevel >= 3") { @loglevel >= 3 }
        main.sub(/^ *send.*\n/) { indent(4, dispatch) }
      end

      # inline method calls
      def cpu_expand_methods(code, mdefs)
        code = expand_methods(code, mdefs, mdefs.keys.grep(/^_/))
        [
          [:_adc, :_sbc, :_cmp, :store_mem, :store_zpg],
          [:imm, :abs, :zpg, :abs_x, :abs_y, :zpg_x, :zpg_y, :ind_x, :ind_y],
          [:abs_reg, :zpg_reg],
          [:read_write],
          [:do_clock],
          [:do_isr],
          [:branch, :push16],
          [:push8],
        ].each do |meths|
          code = expand_methods(code, mdefs, meths)
        end
        [:fetch, :peek16, :store, :pull16, :pull8].each do |meth|
          code = expand_inline_methods(code, meth, mdefs[meth])
        end
        code
      end

      # inline constants
      def expand_constants(handlers)
        handlers = handlers.gsub(/CLK_(\d+)/) { eval($&) }
        handlers = handlers.gsub(/FOREVER_CLOCK/) { "0xffffffff" }
        handlers
      end
    end
  end
end


# optcarrot/apu

module Optcarrot
  # APU implementation (audio output)
  class APU
    CLK_M2_MUL   = 6
    CLK_NTSC     = 39_375_000 * CLK_M2_MUL
    CLK_NTSC_DIV = 11

    CHANNEL_OUTPUT_MUL   = 256
    CHANNEL_OUTPUT_DECAY = CHANNEL_OUTPUT_MUL / 4 - 1

    FRAME_CLOCKS = [29830, 1, 1, 29828].map {|n| RP2A03_CC * n }
    OSCILLATOR_CLOCKS = [
      [7458, 7456, 7458, 7458],
      [7458, 7456, 7458, 7458 + 7452]
    ].map {|a| a.map {|n| RP2A03_CC * n } }

    def inspect
      "#<#{ self.class }>"
    end

    ###########################################################################
    # initialization

    def initialize(conf, cpu, rate, bits)
      @conf = conf
      @cpu = cpu

      @pulse_0, @pulse_1 = Pulse.new(self), Pulse.new(self)
      @triangle = Triangle.new(self)
      @noise = Noise.new(self)
      @dmc = DMC.new(@cpu, self)
      @mixer = Mixer.new(@pulse_0, @pulse_1, @triangle, @noise, @dmc)

      @conf.fatal("audio sample rate must be >= 11050") if rate < 11050
      @conf.fatal("audio bit depth must be 8 or 16") if bits != 8 && bits != 16

      @settings_rate = rate

      @output = []
      @buffer = []

      @fixed_clock = 1
      @rate_clock = 1
      @rate_counter = 0
      @frame_counter = 0
      @frame_divider = 0
      @frame_irq_clock = 0
      @frame_irq_repeat = 0
      @dmc_clock = 0

      reset(false)
    end

    def reset_mapping
      @frame_counter /= @fixed_clock
      @rate_counter /= @fixed_clock
      multiplier = 0
      while true
        multiplier += 1
        break if multiplier >= 512
        break if CLK_NTSC * multiplier % @settings_rate == 0
      end
      @rate_clock = CLK_NTSC * multiplier / @settings_rate
      @fixed_clock = CLK_NTSC_DIV * multiplier
      @frame_counter *= @fixed_clock
      @rate_counter *= @fixed_clock

      @mixer.reset
      @buffer.clear

      multiplier = 0
      while true
        multiplier += 1
        break if multiplier >= 0x1000
        break if CLK_NTSC * (multiplier + 1) / @settings_rate > 0x7ffff
        break if CLK_NTSC * multiplier % @settings_rate == 0
      end
      rate = CLK_NTSC * multiplier / @settings_rate
      fixed = CLK_NTSC_DIV * CPU::CLK_1 * multiplier

      @pulse_0 .update_settings(rate, fixed)
      @pulse_1 .update_settings(rate, fixed)
      @triangle.update_settings(rate, fixed)
      @noise   .update_settings(rate, fixed)

      @cpu.add_mappings(0x4000, method(:peek_40xx), @pulse_0 .method(:poke_0))
      @cpu.add_mappings(0x4001, method(:peek_40xx), @pulse_0 .method(:poke_1))
      @cpu.add_mappings(0x4002, method(:peek_40xx), @pulse_0 .method(:poke_2))
      @cpu.add_mappings(0x4003, method(:peek_40xx), @pulse_0 .method(:poke_3))
      @cpu.add_mappings(0x4004, method(:peek_40xx), @pulse_1 .method(:poke_0))
      @cpu.add_mappings(0x4005, method(:peek_40xx), @pulse_1 .method(:poke_1))
      @cpu.add_mappings(0x4006, method(:peek_40xx), @pulse_1 .method(:poke_2))
      @cpu.add_mappings(0x4007, method(:peek_40xx), @pulse_1 .method(:poke_3))
      @cpu.add_mappings(0x4008, method(:peek_40xx), @triangle.method(:poke_0))
      @cpu.add_mappings(0x400a, method(:peek_40xx), @triangle.method(:poke_2))
      @cpu.add_mappings(0x400b, method(:peek_40xx), @triangle.method(:poke_3))
      @cpu.add_mappings(0x400c, method(:peek_40xx), @noise   .method(:poke_0))
      @cpu.add_mappings(0x400e, method(:peek_40xx), @noise   .method(:poke_2))
      @cpu.add_mappings(0x400f, method(:peek_40xx), @noise   .method(:poke_3))
      @cpu.add_mappings(0x4010, method(:peek_40xx), @dmc     .method(:poke_0))
      @cpu.add_mappings(0x4011, method(:peek_40xx), @dmc     .method(:poke_1))
      @cpu.add_mappings(0x4012, method(:peek_40xx), @dmc     .method(:poke_2))
      @cpu.add_mappings(0x4013, method(:peek_40xx), @dmc     .method(:poke_3))
      @cpu.add_mappings(0x4015, method(:peek_4015), method(:poke_4015))
      @frame_irq_clock = (@frame_counter / @fixed_clock) - CPU::CLK_1
    end

    def reset(mapping = true)
      @cycles_ratecounter = 0
      @frame_divider = 0
      @frame_irq_clock = FOREVER_CLOCK
      @frame_irq_repeat = 0
      @dmc_clock = DMC::LUT[0]
      @frame_counter = FRAME_CLOCKS[0] * @fixed_clock

      reset_mapping if mapping

      @pulse_0.reset
      @pulse_1.reset
      @triangle.reset
      @noise.reset
      @dmc.reset
      @mixer.reset
      @buffer.clear
      @oscillator_clocks = OSCILLATOR_CLOCKS[0]
    end

    ###########################################################################
    # other APIs

    attr_reader :output

    def do_clock
      clock_dma(@cpu.current_clock)
      clock_frame_irq(@cpu.current_clock) if @frame_irq_clock <= @cpu.current_clock
      @dmc_clock < @frame_irq_clock ? @dmc_clock : @frame_irq_clock
    end

    def clock_dma(clk)
      clock_dmc(clk) if @dmc_clock <= clk
    end

    def update(target = @cpu.update)
      target *= @fixed_clock
      proceed(target)
      clock_frame_counter if @frame_counter < target
    end

    def update_latency
      update(@cpu.update + 1)
    end

    def update_delta
      elapsed = @cpu.update
      delta = @frame_counter != elapsed * @fixed_clock
      update(elapsed + 1)
      delta
    end

    def vsync
      flush_sound
      update(@cpu.current_clock)
      frame = @cpu.next_frame_clock
      @dmc_clock -= frame
      @frame_irq_clock -= frame if @frame_irq_clock != FOREVER_CLOCK
      frame *= @fixed_clock
      @rate_counter -= frame
      @frame_counter -= frame
    end

    ###########################################################################
    # helpers

    def clock_oscillators(two_clocks)
      @pulse_0.clock_envelope
      @pulse_1.clock_envelope
      @triangle.clock_linear_counter
      @noise.clock_envelope
      return unless two_clocks
      @pulse_0.clock_sweep(-1)
      @pulse_1.clock_sweep(0)
      @triangle.clock_length_counter
      @noise.clock_length_counter
    end

    def clock_dmc(target)
      begin
        if @dmc.clock_dac
          update(@dmc_clock)
          @dmc.update
        end
        @dmc_clock += @dmc.freq
        @dmc.clock_dma
      end while @dmc_clock <= target
    end

    def clock_frame_counter
      clock_oscillators(@frame_divider[0] == 1)
      @frame_divider = (@frame_divider + 1) & 3
      @frame_counter += @oscillator_clocks[@frame_divider] * @fixed_clock
    end

    def clock_frame_irq(target)
      @cpu.do_irq(CPU::IRQ_FRAME, @frame_irq_clock)
      begin
        @frame_irq_clock += FRAME_CLOCKS[1 + @frame_irq_repeat % 3]
        @frame_irq_repeat += 1
      end while @frame_irq_clock <= target
    end

    def flush_sound
      if @buffer.size < @settings_rate / 60
        target = @cpu.current_clock * @fixed_clock
        proceed(target)
        if @buffer.size < @settings_rate / 60
          clock_frame_counter if @frame_counter < target
          @buffer << @mixer.sample while @buffer.size < @settings_rate / 60
        end
      end
      @output.clear
      @output.concat(@buffer) # Array#replace creates an object internally
      @buffer.clear
    end

    def proceed(target)
      while @rate_counter < target && @buffer.size < @settings_rate / 60
        @buffer << @mixer.sample
        clock_frame_counter if @frame_counter <= @rate_counter
        @rate_counter += @rate_clock
      end
    end

    ###########################################################################
    # mapped memory handlers

    # Control
    def poke_4015(_addr, data)
      update
      @pulse_0 .enable(data[0] == 1)
      @pulse_1 .enable(data[1] == 1)
      @triangle.enable(data[2] == 1)
      @noise   .enable(data[3] == 1)
      @dmc     .enable(data[4] == 1)
    end

    # Status
    def peek_4015(_addr)
      elapsed = @cpu.update
      clock_frame_irq(elapsed) if @frame_irq_clock <= elapsed
      update(elapsed) if @frame_counter < elapsed * @fixed_clock
      @cpu.clear_irq(CPU::IRQ_FRAME) |
        (@pulse_0 .status ? 0x01 : 0) |
        (@pulse_1 .status ? 0x02 : 0) |
        (@triangle.status ? 0x04 : 0) |
        (@noise   .status ? 0x08 : 0) |
        (@dmc     .status ? 0x10 : 0)
    end

    # Frame counter (NOTE: this handler is called via Pads)
    def poke_4017(_addr, data)
      n = @cpu.update
      n += CPU::CLK_1 if @cpu.odd_clock?
      update(n)
      clock_frame_irq(n) if @frame_irq_clock <= n
      n += CPU::CLK_1
      @oscillator_clocks = OSCILLATOR_CLOCKS[data[7]]
      @frame_counter = (n + @oscillator_clocks[0]) * @fixed_clock
      @frame_divider = 0
      @frame_irq_clock = data & 0xc0 != 0 ? FOREVER_CLOCK : n + FRAME_CLOCKS[0]
      @frame_irq_repeat = 0
      @cpu.clear_irq(CPU::IRQ_FRAME) if data[6] != 0
      clock_oscillators(true) if data[7] != 0
    end

    def peek_40xx(_addr)
      0x40
    end

    ###########################################################################
    # helper classes

    # A counter for note length
    class LengthCounter
      LUT = [
        0x0a, 0xfe, 0x14, 0x02, 0x28, 0x04, 0x50, 0x06, 0xa0, 0x08, 0x3c, 0x0a, 0x0e, 0x0c, 0x1a, 0x0e,
        0x0c, 0x10, 0x18, 0x12, 0x30, 0x14, 0x60, 0x16, 0xc0, 0x18, 0x48, 0x1a, 0x10, 0x1c, 0x20, 0x1e,
      ]
      def reset
        @enabled = false
        @count = 0
      end

      attr_reader :count

      def enable(enabled)
        @enabled = enabled
        @count = 0 unless @enabled
        @enabled
      end

      def write(data, frame_counter_delta)
        @count = @enabled ? LUT[data] : 0 if frame_counter_delta || @count == 0
      end

      def clock
        return false if @count == 0
        @count -= 1
        return @count == 0
      end
    end

    # Wave envelope
    class Envelope
      attr_reader :output, :looping

      def reset_clock
        @reset = true
      end

      def reset
        @output = 0
        @count = 0
        @volume_base = @volume = 0
        @constant = true
        @looping = false
        @reset = false
        update_output
      end

      def clock
        if @reset
          @reset = false
          @volume = 0x0f
        else
          if @count != 0
            @count -= 1
            return
          end
          @volume = (@volume - 1) & 0x0f if @volume != 0 || @looping
        end
        @count = @volume_base
        update_output
      end

      def write(data)
        @volume_base = data & 0x0f
        @constant = data[4] == 1
        @looping = data[5] == 1
        update_output
      end

      def update_output
        @output = (@constant ? @volume_base : @volume) * CHANNEL_OUTPUT_MUL
      end
    end

    # Mixer (with DC Blocking filter)
    class Mixer
      VOL   = 192
      P_F   = 900
      P_0   = 9552 * CHANNEL_OUTPUT_MUL * VOL * (P_F / 100)
      P_1   = 8128 * CHANNEL_OUTPUT_MUL * P_F
      P_2   = P_F * 100
      TND_F = 500
      TND_0 = 16367 * CHANNEL_OUTPUT_MUL * VOL * (TND_F / 100)
      TND_1 = 24329 * CHANNEL_OUTPUT_MUL * TND_F
      TND_2 = TND_F * 100

      def initialize(pulse_0, pulse_1, triangle, noise, dmc)
        @pulse_0, @pulse_1, @triangle, @noise, @dmc = pulse_0, pulse_1, triangle, noise, dmc
      end

      def reset
        @acc = @prev = @next = 0
      end

      def sample
        dac0 = @pulse_0.sample + @pulse_1.sample
        dac1 = @triangle.sample + @noise.sample + @dmc.sample
        sample = (P_0 * dac0 / (P_1 + P_2 * dac0)) + (TND_0 * dac1 / (TND_1 + TND_2 * dac1))

        @acc -= @prev
        @prev = sample << 15
        @acc += @prev - @next * 3 # POLE
        sample = @next = @acc >> 15

        sample = -0x7fff if sample < -0x7fff
        sample = 0x7fff if sample > 0x7fff
        sample
      end
    end

    # base class for oscillator channels (Pulse, Triangle, and Noise)
    class Oscillator
      def inspect
        "#<#{ self.class }>"
      end

      def initialize(apu)
        @apu = apu
        @rate = @fixed = 1
        @envelope = @length_counter = @wave_length = nil
      end

      def reset
        @timer = 2048 * @fixed # 2048: reset cycles
        @freq = @fixed
        @amp = 0

        @wave_length = 0 if @wave_length
        @envelope.reset if @envelope
        @length_counter.reset if @length_counter
        @active = active?
      end

      def active?
        return false if @length_counter && @length_counter.count == 0
        return false if @envelope && @envelope.output == 0
        return true
      end

      def poke_0(_addr, data)
        if @envelope
          @apu.update_latency
          @envelope.write(data)
          @active = active?
        end
      end

      def poke_2(_addr, data)
        @apu.update
        if @wave_length
          @wave_length = (@wave_length & 0x0700) | (data & 0x00ff)
          update_freq
        end
      end

      def poke_3(_addr, data)
        delta = @apu.update_delta
        if @wave_length
          @wave_length = (@wave_length & 0x00ff) | ((data & 0x07) << 8)
          update_freq
        end
        @envelope.reset_clock if @envelope
        @length_counter.write(data >> 3, delta) if @length_counter
        @active = active?
      end

      def enable(enabled)
        @length_counter.enable(enabled)
        @active = active?
      end

      def update_settings(r, f)
        @freq = @freq / @fixed * f
        @timer = @timer / @fixed * f
        @rate, @fixed = r, f
      end

      def status
        @length_counter.count > 0
      end

      def clock_envelope
        @envelope.clock
        @active = active?
      end
    end

    #--------------------------------------------------------------------------

    ### Pulse channel ###
    class Pulse < Oscillator
      MIN_FREQ = 0x0008
      MAX_FREQ = 0x07ff
      WAVE_FORM = [0b11111101, 0b11111001, 0b11100001, 0b00000110].map {|n| (0..7).map {|i| n[i] * 0x1f } }

      def initialize(_apu)
        super
        @wave_length = 0
        @envelope = Envelope.new
        @length_counter = LengthCounter.new
      end

      def reset
        super
        @freq = @fixed * 2
        @valid_freq = false
        @step = 0
        @form = WAVE_FORM[0]
        @sweep_rate = 0
        @sweep_count = 1
        @sweep_reload = false
        @sweep_increase = -1
        @sweep_shift = 0
      end

      def active?
        super && @valid_freq
      end

      def update_freq
        if @wave_length >= MIN_FREQ && @wave_length + (@sweep_increase & @wave_length >> @sweep_shift) <= MAX_FREQ
          @freq = (@wave_length + 1) * 2 * @fixed
          @valid_freq = true
        else
          @valid_freq = false
        end
        @active = active?
      end

      def poke_0(_addr, data)
        super
        @form = WAVE_FORM[data >> 6 & 3]
      end

      def poke_1(_addr, data)
        @apu.update
        @sweep_increase = data[3] != 0 ? 0 : -1
        @sweep_shift = data & 0x07
        @sweep_rate = 0
        if data[7] == 1 && @sweep_shift > 0
          @sweep_rate = ((data >> 4) & 0x07) + 1
          @sweep_reload = true
        end
        update_freq
      end

      def poke_3(_addr, _data)
        super
        @step = 0
      end

      def clock_sweep(complement)
        @active = false if !@envelope.looping && @length_counter.clock
        if @sweep_rate != 0
          @sweep_count -= 1
          if @sweep_count == 0
            @sweep_count = @sweep_rate
            if @wave_length >= MIN_FREQ
              shifted = @wave_length >> @sweep_shift
              if @sweep_increase == 0
                @wave_length += complement - shifted
                update_freq
              elsif @wave_length + shifted <= MAX_FREQ
                @wave_length += shifted
                update_freq
              end
            end
          end
        end

        return unless @sweep_reload

        @sweep_reload = false
        @sweep_count = @sweep_rate
      end

      def sample
        sum = @timer
        @timer -= @rate
        if @active
          if @timer < 0
            sum >>= @form[@step]
            begin
              v = -@timer
              v = @freq if v > @freq
              sum += v >> @form[@step = (@step + 1) & 7]
              @timer += @freq
            end while @timer < 0
            @amp = (sum * @envelope.output + @rate / 2) / @rate
          else
            @amp = @envelope.output >> @form[@step]
          end
        else
          if @timer < 0
            count = (-@timer + @freq - 1) / @freq
            @step = (@step + count) & 7
            @timer += count * @freq
          end
          return 0 if @amp < CHANNEL_OUTPUT_DECAY
          @amp -= CHANNEL_OUTPUT_DECAY
        end
        @amp
      end
    end

    #--------------------------------------------------------------------------

    ### Triangle channel ###
    class Triangle < Oscillator
      MIN_FREQ = 2 + 1
      WAVE_FORM = (0..15).to_a + (0..15).to_a.reverse

      def initialize(_apu)
        super
        @wave_length = 0
        @length_counter = LengthCounter.new
      end

      def reset
        super
        @step = 7
        @status = :counting
        @linear_counter_load = 0
        @linear_counter_start = true
        @linear_counter = 0
      end

      def active?
        super && @linear_counter != 0 && @wave_length >= MIN_FREQ
      end

      def update_freq
        @freq = (@wave_length + 1) * @fixed
        @active = active?
      end

      def poke_0(_addr, data)
        super
        @apu.update
        @linear_counter_load = data & 0x7f
        @linear_counter_start = data[7] == 0
      end

      def poke_3(_addr, _data)
        super
        @status = :reload
      end

      def clock_linear_counter
        if @status == :counting
          @linear_counter -= 1 if @linear_counter != 0
        else
          @status = :counting if @linear_counter_start
          @linear_counter = @linear_counter_load
        end
        @active = active?
      end

      def clock_length_counter
        @active = false if @linear_counter_start && @length_counter.clock
      end

      def sample
        if @active
          sum = @timer
          @timer -= @rate
          if @timer < 0
            sum *= WAVE_FORM[@step]
            begin
              v = -@timer
              v = @freq if v > @freq
              sum += v * WAVE_FORM[@step = (@step + 1) & 0x1f]
              @timer += @freq
            end while @timer < 0
            @amp = (sum * CHANNEL_OUTPUT_MUL + @rate / 2) / @rate * 3
          else
            @amp = WAVE_FORM[@step] * CHANNEL_OUTPUT_MUL * 3
          end
        else
          return 0 if @amp < CHANNEL_OUTPUT_DECAY
          @amp -= CHANNEL_OUTPUT_DECAY
          @step = 0
        end
        @amp
      end
    end

    #--------------------------------------------------------------------------

    ### Noise channel ###
    class Noise < Oscillator
      LUT = [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]
      NEXT_BITS_1, NEXT_BITS_6 = [1, 6].map do |shifter|
        (0..0x7fff).map {|bits| bits[0] == bits[shifter] ? bits / 2 : bits / 2 + 0x4000 }
      end

      def initialize(_apu)
        super
        @envelope = Envelope.new
        @length_counter = LengthCounter.new
      end

      def reset
        super
        @freq = LUT[0] * @fixed
        @bits = 0x4000
        @shifter = NEXT_BITS_1
      end

      def poke_2(_addr, data)
        @apu.update
        @freq = LUT[data & 0x0f] * @fixed
        @shifter = data[7] != 0 ? NEXT_BITS_6 : NEXT_BITS_1
      end

      def clock_length_counter
        @active = false if !@envelope.looping && @length_counter.clock
      end

      def sample
        @timer -= @rate
        if @active
          return @bits.even? ? @envelope.output * 2 : 0 if @timer >= 0

          sum = @bits.even? ? (@timer + @rate) : 0
          begin
            @bits = @shifter[@bits]
            if @bits.even?
              v = -@timer
              v = @freq if v > @freq
              sum += v
            end
            @timer += @freq
          end while @timer < 0
          return (sum * @envelope.output + @rate / 2) / @rate * 2
        else
          while @timer < 0
            @bits = @shifter[@bits]
            @timer += @freq
          end
          return 0
        end
      end
    end

    #--------------------------------------------------------------------------

    ### DMC channel ###
    class DMC
      LUT = [428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54].map {|n| n * RP2A03_CC }

      def initialize(cpu, apu)
        @apu = apu
        @cpu = cpu
        @freq = LUT[0]
      end

      def reset
        @cur_sample          = 0
        @lin_sample          = 0
        @freq                = LUT[0]
        @loop                = false
        @irq_enable          = false
        @regs_length_counter = 1
        @regs_address        = 0xc000
        @out_active          = false
        @out_shifter         = 0
        @out_dac             = 0
        @out_buffer          = 0x00
        @dma_length_counter  = 0
        @dma_buffered        = false
        @dma_address         = 0xc000
        @dma_buffer          = 0x00
      end

      attr_reader :freq

      def enable(enabled)
        @cpu.clear_irq(CPU::IRQ_DMC)
        if !enabled
          @dma_length_counter = 0
        elsif @dma_length_counter == 0
          @dma_length_counter = @regs_length_counter
          @dma_address = @regs_address
          do_dma unless @dma_buffered
        end
      end

      def sample
        if @cur_sample != @lin_sample
          step = CHANNEL_OUTPUT_MUL * 8
          if @lin_sample + step < @cur_sample
            @lin_sample += step
          elsif @cur_sample < @lin_sample - step
            @lin_sample -= step
          else
            @lin_sample = @cur_sample
          end
        end
        @lin_sample
      end

      def do_dma
        @dma_buffer = @cpu.dmc_dma(@dma_address)
        @dma_address = 0x8000 | ((@dma_address + 1) & 0x7fff)
        @dma_buffered = true
        @dma_length_counter -= 1
        if @dma_length_counter == 0
          if @loop
            @dma_address = @regs_address
            @dma_length_counter = @regs_length_counter
          elsif @irq_enable
            @cpu.do_irq(CPU::IRQ_DMC, @cpu.current_clock)
          end
        end
      end

      def update
        @cur_sample = @out_dac * CHANNEL_OUTPUT_MUL
      end

      def poke_0(_addr, data)
        @loop = data[6] != 0
        @irq_enable = data[7] != 0
        @freq = LUT[data & 0x0f]
        @cpu.clear_irq(CPU::IRQ_DMC) unless @irq_enable
      end

      def poke_1(_addr, data)
        @apu.update
        @out_dac = data & 0x7f
        update
      end

      def poke_2(_addr, data)
        @regs_address = 0xc000 | (data << 6)
      end

      def poke_3(_addr, data)
        @regs_length_counter = (data << 4) + 1
      end

      def clock_dac
        if @out_active
          n = @out_dac + ((@out_buffer & 1) << 2) - 2
          @out_buffer >>= 1
          if 0 <= n && n <= 0x7f && n != @out_dac
            @out_dac = n
            return true
          end
        end
        return false
      end

      def clock_dma
        if @out_shifter == 0
          @out_shifter = 7
          @out_active = @dma_buffered
          if @out_active
            @dma_buffered = false
            @out_buffer = @dma_buffer
            do_dma if @dma_length_counter != 0
          end
        else
          @out_shifter -= 1
        end
      end

      def status
        @dma_length_counter > 0
      end
    end
  end
end


# optcarrot/ppu

# optcarrot/opt

module Optcarrot
  # dirty methods manipulating and generating methods...
  module CodeOptimizationHelper
    def initialize(loglevel, enabled_opts)
      @loglevel = loglevel
      options = self.class::OPTIONS
      opts = {}
      enabled_opts ||= [:all]
      default =
        (enabled_opts == [:all] || enabled_opts != [] && enabled_opts.all? {|opt| opt.to_s.start_with?("-") })
      options.each {|opt| opts[opt] = default }
      (enabled_opts - [:none, :all]).each do |opt|
        val = true
        if opt.to_s.start_with?("-")
          opt = opt.to_s[1..-1].to_sym
          val = false
        end
        raise "unknown optimization: `#{ opt }'" unless options.include?(opt)
        opts[opt] = val
      end
      options.each {|opt| instance_variable_set(:"@#{ opt }", opts[opt]) }
    end

    def depends(opt, depended_opt)
      if instance_variable_get(:"@#{ opt }") && !instance_variable_get(:"@#{ depended_opt }")
        raise "`#{ opt }' depends upon `#{ depended_opt }'"
      end
    end

    def gen(*codes)
      codes.map {|code| code.to_s.chomp }.join("\n") + "\n"
    end

    # change indent
    def indent(i, code)
      if i > 0
        code.gsub(/^(.+)$/) { " " * i + $1 }
      elsif i < 0
        code.gsub(/^ {#{ -i }}/, "")
      else
        code
      end
    end

    # generate a branch
    def branch(cond, code1, code2)
      gen(
        "if #{ cond }",
        indent(2, code1),
        "else",
        indent(2, code2),
        "end",
      )
    end

    MethodDef = Struct.new(:params, :body)

    METHOD_DEFINITIONS_RE = true || /
      ^(\ +)def\s+(\w+)(?:\((.*)\))?\n
      ^((?:\1\ +.*\n|\n)*)
      ^\1end$
    /x
    # extract all method definitions
    def parse_method_definitions(file)
      return {}
      src = File.read(file)
      mdefs = {}
      src.scan(METHOD_DEFINITIONS_RE) do |indent, meth, params, body|
        body = indent(-indent.size - 2, body)

        # noramlize: break `when ... then`
        body = body.gsub(/^( *)when +(.*?) +then +(.*)/) { $1 + "when #{ $2 }\n" + $1 + "  " + $3 }

        # normalize: return unless
        body = "if " + $1 + indent(2, $') + "end\n" if body =~ /\Areturn unless (.*)/

        # normalize: if modifier -> if statement
        nil while body.gsub!(/^( *)((?!#)\S.*) ((?:if|unless) .*\n)/) { indent($1.size, gen($3, "  " + $2, "end")) }

        mdefs[meth.to_sym] = MethodDef[params ? params.split(", ") : nil, body]
      end
      mdefs
    end

    # inline method calls with no arguments
    def expand_methods(code, mdefs, meths = mdefs.keys)
      code.gsub(/^( *)\b(#{ meths * "|" })\b(?:\((.*?)\))?\n/) do
        indent, meth, args = $1, $2, $3
        body = mdefs[meth.to_sym]
        body = body.body if body.is_a?(MethodDef)
        if args
          mdefs[meth.to_sym].params.zip(args.split(", ")) do |param, arg|
            body = replace_var(body, param, arg)
          end
        end
        indent(indent.size, body)
      end
    end

    def expand_inline_methods(code, meth, mdef)
      code.gsub(/\b#{ meth }\b(?:\(((?:@?\w+, )*@?\w+)\))?/) do
        args = $1
        b = "(#{ mdef.body.chomp.gsub(/ *#.*/, "").gsub("\n", "; ") })"
        if args
          mdef.params.zip(args.split(", ")) do |param, arg|
            b = replace_var(b, param, arg)
          end
        end
        b
      end
    end

    def replace_var(code, var, bool)
      re = var.start_with?("@") ? /#{ var }\b/ : /\b#{ var }\b/
      code.gsub(re) { bool }
    end

    def replace_cond_var(code, var, bool)
      code.gsub(/(if|unless)\s#{ var }\b/) { $1 + " " + bool }
    end

    TRIVIAL_BRANCH_RE = true || /
      ^(\ *)(if|unless)\ (true|false)\n
      ^((?:\1\ +.*\n|\n)*)
       (?:
         \1else\n
         ((?:\1\ +.*\n|\n)*)
       )?
      ^\1end\n
    /x
    # remove "if true" or "if false"
    def remove_trivial_branches(code)
      return code
      code = code.dup
      nil while
        code.gsub!(TRIVIAL_BRANCH_RE) do
          if ($2 == "if") == ($3 == "true")
            indent(-2, $4)
          else
            $5 ? indent(-2, $5) : ""
          end
        end
      code
    end

    # replace instance variables with temporal local variables
    # CAUTION: the instance variable must not be accessed out of CPU#run
    def localize_instance_variables(code, ivars = code.scan(/@\w+/).uniq.sort)
      ivars = ivars.map {|ivar| ivar.to_s[1..-1] }

      inits, finals = [], []
      ivars.each do |ivar|
        lvar = "__#{ ivar }__"
        inits << "#{ lvar } = @#{ ivar }"
        finals << "@#{ ivar } = #{ lvar }"
      end

      code = code.gsub(/@(#{ ivars * "|" })\b/) { "__#{ $1 }__" }

      gen(
        "begin",
        indent(2, inits.join("\n")),
        indent(2, code),
        "ensure",
        indent(2, finals.join("\n")),
        "end",
      )
    end
  end
end



module Optcarrot
  # PPU implementation (video output)
  class PPU
    # clock/timing constants (stolen from Nestopia)
    RP2C02_CC         = 4
    RP2C02_HACTIVE    = RP2C02_CC * 256
    RP2C02_HBLANK     = RP2C02_CC * 85
    RP2C02_HSYNC      = RP2C02_HACTIVE + RP2C02_HBLANK
    RP2C02_VACTIVE    = 240
    RP2C02_VSLEEP     = 1
    RP2C02_VINT       = 20
    RP2C02_VDUMMY     = 1
    RP2C02_VBLANK     = RP2C02_VSLEEP + RP2C02_VINT + RP2C02_VDUMMY
    RP2C02_VSYNC      = RP2C02_VACTIVE + RP2C02_VBLANK
    RP2C02_HVSYNCBOOT = RP2C02_VACTIVE * RP2C02_HSYNC + RP2C02_CC * 312
    RP2C02_HVINT      = RP2C02_VINT * RP2C02_HSYNC
    RP2C02_HVSYNC_0   = RP2C02_VSYNC * RP2C02_HSYNC
    RP2C02_HVSYNC_1   = RP2C02_VSYNC * RP2C02_HSYNC - RP2C02_CC

    # special scanlines
    SCANLINE_HDUMMY = -1  # pre-render scanline
    SCANLINE_VBLANK = 240 # post-render scanline

    # special horizontal clocks
    HCLOCK_DUMMY    = 341
    HCLOCK_VBLANK_0 = 681
    HCLOCK_VBLANK_1 = 682
    HCLOCK_VBLANK_2 = 684
    HCLOCK_BOOT     = 685
    DUMMY_FRAME = [RP2C02_HVINT / RP2C02_CC - HCLOCK_DUMMY, RP2C02_HVINT, RP2C02_HVSYNC_0]
    BOOT_FRAME = [RP2C02_HVSYNCBOOT / RP2C02_CC - HCLOCK_BOOT, RP2C02_HVSYNCBOOT, RP2C02_HVSYNCBOOT]

    # constants related to OAM (sprite)
    SP_PIXEL_POSITIONS = {
      0 => [3, 7, 2, 6, 1, 5, 0, 4], # normal
      1 => [4, 0, 5, 1, 6, 2, 7, 3], # flip
    }

    # A look-up table mapping: (two pattern bytes * attr) -> eight pixels
    #   TILE_LUT[attr][high_byte * 0x100 + low_byte] = [pixels] * 8
    TILE_LUT = [0x0, 0x4, 0x8, 0xc].map do |attr|
      (0..7).map do |j|
        (0...0x10000).map do |i|
          clr = i[15 - j] * 2 + i[7 - j]
          clr != 0 ? attr | clr : 0
        end
      end.transpose
      # Super dirty hack: This Array#transpose reduces page-faults.
      # It might generate cache-friendly memory layout...
    end

    def inspect
      "#<#{ self.class }>"
    end

    ###########################################################################
    # initialization

    def initialize(conf, cpu, palette)
      @conf = conf
      @cpu = cpu
      @palette = palette

      if @conf.load_ppu
        eval(File.read(@conf.load_ppu))
      elsif @conf.opt_ppu and false
        eval(OptimizedCodeBuilder.new(@conf.loglevel, @conf.opt_ppu).build, nil, "(generated PPU core)")
      end

      @nmt_mem = [[0xff] * 0x400, [0xff] * 0x400]
      @nmt_ref = [0, 1, 0, 1].map {|i| @nmt_mem[i] }

      @output_pixels = []
      @output_color = [@palette[0]] * 0x20 # palette size is 0x20

      reset(mapping: false)
      setup_lut
    end

    def reset(opt = {})
      if opt.fetch(:mapping, true)
        # setup mapped memory
        @cpu.add_mappings(0x2000.step(0x3fff, 8), method(:peek_2xxx), method(:poke_2000))
        @cpu.add_mappings(0x2001.step(0x3fff, 8), method(:peek_2xxx), method(:poke_2001))
        @cpu.add_mappings(0x2002.step(0x3fff, 8), method(:peek_2002), method(:poke_2xxx))
        @cpu.add_mappings(0x2003.step(0x3fff, 8), method(:peek_2xxx), method(:poke_2003))
        @cpu.add_mappings(0x2004.step(0x3fff, 8), method(:peek_2004), method(:poke_2004))
        @cpu.add_mappings(0x2005.step(0x3fff, 8), method(:peek_2xxx), method(:poke_2005))
        @cpu.add_mappings(0x2006.step(0x3fff, 8), method(:peek_2xxx), method(:poke_2006))
        @cpu.add_mappings(0x2007.step(0x3fff, 8), method(:peek_2007), method(:poke_2007))
        @cpu.add_mappings(0x3000, method(:peek_3000), method(:poke_2000))
        @cpu.add_mappings(0x4014, method(:peek_4014), method(:poke_4014))
      end

      @palette_ram = [
        0x3f, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0d,
        0x08, 0x10, 0x08, 0x24, 0x00, 0x00, 0x04, 0x2c,
        0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14,
        0x08, 0x3a, 0x00, 0x02, 0x00, 0x20, 0x2c, 0x08,
      ]
      @coloring = 0x3f # not monochrome
      @emphasis = 0
      update_output_color

      # clock management
      @hclk = HCLOCK_BOOT
      @vclk = 0
      @hclk_target = FOREVER_CLOCK

      # CPU-PPU interface
      @io_latch = 0
      @io_buffer = 0xe8 # garbage

      @regs_oam = 0

      # misc
      @vram_addr_inc = 1 # 1 or 32
      @need_nmi = false
      @pattern_end = 0x0ff0
      @any_show = false # == @bg_show || @sp_show
      @sp_overflow = false
      @sp_zero_hit = false
      @vblanking = @vblank = false

      # PPU-nametable interface
      @io_addr = 0
      @io_pattern = 0

      @a12_monitor = nil
      @a12_state = nil

      # the current scanline
      @odd_frame = false
      @scanline = SCANLINE_VBLANK

      # scroll state
      @scroll_toggle = false
      @scroll_latch = 0
      @scroll_xfine = 0
      @scroll_addr_0_4 = @scroll_addr_5_14 = 0
      @name_io_addr = 0x2000 # == (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x0fff | 0x2000

      ### BG-sprite state
      @bg_enabled = false
      @bg_show = false
      @bg_show_edge = false
      @bg_pixels = [0] * 16
      @bg_pattern_base = 0 # == 0 or 0x1000
      @bg_pattern_base_15 = 0 # == @bg_pattern_base[12] << 15
      @bg_pattern = 0
      @bg_pattern_lut = TILE_LUT[0]
      @bg_pattern_lut_fetched = TILE_LUT[0]
      # invariant:
      #   @bg_pattern_lut_fetched == TILE_LUT[
      #     @nmt_ref[@io_addr >> 10 & 3][@io_addr & 0x03ff] >>
      #       ((@scroll_addr_0_4 & 0x2) | (@scroll_addr_5_14[6] * 0x4)) & 3
      #   ]

      ### OAM-sprite state
      @sp_enabled = false
      @sp_active = false # == @sp_visible && @sp_enabled
      @sp_show = false
      @sp_show_edge = false

      # for CPU-PPU interface
      @sp_base = 0
      @sp_height = 8

      # for OAM fetcher
      @sp_phase = 0
      @sp_ram = [0xff] * 0x100 # ram size is 0x100, 0xff is a OAM garbage
      @sp_index = 0
      @sp_addr = 0
      @sp_latch = 0

      # for internal state
      # 8 sprites per line are allowed in standard NES, but a user may remove this limit.
      @sp_limit = (@conf.sprite_limit ? 8 : 32) * 4
      @sp_buffer = [0] * @sp_limit
      @sp_buffered = 0
      @sp_visible = false
      @sp_map = [nil] * 264 # [[behind?, zero?, color]]
      @sp_map_buffer = (0...264).map { [false, false, 0] } # preallocation for @sp_map
      @sp_zero_in_line = false
    end

    def update_output_color
      0x20.times do |i|
        @output_color[i] = @palette[@palette_ram[i] & @coloring | @emphasis]
      end
    end

    def setup_lut
      @lut_update = {}.compare_by_identity

      @name_lut = (0..0xffff).map do |i|
        nmt_bank = @nmt_ref[i >> 10 & 3]
        nmt_idx = i & 0x03ff
        fixed = (i >> 12 & 7) | (i[15] << 12)
        (((@lut_update[nmt_bank] ||= [])[nmt_idx] ||= [nil, nil])[0] ||= []) << [i, fixed]
        nmt_bank[nmt_idx] << 4 | fixed
      end

      entries = {}
      @attr_lut = (0..0x7fff).map do |i|
        io_addr = 0x23c0 | (i & 0x0c00) | (i >> 4 & 0x0038) | (i >> 2 & 0x0007)
        nmt_bank = @nmt_ref[io_addr >> 10 & 3]
        nmt_idx = io_addr & 0x03ff
        attr_shift = (i & 2) | (i >> 4 & 4)
        key = [io_addr, attr_shift]
        entries[key] ||= [io_addr, TILE_LUT[nmt_bank[nmt_idx] >> attr_shift & 3], attr_shift]
        (((@lut_update[nmt_bank] ||= [])[nmt_idx] ||= [nil, nil])[1] ||= []) << entries[key]
        entries[key]
      end.freeze
      entries.each_value {|a| a.uniq! {|entry| entry.object_id } }
    end

    ###########################################################################
    # other APIs

    attr_reader :output_pixels

    def set_chr_mem(mem, writable)
      @chr_mem = mem
      @chr_mem_writable = writable
    end

    NMT_TABLE = {
      horizontal:  [0, 0, 1, 1],
      vertical:    [0, 1, 0, 1],
      four_screen: [0, 1, 2, 3],
      first:       [0, 0, 0, 0],
      second:      [1, 1, 1, 1],
    }
    def nametables=(mode)
      update(RP2C02_CC)
      idxs = NMT_TABLE[mode]
      return if (0..3).all? {|i| @nmt_ref[i].equal?(@nmt_mem[idxs[i]]) }
      @nmt_ref[0] = @nmt_mem[idxs[0]]
      @nmt_ref[1] = @nmt_mem[idxs[1]]
      @nmt_ref[2] = @nmt_mem[idxs[2]]
      @nmt_ref[3] = @nmt_mem[idxs[3]]
      setup_lut
    end

    def update(data_setup)
      sync(data_setup + @cpu.update)
    end

    def setup_frame
      @output_pixels.clear
      @odd_frame = !@odd_frame
      @vclk, @hclk_target, @cpu.next_frame_clock = @hclk == HCLOCK_DUMMY ? DUMMY_FRAME : BOOT_FRAME
    end

    def vsync
      if @hclk_target != FOREVER_CLOCK
        @hclk_target = FOREVER_CLOCK
        run
      end
      @output_pixels << @palette[15] while @output_pixels.size < 256 * 240 # fill black
    end

    def monitor_a12_rising_edge(monitor)
      @a12_monitor = monitor
    end

    ###########################################################################
    # helpers

    def update_vram_addr
      if @vram_addr_inc == 32
        if active?
          if @scroll_addr_5_14 & 0x7000 == 0x7000
            @scroll_addr_5_14 &= 0x0fff
            case @scroll_addr_5_14 & 0x03e0
            when 0x03a0 then @scroll_addr_5_14 ^= 0x0800
            when 0x03e0 then @scroll_addr_5_14 &= 0x7c00
            else             @scroll_addr_5_14 += 0x20
            end
          else
            @scroll_addr_5_14 += 0x1000
          end
        else
          @scroll_addr_5_14 += 0x20
        end
      elsif @scroll_addr_0_4 < 0x1f
        @scroll_addr_0_4 += 1
      else
        @scroll_addr_0_4 = 0
        @scroll_addr_5_14 += 0x20
      end
      update_scroll_address_line
    end

    def update_scroll_address_line
      @name_io_addr = (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x0fff | 0x2000
      if @a12_monitor
        a12_state = @scroll_addr_5_14 & 0x3000 == 0x1000
        @a12_monitor.a12_signaled(@cpu.current_clock) if !@a12_state && a12_state
        @a12_state = a12_state
      end
    end

    def active?
      @scanline != SCANLINE_VBLANK && @any_show
    end

    def sync(elapsed)
      return unless @hclk_target < elapsed
      @hclk_target = elapsed / RP2C02_CC - @vclk
      run
    end

    def make_sure_invariants
      @name_io_addr = (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x0fff | 0x2000
      @bg_pattern_lut_fetched = TILE_LUT[
        @nmt_ref[@io_addr >> 10 & 3][@io_addr & 0x03ff] >> ((@scroll_addr_0_4 & 0x2) | (@scroll_addr_5_14[6] * 0x4)) & 3
      ]
    end

    def io_latch_mask(data)
      if active?
        0xff
      elsif @regs_oam & 0x03 == 0x02
        data & 0xe3
      else
        data
      end
    end

    ###########################################################################
    # mapped memory handlers

    # PPUCTRL
    def poke_2000(_addr, data)
      update(RP2C02_CC)
      need_nmi_old = @need_nmi

      @scroll_latch    = (@scroll_latch & 0x73ff) | (data & 0x03) << 10
      @vram_addr_inc   = data[2] == 1 ? 32 : 1
      @sp_base         = data[3] == 1 ? 0x1000 : 0x0000
      @bg_pattern_base = data[4] == 1 ? 0x1000 : 0x0000
      @sp_height       = data[5] == 1 ? 16 : 8
      @need_nmi        = data[7] == 1

      @io_latch = data
      @pattern_end = @sp_base != 0 || @sp_height == 16 ? 0x1ff0 : 0x0ff0
      @bg_pattern_base_15 = @bg_pattern_base[12] << 15

      if @need_nmi && @vblank && !need_nmi_old
        clock = @cpu.current_clock + RP2C02_CC
        @cpu.do_nmi(clock) if clock < RP2C02_HVINT
      end
    end

    # PPUMASK
    def poke_2001(_addr, data)
      update(RP2C02_CC)
      bg_show_old, bg_show_edge_old = @bg_show, @bg_show_edge
      sp_show_old, sp_show_edge_old = @sp_show, @sp_show_edge
      any_show_old = @any_show
      coloring_old, emphasis_old = @coloring, @emphasis

      @bg_show      = data[3] == 1
      @bg_show_edge = data[1] == 1 && @bg_show
      @sp_show      = data[4] == 1
      @sp_show_edge = data[2] == 1 && @sp_show
      @any_show = @bg_show || @sp_show
      @coloring = data[0] == 1 ? 0x30 : 0x3f # 0x30: monochrome
      @emphasis = (data & 0xe0) << 1

      @io_latch = data

      if bg_show_old != @bg_show || bg_show_edge_old != @bg_show_edge ||
         sp_show_old != @sp_show || sp_show_edge_old != @sp_show_edge

        if @hclk < 8 || @hclk >= 248
          update_enabled_flags_edge
        else
          update_enabled_flags
        end
        update_scroll_address_line if any_show_old && !@any_show
      end

      update_output_color if coloring_old != @coloring || emphasis_old != @emphasis
    end

    # PPUSTATUS
    def peek_2002(_addr)
      update(RP2C02_CC)
      v = @io_latch & 0x1f
      v |= 0x80 if @vblank
      v |= 0x40 if @sp_zero_hit
      v |= 0x20 if @sp_overflow
      @io_latch = v
      @scroll_toggle = false
      @vblanking = @vblank = false
      @io_latch
    end

    # OAMADDR
    def poke_2003(_addr, data)
      update(RP2C02_CC)
      @regs_oam = @io_latch = data
    end

    # OAMDATA (write)
    def poke_2004(_addr, data)
      update(RP2C02_CC)
      @io_latch = @sp_ram[@regs_oam] = io_latch_mask(data)
      @regs_oam = (@regs_oam + 1) & 0xff
    end

    # OAMDATA (read)
    def peek_2004(_addr)
      if !@any_show || @cpu.current_clock - (@cpu.next_frame_clock - (341 * 241) * RP2C02_CC) >= (341 * 240) * RP2C02_CC
        @io_latch = @sp_ram[@regs_oam]
      else
        update(RP2C02_CC)
        @io_latch = @sp_latch
      end
    end

    # PPUSCROLL
    def poke_2005(_addr, data)
      update(RP2C02_CC)
      @io_latch = data
      @scroll_toggle = !@scroll_toggle
      if @scroll_toggle
        @scroll_latch = @scroll_latch & 0x7fe0 | (data >> 3)
        xfine = 8 - (data & 0x7)
        @bg_pixels.rotate!(@scroll_xfine - xfine)
        @scroll_xfine = xfine
      else
        @scroll_latch = (@scroll_latch & 0x0c1f) | ((data << 2 | data << 12) & 0x73e0)
      end
    end

    # PPUADDR
    def poke_2006(_addr, data)
      update(RP2C02_CC)
      @io_latch = data
      @scroll_toggle = !@scroll_toggle
      if @scroll_toggle
        @scroll_latch = @scroll_latch & 0x00ff | (data & 0x3f) << 8
      else
        @scroll_latch = (@scroll_latch & 0x7f00) | data
        @scroll_addr_0_4  = @scroll_latch & 0x001f
        @scroll_addr_5_14 = @scroll_latch & 0x7fe0
        update_scroll_address_line
      end
    end

    # PPUDATA (write)
    def poke_2007(_addr, data)
      update(RP2C02_CC * 4)
      addr = @scroll_addr_0_4 | @scroll_addr_5_14
      update_vram_addr
      @io_latch = data
      if addr & 0x3f00 == 0x3f00
        addr &= 0x1f
        final = @palette[data & @coloring | @emphasis]
        @palette_ram[addr] = data
        @output_color[addr] = final
        if addr & 3 == 0
          @palette_ram[addr ^ 0x10] = data
          @output_color[addr ^ 0x10] = final
        end
        @output_bg_color = @palette_ram[0] & 0x3f
      else
        addr &= 0x3fff
        if addr >= 0x2000
          nmt_bank = @nmt_ref[addr >> 10 & 0x3]
          nmt_idx = addr & 0x03ff
          if nmt_bank[nmt_idx] != data
            nmt_bank[nmt_idx] = data

            name_lut_update, attr_lut_update = @lut_update[nmt_bank][nmt_idx]
            name_lut_update.each {|i, b| @name_lut[i] = data << 4 | b } if name_lut_update
            attr_lut_update.each {|a| a[1] = TILE_LUT[data >> a[2] & 3] } if attr_lut_update
          end
        elsif @chr_mem_writable
          @chr_mem[addr] = data
        end
      end
    end

    # PPUDATA (read)
    def peek_2007(_addr)
      update(RP2C02_CC)
      addr = (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x3fff
      update_vram_addr
      @io_latch = (addr & 0x3f00) != 0x3f00 ? @io_buffer : @palette_ram[addr & 0x1f] & @coloring
      @io_buffer = addr >= 0x2000 ? @nmt_ref[addr >> 10 & 0x3][addr & 0x3ff] : @chr_mem[addr]
      @io_latch
    end

    def poke_2xxx(_addr, data)
      @io_latch = data
    end

    def peek_2xxx(_addr)
      @io_latch
    end

    def peek_3000(_addr)
      update(RP2C02_CC)
      @io_latch
    end

    # OAMDMA
    def poke_4014(_addr, data) # DMA
      @cpu.steal_clocks(CPU::CLK_1) if @cpu.odd_clock?
      update(RP2C02_CC)
      @cpu.steal_clocks(CPU::CLK_1)
      data <<= 8
      if @regs_oam == 0 && data < 0x2000 && (!@any_show || @cpu.current_clock <= RP2C02_HVINT - CPU::CLK_1 * 512)
        @cpu.steal_clocks(CPU::CLK_1 * 512)
        @cpu.sprite_dma(data & 0x7ff, @sp_ram)
        @io_latch = @sp_ram[0xff]
      else
        begin
          @io_latch = @cpu.fetch(data)
          data += 1
          @cpu.steal_clocks(CPU::CLK_1)
          update(RP2C02_CC)
          @cpu.steal_clocks(CPU::CLK_1)
          @io_latch = io_latch_mask(@io_latch)
          @sp_ram[@regs_oam] = @io_latch
          @regs_oam = (@regs_oam + 1) & 0xff
        end while data & 0xff != 0
      end
    end

    def peek_4014(_addr)
      0x40
    end

    ###########################################################################
    # helper methods for PPU#run

    # NOTE: These methods will be adhocly-inlined.  Keep compatibility with
    # OptimizedCodeBuilder (e.g., do not change the parameter names blindly).

    def open_pattern(exp)
      return unless @any_show
      @io_addr = exp
      update_address_line
    end

    def open_sprite(buffer_idx)
      flip_v = @sp_buffer[buffer_idx + 2][7] # OAM byte2 bit7: "Flip vertically" flag
      tmp = (@scanline - @sp_buffer[buffer_idx]) ^ (flip_v * 0xf)
      byte1 = @sp_buffer[buffer_idx + 1]
      addr = @sp_height == 16 ? ((byte1 & 0x01) << 12) | ((byte1 & 0xfe) << 4) | (tmp[3] * 0x10) : @sp_base | byte1 << 4
      addr | (tmp & 7)
    end

    def load_sprite(pat0, pat1, buffer_idx)
      byte2 = @sp_buffer[buffer_idx + 2]
      pos = SP_PIXEL_POSITIONS[byte2[6]] # OAM byte2 bit6: "Flip horizontally" flag
      pat = (pat0 >> 1 & 0x55) | (pat1 & 0xaa) | ((pat0 & 0x55) | (pat1 << 1 & 0xaa)) << 8
      x_base = @sp_buffer[buffer_idx + 3]
      palette_base = 0x10 + ((byte2 & 3) << 2) # OAM byte2 bit0-1: Palette
      @sp_visible ||= @sp_map.clear
      8.times do |dx|
        x = x_base + dx
        clr = (pat >> (pos[dx] * 2)) & 3
        next if @sp_map[x] || clr == 0
        @sp_map[x] = sprite = @sp_map_buffer[x]
        # sprite[0]: behind flag, sprite[1]: zero hit flag, sprite[2]: color
        sprite[0] = byte2[5] == 1 # OAM byte2 bit5: "Behind background" flag
        sprite[1] = buffer_idx == 0 && @sp_zero_in_line
        sprite[2] = palette_base + clr
      end
      @sp_active = @sp_enabled
    end

    def update_address_line
      if @a12_monitor
        a12_state = @io_addr[12] == 1
        @a12_monitor.a12_signaled((@vclk + @hclk) * RP2C02_CC) if !@a12_state && a12_state
        @a12_state = a12_state
      end
    end

    ###########################################################################
    # actions for PPU#run

    def open_name
      return unless @any_show
      @io_addr = @name_io_addr
      update_address_line
    end

    def fetch_name
      return unless @any_show
      @io_pattern = @name_lut[@scroll_addr_0_4 + @scroll_addr_5_14 + @bg_pattern_base_15]
    end

    def open_attr
      return unless @any_show
      @io_addr, @bg_pattern_lut_fetched, = @attr_lut[@scroll_addr_0_4 + @scroll_addr_5_14]
      update_address_line
    end

    def fetch_attr
      return unless @any_show
      @bg_pattern_lut = @bg_pattern_lut_fetched
      # raise unless @bg_pattern_lut_fetched ==
      #   @nmt_ref[@io_addr >> 10 & 3][@io_addr & 0x03ff] >>
      #     ((@scroll_addr_0_4 & 0x2) | (@scroll_addr_5_14[6] * 0x4)) & 3
    end

    def fetch_bg_pattern_0
      return unless @any_show
      @bg_pattern = @chr_mem[@io_addr & 0x1fff]
    end

    def fetch_bg_pattern_1
      return unless @any_show
      @bg_pattern |= @chr_mem[@io_addr & 0x1fff] * 0x100
    end

    def scroll_clock_x
      return unless @any_show
      if @scroll_addr_0_4 < 0x001f
        @scroll_addr_0_4 += 1
        @name_io_addr += 1 # make cache consistent
      else
        @scroll_addr_0_4 = 0
        @scroll_addr_5_14 ^= 0x0400
        @name_io_addr ^= 0x041f # make cache consistent
      end
    end

    def scroll_reset_x
      return unless @any_show
      @scroll_addr_0_4 = @scroll_latch & 0x001f
      @scroll_addr_5_14 = (@scroll_addr_5_14 & 0x7be0) | (@scroll_latch & 0x0400)
      @name_io_addr = (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x0fff | 0x2000 # make cache consistent
    end

    def scroll_clock_y
      return unless @any_show
      if @scroll_addr_5_14 & 0x7000 != 0x7000
        @scroll_addr_5_14 += 0x1000
      else
        mask = @scroll_addr_5_14 & 0x03e0
        # rubocop:disable Style/CaseLikeIf
        if mask == 0x03a0
          @scroll_addr_5_14 ^= 0x0800
          @scroll_addr_5_14 &= 0x0c00
        elsif mask == 0x03e0
          @scroll_addr_5_14 &= 0x0c00
        else
          @scroll_addr_5_14 = (@scroll_addr_5_14 & 0x0fe0) + 32
        end
        # rubocop:enable Style/CaseLikeIf
      end

      @name_io_addr = (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x0fff | 0x2000 # make cache consistent
    end

    def preload_tiles
      return unless @any_show
      @bg_pixels[@scroll_xfine, 8] = @bg_pattern_lut[@bg_pattern]
    end

    def load_tiles
      return unless @any_show
      @bg_pixels.rotate!(8)
      @bg_pixels[@scroll_xfine, 8] = @bg_pattern_lut[@bg_pattern]
    end

    def evaluate_sprites_even
      return unless @any_show
      @sp_latch = @sp_ram[@sp_addr]
    end

    def evaluate_sprites_odd
      return unless @any_show

      # we first check phase 1 since it is the most-likely case
      if @sp_phase # nil represents phase 1
        # the second most-likely case is phase 9
        if @sp_phase == 9
          evaluate_sprites_odd_phase_9
        else
          # other cases are relatively rare
          case @sp_phase
          # when 1 then evaluate_sprites_odd_phase_1
          # when 9 then evaluate_sprites_odd_phase_9
          when 2 then evaluate_sprites_odd_phase_2
          when 3 then evaluate_sprites_odd_phase_3
          when 4 then evaluate_sprites_odd_phase_4
          when 5 then evaluate_sprites_odd_phase_5
          when 6 then evaluate_sprites_odd_phase_6
          when 7 then evaluate_sprites_odd_phase_7
          when 8 then evaluate_sprites_odd_phase_8
          end
        end
      else
        evaluate_sprites_odd_phase_1
      end
    end

    def evaluate_sprites_odd_phase_1
      @sp_index += 1
      if @sp_latch <= @scanline && @scanline < @sp_latch + @sp_height
        @sp_addr += 1
        @sp_phase = 2
        @sp_buffer[@sp_buffered] = @sp_latch
      elsif @sp_index == 64
        @sp_addr = 0
        @sp_phase = 9
      elsif @sp_index == 2
        @sp_addr = 8
      else
        @sp_addr += 4
      end
    end

    def evaluate_sprites_odd_phase_2
      @sp_addr += 1
      @sp_phase = 3
      @sp_buffer[@sp_buffered + 1] = @sp_latch
    end

    def evaluate_sprites_odd_phase_3
      @sp_addr += 1
      @sp_phase = 4
      @sp_buffer[@sp_buffered + 2] = @sp_latch
    end

    def evaluate_sprites_odd_phase_4
      @sp_buffer[@sp_buffered + 3] = @sp_latch
      @sp_buffered += 4
      if @sp_index != 64
        @sp_phase = @sp_buffered != @sp_limit ? nil : 5
        if @sp_index != 2
          @sp_addr += 1
          @sp_zero_in_line ||= @sp_index == 1
        else
          @sp_addr = 8
        end
      else
        @sp_addr = 0
        @sp_phase = 9
      end
    end

    def evaluate_sprites_odd_phase_5
      if @sp_latch <= @scanline && @scanline < @sp_latch + @sp_height
        @sp_phase = 6
        @sp_addr = (@sp_addr + 1) & 0xff
        @sp_overflow = true
      else
        @sp_addr = ((@sp_addr + 4) & 0xfc) + ((@sp_addr + 1) & 3)
        if @sp_addr <= 5
          @sp_phase = 9
          @sp_addr &= 0xfc
        end
      end
    end

    def evaluate_sprites_odd_phase_6
      @sp_phase = 7
      @sp_addr = (@sp_addr + 1) & 0xff
    end

    def evaluate_sprites_odd_phase_7
      @sp_phase = 8
      @sp_addr = (@sp_addr + 1) & 0xff
    end

    def evaluate_sprites_odd_phase_8
      @sp_phase = 9
      @sp_addr = (@sp_addr + 1) & 0xff
      @sp_addr += 1 if @sp_addr & 3 == 3
      @sp_addr &= 0xfc
    end

    def evaluate_sprites_odd_phase_9
      @sp_addr = (@sp_addr + 4) & 0xff
    end

    def load_extended_sprites
      return unless @any_show
      if 32 < @sp_buffered
        buffer_idx = 32
        begin
          addr = open_sprite(buffer_idx)
          pat0 = @chr_mem[addr]
          pat1 = @chr_mem[addr | 8]
          load_sprite(pat0, pat1, buffer_idx) if pat0 != 0 || pat1 != 0
          buffer_idx += 4
        end while buffer_idx != @sp_buffered
      end
    end

    def render_pixel
      if @any_show
        pixel = @bg_enabled ? @bg_pixels[@hclk % 8] : 0
        if @sp_active && (sprite = @sp_map[@hclk])
          if pixel % 4 == 0
            pixel = sprite[2]
          else
            @sp_zero_hit = true if sprite[1] && @hclk != 255
            pixel = sprite[2] unless sprite[0]
          end
        end
      else
        pixel = @scroll_addr_5_14 & 0x3f00 == 0x3f00 ? @scroll_addr_0_4 : 0
        @bg_pixels[@hclk % 8] = 0
      end
      @output_pixels << @output_color[pixel]
    end

    # just a placeholder; used for batch_render_pixels optimization
    def batch_render_eight_pixels
    end

    def boot
      @vblank = true
      @hclk = HCLOCK_DUMMY
      @hclk_target = FOREVER_CLOCK
    end

    def vblank_0
      @vblanking = true
      @hclk = HCLOCK_VBLANK_1
    end

    def vblank_1
      @vblank ||= @vblanking
      @vblanking = false
      @sp_visible = false
      @sp_active = false
      @hclk = HCLOCK_VBLANK_2
    end

    def vblank_2
      @vblank ||= @vblanking
      @vblanking = false
      @hclk = HCLOCK_DUMMY
      @hclk_target = FOREVER_CLOCK
      @cpu.do_nmi(@cpu.next_frame_clock) if @need_nmi && @vblank
    end

    def update_enabled_flags
      return unless @any_show
      @bg_enabled = @bg_show
      @sp_enabled = @sp_show
      @sp_active = @sp_enabled && @sp_visible
    end

    def update_enabled_flags_edge
      @bg_enabled = @bg_show_edge
      @sp_enabled = @sp_show_edge
      @sp_active = @sp_enabled && @sp_visible
    end

    ###########################################################################
    # default core

    def debug_logging(scanline, hclk, hclk_target)
      hclk = "forever" if hclk == FOREVER_CLOCK
      hclk_target = "forever" if hclk_target == FOREVER_CLOCK

      @conf.debug("ppu: scanline #{ scanline }, hclk #{ hclk }->#{ hclk_target }")
    end

    def run
      @fiber ||= Fiber.new { main_loop }

      debug_logging(@scanline, @hclk, @hclk_target) if @conf.loglevel >= 3

      make_sure_invariants

      @hclk_target = (@vclk + @hclk) * RP2C02_CC unless @fiber.resume
    end

    def wait_frame
      Fiber.yield true
    end

    def wait_zero_clocks
      Fiber.yield if @hclk_target <= @hclk
    end

    def wait_one_clock
      @hclk += 1
      Fiber.yield if @hclk_target <= @hclk
    end

    def wait_two_clocks
      @hclk += 2
      Fiber.yield if @hclk_target <= @hclk
    end

    ### main-loop structure
    #
    # # wait for boot
    # clk_685
    #
    # loop do
    #   # pre-render scanline
    #   clk_341, clk_342, ..., clk_659
    #   while true
    #     # visible scanline (not shown)
    #     clk_320, clk_321, ..., clk_337
    #
    #     # increment scanline
    #     clk_338
    #     break if @scanline == 240
    #
    #     # visible scanline (shown)
    #     clk_0, clk_1, ..., clk_319
    #   end
    #
    #   # post-render sacnline (vblank)
    #   do_681,682,684
    # end
    #
    # This method definition also serves as a template for OptimizedCodeBuilder.
    # Comments like "when NNN" are markers for the purpose.
    #
    # rubocop:disable Metrics/MethodLength, Metrics/CyclomaticComplexity, Metrics/PerceivedComplexity, Metrics/AbcSize, Style/SoleNestedConditional
    def main_loop
      # when 685

      # wait for boot
      boot
      wait_frame

      while true
        # pre-render scanline

        341.step(589, 8) do
          # when 341, 349, ..., 589
          if @hclk == 341
            @sp_overflow = @sp_zero_hit = @vblanking = @vblank = false
            @scanline = SCANLINE_HDUMMY
          end
          open_name
          wait_two_clocks

          # when 343, 351, ..., 591
          open_attr
          wait_two_clocks

          # when 345, 353, ..., 593
          open_pattern(@bg_pattern_base)
          wait_two_clocks

          # when 347, 355, ..., 595
          open_pattern(@io_addr | 8)
          wait_two_clocks
        end

        597.step(653, 8) do
          # when 597, 605, ..., 653
          if @any_show
            if @hclk == 645
              @scroll_addr_0_4  = @scroll_latch & 0x001f
              @scroll_addr_5_14 = @scroll_latch & 0x7fe0
              @name_io_addr = (@scroll_addr_0_4 | @scroll_addr_5_14) & 0x0fff | 0x2000 # make cache consistent
            end
          end
          open_name
          wait_two_clocks

          # when 599, 607, ..., 655
          # Nestopia uses open_name here?
          open_attr
          wait_two_clocks

          # when 601, 609, ..., 657
          open_pattern(@pattern_end)
          wait_two_clocks

          # when 603, 611, ..., 659
          open_pattern(@io_addr | 8)
          if @hclk == 659
            @hclk = 320
            @vclk += HCLOCK_DUMMY
            @hclk_target -= HCLOCK_DUMMY
          else
            wait_two_clocks
          end
          wait_zero_clocks
        end

        while true
          # visible scanline (not shown)

          # when 320
          load_extended_sprites
          open_name
          @sp_latch = @sp_ram[0] if @any_show
          @sp_buffered = 0
          @sp_zero_in_line = false
          @sp_index = 0
          @sp_phase = 0
          wait_one_clock

          # when 321
          fetch_name
          wait_one_clock

          # when 322
          open_attr
          wait_one_clock

          # when 323
          fetch_attr
          scroll_clock_x
          wait_one_clock

          # when 324
          open_pattern(@io_pattern)
          wait_one_clock

          # when 325
          fetch_bg_pattern_0
          wait_one_clock

          # when 326
          open_pattern(@io_pattern | 8)
          wait_one_clock

          # when 327
          fetch_bg_pattern_1
          wait_one_clock

          # when 328
          preload_tiles
          open_name
          wait_one_clock

          # when 329
          fetch_name
          wait_one_clock

          # when 330
          open_attr
          wait_one_clock

          # when 331
          fetch_attr
          scroll_clock_x
          wait_one_clock

          # when 332
          open_pattern(@io_pattern)
          wait_one_clock

          # when 333
          fetch_bg_pattern_0
          wait_one_clock

          # when 334
          open_pattern(@io_pattern | 8)
          wait_one_clock

          # when 335
          fetch_bg_pattern_1
          wait_one_clock

          # when 336
          open_name
          wait_one_clock

          # when 337
          if @any_show
            update_enabled_flags_edge
            @cpu.next_frame_clock = RP2C02_HVSYNC_1 if @scanline == SCANLINE_HDUMMY && @odd_frame
          end
          wait_one_clock

          # when 338
          open_name
          @scanline += 1
          if @scanline != SCANLINE_VBLANK
            if @any_show
              line = @scanline != 0 || !@odd_frame ? 341 : 340
            else
              update_enabled_flags_edge
              line = 341
            end
            @hclk = 0
            @vclk += line
            @hclk_target = @hclk_target <= line ? 0 : @hclk_target - line
          else
            @hclk = HCLOCK_VBLANK_0
            wait_zero_clocks
            break
          end
          wait_zero_clocks

          # visible scanline (shown)
          0.step(248, 8) do
            # when 0, 8, ..., 248
            if @any_show
              if @hclk == 64
                @sp_addr = @regs_oam & 0xf8 # SP_OFFSET_TO_0_1
                @sp_phase = nil
                @sp_latch = 0xff
              end
              load_tiles
              batch_render_eight_pixels
              evaluate_sprites_even if @hclk >= 64
              open_name
            end
            render_pixel
            wait_one_clock

            # when 1, 9, ..., 249
            if @any_show
              fetch_name
              evaluate_sprites_odd if @hclk >= 64
            end
            render_pixel
            wait_one_clock

            # when 2, 10, ..., 250
            if @any_show
              evaluate_sprites_even if @hclk >= 64
              open_attr
            end
            render_pixel
            wait_one_clock

            # when 3, 11, ..., 251
            if @any_show
              fetch_attr
              evaluate_sprites_odd if @hclk >= 64
              scroll_clock_y if @hclk == 251
              scroll_clock_x
            end
            render_pixel
            wait_one_clock

            # when 4, 12, ..., 252
            if @any_show
              evaluate_sprites_even if @hclk >= 64
              open_pattern(@io_pattern)
            end
            render_pixel
            wait_one_clock

            # when 5, 13, ..., 253
            if @any_show
              fetch_bg_pattern_0
              evaluate_sprites_odd if @hclk >= 64
            end
            render_pixel
            wait_one_clock

            # when 6, 14, ..., 254
            if @any_show
              evaluate_sprites_even if @hclk >= 64
              open_pattern(@io_pattern | 8)
            end
            render_pixel
            wait_one_clock

            # when 7, 15, ..., 255
            if @any_show
              fetch_bg_pattern_1
              evaluate_sprites_odd if @hclk >= 64
            end
            render_pixel
            # rubocop:disable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
            update_enabled_flags if @hclk != 255 if @any_show
            # rubocop:enable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
            wait_one_clock
          end

          256.step(312, 8) do
            # rubocop:disable Style/IdenticalConditionalBranches
            if @hclk == 256
              # when 256
              open_name
              @sp_latch = 0xff if @any_show
              wait_one_clock

              # when 257
              scroll_reset_x
              @sp_visible = false
              @sp_active = false
              wait_one_clock
            else
              # when 264, 272, ..., 312
              open_name
              wait_two_clocks
            end
            # rubocop:enable Style/IdenticalConditionalBranches

            # when 258, 266, ..., 314
            # Nestopia uses open_name here?
            open_attr
            wait_two_clocks

            # when 260, 268, ..., 316
            if @any_show
              buffer_idx = (@hclk - 260) / 2
              open_pattern(buffer_idx >= @sp_buffered ? @pattern_end : open_sprite(buffer_idx))
              # rubocop:disable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
              @regs_oam = 0 if @scanline == 238 if @hclk == 316
              # rubocop:enable Style/NestedModifier, Style/IfUnlessModifierOfIfUnless:
            end
            wait_one_clock

            # when 261, 269, ..., 317
            if @any_show
              @io_pattern = @chr_mem[@io_addr & 0x1fff] if (@hclk - 261) / 2 < @sp_buffered
            end
            wait_one_clock

            # when 262, 270, ..., 318
            open_pattern(@io_addr | 8)
            wait_one_clock

            # when 263, 271, ..., 319
            if @any_show
              buffer_idx = (@hclk - 263) / 2
              if buffer_idx < @sp_buffered
                pat0 = @io_pattern
                pat1 = @chr_mem[@io_addr & 0x1fff]
                load_sprite(pat0, pat1, buffer_idx) if pat0 != 0 || pat1 != 0
              end
            end
            wait_one_clock
          end
        end

        # post-render scanline (vblank)

        # when 681
        vblank_0
        wait_zero_clocks

        # when 682
        vblank_1
        wait_zero_clocks

        # when 684
        vblank_2
        wait_frame
      end
    end
    # rubocop:enable Metrics/MethodLength, Metrics/CyclomaticComplexity, Metrics/PerceivedComplexity, Metrics/AbcSize, Style/SoleNestedConditional

    ###########################################################################
    # optimized core generator
    class OptimizedCodeBuilder
      include CodeOptimizationHelper

      OPTIONS = [
        :method_inlining, :ivar_localization,
        :split_show_mode, :split_a12_checks, :clock_specialization,
        :fastpath, :batch_render_pixels, :oneline,
      ]

      def build
        depends(:ivar_localization, :method_inlining)
        depends(:batch_render_pixels, :fastpath)

        mdefs = parse_method_definitions(__FILE__)
        handlers = parse_clock_handlers(mdefs[:main_loop].body)

        handlers = specialize_clock_handlers(handlers) if @clock_specialization
        if @fastpath
          handlers = add_fastpath(handlers) do |fastpath, hclk|
            @batch_render_pixels ? batch_render_pixels(fastpath, hclk) : fastpath
          end
        end
        code = build_loop(handlers)
        code = ppu_expand_methods(code, mdefs) if @method_inlining

        if @split_show_mode
          code, code_no_show = split_mode(code, "@any_show")
          if @split_a12_checks
            code, code_no_a12 = split_mode(code, "@a12_monitor")
            code = branch("@a12_monitor", code, code_no_a12)
          end
          code = branch("@any_show", code, code_no_show)
        end

        code = gen(
          mdefs[:make_sure_invariants].body,
          code,
          "@hclk_target = (@vclk + @hclk) * RP2C02_CC"
        )

        code = localize_instance_variables(code) if @ivar_localization

        code = gen(
          "def self.run",
          *(@loglevel >= 3 ? ["  debug_logging(@scanline, @hclk, @hclk_target)"] : []),
          indent(2, code),
          "end",
        )

        code = oneline(code) if @oneline

        code
      end

      COMMANDS = {
        wait_zero_clocks: "",
        wait_one_clock:   "@hclk += 1\n",
        wait_two_clocks:  "@hclk += 2\n",
        wait_frame:       "return\n",
      }

      # extracts the actions for each clock from CPU#main_loop
      def parse_clock_handlers(main_loop)
        handlers = {}
        main_loop.scan(/^( *)# when (.*)\n((?:\1.*\n|\n)*?\1wait_.*\n)/) do |indent, hclks, body|
          body = indent(-indent.size, body)
          body = body.gsub(/^( *)break\n/, "")
          body = expand_methods(body, COMMANDS)
          if hclks =~ /^(\d+), (\d+), \.\.\., (\d+)$/
            first, second, last = $1.to_i, $2.to_i, $3.to_i
            first.step(last, second - first) do |hclk|
              handlers[hclk] = body
            end
          else
            handlers[hclks.to_i] = body
          end
        end
        handlers
      end

      # split clock handlers that contains a branch depending on clock
      def specialize_clock_handlers(handlers)
        handlers.each do |hclk, handler|
          # pre-caluculate some conditions like `@hclk == 64` with `false`
          handler = handler.gsub(/@hclk (==|>=|!=) (\d+)/) { hclk.send($1.to_sym, $2.to_i) }

          # remove disabled branches like `if false ... end`
          handlers[hclk] = remove_trivial_branches(handler)
        end
      end

      # pass a fastpath
      def add_fastpath(handlers)
        handlers.each do |hclk, handler|
          next unless hclk % 8 == 0 && hclk < 256
          fastpath = gen(*(0..7).map {|i| handlers[hclk + i] })
          fastpath = yield fastpath, hclk
          handlers[hclk] = branch("@hclk + 8 <= @hclk_target", fastpath, handler)
        end
      end

      # replace eight `render_pixel` calls with one optimized batch version
      def batch_render_pixels(fastpath, hclk)
        fastpath = expand_methods(fastpath, render_pixel: gen(
          "unless @any_show",
          "  @bg_pixels[@hclk % 8] = 0",
          "  @output_pixels << @output_color[@scroll_addr_5_14 & 0x3f00 == 0x3f00 ? @scroll_addr_0_4 : 0]",
          "end",
        ))
        expand_methods(fastpath, batch_render_eight_pixels: gen(
          "# batch-version of render_pixel",
          "if @any_show",
          "  if @sp_active",
          "    if @bg_enabled",
          *(0..7).flat_map do |i|
            [
              "      pixel#{ i } = @bg_pixels[#{ i }]",
              "      if sprite = @sp_map[@hclk#{ i != 0 ? " + #{ i }" : "" }]",
              "        if pixel#{ i } % 4 == 0",
              "          pixel#{ i } = sprite[2]",
              "        else",
              *(hclk + i == 255 ? [] : ["          @sp_zero_hit = true if sprite[1]"]),
              "          pixel#{ i } = sprite[2] unless sprite[0]",
              "        end",
              "      end",
            ]
          end,
          "      @output_pixels << " + (0..7).map {|n| "@output_color[pixel#{ n }]" } * " << ",
          "    else",
          *(0..7).map do |i|
            "      pixel#{ i } = (sprite = @sp_map[@hclk #{ i != 0 ? " + #{ i }" : "" }]) ? sprite[2] : 0"
          end,
          "      @output_pixels << " + (0..7).map {|n| "@output_color[pixel#{ n }]" } * " << ",
          "    end",
          "  else",
          "    if @bg_enabled # this is the true hot-spot",
          "      @output_pixels << " + (0..7).map {|n| "@output_color[@bg_pixels[#{ n }]]" } * " << ",
          "    else",
          "      clr = @output_color[0]",
          "      @output_pixels << " + ["clr"] * 8 * " << ",
          "    end",
          "  end",
          "end",
        ))
      end

      # remove all newlines (this will reduce `trace` instructions)
      def oneline(code)
        code.gsub(/^ *|#.*/, "").gsub("[\n", "[").gsub(/\n *\]/, "]").tr("\n", ";")
      end

      # inline method calls
      def ppu_expand_methods(code, mdefs)
        code = expand_inline_methods(code, :open_sprite, mdefs[:open_sprite])

        # twice is enough
        expand_methods(expand_methods(code, mdefs), mdefs)
      end

      # create two version of the same code by evaluating easy branches
      # CAUTION: the condition must be invariant during PPU#run
      def split_mode(code, cond)
        %w(true false).map do |bool|
          rebuild_loop(remove_trivial_branches(replace_cond_var(code, cond, bool)))
        end
      end

      # generate a main code
      def build_loop(handlers)
        clauses = {}
        handlers.sort.each do |hclk, handler|
          (clauses[handler] ||= []) << hclk
        end

        gen(
          "while @hclk_target > @hclk",
          "  case @hclk",
          *clauses.invert.sort.map do |hclks, handler|
            "  when #{ hclks * ", " }\n" + indent(4, handler)
          end,
          "  end",
          "end",
        )
      end

      # deconstruct a loop, unify handlers, and re-generate a new loop
      def rebuild_loop(code)
        handlers = {}
        code.scan(/^  when ((?:\d+, )*\d+)\n((?:    .*\n|\n)*)/) do |hclks, handler|
          hclks.split(", ").each do |hclk|
            handlers[hclk.to_i] = indent(-4, handler)
          end
        end
        build_loop(handlers)
      end
    end
  end
end


# optcarrot/palette

module Optcarrot
  # NES palette generators
  module Palette
    module_function

    # I don't know where this palette definition came from, but many emulators are using this palette
    def defacto_palette
      [
        [1.00, 1.00, 1.00], # default
        [1.00, 0.80, 0.81], # emphasize R
        [0.78, 0.94, 0.66], # emphasize G
        [0.79, 0.77, 0.63], # emphasize RG
        [0.82, 0.83, 1.12], # emphasize B
        [0.81, 0.71, 0.87], # emphasize RB
        [0.68, 0.79, 0.79], # emphasize GB
        [0.70, 0.70, 0.70], # emphasize RGB
      ].flat_map do |rf, gf, bf|
        # RGB default palette (I don't know where this palette came from)
        [
          0x666666, 0x002a88, 0x1412a7, 0x3b00a4, 0x5c007e, 0x6e0040, 0x6c0600, 0x561d00,
          0x333500, 0x0b4800, 0x005200, 0x004f08, 0x00404d, 0x000000, 0x000000, 0x000000,
          0xadadad, 0x155fd9, 0x4240ff, 0x7527fe, 0xa01acc, 0xb71e7b, 0xb53120, 0x994e00,
          0x6b6d00, 0x388700, 0x0c9300, 0x008f32, 0x007c8d, 0x000000, 0x000000, 0x000000,
          0xfffeff, 0x64b0ff, 0x9290ff, 0xc676ff, 0xf36aff, 0xfe6ecc, 0xfe8170, 0xea9e22,
          0xbcbe00, 0x88d800, 0x5ce430, 0x45e082, 0x48cdde, 0x4f4f4f, 0x000000, 0x000000,
          0xfffeff, 0xc0dfff, 0xd3d2ff, 0xe8c8ff, 0xfbc2ff, 0xfec4ea, 0xfeccc5, 0xf7d8a5,
          0xe4e594, 0xcfef96, 0xbdf4ab, 0xb3f3cc, 0xb5ebf2, 0xb8b8b8, 0x000000, 0x000000,
        ].map do |rgb|
          r = [((rgb >> 16 & 0xff) * rf).floor, 0xff].min
          g = [((rgb >>  8 & 0xff) * gf).floor, 0xff].min
          b = [((rgb >>  0 & 0xff) * bf).floor, 0xff].min
          [r, g, b]
        end
      end
    end

    # Nestopia generates a palette systematically (cool!), but it is not compatible with nes-tests-rom
    def nestopia_palette
      (0..511).map do |n|
        tint, level, color = n >> 6 & 7, n >> 4 & 3, n & 0x0f
        level0, level1 = [[-0.12, 0.40], [0.00, 0.68], [0.31, 1.00], [0.72, 1.00]][level]
        level0 = level1 if color == 0x00
        level1 = level0 if color == 0x0d
        level0 = level1 = 0 if color >= 0x0e
        y = (level1 + level0) * 0.5
        s = (level1 - level0) * 0.5
        iq = Complex.polar(s, Math::PI / 6 * (color - 3))
        if tint != 0 && color <= 0x0d
          if tint == 7
            y = (y * 0.79399 - 0.0782838) * 1.13
          else
            level1 = (level1 * (1 - 0.79399) + 0.0782838) * 0.5
            y -= level1 * 0.5
            y -= level1 *= 0.6 if [3, 5, 6].include?(tint)
            iq += Complex.polar(level1, Math::PI / 12 * ([0, 6, 10, 8, 2, 4, 0, 0][tint] * 2 - 7))
          end
        end
        [[105, 0.570], [251, 0.351], [15, 1.015]].map do |angle, gain|
          clr = y + (Complex.polar(gain * 2, (angle - 33) * Math::PI / 180) * iq.conjugate).real
          [0, (clr * 255).round, 255].sort[1]
        end
      end
    end
  end
end


# optcarrot/driver

module Optcarrot
  # A manager class for drivers (user frontend)
  module Driver
    DRIVER_DB = {
      video: {
        sdl2:  :SDL2Video,
        sfml:  :SFMLVideo,
        png:   :PNGVideo,
        gif:   :GIFVideo,
        sixel: :SixelVideo,
        mplayer: :MPlayerVideo,
        none:  :Video,
      },
      audio: {
        sdl2: :SDL2Audio,
        sfml: :SFMLAudio,
        ao:   :AoAudio,
        wav:  :WAVAudio,
        none: :Audio,
      },
      input: {
        sdl2: :SDL2Input,
        sfml: :SFMLInput,
        term: :TermInput,
        log:  :LogInput,
        none: :Input,
      }
    }

    module_function

    def load(conf)
      video = load_each(conf, :video, conf.video).new(conf)
      audio = load_each(conf, :audio, conf.audio).new(conf)
      input = load_each(conf, :input, conf.input).new(conf, video)
      return video, audio, input
    end

    def load_each(conf, type, name)
      if name
        klass = DRIVER_DB[type][name]
        raise "unknown #{ type } driver: #{ name }" unless klass
        conf.debug("`#{ name }' #{ type } driver is selected")
        Optcarrot.const_get(klass)
      else
        selected = nil
        DRIVER_DB[type].each_key do |n|
          begin
            selected = load_each(conf, type, n)
            break
          rescue LoadError
            conf.debug("fail to use `#{ n }' #{ type } driver")
          end
        end
        selected
      end
    end
  end

  # A base class of video output driver
  class Video
    WIDTH = 256
    TV_WIDTH = 292
    HEIGHT = 224

    def initialize(conf)
      @conf = conf
      @palette_rgb = @conf.nestopia_palette ? Palette.nestopia_palette : Palette.defacto_palette
      @palette = [*0..4096] # dummy palette
      init
    end

    attr_reader :palette

    def init
      @times = []
    end

    def dispose
    end

    def tick(_output)
      @times << Process.clock_gettime(Process::CLOCK_MONOTONIC)
      @times.shift if @times.size > 10
      @times.size < 2 ? 0 : ((@times.last - @times.first) / (@times.size - 1)) ** -1
    end

    def change_window_size(_scale)
    end

    def on_resize(_width, _height)
    end
  end

  # A base class of audio output driver
  class Audio
    PACK_FORMAT = { 8 => "c*", 16 => "v*" }
    BUFFER_IN_FRAME = 3 # keep audio buffer during this number of frames

    def initialize(conf)
      @conf = conf
      @rate = conf.audio_sample_rate
      @bits = conf.audio_bit_depth
      raise "sample bits must be 8 or 16" unless @bits == 8 || @bits == 16
      @pack_format = PACK_FORMAT[@bits]

      init
    end

    def spec
      return @rate, @bits
    end

    def init
    end

    def dispose
    end

    def tick(_output)
    end
  end

  # A base class of input driver
  class Input
    def initialize(conf, video)
      @conf = conf
      @video = video
      init
    end

    def init
    end

    def dispose
    end

    def tick(_frame, _pads)
    end

    def event(pads, type, code, player)
      case code
      when :start  then pads.send(type, player, Pad::START)
      when :select then pads.send(type, player, Pad::SELECT)
      when :a      then pads.send(type, player, Pad::A)
      when :b      then pads.send(type, player, Pad::B)
      when :right  then pads.send(type, player, Pad::RIGHT)
      when :left   then pads.send(type, player, Pad::LEFT)
      when :down   then pads.send(type, player, Pad::DOWN)
      when :up     then pads.send(type, player, Pad::UP)
      else
        return if type != :keydown
        case code
        when :screen_x1   then @video.change_window_size(1)
        when :screen_x2   then @video.change_window_size(2)
        when :screen_x3   then @video.change_window_size(3)
        when :screen_full then @video.change_window_size(nil)
        when :quit        then exit
        end
      end
    end
  end
end


# optcarrot/config

module Optcarrot
  # config manager and logger
  class Config
    OPTIONS = {
      optimization: {
        opt_ppu: {
          type: :opts,
          desc: "select PPU optimizations",
          candidates: PPU::OptimizedCodeBuilder::OPTIONS,
          default: nil,
        },
        opt_cpu: {
          type: :opts,
          desc: "select CPU optimizations",
          candidates: CPU::OptimizedCodeBuilder::OPTIONS,
          default: nil,
        },
        opt: { shortcut: %w(--opt-ppu=all --opt-cpu=all) },
        list_opts: { type: :info, desc: "list available optimizations" },
        dump_ppu: { type: :info, desc: "print generated PPU source code" },
        dump_cpu: { type: :info, desc: "print generated CPU source code" },
        load_ppu: { type: "FILE", desc: "use generated PPU source code" },
        load_cpu: { type: "FILE", desc: "use generated CPU source code" },
      },
      emulation: {
        sprite_limit:      { type: :switch, desc: "enable/disable sprite limit", default: false },
        frames:            { type: :int, desc: "execute N frames (0 = no limit)", default: 0, aliases: [:f, :frame] },
        audio_sample_rate: { type: :int, desc: "set audio sample rate", default: 44100 },
        audio_bit_depth:   { type: :int, desc: "set audio bit depth", default: 16 },
        nestopia_palette:  { type: :switch, desc: "use Nestopia palette instead of de facto", default: false },
      },
      driver: {
        video:  { type: :driver, desc: "select video driver", candidates: Driver::DRIVER_DB[:video].keys },
        audio:  { type: :driver, desc: "select audio driver", candidates: Driver::DRIVER_DB[:audio].keys },
        input:  { type: :driver, desc: "select input driver", candidates: Driver::DRIVER_DB[:input].keys },
        list_drivers: { type: :info, desc: "print available drivers" },
        sdl2:      { shortcut: %w(--video=sdl2 --audio=sdl2 --input=sdl2) },
        sfml:      { shortcut: %w(--video=sfml --audio=sfml --input=sfml) },
        headless:  { shortcut: %w(--video=none --audio=none --input=none) },
        video_output: { type: "FILE", desc: "save video to file", default: "video.EXT" },
        audio_output: { type: "FILE", desc: "save audio to file", default: "audio.wav" },
        show_fps: { type: :switch, desc: "show fps in the right-bottom corner", default: true },
        key_log: { type: "FILE", desc: "use recorded input file" },
        # key_config: { type: "KEY", desc: "key configuration" },
      },
      profiling: {
        print_fps: { type: :switch, desc: "print fps of last 10 frames", default: false },
        print_p95fps: { type: :switch, desc: "print 95th percentile fps", default: false },
        print_fps_history: { type: :switch, desc: "print all fps values for each frame", default: false },
        print_video_checksum: { type: :switch, desc: "print checksum of the last video output", default: false },
        stackprof: { shortcut: "--stackprof-mode=cpu", aliases: :p },
        stackprof_mode: { type: "MODE", desc: "run under stackprof", default: nil },
        stackprof_output: { type: "FILE", desc: "stackprof output file", default: "stackprof-MODE.dump" }
      },
      misc: {
        benchmark: { shortcut: %w(--headless --print-fps --print-video-checksum --frames 180), aliases: :b },
        loglevel: { type: :int, desc: "set loglevel", default: 1 },
        quiet:    { shortcut: "--loglevel=0", aliases: :q },
        verbose:  { shortcut: "--loglevel=2", aliases: :v },
        debug:    { shortcut: "--loglevel=3", aliases: :d },
        version: { type: :info, desc: "print version" },
        help:    { type: :info, desc: "print this message", aliases: :h },
      },
    }

    DEFAULT_OPTIONS = {}
    OPTIONS.each_value do |opts|
      opts.each do |id, opt|
        next if opt[:shortcut]
        DEFAULT_OPTIONS[id] = opt[:default] if opt.key?(:default)
        attr_reader id
      end
    end
    attr_reader :romfile

    def initialize(opt)
      opt = Parser.new(opt).options if opt.is_a?(Array)
      DEFAULT_OPTIONS.merge(opt).each {|id, val| instance_variable_set(:"@#{ id }", val) }
    end

    def debug(msg)
      puts "[DEBUG] " + msg if @loglevel >= 3
    end

    def info(msg)
      puts "[INFO] " + msg if @loglevel >= 2
    end

    def warn(msg)
      puts "[WARN] " + msg if @loglevel >= 1
    end

    def error(msg)
      puts "[ERROR] " + msg
    end

    def fatal(msg)
      puts "[FATAL] " + msg
      abort
    end

    # command-line option parser
    class Parser
      def initialize(argv)
        @argv = argv
        @options = DEFAULT_OPTIONS.dup
        parse_option until @argv.empty?
        error "ROM file is not given" unless @options[:romfile]
      rescue Invalid => e
        puts "[FATAL] #{ e }"
        exit 1
      end

      attr_reader :options

      class Invalid < RuntimeError; end

      def error(msg)
        raise Invalid, msg
      end

      def find_option(arg)
        OPTIONS.each_value do |opts|
          opts.each do |id_base, opt|
            [id_base, *opt[:aliases]].each do |id|
              id = id.to_s.tr("_", "-")
              return opt, id_base if id.size == 1 && arg == "-#{ id }"
              return opt, id_base if arg == "--#{ id }"
              return opt, id_base, true if opt[:type] == :switch && arg == "--no-#{ id }"
            end
          end
        end
        return nil
      end

      def parse_option
        arg, operand = @argv.shift.split("=", 2)
        puts "parse_option.. arg #{arg} operand #{operand}"
        # if arg.chars.all? { |c| [*(?a..?z), *(?A..?Z), *(0..9).map(&:to_s), '-'].include?(c) }
        #   args = arg.
        #   args = $1.chars.map {|a| "-#{ a }" }
        #   args.last << "=" << operand if operand
        #   @argv.unshift(*args)
        #   return
        # end
        opt, id, no = find_option(arg)
        if opt
          if opt[:shortcut]
            @argv.unshift(*opt[:shortcut])
            return
          elsif opt[:type] == :info
            send(id)
            exit
          elsif opt[:type] == :switch
            error "option `#{ arg }' doesn't allow an operand" if operand
            @options[id] = !no
          else
            @options[id] = parse_operand(operand, arg, opt)
          end
        else
          arg = @argv.shift if arg == "--"
          error "invalid option: `#{ arg }'" if arg && arg.start_with?("-")
          if arg
            error "extra argument: `#{ arg }'" if @options[:romfile]
            @options[:romfile] = arg
          end
        end
      end

      def parse_operand(operand, arg, opt)
        type = opt[:type]
        operand ||= @argv.shift
        case type
        when :opts
          operand = operand.split(",").map {|s| s.to_sym }
        when :driver
          operand = operand.to_sym
          error "unknown driver: `#{ operand }'" unless opt[:candidates].include?(operand)
        when :int
          begin
            operand = Integer(operand)
          rescue
            error "option `#{ arg }' requires numerical operand"
          end
        end
        operand
      end

      def help
        tbl = ["Usage: #{ $PROGRAM_NAME } [OPTION]... FILE"]
        long_name_width = 0
        OPTIONS.each do |kind, opts|
          tbl << "" << "#{ kind } options:"
          opts.each do |id_base, opt|
            short_name = [*opt[:aliases]][0]
            switch = args = ""
            case opt[:type]
            when :switch then switch = "[no-]"
            when :opts   then args = "=OPTS,..."
            when :driver then args = "=DRIVER"
            when :int    then args = "=N"
            when String  then args = "=" + opt[:type]
            end
            short_name = "-#{ switch }#{ short_name }, " if short_name && short_name.size == 1
            long_name = "--" + switch + id_base.to_s.tr("_", "-") + args
            if opt[:shortcut]
              desc = "same as `#{ [*opt[:shortcut]].join(" ") }'"
            else
              desc = opt[:desc]
              desc += " (default: #{ opt[:default] || "none" })" if opt.key?(:default)
            end
            long_name_width = [long_name_width, long_name.size].max
            tbl << [short_name, long_name, desc]
          end
        end
        tbl.each do |arg|
          if arg.is_a?(String)
            puts arg
          else
            short_name, long_name, desc = arg
            puts "    %4s%-*s %s" % [short_name, long_name_width, long_name, desc]
          end
        end
      end

      def version
        puts "optcarrot #{ VERSION }"
      end

      def list_drivers
        Driver::DRIVER_DB.each do |kind, drivers|
          puts "#{ kind } drivers: #{ drivers.keys * " " }"
        end
      end

      def list_opts
        puts "CPU core optimizations:"
        CPU::OptimizedCodeBuilder::OPTIONS.each do |opt|
          puts "  * #{ opt }"
        end
        puts
        puts "PPU core optimizations:"
        PPU::OptimizedCodeBuilder::OPTIONS.each do |opt|
          puts "  * #{ opt }"
        end
        puts
        puts "(See `doc/internal.md' in detail.)"
      end

      def dump_ppu
        puts PPU::OptimizedCodeBuilder.new(@options[:loglevel], @options[:opt_ppu] || []).build
      end

      def dump_cpu
        puts CPU::OptimizedCodeBuilder.new(@options[:loglevel], @options[:opt_cpu] || []).build
      end
    end
  end
end


# Optcarrot namespace
module Optcarrot
  VERSION = "0.9.0"
end



