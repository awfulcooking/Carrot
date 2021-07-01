def self.run
  do_clock
  begin
    begin
      @opcode = fetch(@_pc)

      if false
        @conf.debug("PC:%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d : OPCODE:%02X (%d, %d)" % [
          @_pc, @_a, @_x, @_y, flags_pack, @_sp, @clk / 4 % 341, @opcode, @clk, @clk_target
        ])
      end

      @_pc += 1

      case @opcode
      when 0x00 # _brk
        _brk
      when 0x01 # _ora
        ind_x(true, false)
        _ora
      when 0x02 # _jam
        _jam
      when 0x03 # _slo
        ind_x(true, true)
        _slo
        store_mem
      when 0x04 # _nop
        @_pc += 1
        @clk += 3 * RP2A03_CC
      when 0x05 # _ora
        zpg(true, false)
        _ora
      when 0x06 # _asl
        zpg(true, true)
        _asl
        store_zpg
      when 0x07 # _slo
        zpg(true, true)
        _slo
        store_zpg
      when 0x08 # _php
        _php
      when 0x09 # _ora
        imm(true, false)
        _ora
      when 0x0a # _asl
        @clk += CLK_2
        @data = @_a
        _asl
        @_a = @data
      when 0x0b # _anc
        imm(true, false)
        _anc
      when 0x0c # _nop
        @_pc += 2
        @clk += 4 * RP2A03_CC
      when 0x0d # _ora
        abs(true, false)
        _ora
      when 0x0e # _asl
        abs(true, true)
        _asl
        store_mem
      when 0x0f # _slo
        abs(true, true)
        _slo
        store_mem
      when 0x10 # _bpl
        _bpl
      when 0x11 # _ora
        ind_y(true, false)
        _ora
      when 0x12 # _jam
        _jam
      when 0x13 # _slo
        ind_y(true, true)
        _slo
        store_mem
      when 0x14 # _nop
        @_pc += 1
        @clk += 4 * RP2A03_CC
      when 0x15 # _ora
        zpg_x(true, false)
        _ora
      when 0x16 # _asl
        zpg_x(true, true)
        _asl
        store_zpg
      when 0x17 # _slo
        zpg_x(true, true)
        _slo
        store_zpg
      when 0x18 # _clc
        _clc
      when 0x19 # _ora
        abs_y(true, false)
        _ora
      when 0x1a # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0x1b # _slo
        abs_y(true, true)
        _slo
        store_mem
      when 0x1c # _nop
        abs_x(true, false)
        _nop
      when 0x1d # _ora
        abs_x(true, false)
        _ora
      when 0x1e # _asl
        abs_x(true, true)
        _asl
        store_mem
      when 0x1f # _slo
        abs_x(true, true)
        _slo
        store_mem
      when 0x20 # _jsr
        _jsr
      when 0x21 # _and
        ind_x(true, false)
        _and
      when 0x22 # _jam
        _jam
      when 0x23 # _rla
        ind_x(true, true)
        _rla
        store_mem
      when 0x24 # _bit
        zpg(true, false)
        _bit
      when 0x25 # _and
        zpg(true, false)
        _and
      when 0x26 # _rol
        zpg(true, true)
        _rol
        store_zpg
      when 0x27 # _rla
        zpg(true, true)
        _rla
        store_zpg
      when 0x28 # _plp
        _plp
      when 0x29 # _and
        imm(true, false)
        _and
      when 0x2a # _rol
        @clk += CLK_2
        @data = @_a
        _rol
        @_a = @data
      when 0x2b # _anc
        imm(true, false)
        _anc
      when 0x2c # _bit
        abs(true, false)
        _bit
      when 0x2d # _and
        abs(true, false)
        _and
      when 0x2e # _rol
        abs(true, true)
        _rol
        store_mem
      when 0x2f # _rla
        abs(true, true)
        _rla
        store_mem
      when 0x30 # _bmi
        _bmi
      when 0x31 # _and
        ind_y(true, false)
        _and
      when 0x32 # _jam
        _jam
      when 0x33 # _rla
        ind_y(true, true)
        _rla
        store_mem
      when 0x34 # _nop
        @_pc += 1
        @clk += 4 * RP2A03_CC
      when 0x35 # _and
        zpg_x(true, false)
        _and
      when 0x36 # _rol
        zpg_x(true, true)
        _rol
        store_zpg
      when 0x37 # _rla
        zpg_x(true, true)
        _rla
        store_zpg
      when 0x38 # _sec
        _sec
      when 0x39 # _and
        abs_y(true, false)
        _and
      when 0x3a # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0x3b # _rla
        abs_y(true, true)
        _rla
        store_mem
      when 0x3c # _nop
        abs_x(true, false)
        _nop
      when 0x3d # _and
        abs_x(true, false)
        _and
      when 0x3e # _rol
        abs_x(true, true)
        _rol
        store_mem
      when 0x3f # _rla
        abs_x(true, true)
        _rla
        store_mem
      when 0x40 # _rti
        _rti
      when 0x41 # _eor
        ind_x(true, false)
        _eor
      when 0x42 # _jam
        _jam
      when 0x43 # _sre
        ind_x(true, true)
        _sre
        store_mem
      when 0x44 # _nop
        @_pc += 1
        @clk += 3 * RP2A03_CC
      when 0x45 # _eor
        zpg(true, false)
        _eor
      when 0x46 # _lsr
        zpg(true, true)
        _lsr
        store_zpg
      when 0x47 # _sre
        zpg(true, true)
        _sre
        store_zpg
      when 0x48 # _pha
        _pha
      when 0x49 # _eor
        imm(true, false)
        _eor
      when 0x4a # _lsr
        @clk += CLK_2
        @data = @_a
        _lsr
        @_a = @data
      when 0x4b # _asr
        imm(true, false)
        _asr
      when 0x4c # _jmp_a
        _jmp_a
      when 0x4d # _eor
        abs(true, false)
        _eor
      when 0x4e # _lsr
        abs(true, true)
        _lsr
        store_mem
      when 0x4f # _sre
        abs(true, true)
        _sre
        store_mem
      when 0x50 # _bvc
        _bvc
      when 0x51 # _eor
        ind_y(true, false)
        _eor
      when 0x52 # _jam
        _jam
      when 0x53 # _sre
        ind_y(true, true)
        _sre
        store_mem
      when 0x54 # _nop
        @_pc += 1
        @clk += 4 * RP2A03_CC
      when 0x55 # _eor
        zpg_x(true, false)
        _eor
      when 0x56 # _lsr
        zpg_x(true, true)
        _lsr
        store_zpg
      when 0x57 # _sre
        zpg_x(true, true)
        _sre
        store_zpg
      when 0x58 # _cli
        _cli
      when 0x59 # _eor
        abs_y(true, false)
        _eor
      when 0x5a # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0x5b # _sre
        abs_y(true, true)
        _sre
        store_mem
      when 0x5c # _nop
        abs_x(true, false)
        _nop
      when 0x5d # _eor
        abs_x(true, false)
        _eor
      when 0x5e # _lsr
        abs_x(true, true)
        _lsr
        store_mem
      when 0x5f # _sre
        abs_x(true, true)
        _sre
        store_mem
      when 0x60 # _rts
        _rts
      when 0x61 # _adc
        ind_x(true, false)
        _adc
      when 0x62 # _jam
        _jam
      when 0x63 # _rra
        ind_x(true, true)
        _rra
        store_mem
      when 0x64 # _nop
        @_pc += 1
        @clk += 3 * RP2A03_CC
      when 0x65 # _adc
        zpg(true, false)
        _adc
      when 0x66 # _ror
        zpg(true, true)
        _ror
        store_zpg
      when 0x67 # _rra
        zpg(true, true)
        _rra
        store_zpg
      when 0x68 # _pla
        _pla
      when 0x69 # _adc
        imm(true, false)
        _adc
      when 0x6a # _ror
        @clk += CLK_2
        @data = @_a
        _ror
        @_a = @data
      when 0x6b # _arr
        imm(true, false)
        _arr
      when 0x6c # _jmp_i
        _jmp_i
      when 0x6d # _adc
        abs(true, false)
        _adc
      when 0x6e # _ror
        abs(true, true)
        _ror
        store_mem
      when 0x6f # _rra
        abs(true, true)
        _rra
        store_mem
      when 0x70 # _bvs
        _bvs
      when 0x71 # _adc
        ind_y(true, false)
        _adc
      when 0x72 # _jam
        _jam
      when 0x73 # _rra
        ind_y(true, true)
        _rra
        store_mem
      when 0x74 # _nop
        @_pc += 1
        @clk += 4 * RP2A03_CC
      when 0x75 # _adc
        zpg_x(true, false)
        _adc
      when 0x76 # _ror
        zpg_x(true, true)
        _ror
        store_zpg
      when 0x77 # _rra
        zpg_x(true, true)
        _rra
        store_zpg
      when 0x78 # _sei
        _sei
      when 0x79 # _adc
        abs_y(true, false)
        _adc
      when 0x7a # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0x7b # _rra
        abs_y(true, true)
        _rra
        store_mem
      when 0x7c # _nop
        abs_x(true, false)
        _nop
      when 0x7d # _adc
        abs_x(true, false)
        _adc
      when 0x7e # _ror
        abs_x(true, true)
        _ror
        store_mem
      when 0x7f # _rra
        abs_x(true, true)
        _rra
        store_mem
      when 0x80 # _nop
        @_pc += 1
        @clk += 2 * RP2A03_CC
      when 0x81 # _sta
        ind_x(false, true)
        _sta
        store_mem
      when 0x82 # _nop
        @_pc += 1
        @clk += 2 * RP2A03_CC
      when 0x83 # _sax
        ind_x(false, true)
        _sax
        store_mem
      when 0x84 # _sty
        zpg(false, true)
        _sty
        store_zpg
      when 0x85 # _sta
        zpg(false, true)
        _sta
        store_zpg
      when 0x86 # _stx
        zpg(false, true)
        _stx
        store_zpg
      when 0x87 # _sax
        zpg(false, true)
        _sax
        store_zpg
      when 0x88 # _dey
        _dey
      when 0x89 # _nop
        @_pc += 1
        @clk += 2 * RP2A03_CC
      when 0x8a # _txa
        _txa
      when 0x8b # _ane
        imm(true, false)
        _ane
      when 0x8c # _sty
        abs(false, true)
        _sty
        store_mem
      when 0x8d # _sta
        abs(false, true)
        _sta
        store_mem
      when 0x8e # _stx
        abs(false, true)
        _stx
        store_mem
      when 0x8f # _sax
        abs(false, true)
        _sax
        store_mem
      when 0x90 # _bcc
        _bcc
      when 0x91 # _sta
        ind_y(false, true)
        _sta
        store_mem
      when 0x92 # _jam
        _jam
      when 0x93 # _sha
        ind_y(false, true)
        _sha
        store_mem
      when 0x94 # _sty
        zpg_x(false, true)
        _sty
        store_zpg
      when 0x95 # _sta
        zpg_x(false, true)
        _sta
        store_zpg
      when 0x96 # _stx
        zpg_y(false, true)
        _stx
        store_zpg
      when 0x97 # _sax
        zpg_y(false, true)
        _sax
        store_zpg
      when 0x98 # _tya
        _tya
      when 0x99 # _sta
        abs_y(false, true)
        _sta
        store_mem
      when 0x9a # _txs
        _txs
      when 0x9b # _shs
        abs_y(false, true)
        _shs
        store_mem
      when 0x9c # _shy
        abs_x(false, true)
        _shy
        store_mem
      when 0x9d # _sta
        abs_x(false, true)
        _sta
        store_mem
      when 0x9e # _shx
        abs_y(false, true)
        _shx
        store_mem
      when 0x9f # _sha
        abs_y(false, true)
        _sha
        store_mem
      when 0xa0 # _ldy
        imm(true, false)
        _ldy
      when 0xa1 # _lda
        ind_x(true, false)
        _lda
      when 0xa2 # _ldx
        imm(true, false)
        _ldx
      when 0xa3 # _lax
        ind_x(true, false)
        _lax
      when 0xa4 # _ldy
        zpg(true, false)
        _ldy
      when 0xa5 # _lda
        zpg(true, false)
        _lda
      when 0xa6 # _ldx
        zpg(true, false)
        _ldx
      when 0xa7 # _lax
        zpg(true, false)
        _lax
      when 0xa8 # _tay
        _tay
      when 0xa9 # _lda
        imm(true, false)
        _lda
      when 0xaa # _tax
        _tax
      when 0xab # _lxa
        imm(true, false)
        _lxa
      when 0xac # _ldy
        abs(true, false)
        _ldy
      when 0xad # _lda
        abs(true, false)
        _lda
      when 0xae # _ldx
        abs(true, false)
        _ldx
      when 0xaf # _lax
        abs(true, false)
        _lax
      when 0xb0 # _bcs
        _bcs
      when 0xb1 # _lda
        ind_y(true, false)
        _lda
      when 0xb2 # _jam
        _jam
      when 0xb3 # _lax
        ind_y(true, false)
        _lax
      when 0xb4 # _ldy
        zpg_x(true, false)
        _ldy
      when 0xb5 # _lda
        zpg_x(true, false)
        _lda
      when 0xb6 # _ldx
        zpg_y(true, false)
        _ldx
      when 0xb7 # _lax
        zpg_y(true, false)
        _lax
      when 0xb8 # _clv
        _clv
      when 0xb9 # _lda
        abs_y(true, false)
        _lda
      when 0xba # _tsx
        _tsx
      when 0xbb # _las
        abs_y(true, false)
        _las
      when 0xbc # _ldy
        abs_x(true, false)
        _ldy
      when 0xbd # _lda
        abs_x(true, false)
        _lda
      when 0xbe # _ldx
        abs_y(true, false)
        _ldx
      when 0xbf # _lax
        abs_y(true, false)
        _lax
      when 0xc0 # _cpy
        imm(true, false)
        _cpy
      when 0xc1 # _cmp
        ind_x(true, false)
        _cmp
      when 0xc2 # _nop
        @_pc += 1
        @clk += 2 * RP2A03_CC
      when 0xc3 # _dcp
        ind_x(true, true)
        _dcp
        store_mem
      when 0xc4 # _cpy
        zpg(true, false)
        _cpy
      when 0xc5 # _cmp
        zpg(true, false)
        _cmp
      when 0xc6 # _dec
        zpg(true, true)
        _dec
        store_zpg
      when 0xc7 # _dcp
        zpg(true, true)
        _dcp
        store_zpg
      when 0xc8 # _iny
        _iny
      when 0xc9 # _cmp
        imm(true, false)
        _cmp
      when 0xca # _dex
        _dex
      when 0xcb # _sbx
        imm(true, false)
        _sbx
      when 0xcc # _cpy
        abs(true, false)
        _cpy
      when 0xcd # _cmp
        abs(true, false)
        _cmp
      when 0xce # _dec
        abs(true, true)
        _dec
        store_mem
      when 0xcf # _dcp
        abs(true, true)
        _dcp
        store_mem
      when 0xd0 # _bne
        _bne
      when 0xd1 # _cmp
        ind_y(true, false)
        _cmp
      when 0xd2 # _jam
        _jam
      when 0xd3 # _dcp
        ind_y(true, true)
        _dcp
        store_mem
      when 0xd4 # _nop
        @_pc += 1
        @clk += 4 * RP2A03_CC
      when 0xd5 # _cmp
        zpg_x(true, false)
        _cmp
      when 0xd6 # _dec
        zpg_x(true, true)
        _dec
        store_zpg
      when 0xd7 # _dcp
        zpg_x(true, true)
        _dcp
        store_zpg
      when 0xd8 # _cld
        _cld
      when 0xd9 # _cmp
        abs_y(true, false)
        _cmp
      when 0xda # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0xdb # _dcp
        abs_y(true, true)
        _dcp
        store_mem
      when 0xdc # _nop
        abs_x(true, false)
        _nop
      when 0xdd # _cmp
        abs_x(true, false)
        _cmp
      when 0xde # _dec
        abs_x(true, true)
        _dec
        store_mem
      when 0xdf # _dcp
        abs_x(true, true)
        _dcp
        store_mem
      when 0xe0 # _cpx
        imm(true, false)
        _cpx
      when 0xe1 # _sbc
        ind_x(true, false)
        _sbc
      when 0xe2 # _nop
        @_pc += 1
        @clk += 2 * RP2A03_CC
      when 0xe3 # _isb
        ind_x(true, true)
        _isb
        store_mem
      when 0xe4 # _cpx
        zpg(true, false)
        _cpx
      when 0xe5 # _sbc
        zpg(true, false)
        _sbc
      when 0xe6 # _inc
        zpg(true, true)
        _inc
        store_zpg
      when 0xe7 # _isb
        zpg(true, true)
        _isb
        store_zpg
      when 0xe8 # _inx
        _inx
      when 0xe9 # _sbc
        imm(true, false)
        _sbc
      when 0xea # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0xeb # _sbc
        imm(true, false)
        _sbc
      when 0xec # _cpx
        abs(true, false)
        _cpx
      when 0xed # _sbc
        abs(true, false)
        _sbc
      when 0xee # _inc
        abs(true, true)
        _inc
        store_mem
      when 0xef # _isb
        abs(true, true)
        _isb
        store_mem
      when 0xf0 # _beq
        _beq
      when 0xf1 # _sbc
        ind_y(true, false)
        _sbc
      when 0xf2 # _jam
        _jam
      when 0xf3 # _isb
        ind_y(true, true)
        _isb
        store_mem
      when 0xf4 # _nop
        @_pc += 1
        @clk += 4 * RP2A03_CC
      when 0xf5 # _sbc
        zpg_x(true, false)
        _sbc
      when 0xf6 # _inc
        zpg_x(true, true)
        _inc
        store_zpg
      when 0xf7 # _isb
        zpg_x(true, true)
        _isb
        store_zpg
      when 0xf8 # _sed
        _sed
      when 0xf9 # _sbc
        abs_y(true, false)
        _sbc
      when 0xfa # _nop
        @_pc += 0
        @clk += 2 * RP2A03_CC
      when 0xfb # _isb
        abs_y(true, true)
        _isb
        store_mem
      when 0xfc # _nop
        abs_x(true, false)
        _nop
      when 0xfd # _sbc
        abs_x(true, false)
        _sbc
      when 0xfe # _inc
        abs_x(true, true)
        _inc
        store_mem
      when 0xff # _isb
        abs_x(true, true)
        _isb
        store_mem
      end

      if @ppu_sync
        @ppu.sync(@clk)
      end
    end while @clk < @clk_target
    do_clock
  end while @clk < @clk_frame
end
