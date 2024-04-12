
;----------------------------------------------------------------------------------------------
; instrdisp / decode / prt
;
; Swap the high- and low nybble of the opcode. The (now) high nybble then becomes an index to
; a column in the 6502 opcode matrix. 4 columns are empty (the xxxxxx11 opcodes), column2
; contains only 1 instruction. So 5 columns can be eliminated. Furthermore, no column contains
; more than 4 different addressmodes. The whole matrix can thus be packed into a 176-byte
; lookup table (6 bits mnem, 2 bits mode).
; table entry = MMMMMMmm (M = mnemonic, m = mode)
; And more : a further 4 columns can be dropped because the opcodes are equal or almost equal to 
; others. Just catch 2 invalid opcodes, 89 and 9e.
; This is still larger than the famous 6502 disassembler by Steve Wozniaks, but it's simple
; and fast.
;----------------------------------------------------------------------------------------------

newnyb:        DB      0,1,$ff,$ff,2,3,4,$ff,5,6,7,$ff,8,9,10,$ff
newnewnyb:     DB      0,1,2,1,3,4,1,5,6,1,3

;-------------------------------------------------------------------------------
; InstrDisp
;-------------------------------------------------------------------------------
               
InstrDisp:     jsr     InstrDec
               jmp     InstrPrt
               
;-------------------------------------------------------------------------------
; UnAssemble (xxxx) 
;-------------------------------------------------------------------------------

UnAssemble:
               jsr     ReadHex
               beq     unoaddr
               lda     hnuml
               sta     unassadr
               lda     hnumh
               sta     unassadr+1
unoaddr:       lda     unassadr
               sta     wp0
               lda     unassadr+1
               sta     wp0+1
               lda     #16
               sta     dcount
uas030:        jsr     InstrDisp
               jsr     CROut
               lda     wp0
               clc
               adc     length
               sta     wp0
               bcc     uas050
               inc     wp0+1
uas050:        dec     dcount
               bne     uas030
               lda     wp0
               sta     unassadr
               lda     wp0+1
               sta     unassadr+1
               jmp     mon0200
;-------------------------------------------------------------------------------
; CalcTarget
; calculate branch target
; wp0 points to a branch instruction, target to wp1
;-------------------------------------------------------------------------------

CalcTarget:   
               lda     wp0             ; wp1 must point to branch
               sta     wp1
               lda     wp0+1
               sta     wp1+1
               ldy     #1              ; point to displacement
               ldx     #0              ; x = sign extension
               lda     (wp1),y         ; displacement
               bpl     ct020     
               dex                     ; ff if displacement is negative (sign extension)
ct020:         clc
               adc     wp1             ; pcl + displacement
               sta     wp1
               txa                     ; sign extension
               adc     wp1+1
               sta     wp1+1
               clc                     ; now add 2, branch starts AFTER the bxx dd instr.
               lda     wp1
               adc     #2
               sta     wp1
               bcc     ct099   
               inc     wp1+1
ct099:         rts

;----------------------------------------------------------------------------------------------
; InstrDec : decode, setup mnem (0 = invalid) , length (1..3) and amode (all 8 bits)
; wp0 is pointer to instruction.
;----------------------------------------------------------------------------------------------

InstrDec:
               ldy     #1
               sty     length
               dey
               sty     mnem            ; init with invalid mnem (0)
               sty     amode
               lda     (wp0),y
               cmp     #$a2            ; deal with single instruction in column 2
               bne     isd0020
               lda     #_LDX / 4       ; lsr 2
               sta     mnem
               lda     #_IMM           ; immediate. 2 byte instr.
               bne     isd0200         ; always, go set mode & length
isd0020:       cmp     #$89
               beq     isd0920
               cmp     #$9e
               beq     isd0920
               
               and     #$0f            ; low nyb
               tax
               lda     newnyb,x
               tax
               cmp     #$ff            ; check invalid columns
               beq     isd0920         ; invalid opcode, exit
               asl                     ; a now index for 11 columns
               asl
               sta     temp4           ; * 4 = lookup index for mode for each row based on 11 columns
               txa
               lda     newnewnyb,x     ; eliminate more, now 0..6
               asl                     ; 
               asl                     ; now high nyb in lookup
               asl
               asl                     ; high nybble now 0..6
;
               sta     temp
               lda     (wp0),y         ; org. opcode
               lsr
               lsr
               lsr
               lsr
               ora     temp
               tax                     ; swapped adjusted opcode
               
               lda     lookup,x        ; 112-byte lookuptable
               cmp     #$00            ; invalid ?
               beq     isd0920         ; invalid, exit
               pha
               and     #$fc            ; mnemonic
               lsr
               lsr
               sta     mnem
               pla
               and     #$03            ; column mode 0..3
               clc
               adc     temp4
               tax
               lda     cmodes,x        ; make true mode
isd0200:       sta     amode
               bit     amode           ; test bits 7 and 6
               bpl     +               ; (must NOT both be set)
               inc     length          ; inc doesn't touch V flag
               inc     length
+:             bvc     isd0900
               inc     length
isd0900:       sta     amode
isd0920:       rts
               
;----------------------------------------------------------------------------------------------
; InstrPrt
; print aaaa: xx yy xx  (or spaces if length < 3) mnem  operand
; wp0 points to instruction
;----------------------------------------------------------------------------------------------

InstrPrt:      ldx     #<wp0           ; print address in upl,h
               jsr     PrAddressX
               lda     #$3a            ; ':'
               jsr     CHROUT
               ldx     length          ; print the code bytes
isdd030:       lda     (wp0),y         ; y is still 0 
               jsr     PrHex2Spc
               iny
               dex
               bne     isdd030
isdd050:       cpy     #3              ; done 3 bytes (length = 3) ?
               beq     isdd080
               jsr     PrSpace3        ; fill with spaces
               iny
               bne     isdd050
               
; print mnem

isdd080:       
               jsr     PrSpace
               
               lda     mnem
               asl                     ; 2 bytes/mnem
               tax
               lda     mnems+1,x
               sta     mnemh
               lda     mnems,x
               sta     mneml           ; 1st char already in lower 5 bits, doesn't need shifting
               ldx     #2
pmcout:        and     #$1f            
               clc
               adc     #63
               jsr     CHROUT
               ldy     #4
-:             lsr     mnemh           ; rotate 2nd/3th char to lower 5 bits of mneml
               ror     mneml
               dey
               bpl     -
               lda     mneml
               dex
               bpl     pmcout
               jsr     PrSpace2
;                                      
               lda     mnem            ; no operand print if invalid opcode
               beq     isdexit


; print operand
                          
               lda     amode
               and     #$0f            ; mode index
               tax
               lda     fmtbytes,x
               cmp     #$ff            ; relative ?
               beq     isd0400         ; go print target address
               ldx     #0
isd0300:       asl
               pha
               bcc     isd0320
               lda     prbytes,x
               cmp     #'$'            ; byte or word print ?
               bne     isd0318
               lda     #'$'
               jsr     CHROUT
               ldy     #2              ; assume length = 3
               lda     length
               cmp     #3
               bne     +
               lda     (wp0),y         ; if length == 3, print hi byte of address
               jsr     PrHex2
+              dey
               lda     (wp0),y         ; print low (or only) byte
               jsr     PrHex2
               jmp     isd0320
isd0318:       jsr     CHROUT          ; #,(,x,y,),
isd0320:       pla
               inx
               cpx     #8
               bne     isd0300
isdexit:       rts                     ; exit 1
               
; print branch target

isd0400:   
               jsr     CalcTarget      ; wp1 now has targetaddress. print it
               lda     #'$'
               jsr     CHROUT
               ldx     #wp1
               jmp     PrAddressX      ; exit 2
;
; fmtbytes : each bit represents a char to print, $ff = relative
;            prbytes :  #($),xy)     $ = 1 or 2 bytes, depending on length   
;
prbytes:       DB      "#($),XY)"
fmtbytes:      DB      %10100000    ; _IMM  , #$hh
               DB      %00100000    ; _ZP   , $hh   
               DB      %00100000    ; _ABS  , $hhhh
               DB      %00000000    ; _IMP
               DB      %00000000    ; _ACC
               DB      %01101101    ; _IZPX , ($hh,x)
               DB      %01111010    ; _IZPY , ($hh),y
               DB      %00101100    ; _ZPX  , $hh,x
               DB      %00101100    ; _ABSX , $hhhh,x
               DB      %00101010    ; _ABSY , $hhhh,y
               DB      %01110000    ; _IABS , ($hhhh)
               DB      %00101010    ; _ZPY  , $hh,y
               DB      %11111111    ; _REL  , $target
               
               nop                                             ; table separator for .lst file
;              
; MODES for each column :               
;
;   column0  : 00 = implied,  01 = relative,   02 = abs, 03 = immediate               
;   column1  : 00 = (zp,x),   01 = (zpage),y
;   column2  : 00 = zp,       01 = zp,x
;   column3  : 00 = zp,       01 = zp,x
;   column4  : 00 = zp,       01 = zp,x        02 = zp,y
;   column5  : 00 = implied
;   column6  : 00 = immediate,01 = abs,y
;   column7  : 00 = acc       01 = implied
;   column8  : 00 = abs       01 = (abs)       02 = abs,x
;   column9  : 00 = abs       01 = abs,x
;   column10 : 00 = abs       01 = abs,x       02 = abs,y
; 
; this is the lookup table after the elimination of 5 empty columns :
;
;lookup:        .byte   _BRK|0, _BPL|1, _JSR|2, _BMI|1, _RTI|0, _BVC|1, _RTS|0, _BVS|1,    $00, _BCC|1, _LDY|3, _BCS|1, _CPY|3, _BNE|1, _CPX|3, _BEQ|1
;               .byte   _ORA|0, _ORA|1, _AND|0, _AND|1, _EOR|0, _EOR|1, _ADC|0, _ADC|1, _STA|0, _STA|1, _LDA|0, _LDA|1, _CMP|0, _CMP|1, _SBC|0, _SBC|1
;               .byte      $00,    $00, _BIT|0,    $00,    $00,    $00,    $00,    $00, _STY|0, _STY|1, _LDY|0, _LDY|1, _CPY|0,    $00, _CPX|0,    $00
;               .byte   _ORA|0, _ORA|1, _AND|0, _AND|1, _EOR|0, _EOR|1, _ADC|0, _ADC|1, _STA|0, _STA|1, _LDA|0, _LDA|1, _CMP|0, _CMP|1, _SBC|0, _SBC|1
;               .byte   _ASL|0, _ASL|1, _ROL|0, _ROL|1, _LSR|0, _LSR|1, _ROR|0, _ROR|1, _STX|0, _STX|2, _LDX|0, _LDX|2, _DEC|0, _DEC|1, _INC|0, _INC|1
;               .byte   _PHP|0, _CLC|0, _PLP|0, _SEC|0, _PHA|0, _CLI|0, _PLA|0, _SEI|0, _DEY|0, _TYA|0, _TAY|0, _CLV|0, _INY|0, _CLD|0, _INX|0, _SED|0
;               .byte   _ORA|0, _ORA|1, _AND|0, _AND|1, _EOR|0, _EOR|1, _ADC|0, _ADC|1,    $00, _STA|1, _LDA|0, _LDA|1, _CMP|0, _CMP|1, _SBC|0, _SBC|1
;               .byte   _ASL|0,    $00, _ROL|0,    $00, _LSR|0,    $00, _ROR|0,    $00, _TXA|1, _TXS|1, _TAX|1, _TSX|1, _DEX|1,    $00, _NOP|1,    $00
;               .byte      $00,    $00, _BIT|0,    $00, _JMP|0,    $00, _JMP|1,    $00, _STY|0,    $00, _LDY|0, _LDY|2, _CPY|0,    $00, _CPX|0,    $00
;               .byte   _ORA|0, _ORA|1, _AND|0, _AND|1, _EOR|0, _EOR|1, _ADC|0, _ADC|1, _STA|0, _STA|1, _LDA|0, _LDA|1, _CMP|0, _CMP|1, _SBC|0, _SBC|1
;               .byte   _ASL|0, _ASL|1, _ROL|0, _ROL|1, _LSR|0, _LSR|1, _ROR|0, _ROR|1, _STX|0,    $00, _LDX|0, _LDX|2, _DEC|0, _DEC|1, _INC|0, _INC|1

; final lookup table, 4 more columns deleted, but the original indexes into the 'cmodes' table are kept (temp4).

lookup:        DB      _BRK|0, _BPL|1, _JSR|2, _BMI|1, _RTI|0, _BVC|1, _RTS|0, _BVS|1,    $00, _BCC|1, _LDY|3, _BCS|1, _CPY|3, _BNE|1, _CPX|3, _BEQ|1
               DB      _ORA|0, _ORA|1, _AND|0, _AND|1, _EOR|0, _EOR|1, _ADC|0, _ADC|1, _STA|0, _STA|1, _LDA|0, _LDA|1, _CMP|0, _CMP|1, _SBC|0, _SBC|1
               DB         $00,    $00, _BIT|0,    $00,    $00,    $00,    $00,    $00, _STY|0, _STY|1, _LDY|0, _LDY|1, _CPY|0,    $00, _CPX|0,    $00
               DB      _ASL|0, _ASL|1, _ROL|0, _ROL|1, _LSR|0, _LSR|1, _ROR|0, _ROR|1, _STX|0, _STX|2, _LDX|0, _LDX|2, _DEC|0, _DEC|1, _INC|0, _INC|1
               DB      _PHP|0, _CLC|0, _PLP|0, _SEC|0, _PHA|0, _CLI|0, _PLA|0, _SEI|0, _DEY|0, _TYA|0, _TAY|0, _CLV|0, _INY|0, _CLD|0, _INX|0, _SED|0
               DB      _ASL|0,    $00, _ROL|0,    $00, _LSR|0,    $00, _ROR|0,    $00, _TXA|1, _TXS|1, _TAX|1, _TSX|1, _DEX|1,    $00, _NOP|1,    $00
               DB         $00,    $00, _BIT|0,    $00, _JMP|0,    $00, _JMP|1,    $00, _STY|0,    $00, _LDY|0, _LDY|2, _CPY|0,    $00, _CPX|0,    $00
               
               nop                                             ; table separator for .lst file
               
               
cmodes:        DB      _IMP ,_REL ,_ABS,_IMM
               DB      _IZPX,_IZPY, 0    , 0  
               DB      _ZP  ,_ZPX , 0    , 0  
               DB      _ZP  ,_ZPX , 0    , 0  
               DB      _ZP  ,_ZPX ,_ZPY  , 0
               DB      _IMP , 0   , 0    , 0
               DB      _IMM ,_ABSY, 0    , 0
               DB      _ACC ,_IMP , 0    , 0
               DB      _ABS ,_IABS,_ABSX , 0
               DB      _ABS ,_ABSX, 0    , 0
               DB      _ABS ,_ABSX,_ABSY , 0
