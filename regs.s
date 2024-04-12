;-------------------------------------------------------------------------------
; regs.s
;
; DispRegs
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Data
;-------------------------------------------------------------------------------

regnames:      .byte   "AXYSFP"       
flagnames:     .byte   "NV.BDIZC"

;-------------------------------------------------------------------------------
; DispRegs
; SetP
;-------------------------------------------------------------------------------

SetP:          dex                     ; backup to 'P'
DispRegs:      jsr     SkpSpace
               ldy     #0
rd020:         lda     inbuf,x
               beq     rd090           ; found eol, no further search
               cmp     regnames,y
               bne     rd030
               inx
               jsr     ReadHex
               beq     rd090           ; no hexnum found, just disp
               lda     hnuml
               sta     areg,y
               cpy     #5              ; changing pc ?
               bne     rd090
               lda     hnumh
               sta     areg+1,y
               jmp     rd090
rd030:         iny
               cpy     #6
               bcc     rd020           ; try next
               jmp     mon0900         ; not a regname, error
;              
; display (new) regs. entry to disp regs without commandline
;
DispRegs2:
rd090:         ldx     #0
rd100:         lda     regnames,x
               jsr     CHROUT
               lda     #$3a            ; :
               jsr     CHROUT
               lda     areg,x
               jsr     PrHex2Spc
               inx
               cpx     #5              ; printing flags ?
               bne     rd100
               
               lda     flags
               ldx     #0
fd050:         asl
               pha
               bcs     fd060
               lda     #18             ; reverse on
               jsr     CHROUT
fd060:         lda     flagnames,x
               jsr     CHROUT
               lda     #146            ; reverse off
               jsr     CHROUT
               pla
               inx
               cpx     #8
               bcc     fd050
               jsr     PrSpace2
               lda     tmode           ; disp tmode (0 = simulate, else execute)
               beq     fd070
               lda     #'X'
               .byte   $2C
fd070:         lda     #'S'
               jsr     CHROUT
               jsr     CROut
               lda     pcl             ; copy user pc to wp0 for unassemble
               sta     unassadr        ; also copy to memory
               sta     wp0
               lda     pch
               sta     unassadr+1
               sta     wp0+1
               jsr     InstrDisp       ; display the instruction at (pcl,pch)
               jsr     SetupEA         ; wp0 still has address of instr, EA to wp1
               bcc     fd090
               sec
               jsr     PLOT            ; !! plot returns x-pos in y reg and vv
               clc
               ldy     #29             ; x-pos
               jsr     PLOT
               ldx     #wp1
               jsr     PrAddressX
               jsr     PrSpace
               ldy     #0
               lda     (wp1),y
               jsr     PrHex2
fd090:         jsr     CROut
               jmp     mon0200
               
;------------------------------------------------------------------------------
; SetupEA : setup effective address in wp1
;           wp0 has address of instruction
;           must be predeeded by a call to 'InstrDec' !
;           return CY set = ea has address, CC = no address (implied,..)
;------------------------------------------------------------------------------

SetupEA:       ldy     #1
               lda     (wp0),y         ; get operand lo
               sta     wp1
               iny
               lda     (wp0),y         ; get operand hi (for 3 byte instructions)
               sta     wp1+1           ; hi byte will be cleared when needed
               ldy     #0
               lda     amode           ; InstrDec has setup this
               and     #$0f            ; mode bits only
               tax
               lda     eatabh,x
               pha
               lda     eatabl,x
               pha
               rts                     ; dispatch
               
;                        _IMM           _ZP         _ABS        _IMM          _ACC        _IZPX        _IZPY     
eatabl:        .byte   <(seanone-1),<(seanoix-1),<(seanoix-1),<(seanone-1),<(seanone-1),<(seaizpx-1),<(seaizpy-1)
;                        _ZPX           _ABSX       _ABSY       _IABS         _ZPY        _REL         _IZPY     
               .byte   <(seaxix-1) ,<(seaxix-1) ,<(seayix-1) ,<(seaiabs-1),<(seayix-1) ,<(seanone-1)
               
;                        _IMM           _ZP         _ABS        _IMM          _ACC        _IZPX        _IZPY     
eatabh:        .byte   >(seanone-1),>(seanoix-1),>(seanoix-1),>(seanone-1),>(seanone-1),>(seaizpx-1),>(seaizpy-1)
;                        _ZPX           _ABSX       _ABSY       _IABS         _ZPY        _REL         _IZPY     
               .byte   >(seaxix-1) ,>(seaxix-1) ,>(seayix-1) ,>(seaiabs-1),>(seayix-1) ,>(seanone-1)
               
seanone:       clc                     ; CY = 0 : no address
               rts
seanoix:       lda     #0
               beq     seaix
seaxix:        lda     xreg
               jmp     seaix
seayix:        lda     yreg  
seaix:         clc
               adc     wp1
               sta     wp1             ; inc eal
               lda     #0
               ldx     amode           ; bit 7 set is 3-byte instr
               bpl     sea060          ; if ZP, store 0 into eah..
               adc     wp1+1           ; cy is still there, if 3 byte instr, inc eah
sea060:        sta     wp1+1
seaex1:        sec
               rts
seaizpy:       ldx     wp1             ; has first byte of ptr
               lda     $00,x
               clc
               adc     yreg
               sta     wp1
               lda     $01,x
               adc     #0
               sta     wp1+1
               bcc     seaex1          ; always
seaizpx:       lda     wp1
               clc
               adc     xreg            ; actual pointer
               tax
               lda     $00,x
               sta     wp1
               lda     $01,x
               sta     wp1+1
               sec
               rts
seaiabs:       lda     (wp1),y         ; y = 0
               tax
               iny
               lda     (wp1),y
               sta     wp1+1
               stx     wp1
               sec
               rts
               
