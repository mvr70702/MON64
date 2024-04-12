;-------------------------------------------------------------------------------
; mem.s
;
; Dump
; Edit
; Fill
; Hunt
; Move
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; Dump         xxxx
;-------------------------------------------------------------------------------

Dump:   
               jsr     ReadHex         ; get the optional address
               beq     dnoaddr         ; there is none
               lda     hnuml
               sta     dumpadr
               lda     hnumh
               sta     dumpadr+1
dnoaddr:       lda     dumpadr
               sta     wp0
               lda     dumpadr+1
               sta     wp0+1
               ldy     #7
               sty     dcount          ; dump 8 lines of 8 bytes
dmp010:        ldx     #<wp0
               jsr     PrAddressX      ; print address at (wp0)
               jsr     PrSpace2
               ldy     #0
dmp020:        lda     (wp0),y
               jsr     PrHex2Spc
               iny
               cpy     #8
               bne     dmp020
               jsr     PrSpace
               ldy     #0
dmp040:        lda     (wp0),y
               bmi     dmp050
               cmp     #$20            ; less than space ?
               bcs     dmp060
dmp050:        lda     #'.'
dmp060:        jsr     CHROUT
               iny 
               cpy     #8
               bne     dmp040
               jsr     CROut
               lda     wp0             ; update ptr
               clc
               adc     #8
               sta     wp0
               bcc     dmp090
               inc     wp0+1
dmp090:        dec     dcount
               bpl     dmp010
               lda     wp0             ; update dumpadr
               sta     dumpadr
               lda     wp0+1 
               sta     dumpadr+1
               jmp     mon0200         ; return to main
               
;-------------------------------------------------------------------------------
; ReadBytes
; scan inbuf for hexbytes and/or quoted strings
; used in Edit, Hunt, Fill
; store everything in bytebuf
;-------------------------------------------------------------------------------

StoIncY:       sta     bytebuf,y       ; helper, returns NEQ
               iny
               inc     nbytes
               rts

ReadBytes:     tya
               pha
               lda     #0
               tay
               sta     nbytes
rb0100:        jsr     ReadHex         ; a hex byte ?
               bne     rb0200          ; yes, go store
               jsr     ReadString      ; a string ?
               lda     strlen
               beq     rb0900          ; no, end
               stx     xsav            ; must save x, it's the readindex into inbuf
               ldx     #0
rb0150:        lda     string,x        ; copy the string to bytebuf
               jsr     StoIncY
               inx
               dec     strlen
               bne     rb0150
               ldx     xsav
               bne     rb0100          ; always back for more
rb0200:        lda     hnuml           ; copy the hexbyte to bytebuf
               jsr     StoIncY
               bne     rb0100          ; always, back for more
rb0900:        pla
               tay
               rts
               
;-------------------------------------------------------------------------------
; Edit
;-------------------------------------------------------------------------------
               
Edit:          jsr     ReadHex
               beq     edit0990        ; no address, error
               lda     hnuml
               sta     wp0
               lda     hnumh
               sta     wp0+1
               jsr     ReadBytes       ; read hexbytes,strings etc. to bytebuf
               lda     nbytes          ; anything read ?
               beq     edit0990        ; no, error
               ldy     #0
edit0100:      lda     bytebuf,y
               sta     (wp0),y
               iny
               dec     nbytes
               bne     edit0100
               jmp     mon0200         ; OK
edit0990:      jmp     mon0900         ; error

;-------------------------------------------------------------------------------
; GetRange
; read 2 hex numbers to wp0,wp1
; subtract 2-1, store result in 'nrange'. Error if 2 < 1
; CY set if any error
;-------------------------------------------------------------------------------

GetRange:      jsr     ReadHex         ; get first, low end range
               beq     gr0999          ; nothing there, error
               lda     hnuml
               sta     wp0
               lda     hnumh
               sta     wp0+1
               jsr     ReadHex         ; get second, hi end range
               beq     gr0999          ; nothing there, error
               lda     hnuml
               sta     wp1
               lda     hnumh
               sta     wp1+1
               sec
               lda     wp1
               sbc     wp0
               sta     nrange
               lda     wp1+1
               sbc     wp0+1
               sta     nrange+1
               bcc     gr0999          ; error, lo > hi
               clc                     ; good return
               rts
gr0999:        sec                     ; indicate error
               rts
               
;-------------------------------------------------------------------------------
; Fill    xxxx yyyy bytes
; Hunt    xxxx yyyy bytes
;
; Not the fastest possible, but OK
;-------------------------------------------------------------------------------

wp0towp1:      lda     wp0
               sta     wp1
               lda     wp0+1
               sta     wp1+1
               inc     wp1
               bne     +
               inc     wp1+1
+:             rts

Hunt:          lda     #$80
               bne     fl0010
Fill:          lda     #$00
fl0010:        sta     mmode
               jsr     GetRange        ; read range, wp0,wp1
               bcs     edit0990        ; CY indicates error
               jsr     ReadBytes       ; get fill value(s)
               lda     nbytes          ; anything ?
               beq     edit0990        ; no, error
               ldx     #0              ; zero for store
fl0040:        ldy     #0              ; index into inbuf (nbytes)
               jsr     wp0towp1
fl0050:        lda     bytebuf,y       ; get a filler byte
               bit     mmode
               bpl     fl0080
               cmp     (wp0,x)         ; Hunt
               beq     fl0090
               jsr     wp0towp1
               ldy     #$ff
               bne     fl0090
fl0080:        sta     (wp0,x)
fl0090:        inc     wp0             ; inc storage ptr
               bne     fl0100
               inc     wp0+1
fl0100:        lda     nrange          ; nr. bytes in range left == 0 ?
               ora     nrange+1
               beq     fl0900          ; yes.done
               lda     nrange          ; decr. nr.bytes
               bne     fl0120
               dec     nrange+1
fl0120:        dec     nrange
               iny                     ; filler byte(s) index
               cpy     nbytes          ; at end ?
               bcc     fl0050          ; not yet, continue
               bit     mmode
               bpl     fl0040          ; go reset y & continue
               lda     wp1+1
               jsr     PrHex2
               lda     wp1
               jsr     PrHex2
               jsr     CROut
               jmp     fl0040
fl0900:        jmp     mon0200

               

