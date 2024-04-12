;-------------------------------------------------------------------------------
; output.s
;
; StrOut           uses wp1
; PrtHex2
; PrSpace
; PrSpace2
; PrSpace3
; CROut
; CROut2
; PrAddressX       Zpage x indexed
; PrHex2Spc
; Zshow
;-------------------------------------------------------------------------------


;----------------------------------------------------------------------------------------------
; StrOut. a = low, y = high byte of address of zero-terminated string
; on exit y = length of string (not counting the 00-terminator)
;----------------------------------------------------------------------------------------------

StrOut:
               sty     wp1+1
               sta     wp1
               ldy     #0
stro10:        lda     (wp1),y
               beq     stro99
               jsr     CHROUT
               iny
               bne     stro10          ; !! string must be shorter than 255 bytes 
stro99:        rts

;-------------------------------------------------------------------------------
; PrHex2CR
;-------------------------------------------------------------------------------

PrHex2CR:      jsr     PrHex2
               jmp     CROut

;-------------------------------------------------------------------------------
; PrHex2
; ! bpreset means output goes to keyboard buffer
;-------------------------------------------------------------------------------

PrHex2:        pha
               lsr                                  
               lsr
               lsr
               lsr
               jsr     prnyb
               pla    
               and     #$0f
prnyb:         clc
               adc     #$30 
               cmp     #$3a
               bcc     prnyb10
               adc     #$06
prnyb10:       bit     btokbb
               bpl     prnyb50
               sta     KEYBB,x
               inx
               rts
prnyb50:       jmp     CHROUT

;-------------------------------------------------------------------------------
; PrHex4AY
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; PrSpacex
;-------------------------------------------------------------------------------

PrSpace3:      jsr     PrSpace
PrSpace2:      jsr     PrSpace
PrSpace:       lda     #$20   
               jmp     CHROUT
               
;-------------------------------------------------------------------------------
; CROut, CROut2
;-------------------------------------------------------------------------------
               
CROut2:        jsr     CROut
CROut:         lda     #$0d
               jmp     CHROUT
               
;-------------------------------------------------------------------------------
; PrAddressX
; print address at ZP loc indexed 
;-------------------------------------------------------------------------------
               
PrAddressX:    lda     $01,x           ; x has offset of zeropage ptr
               jsr     PrHex2
               lda     $00,x
               jmp     PrHex2
               
;-------------------------------------------------------------------------------
; PrHex2Spc
; print hex byte followed by space
;-------------------------------------------------------------------------------
               
PrHex2Spc:     jsr     PrHex2    
               lda     #$20
               jmp     CHROUT
               
