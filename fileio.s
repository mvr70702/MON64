;==============================================================================================
; Fileio.s
;
; PrStat
; LoadF
; SaveF
; Dir
;==============================================================================================


;----------------------------------------------------------------------------------------------
; PrStat       read & print IO status byte
;----------------------------------------------------------------------------------------------

PrStat:
               jsr     CROut
               jsr     READST
               jsr     PrHex2
               jmp     CROut

;----------------------------------------------------------------------------------------------
; LoadF "name" dev (xxxx)  
;----------------------------------------------------------------------------------------------

LoadF:         tya
               pha
               jsr     ReadString      ; read to 'filename'
               lda     strlen 
               beq     load090         ; missing filename
               jsr     ReadHex         ; get device num
               beq     load090         ; missing device nr
               lda     hnuml           ; devnum
               pha
               ldy     #1              ; sa 1 : use header
               jsr     ReadHex         ; alt. loadaddress ?
               beq     load050
               dey                     ; sa 0 : use supplied address
load050:       pla                     ; retrieve devnum
               tax                     ; destroys inbuf pointer, it's no longer needed
               lda     #0              ; Logical filenum for LOAD
               jsr     SETLFS
               lda     strlen 
               ldx     #<string  
               ldy     #>string  
               jsr     SETNAM
               lda     #0              ; LOAD (1= VERIFY)
               ldx     hnuml           ; set alternative load address
               ldy     hnumh           ; (not used if sa = 1)
               jsr     LOAD
               jsr     PrStat
               tya                     ; print high address of LOAD+1 (!)
               jsr     PrHex2
               txa
               jsr     PrHex2
               jsr     CROut
               pla
               tay
               jmp     mon0200
load090:       pla
               tay
               jmp     mon0900
               
;----------------------------------------------------------------------------------------------
; SaveF "name" dev rangelo rangehi
;----------------------------------------------------------------------------------------------

SaveF:         tya
               pha
               jsr     ReadString
               lda     strlen  
               beq     save090         ; missing filename
               jsr     ReadHex
               beq     save090         ; missing devnum
               stx     tmpx            ; save inbuf pointer
               ldx     hnuml           ; devnum
               ldy     #$ff
               lda     #1              ; logical file nr 1 (reserved for SAVE)
               jsr     SETLFS
               lda     strlen 
               ldx     #<string  
               ldy     #>string  
               jsr     SETNAM
               ldx     tmpx
               jsr     ReadHex         ; rangelo
               beq     save090         ; not present
               lda     hnuml
               sta     wp0             ; setup page 0 pointer, use epl,h
               lda     hnumh
               sta     wp0+1
               jsr     ReadHex         ; rangehi
               beq     save090         ; not present
               lda     #wp0            ; a = zp address of *rangelo
               ldx     hnuml           ; x,y = rangehi
               ldy     hnumh
               inx                     ; SAVE not inclusive hi, bump it
               bne     +
               iny
+:             jsr     SAVE
               jsr     PrStat
               pla 
               tay
               jmp     mon0200
save090:       pla
               tay
               jmp     mon0900
               
               
;----------------------------------------------------------------------------------------------
; Dir (xx)
;----------------------------------------------------------------------------------------------

skip:          nop
-:             jsr     ACPTR
               dex
               bne     -
               rts

Dir:           lda     #0

               sta     STATUS
               lda     #8
               sta     drv
               jsr     ReadHex
               beq     +
               lda     hnuml
               sta     drv
+:             lda     drv
               jsr     LISTN
               lda     #$f0            ; channel 0
               jsr     SECND
               lda     #'$'
               jsr     CIOUT
               jsr     UNLSN
               lda     drv
               jsr     TALK
               lda     #$60
               jsr     TKSA
               ldx     #2
               jsr     skip
dir020:        ldx     #2
               jsr     skip
               lda     STATUS
               bne     dir090
               jsr     ACPTR           ; size (blocks) lo
               pha
               jsr     ACPTR           ; size (blocks) hi
               jsr     PrHex2
               pla 
               pha
               jsr     PrHex2Spc
               pla
               cmp     #$0a
               bcc     +
               jsr     PrSpace
+:             lda     STATUS
               bne     dir090
dir040:        jsr     ACPTR           ; get the filename
               tax
               cmp     #0
               beq     dir060
               lda     STATUS
               bne     dir090
               txa
               jsr     CHROUT          ; print it
               jmp     dir040
dir060:        jsr     CROut
               lda     STATUS
               beq     dir020
dir090:        
               jsr     UNTLK
               lda     drv
               jsr     LISTN
               lda     #$e8
               jsr     SECND
               jsr     UNLSN
               jmp     mon0200
               
