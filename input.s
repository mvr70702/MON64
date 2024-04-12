;-------------------------------------------------------------------------------
; input.s
;
; ChkStop
; CurOn
; CurOff
; GetLineZ
; SetLine
; SkpSpace
; ReadHex
; ReadString
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; ChkStop
; check STOP key pressed, return EQ if so
;-------------------------------------------------------------------------------

ChkStop:       lda     STKEY
               cmp     #$7f
               bne     chks90
               lda     #0
               sta     NDX             ; remove STOP from keybuf, set Z
chks90:        rts

;-------------------------------------------------------------------------------
; Fx shortcuts for GetLineZ
;-------------------------------------------------------------------------------

;                      F1  F3  F5  F7  F2  F4   F6   F8
shorttab:      DB      'I',0 ,'R' ,'O','U' ,0,  0,  'O'   ; F3 not available !

;-------------------------------------------------------------------------------
; CurOn
;-------------------------------------------------------------------------------

CurOn:         lda     #0
               sta     BLNSW           ; flash cursor
               rts

;-------------------------------------------------------------------------------
; CurOff
;-------------------------------------------------------------------------------

CurOff:        php
               sei                     ; disable cursor flash so it won't interfere
               lda     GDBLN           ; character under cursor
               jsr     CHROUT          ; restores character, not color (MON doesn't change it)
               lda     #1
               sta     BLNSW           ; stop cursor flash
               plp
               rts

;-------------------------------------------------------------------------------
; GetLineZ     uses GETIN, max (36-prompt) char's.
;              flash cursor.
;              input to 'inbuf', 0x00-terminated
;              handle Fx shortcuts
;-------------------------------------------------------------------------------

GetLineZ       ldx     #0
               jsr     CurOn
glz020:        lda     NDX             ; key waiting ?
               beq     glz020
               stx     tmpx            ; !! GETIN scrambles xreg
               php
               sei                     ; thread safe, IRQ may run, adding keystrokes in the middle of this proc
               jsr     GETIN           ; should be safe, but just in case..
               ldx     tmpx
               plp                     ; restore interrupt flag as it was
               cmp     #$0d
               bne     glz030
glz027:        lda     #0              ; terminate line
               sta     inbuf,x
               stx     inbufmax
               beq     glz099          ; EXIT, branch always
               
glz030:        cmp     #$14            ; backspace/del ?
               bne     glz033
               cpx     #0              ; not at pos 0 ?
               beq     glz020
               dex
               lda     #$14
               jsr     CHROUT
               jmp     glz020
               
glz033:        cmp     #134            ; F3 ?
               bne     glz040
;              dec     inbufmax
               ldx     #0
glz035:        lda     inbuf,x
               beq     glz020
               jsr     CHROUT
               inx
               bne     glz035
               
glz040:        cmp     #$85            ; >= F1 ?   (85 89 86 8a 87 8b 88 8c)
               bcc     glz070
               cmp     #$8d            ; <= F8 ?
               bcs     glz070
               sbc     #$84            ; make index 0..7 (CY = clear)
               tay
               lda     shorttab,y
               beq     glz020          ; no shortcut for this Fn key
               sta     inbuf
               ldx     #1              ; terminator comes at pos 1
               bne     glz027          ; always
               
               
               
glz070:        cmp     #$20            ; accept only $20..$5f
               bcc     glz020
               cmp     #$60
               bcs     glz020
               cpx     #36             ; maxlen inbuf reached ?
               bcs     glz020          ; yes, don't store, just get more until CR 
               sta     inbuf,x
               jsr     CHROUT
               inx
               bne     glz020          ; always
               
glz099:        jsr     CurOff
               rts
               
;-------------------------------------------------------------------------------
; SkpSpace     skips spaces in inbuf
;-------------------------------------------------------------------------------

SkpSpace:      lda     inbuf,x
               inx
               cmp     #$20
               beq     SkpSpace
               dex
               rts

;----------------------------------------------------------------------------------------------               
; ReadHex return Z = 0 (NEQ, a = $ff) if any digit found
; on exit, x points to 1st non-dig. 
;----------------------------------------------------------------------------------------------

ReadHex:       tya
               pha
               jsr     SkpSpace
               ldy     #0
               sty     temp4
               sty     hnuml
               sty     hnumh
rh020:         lda     inbuf,x
               sec
               sbc     #$30
               cmp     #$0a
               bcc     gotdig          ; 0..9
               adc     #$e8            ; e9, but carry is set
               cmp     #$fa
               bcc     nodig
               and     #$0f            ; fa..ff, drop hi nybble
gotdig:        ldy     #3
rh060:         asl     hnuml
               rol     hnumh
               dey
               bpl     rh060           ; if any digit found, temp4 = $ff
               sty     temp4
               ora     hnuml           ; OR in new digit
               sta     hnuml
               inx
               bne     rh020           ; branch always
nodig:         pla
               tay
               lda     temp4           ; temp 4 = ff if hexnum found
               rts
               
;----------------------------------------------------------------------------------------------
; ReadString   read double-quoted string,set strlen,string[](limit = 16 chars)
; on exit, x points to 1st char after closing " (if present)
;----------------------------------------------------------------------------------------------

ReadString:    tya
               pha
               ldy     #0
               sty     strlen 
               jsr     SkpSpace
               lda     inbuf,x
               cmp     #$22            ; "
               bne     rfn099
rfn040:        inx
               lda     inbuf,x         
               beq     rfn099          ; unexpected EOL ? 
               cmp     #$22            ; closing " ?
               beq     rfn098          ; go bump x and exit
               cpy     #16             ; maxlen
               bcs     rfn040
               sta     string,y   
               iny
               inc     strlen
               bne     rfn040          ; branch always
rfn098:        inx                     ; bump past last "
rfn099:        pla
               tay
               rts


