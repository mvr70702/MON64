;-------------------------------------------------------------------------------
; Assemble.s
;
; Based on character matching.
; The input is converted to 3 char's mnem, operand, all hexdigs set to 0, no spaces
; (no checks apart from skipping spaces and EOL test) 
; ! hex numbers must be either 2 or 4 digits
; ! branch targets must be 4 digits 
; ! no $-signs, input must must unassembly output
; Then, for all opcodes 0..255 :
;   call InstrDec (unass.s) to get the mnemonic, length and addressmode
;   if valid opcode :
;      char by char :
;        generate the mnemonic ONLY while it matches the input
;        generate the operand, hexdigs 0, ONLY while it matches the input
;        if not exited yet : match ! assemble the opcode + hexnum in input
;      
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; MatchHex     Helper. check if 2 bytes at string,y are equal to $30
;              returns Z=1 if OK. bumps y
;-------------------------------------------------------------------------------

MatchHex:      lda     #$30            ; all hexdigs are '0'
               cmp     string,y
               bne     mtc090
               iny
               iny
               cmp     string-1,y
               bne     mtc090
mtc090:        rts

;-------------------------------------------------------------------------------
; IsHexDig     : return CY set if a has hexdig. returns Z=0
;-------------------------------------------------------------------------------

IsHexDig:      pha
               sec
               sbc     #$30
               cmp     #$0a
               bcc     ihd050          ; 0..9
               adc     #$e8            ; e9, but carry is set
               cmp     #$fa
               bcc     ihd090
ihd050:        sec
ihd090:        pla
               rts

;-------------------------------------------------------------------------------
; Assemble
;-------------------------------------------------------------------------------

Assemble:      jsr     ReadHex
               bne     asm0020
asm0010:       jmp     mon0950
asm0020:       lda     hnuml           ; read the (required) address
               sta     asmadr
               lda     hnumh
               sta     asmadr+1
;               
; first read input, copy to 'string', but don't copy spaces and '$', change hexdigits to '0', terminate with $00
;
               jsr     SkpSpace   
               ldy     #0
               sty     numread         ; flag for ReadHex
;              
; check line empty after address, quit Assemble mode
;
               lda     inbuf,x
               bne     asm0100
               jmp     mon0200
;               
; 3 chars mnemonic must be there
;
asm0100:       lda     inbuf,x         ; read chars input  
               beq     asm0010         ; EOL before 3 chars read -> error
               sta     string,y
               inx
               iny
               cpy     #3
               bne     asm0100
               dex                     ; decr. for 'inx' bvelow
;               
; now read & convert operand
;
asm0110:       inx
               lda     inbuf,x
               beq     asm0180         ; check EOL
               cmp     #$20            ; space ?
               beq     asm0110         ; throw away
               cmp     #'$'            ; $-char ?
               beq     asm0110         ; throw away
               jsr     IsHexDig        ; is it a hex. digit
               bcc     asm0180         ; no
               lda     numread         ; first time here ?
               bne     asm0120         ; no, skip ReadHex
               stx     xsav            ; temp save input ptr
               jsr     ReadHex         ; read the whole number for assembly
               inc     numread
               ldx     xsav
asm0120:       lda     #$30            ; replace with '0';
asm0180:       sta     string,y        ; copy 
               beq     asm0200         ; only EQ if input was 0
asm0190:       iny
               bne     asm0110
;              
; input processed
;
asm0200:       lda     #0
               sta     tryop
               lda     #<tryop
               sta     wp0
               lda     #>tryop
               sta     wp0+1
asm0240:       jsr     InstrDec        ; in unass.s. get mnem, length, mode
               lda     mnem            ; invalid opcode ?
               bne     asm0300         ; no, go check further
asm0250:       inc     tryop           ; yes, invalid, next one
               bne     asm0240
               jmp     mon0950         ; done all opcodes, invalid input
;               
; check mnems same
;
asm0300:       ldy     #0              ; 'string' index
               asl                     ; 2 bytes/mnem
               tax
               lda     mnems+1,x
               sta     mnemh
               lda     mnems,x
               sta     mneml           ; 1st char already in lower 5 bits, doesn't need shifting
               ldx     #2
asm0340:       and     #$1f            
               clc
               adc     #63
               cmp     string,y        ; matches input ?
               bne     asm0250         ; no, next opcode
               iny
               stx     temp4
               ldx     #4
asm0350:       lsr     mnemh           ; rotate 2nd/3th char to lower 5 bits of mneml
               ror     mneml
               dex
               bpl     asm0350
               ldx     temp4
               lda     mneml
               dex
               bpl     asm0340
;              
; mnem OK. now mode
;
               lda     amode
               and     #$0f            ; mode index
               tax
               lda     fmtbytes,x      ; in unass.s
               cmp     #$ff            ; relative ?
               bne     asm0390
               jmp     asm0600         ; go compare target address
asm0390:       ldx     #0              ; bit index into formatbits
asm0400:       asl                     ; shift format bit into carry..
               sta     temp            ; .. and save it
               bcc     asm0480         ; bit = 0, go shift further
               lda     prbytes,x       ; in unass.s
               cmp     #'$'            ; byte or word print ?
               bne     asm0430
               jsr     MatchHex
               bne     asm0250         ; no match
               lda     length          ; length was 2 (one hex byte)
               cmp     #2
               beq     asm0470         ; yes, match ok, go handle next format bit
               jsr     MatchHex
asm0420:       bne     asm0250
               beq     asm0470
asm0430:       cmp     string,y        ; #,(,x,y,), same as input ?
               bne     asm0250         ; no match, next tryop
               iny
asm0470:       lda     temp            ; shifted format byte back
asm0480:       inx
               cpx     #8
               bne     asm0400
               lda     string,y        ; format byte matched, but is there more input ?
               bne     asm0420         ; yes, wrong guess
;              
; input match ! found opcode. assemble at 'asmadr'
;
               lda     asmadr
               sta     wp0
               lda     asmadr+1
               sta     wp0+1
               ldy     #0
               lda     tryop
               sta     (wp0),y         ; store opcode
               iny
               lda     length
               cmp     #1              ; any operand bytes ?
               beq     asm0540         ; no, finished
               lda     hnuml
               sta     (wp0),y
               lda     length
               cmp     #3              ; 3 byte instr ?
               bne     asm0540         ; no, finished
               lda     hnumh
               iny
               sta     (wp0),y
asm0540:       lda     length          ; y is 0..2, add 1 for actual length
               clc
               adc     asmadr
               sta     asmadr
               lda     asmadr+1
               adc     #0
               sta     asmadr+1
;              
; prepare next line in keyboardbuf
;
;              jmp     mon0200
               lda     #$80
               sta     btokbb
               ldx     #0
               lda     #'A'
               jsr     StoChr
               jsr     StoSpc
               lda     asmadr+1
               jsr     PrHex2
               lda     asmadr
               jsr     PrHex2
               jsr     StoSpc
               inx
               stx     NDX
               lda     #$00
               sta     btokbb
               jmp     mon0200
asm5555:       jmp     mon0950         ; trampoline
;              
; the instruction is a branch. target is in hnuml,hnumh
;
asm0600:       ldy     hnumh
               ldx     hnuml           ; load target and decrement
               bne     asm0610
               dey
asm0610:       dex
               txa                     ; low byte target
               clc                     ; ! need -2
               sbc     asmadr          ; target-pc-2
               sta     wp1             ; this is the branch offset
               bpl     asm0630         ; branch positive ?
               iny                     ; no, bump hi byte
asm0630:       tya
               sbc     asmadr+1        ; hi byte branch must be 0
               bne     asm5555         ; branch too far
               lda     asmadr
               sta     wp0
               lda     asmadr+1
               sta     wp0+1
               ldy     #0
               lda     tryop
               sta     (wp0),y         ; store opcode
               iny
               lda     wp1
               sta     (wp0),y
               jmp     asm0540
               
;-------------------------------------------------------------------------------
; StoChr/StoSpc
; put char in a or a space in KEYBB
;-------------------------------------------------------------------------------
               
StoSpc:        lda     #$20  
StoChr:        sta     KEYBB,x  
               inx
               rts
               
               
               
               
