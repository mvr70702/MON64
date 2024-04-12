 ;-------------------------------------------------------------------------------
; trace.s
;
; SimEx
;
; Over
; In
; Trace (or To)
; Watch
; 
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; data
; jmpdat to be copied to xeq-area
;-------------------------------------------------------------------------------

jmpdat:        DB      $ea,$ea,$ea,$4c,<nobranch,>nobranch,$4c,<branch,>branch
siminstr:      DB      $00,$20,$4c,$6c,$60,$40,$48,$08,$68,$28,$ba,$9a,$ff
simdispl:      DB      <(xbrk-1),<(xjsr-1),<(xjmp-1),<(xjmpat-1),<(xrts-1),<(xrti-1)
               DB      <(xpha-1),<(xphp-1),<(xpla-1),<(xplp-1),<(xtsx-1),<(xtxs-1)
simdisph:      DB      >(xbrk-1),>(xjsr-1),>(xjmp-1),>(xjmpat-1),>(xrts-1),>(xrti-1)
               DB      >(xpha-1),>(xphp-1),>(xpla-1),>(xplp-1),>(xtsx-1),>(xtxs-1)

;-------------------------------------------------------------------------------
; SimEx
; simulated or controlled execution of instruction at (pcl,pch)
; This is a modified version of Wozniaks 'STEP' routine, from the Apple ][
; monitor ROM. Mine doesn't ever touch the Debuggee's stack.
; !! updates pcl,pch
;-------------------------------------------------------------------------------

SimEx:         lda     pcl             ; setup wp0 (also for InstrDec)
               sta     wp0
               lda     pch
               sta     wp0+1
               jsr     InstrDec        ; setup mnem, mode,length (exits early if invalid mnem)
               lda     mnem            ; check invalid instruction
               bne     sie0100
               rts                     ; if so, exit now
;              
; setup xeq area
;
sie0100:       ldx     #8
sie0110:       lda     jmpdat,x
               sta     xeq,x           ; initialise xeq area
               dex
               bpl     sie0110
;               
; check instruction changes program flow or stack (but not branches, they're executed under control) 
;
               ldx     #0
               ldy     #0
sie0130:       lda     siminstr,x      ; get instr
               cmp     #$ff            ; end of table ?
               bne     sie0135
               jmp     sie0200         ; yes, go deal with normal instructions
sie0135:       cmp     (wp0),y
               beq     sie0140
               inx
               bne     sie0130         ; branch always
sie0140:       lda     simdisph,x
               pha
               lda     simdispl,x
               pha
               ldx     sp              ; needed in almost all x-routines. y is still 0 
               rts                     ; dispatch
               
xbrk:          jsr     pushret         ; push address of instr + 2, like jsr
               lda     flags
               ldx     sp
               sta     $0100,x
               dec     sp
               clc
               
               
               jmp     mon0180         ; cannot simulate the IRQ, just jump to Monitor regdisp
xjsr:          jsr     pushret
xjmp:          ldy     #1
xjmp1:         lda     (wp0),y
               tax
               iny
               lda     (wp0),y
               sta     pch
               sta     wp0+1
               stx     pcl
               stx     wp0
               rts
xjmpat:        jsr     xjmp
               ldy     #0
               jmp     xjmp1
xrts:          lda     $0101,x         ; lo byte
               clc
               adc     #1
               sta     pcl
               lda     $0102,x
               adc     #0
               sta     pch
xrts05:        inx
               inx
               stx     sp
               rts
xrti:          lda     $0101,x
               sta     flags
               lda     $0102,x
               sta     pcl
               lda     $0103,x
               sta     pch
               inx
               jmp     xrts05
xpha:          lda     areg
xpha05:        sta     $0100,x
               dex
               stx     sp
xpha08:        inc     pcl             ; add 1 to pcl,pch
               bne     xpha10
xpha10:        rts
xphp:          lda     flags
               jmp     xpha05
xpla:          lda     $101,x
               sta     areg
xpla05:        inx
               stx     sp
               jmp     xpha08
xplp:          lda     $0101,x
               sta     flags
               jmp     xpla05
xtsx:          stx     xreg            ; x has sp
               jmp     xpha08
xtxs:          lda     xreg
               sta     sp
               jmp     xpha08
pushret:                               ; helper : push current address+2 (JSR,BRK)
               lda     wp0   
               clc
               adc     #2
               tay
               lda     wp0+1
               adc     #0
               sta     $0100,x
               dex
               tya
               sta     $0100,x
               dec     sp
               dec     sp
               rts                     ; running on monitor stack
;               
; a branch or normal instruction, copy the instruction to xeq-area
;
sie0200:       lda     (wp0),y         ; instruction again
               sta     xeq,y           ; copy to xeq-area
               iny
               cpy     length
               bne     sie0200
               ldy     #0
               lda     (wp0),y         ; original instruction back
               and     #$1f            ; branches xxx10000
               cmp     #$10
               bne     sie0240
               lda     #4
               sta     xeq+1           ; change branch display to 4, to point to 'jmp branch'
;              
; execute it
;
sie0240:       jsr     LoadUserRegs    ; get user regs & flags
               jmp     xeq             ; returns to 'branch' or 'nobranch'
;               
; not-taken branches & all other instructions come here after execution.
; save userregs & update pcl,h
;
nobranch:      jsr     StoreUserRegs
               cld
               cli
               lda     pcl
               clc
               adc     length
               sta     pcl
               bcc     sie0300
               inc     pch
sie0300:       rts        
;
; branches taken come here
;
branch:        cld                     ; can be removed (?), branches don't change flags
               cli
               jsr     CalcTarget      ; in unass.s
               lda     wp1
               sta     pcl
               lda     wp1+1
               sta     pch
               rts
               
;-------------------------------------------------------------------------------
; Trace Into (I)
; always simulated
;-------------------------------------------------------------------------------

Into:          jsr     SimEx
               jmp     mon0180
               
;-------------------------------------------------------------------------------
; Trace Over (O)
; simulated unless X-mode and pc is on a JSR instruction
;-------------------------------------------------------------------------------

Over:          lda     pcl
               sta     wp0
               lda     pch
               sta     wp0+1
               ldy     #0
               lda     (wp0),y
               cmp     #$20            ; on a JSR ?
               beq     +
               jsr     SimEx           ; no, just simulate (also in execute mode, much faster
               jmp     mon0180         ; than setting a breakpoint)
+:             lda     pcl             ; JSR, Do a 'To (pcl,pch+3)'
               clc
               adc     #3
               sta     hnuml
               lda     pch
               adc     #0
               sta     hnumh
               jmp     To0200
               
;-------------------------------------------------------------------------------
; To addr
;-------------------------------------------------------------------------------

To:            jsr     ReadHex
               bne     +
               jmp     mon0900
+:             lda     hnuml           ; check if pcl,pch == target address
               cmp     pcl             ; if so, simulate one instruction to avoid
               bne     To0200          ; hitting the breakpoint immediately 
               lda     hnumh
               cmp     pch
               bne     To0200
               jsr     SimEx
To0200:        ldx     #0              
               jsr     RestoreBP
               lda     hnuml
               ldy     hnumh
               jsr     SetBP
               lda     tmode           ; sim/exec ?
               beq     +
               jmp     go0150          ; exec: start run (brkpts.s)
+:             jmp     SimRun          ; sim : start run

;-------------------------------------------------------------------------------
; SimChkBP
;-------------------------------------------------------------------------------

SimChkBP:      lda     pcl
               cmp     bplocs,x
               bne     +
               lda     pch
               cmp     bplocs+1,x
+:             rts
               
;-------------------------------------------------------------------------------
; SimRun
; start simulating from pcl,pch until a breakpoint or a watch-change found
;-------------------------------------------------------------------------------

SimRun:        jsr     SimEx
               lda     STKEY           ; check STOP pressed
               cmp     #$7f
               bne     sir0020
               lda     #0
               sta     NDX             ; remove STOP from keybuf
               jmp     mon0200
sir0020:
;               lda     pch
;               jsr     PrHex2
;               lda     pcl
;               jsr     PrHex2Spc
;               ldy     #0
;               lda     (watchp),y
;               jsr     PrHex2Spc
;               lda     watchval
;               jsr     PrHex2CR
               
               ldy     #0
               lda     #1              ; 'negative' watch bit
               bit     watchflags      ; must watch ?
               bpl     sir0100         ; no
               bvc     sir0090         ; yes, change
               beq     sir0070         ; yes, check loc == watchval
               lda     (watchp),y      ; yes, check loc <> watchval
               cmp     watchval
               beq     sir0100
               bne     sir0080
sir0070:       lda     (watchp),y      ; check watchval set in watchloc
               cmp     watchval
               bne     sir0100
sir0080:       lda     #$44
               sta     reason
               jmp     MonBreak2
sir0090:       lda     (watchp),y      ; check change in watchloc
               cmp     watchold
               beq     sir0100
               bne     sir0080
sir0100:       ldx     #0
               jsr     SimChkBP
               bne     sir0200
               lda     #BP1ENTRY       ; BP2 : simulate break, print reason
sir0150:       sta     reason
               jsr     ClearBP
               jmp     MonBreak2       ; BP1 : go disp regs
sir0200:       ldx     #2
               jsr     SimChkBP
               bne     SimRun
               lda     #BP2ENTRY
               bne     sir0150
               
;-------------------------------------------------------------------------------
; Watch
; W?           query 
; W            clear
; W loc        watch change in loc
; W loc xx     watch loc contains xx
; W loc -xx    watch loc <> xx
;-------------------------------------------------------------------------------

Watch:         lda     tmode           ; watch only possible in simulation mode
               beq     wat0020
               jmp     mon0900
wat0020:       jsr     SkpSpace
               lda     inbuf,x
               cmp     #'?'            ; just query ?
               bne     wat0100
               lda     watchflags
               jsr     PrHex2Spc
               lda     watchp+1
               jsr     PrHex2
               lda     watchp
               jsr     PrHex2CR
               jmp     mon0200
wat0100:       jsr     ReadHex         ; address ?
               beq     wat0300         ; no, clear watch
               lda     hnuml
               sta     watchp
               lda     hnumh
               sta     watchp+1
               lda     #$80            ; flag : watch set
               sta     watchflags
               ldy     #0
               lda     (watchp),y      ; get current value in weatchloc
               sta     watchold        ; save 
               jsr     SkpSpace
               lda     inbuf,x         ; get 1st char after watch address
               cmp     #'-'
               bne     wat0180
               lda     #1
               ora     watchflags      ; flag : check watchval <> loc
wat0180:       jsr     ReadHex         ; 2nd byte given ? (value to watch for)
               beq     wat0200
               lda     hnuml
               sta     watchval
               lda     #$c0
               sta     watchflags      ; flag : watch & watchvalue set
wat0200:       jmp     mon0200
;
; just 'W' entered, clear watch
;
wat0300:       lda     #$00
               sta     watchflags
               
               beq     wat0200         ; always
               
               
