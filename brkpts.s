;-------------------------------------------------------------------------------
; brkpts.s
;-------------------------------------------------------------------------------
;
; Breakpoint0  : 'wild', not set by user/mon64
; Breakpoint1  : used by trace
; Breakpoint2  : set/cleared by user
;
; HookBrk
; Break        routine
; ClearBP
; SetBP
; TestBP
; HandleBP
; RestoreBP
; StoreUserRegs
; LoadUserRegs
; BrkPoint     command
; Go           command
; Call         command
;-------------------------------------------------------------------------------

BP0ENTRY       =       0
BP1ENTRY       =       1
BP2ENTRY       =       2

brkstring      DB      "!! BRK : ",0

;-------------------------------------------------------------------------------
; HookBrk
; check already done
;-------------------------------------------------------------------------------

HookBrk:       lda     CBINV           ; already hooked ?
               cmp     #<Break
               bne     hb0100
               lda     CBINV+1
               cmp     #>Break
               beq     hb0900          ; yes, quit
hb0100:        lda     CBINV  
               sta     oldbrk
               lda     CBINV+1
               sta     oldbrk+1
               lda     #<Break
               sta     CBINV
               lda     #>Break
               sta     CBINV+1
hb0900:        rts       

;----------------------------------------------------------------------------------------------
; Break. stack : y x a flags pcl pch
; check user breakpoint, wild breakpoint. set reason
;----------------------------------------------------------------------------------------------

Break:         pla                     ; copy regs to userregs
               sta     yreg
               pla
               sta     xreg
               pla
               sta     areg
               pla
               sta     flags
               pla
               sta     pcl
               pla
               sta     pch
               
               lda     #BP0ENTRY       ; assume wild breakpoint
               sta     reason
                                       ; check user bp
               ldx     #0              ; bp 1
               jsr     TestBP          ; is it bp1 ?
               bne     break05
               jsr     HandleBP        ; x is still 0
               bcc     break25         ; always
               
break05:       ldx     #2              ; same bp2
               jsr     TestBP
               bne     break25
               jsr     HandleBP
               
break25:       lda     #>MonBreak      ; finish the interrupt, return to monbreak
               pha 
               lda     #<MonBreak
               pha
               lda     flags
               pha
               rti
               
;-------------------------------------------------------------------------------
; MonBreak     come here on finish break interrupt
; user regs 
;-------------------------------------------------------------------------------

MonBreak:      nop
               ldx     #<MONSP         ; !!! switch stack !!!
               txs
               cld                     ; make sure DF off
               cli                     ; and interrupts enabled
MonBreak2:     lda     reason          ; if MON64 trace breakpoint,
               cmp     #1              ; don't display !BRK
               beq     mb0030
               lda     #<brkstring     ; entry for simulated breakpoint
               ldy     #>brkstring
               jsr     StrOut
               lda     reason
               jsr     PrHex2
               jsr     CROut2
mb0030:        jmp     mon0180         ; go disp regs, to main

;-------------------------------------------------------------------------------
; ClearBP      x=0/2
;-------------------------------------------------------------------------------

ClearBP:       lda     #0
               sta     bplocs,x
               sta     bplocs+1,x
               rts
               
;----------------------------------------------------------------------------------------------               
; SetBP (a=adrL,y=adrH,x=0 or 2). save byte replaced
;----------------------------------------------------------------------------------------------               

SetBP:         sta     bplocs,x        ; store BP address, also to wp0
               sta     wp0
               tya
               sta     bplocs+1,x
               sta     wp0+1
               lda     tmode
               beq     +               ; don't change debuggee if simulate !
               ldy     #0
               lda     (wp0),y         ; get byte under BP
               sta     bpsav,x         ; save it (only bpsav,bpsav+2 used)
               lda     #0
               sta     (wp0),y
+:             rts
               
;----------------------------------------------------------------------------------------------               
; TestBP       (x = 0 or 2, indexing bp)
; see if pcl/h-2 equal bpxloc, return EQ if so
;----------------------------------------------------------------------------------------------               

TestBP:        lda     bplocs,x
               clc
               adc     #2
               tay
               lda     bplocs+1,x
               adc     #0
               cmp     pch
               bne     tbp99
               cpy     pcl
tbp99:         rts

;----------------------------------------------------------------------------------------------               
; HandleBP(x)  x = 0 or 2
; updata pcl/h. restore bp. set 'reason' to ubp1/2
; return CY=0 for bcc
;----------------------------------------------------------------------------------------------               

HandleBP:      lda     bplocs,x        ; update pcl to point to breakpoint, not 2 locs behind it
               sta     pcl
               sta     wp0             ; also to wp0, needed later
               lda     bplocs+1,x
               sta     pch
               sta     wp0+1
               lda     #0              ; clear the breakpoint address
               sta     bplocs,x
               sta     bplocs+1,x
               lda     bpsav,x
               ldy     #0
               sta     (wp0),y         ; restore byte under breakpoint
               ldy     #BP1ENTRY       ; set BP1 or BP2
               cpx     #0
               beq     hbp10
               iny
hbp10:         sty     reason
               clc
               rts
               
;----------------------------------------------------------------------------------------------               
; RestoreBP(x=0/2)
; if a breakpoint was already set, restore it
;---------------------------------------------------------------------------2------------------               

RestoreBP:     lda     bplocs,x
               bne     rbp020
               lda     bplocs+1,x
               beq     rbp090          ; not set, no action
rbp020:        lda     bplocs,x
               sta     wp0
               lda     bplocs+1,x
               sta     wp0+1
               lda     tmode
               beq     rbp090          ; don't change debuggee if simulating
               lda     bpsav,x
               ldy     #0
               sta     (wp0),y  
rbp090:        rts

;-------------------------------------------------------------------------------
; BrkPoint
; sets/clears BP2. Displays BP1 & BP2
;-------------------------------------------------------------------------------

BrkPoint:      jsr     SkpSpace
               lda     inbuf,x
               cmp     #'?'            ; print breakpoint locs ?
               bne     brp040
               lda     bplocs+1
               jsr     PrHex2
               lda     bplocs
               jsr     PrHex2
               jsr     PrSpace2
               lda     bplocs+3
               jsr     PrHex2
               lda     bplocs+2
               jsr     PrHex2CR
               jmp     mon0200
brp040:        jsr     ReadHex
               beq     brp070          ; no address, go clear existing bp
               ldx     #2              ; user BP is BP2, index 2
               jsr     RestoreBP       ; restore bp if already set
               lda     hnuml
               ldy     hnumh
               ldx     #2
               jsr     SetBP
               jmp     mon0200
brp070:        ldx     #2              ; just 'B' entered, restore & clear (both)2
               jsr     RestoreBP
               jsr     ClearBP
               ldx     #0
               jsr     RestoreBP
               jsr     ClearBP
               jmp     mon0200
               
;-------------------------------------------------------------------------------
; StoreUserRegs
; 6502 regs to userregs in mem
;-------------------------------------------------------------------------------

StoreUserRegs: sta     areg
               stx     xreg
               sty     yreg
               php
               pla
               sta     flags
               rts
               
;-------------------------------------------------------------------------------
; LoadUserRegs
; userregs in mem to 6510
;-------------------------------------------------------------------------------

LoadUserRegs:  ldx     xreg
               ldy     yreg
               lda     flags
               pha
               lda     areg
               plp
               rts
               
;-------------------------------------------------------------------------------
; Go
; Call
;-------------------------------------------------------------------------------

Call:          lda     #1              ; temp = 1 : Call..
               bne     go0020
Go:            lda     #0              ; .. else Go
go0020:        sta     temp
               jsr     ReadHex
               beq     gnoaddr         ; no num, use current pcl,pch
               lda     hnuml
               sta     pcl
               lda     hnumh
               sta     pch
gnoaddr:       lda     tmode           ; sim/execute ?
               beq     go0200          ; go simulate
;               
; Execute. setup jumpaddress, returm addres for Call, load Userregs and go
;
               ldx     sp              ; user stack to 6510
               txs
               lda     temp            ; Call ?
               beq     go0150
               lda     #>(CallRet1-1)  ; yes, provide return address for sub to be called
               pha
               lda     #<(CallRet1-1)  ; (on user stack ofcourse)
               pha
go0150:        jsr     LoadUserRegs    ; user regs to 6510
               jmp     (pcl)           ; go exec
CallRet1:      jsr     StoreUserRegs   ; 6510 regs to user regs
               tsx                     ; 6510 SP to user sp
               stx     sp
               cld                     ; make sure DF is off
               cli                     ; and interrupts on
               ldx     #<MONSP         ; switch to MON stack
               txs 
               jmp     mon0180         ; disp regs, main
;               
; Simulate. pcl,h are setup, go simulate execution & checks 
;
go0200:        jmp     SimRun          ; trace.s
