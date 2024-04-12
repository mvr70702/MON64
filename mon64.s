;-------------------------------------------------------------------------------
; MON64
;
; ML Monitor/debugger for Commodore 64
; MVR 23-01-2024
;
; Simulated execution (SimEx) principle from APPLE ][ rom monitor by WOZNIAK/BAUM,
; but with several modifications
; DIR command by DMANTIONE (often found at lemon64.com)
;
; All other functionality, including disassembling 6502, is my own.
;
; MON64 uses a minimal amount of RAM normally used for assembler programs.
; 4 bytes of Zeropage, other variables are in BASIC input buffer, $02a7, 033c
;
; 'monloc' provides the baseaddress, currently either $c000, $9000 or
; $8000 (8k cartridge for VICE)
; if monloc==$9000, the top of BASIC is set to $8fff
;
; The assmbler used is ASM64, available at https://github.com/mvr70702/asm64
;
; No licensing, use, modify and distribute however you like. No guarantees
; whatsoever.
;
; In this file :
; init
; get user input
; dispatch commands
;
; Quit
; SetXmode
;-------------------------------------------------------------------------------

               INCLUDE "kernal.inc"
               INCLUDE "ram.s"

monloc         =       $c000
MONSP          =       $0128           ; monitor stack pointer

;-------------------------------------------------------------------------------
; Progstart
;-------------------------------------------------------------------------------

               IF      monloc == $c000
               DW      $c000           ; file header
               ORG     $c000
               ENDIF
               
               IF      monloc == $9000
               DW      $9000           ; file header           ; !! top of basic !!
               ORG     $9000
               ENDIF
               
               IF      monloc == $8000
               ORG     $8000           ; cartridge stuff
               DW      Monitor         ; progstart
               DW      0               ; ?? NMI apparently
               DB      $c3,$c2,$cd,'80'
               ENDIF
               
               jmp     Monitor
               
;-------------------------------------------------------------------------------
; Data
;-------------------------------------------------------------------------------
               
welcome        DB      147,$0d,$0d,"    **** MON64 BY MVR  REV. 1.0 ****",$0d,$0d,$00
errstring      DB      "   ERR",$00

cmdtable:      DB      '@'
               DB      'A'
               DB      'B'
               DB      'C'
               DB      'D'
               DB      'E'
               DB      'F'
               DB      'G'
               DB      'H'
               DB      'I'
               DB      'J'
               DB      'L'
               DB      'O'
               DB      'P'
               DB      'Q'
               DB      'R'
               DB      'S'
               DB      'T'
               DB      'U'
               DB      'W'
               DB      'X'
               DB      0

jtablo:        DB      <(Dir-1)        ; @
               DB      <(Assemble-1)   ; A
               DB      <(BrkPoint-1)   ; B
               DB      <(Continue-1)   ; C
               DB      <(Dump-1)       ; D
               DB      <(Edit-1)       ; E
               DB      <(Fill-1)       ; F
               DB      <(Go-1)         ; G
               DB      <(Hunt-1)       ; H
               DB      <(Into-1)       ; I
               DB      <(Call-1)       ; J
               DB      <(LoadF-1)      ; L
               DB      <(Over-1)       ; O
               DB      <(SetP-1)       ; P
               DB      <(Quit-1)       ; Q
               DB      <(DispRegs-1)   ; R
               DB      <(SaveF-1)      ; S
               DB      <(To-1)         ; T
               DB      <(UnAssemble-1) ; U
               DB      <(Watch-1)      ; W
               DB      <(SetXmode-1)   ; X
               
jtabhi:        DB      >(Dir-1)        ; @
               DB      >(Assemble-1)   ; A
               DB      >(BrkPoint-1)   ; B
               DB      >(Continue-1)   ; C
               DB      >(Dump-1)       ; D
               DB      >(Edit-1)       ; E
               DB      >(Fill-1)       ; F
               DB      >(Go-1)         ; G
               DB      >(Hunt-1)       ; H
               DB      >(Into-1)       ; I
               DB      >(Call-1)       ; J
               DB      >(LoadF-1)      ; L
               DB      >(Over-1)       ; O
               DB      >(SetP-1)       ; P
               DB      >(Quit-1)       ; Q
               DB      >(DispRegs-1)   ; R
               DB      >(SaveF-1)      ; S
               DB      >(To-1)         ; T
               DB      >(UnAssemble-1) ; U
               DB      >(Watch-1)      ; W
               DB      >(SetXmode-1)   ; X
               
;-------------------------------------------------------------------------------
; Monitor      main entry
;-------------------------------------------------------------------------------

Monitor:       
               jsr     StoreUserRegs   ; callers regs to mem
               tsx
               stx     sp              ; user stackptr
               stx     spentry         ; save for quit
               lda     $0101,x         ; user return address (if any)
               sta     pcl
               lda     $0102,x
               sta     pch
               inc     pcl             ; bump it (assumes monitor called by 'jsr'
               bne     mon0050
               inc     pch
;              
; init everything
;
mon0050        lda     #$80
               sta     RPTFLAG         ; repeat all keys
               
               ldx     #<MONSP         ; !! switch to MON stack
               txs
               jsr     HookBrk         ; Hook BRK vector
               ldx     #0
               jsr     ClearBP         ; Init breakpoints (zero)
               ldx     #2
               jsr     ClearBP         ; Init breakpoints (zero)
               
               sta     dumpadr         ; dumpaddress default $0000
               sta     dumpadr+1
               ldx     #1
               stx     tmode           ; trace mode is execute
               dex     
               stx     watchflags
;              
; welcome message, disp regs, main inputloop
;
               lda     #<welcome
               ldy     #>welcome
               jsr     StrOut
mon0180:       jmp     DispRegs2       ; will return to mon0200
mon0200:       lda     #'-'            ; prompt
               jsr     CHROUT
               lda     #0
               sta     btokbb          ; make sure CHROUT redirected to keyboard buffer is OFF
mon0210:       jsr     GetLineZ
mon0220:       jsr     CROut
               ldx     #0              ; x = inbuf index throughout all commands
               jsr     SkpSpace
               
               ldy     #0
mon0300:       lda     cmdtable,y
               beq     mon0900         ; cmd not found
               cmp     inbuf,x
               beq     mon0360
               iny
               bne     mon0300         ; branch always
mon0360:       inx                     ; set x past command
               lda     jtabhi,y
               pha
               lda     jtablo,y
               pha
               rts                     ; Dispatch. All commands must exit with 'jmp mon0200' or 'jmp mon0900'
                                       ; there's no return address on the stack
;                                      
; error in input, print an up arrow under the line, at pos (x), last good input read
;
mon0900:       txa
               pha
               sec
               jsr     PLOT            ; !! plot returns x-pos in y reg and vv
               clc
               pla
               tay
               iny                     ; point to just behind last char read
;              inx                     ; decr. ypos, 2 CR's were printed !!
               jsr     PLOT
               lda     #94             ; Up Arrow.
mon0950:       jsr     CHROUT          ; entry for assembler
               lda     #<errstring
               ldy     #>errstring
               jsr     StrOut
               jsr     CROut
               jmp     mon0200
               
;-------------------------------------------------------------------------------
; SetXmode
; toggle tracemode (SIM=0, EXECUTE=1)
;-------------------------------------------------------------------------------

SetXmode:      lda     tmode
               eor     #1
               sta     tmode
               jmp     DispRegs
               
;------------------------------------------------------------------------------
; Quit
;-------------------------------------------------------------------------------

Quit:          
               php                     ; save IF
               sei                     ; no interrupts while changing BRK vec
               lda     oldbrk
               sta     CBINV
               lda     oldbrk+1
               sta     CBINV+1
               plp                     ; restore IF
Continue:      ldx     #0              ; make sure no breakpoints left
               jsr     RestoreBP
               ldx     #2
               jsr     RestoreBP
               jmp     $fe66           ; orig. BRK vector, warm start

;-------------------------------------------------------------------------------
; Include all other modules
;-------------------------------------------------------------------------------

               LISTOFF
               INCLUDE                 "mnems.inc"
               LISTON                  
               INCLUDE                 "mnemtable.s"
               INCLUDE                 "input.s"
               INCLUDE                 "output.s"
               INCLUDE                 "mem.s"
               INCLUDE                 "regs.s"
               INCLUDE                 "unass.s"
               INCLUDE                 "brkpts.s"
               INCLUDE                 "trace.s"
               INCLUDE                 "fileio.s"
               INCLUDE                 "assemble.s"
                
               IF      monloc == $8000
               PAD     $a000
               ENDIF


               
               
               
               
