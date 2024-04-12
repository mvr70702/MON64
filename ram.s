;-------------------------------------------------------------------------------
; ram.s
;-------------------------------------------------------------------------------
; uses $0200, BASIC input buffer, $10b..$11c, $02a7..$02cd
; zeropage : use 6 cassette-only locs
;-------------------------------------------------------------------------------

;
; Zpage allocations, 3 pointers
;
wp0            =       $009b           ; tape : PRTY/DPSW
wp1            =       $009e           ; tape : PTR1/PTR2
asmadr         =       $00a5           ; tape : 2 locs
watchp         =       $00fb           ; user
hnuml          =       $00fd
hnumh          =       $00fe
               
               DATA    $010b           ; TOS+$10, leaves space for BASIC printbuffer
string         DSB     17              ; input.s, used in mem.s, fileio.s,..
xeq            =       string          ; trace.s,brkpts.s : simulation execute area
               DEND

;-------------------------------------------------------------------------------
; Data, use BASIC INBUF
;-------------------------------------------------------------------------------

               DATA    $0200           ; use BASIC input buffer
areg           DSB     1               ; user regs
xreg           DSB     1
yreg           DSB     1
sp             DSB     1
flags          DSB     1
pcl            DSB     1
pch            DSB     1
spentry        DSB     1               ; stackptr on entry Monitor
btokbb         DSW     1               ; if $80, output to keyboard buffer
inbuf          DSB     38
inbufmax       DSB     2
tmpx           DSB     1               ; input.s
temp4          DSB     1               ; input.s/unass.s
temp           DSB     1               ; unass.s, assemble.s
mneml          DSB     1               ; unass.s
mnemh          DSB     1               ; unass.s
strlen         DSB     1               ; input.s
dumpadr        DSW     1               ; curent startaddress for Dump
unassadr       DSW     1               ; current startaddress for UnAss
dcount         DSB     1               ; dump/unass line counter
tryop          =       dcount          ; assemble.s
tmode          DSB     1               ; trace mode (0=simulate,<>0=execute)
length         DSB     1               ; unass : instr. length (1..3)
amode          DSB     1               ; unass : address mode
mnem           DSB     1               ; unass : mnemonic (0=invalid)
nbytes         DSB     1               ; mem.s : ReadBytes
xsav           DSB     1               ; mem.s : ReadBytes
nrange         DSW     1               ; mem.s : nr of bytes in range (wp0..wp1)
mmode          DSB     1               ; mem.s : fill/hunt flag
numread        =       nrange          ; assemble.s
drv            DSB     1               ; fileio.s
oldbrk         DSW     1               ; brkpts.s : old BRK vector
bplocs         DSW     2               ; brkpts.s : adresses of brkpoints 1 & 2
bpsav          DSB     4               ; brkpts.s : bytes under breakpoints. 2 bytes wasted, but allows for x=0/2 indexing
reason         DSB     1               ; brkpts.s : reason for BRK entry (0,1,2)
watchflags     DSB     1               ; trace.s : $80 if a watch is set, $c0 if a watchval is set, $01 if 'negative' watch
watchval       DSB     1               ; trace.s : val to check 
watchold       DSB     1               ; trace.s : old val watchloc if change is watched
               CHECKPC $0258
               DEND
               
;-------------------------------------------------------------------------------
; Data, end of tape buffer
;-------------------------------------------------------------------------------

               DATA    $02a7
bytebuf        DSB     38
               DEND
               
