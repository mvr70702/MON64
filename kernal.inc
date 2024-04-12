;------------------------------------------------------------------------------
; kernal.inc
;------------------------------------------------------------------------------

STATUS         =       $0090
STKEY          =       $0091           ; $7f = STOP pressed
MSGFLG         =       $009d           ; bit 7 = kernal errors, bit 6 = kernal control messages 
NDX            =       $00c6           ; nchars in keyboard buffer
BLNSW          =       $00cc           ; cursor on/off
GDBLN          =       $00ce           ; character under cursor
KEYBB          =       $0277           ; keyboard buffer
RPTFLAG        =       $028a
CBINV          =       $0316           ; break vector
OPEN           =       $ffc0
CLOSE          =       $ffc3
CHKIN          =       $ffc6
CHKOUT         =       $ffc9
CLRCHN         =       $ffcc
CHRIN          =       $ffcf
CHROUT         =       $ffd2
GETIN          =       $ffe4
SETNAM         =       $ffbd
SETLFS         =       $ffba
LOAD           =       $ffd5
SAVE           =       $ffd8
READST         =       $ffb7
PLOT           =       $fff0
TALK           =       $ffb4
TKSA           =       $ff96
LISTN          =       $ffb1
SECND          =       $ff93
UNLSN          =       $ffae
UNTLK          =       $ffab
ACPTR          =       $ffa5
CIOUT          =       $ffa8
