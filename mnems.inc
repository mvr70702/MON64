;
; mnemonic values are shifted left 2, high 6 bits in lookup table
;

_ADC           =       4
_AND           =       8
_ASL           =      12
_BCC           =      16
_BCS           =      20
_BEQ           =      24
_BIT           =      28
_BMI           =      32
_BNE           =      36
_BPL           =      40
_BRK           =      44
_BVC           =      48
_BVS           =      52
_CLC           =      56
_CLD           =      60
_CLI           =      64
_CLV           =      68
_CMP           =      72
_CPX           =      76
_CPY           =      80
_DEC           =      84
_DEX           =      88
_DEY           =      92
_EOR           =      96
_INC           =     100
_INX           =     104
_INY           =     108
_JMP           =     112
_JSR           =     116
_LDA           =     120
_LDX           =     124
_LDY           =     128
_LSR           =     132
_NOP           =     136
_ORA           =     140
_PHA           =     144
_PHP           =     148
_PLA           =     152
_PLP           =     156
_ROL           =     160
_ROR           =     164
_RTI           =     168
_RTS           =     172
_SBC           =     176
_SEC           =     180
_SED           =     184
_SEI           =     188
_STA           =     192
_STX           =     196
_STY           =     200
_TAX           =     204
_TAY           =     208
_TSX           =     212
_TXA           =     216
_TXS           =     220
_TYA           =     224
               
;              
; address modes : bit 7 -> 3 byte instr , bit 6 -> 2 byte instr  , bit 0..3 are index
;
_IMM           EQU     %01000000
_ZP            EQU     %01000001
_ABS           EQU     %10000010
_IMP           EQU     %00000011
_ACC           EQU     %00000100
_IZPX          EQU     %01000101
_IZPY          EQU     %01000110
_ZPX           EQU     %01000111
_ABSX          EQU     %10001000
_ABSY          EQU     %10001001
_IABS          EQU     %10001010
_ZPY           EQU     %01001011
_REL           EQU     %01001100
               
