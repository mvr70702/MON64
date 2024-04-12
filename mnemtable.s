;----------------------------------------------------------------------------------------------
; mnemtable.asm
; all mnemonics in compacted form, 5 bits per character, packed in two bytes
;----------------------------------------------------------------------------------------------

               MACRO   crunch c1,c2,c3
               DB       (c1-63) | (((c2-63) & 7) << 5)
               DB       ((c2-63)>>3) | ((c3-63) << 2)
               ENDM



mnems:         crunch '?','?','?',
               crunch 'A','D','C',
               crunch 'A','N','D',
               crunch 'A','S','L',
               crunch 'B','C','C',
               crunch 'B','C','S',
               crunch 'B','E','Q',
               crunch 'B','I','T',
               crunch 'B','M','I',
               crunch 'B','N','E',
               crunch 'B','P','L',
               crunch 'B','R','K',
               crunch 'B','V','C',
               crunch 'B','V','S',
               crunch 'C','L','C',
               crunch 'C','L','D',
               crunch 'C','L','I',
               crunch 'C','L','V',
               crunch 'C','M','P',
               crunch 'C','P','X',
               crunch 'C','P','Y',
               crunch 'D','E','C',
               crunch 'D','E','X',
               crunch 'D','E','Y',
               crunch 'E','O','R',
               crunch 'I','N','C',
               crunch 'I','N','X',
               crunch 'I','N','Y',
               crunch 'J','M','P',
               crunch 'J','S','R',
               crunch 'L','D','A',
               crunch 'L','D','X',
               crunch 'L','D','Y',
               crunch 'L','S','R',
               crunch 'N','O','P',
               crunch 'O','R','A',
               crunch 'P','H','A',
               crunch 'P','H','P',
               crunch 'P','L','A',
               crunch 'P','L','P',
               crunch 'R','O','L',
               crunch 'R','O','R',
               crunch 'R','T','I',
               crunch 'R','T','S',
               crunch 'S','B','C',
               crunch 'S','E','C',
               crunch 'S','E','D',
               crunch 'S','E','I',
               crunch 'S','T','A',
               crunch 'S','T','X',
               crunch 'S','T','Y',
               crunch 'T','A','X',
               crunch 'T','A','Y',
               crunch 'T','S','X',
               crunch 'T','X','A',
               crunch 'T','X','S',
               crunch 'T','Y','A',
mnemend        =       $-mnems
               
