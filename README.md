# MON64
powerfull Commodore 64 debugger/ml monitor
MON64 is a native C64 debugger with commands like trace into/over subroutines, breakpoints and a watch-feature.
Using the 'simulated' execution mode (X command), it can even trace/single step/set breakpoints in ROM

MON64 command overview
=================================================================================================================

    A xxxx mnem oper.       Assemble
                            To leave assembly mode, just leave the line empty <Enter>
                            All hexnumbers must be either 2 or 4 digits. leading '$' allowed
                            
    B (xxxx) (?)            Breakpoint                        
                            B xxxx sets a breakpoint, just B clears it. Only 1 BP allowed
                            B ? shows breakpoints (1st one is used by MON64 itself)
                            
    C                       Continue. Like 'Q', but on quitting through 'C' MON64 doesn't  
                            restore the original BRK vector. So if any program hits a BRK-
                            instruction, MON64 pops in again.
                            
    D (xxxx)                Dump memory
                            No addresses : start at current dumpptr
                            
    E xxxx bytes            Edit memory, address required.
                            'bytes' is any combination of hexbytes and quoted strings, separated by spaces,
                            e.g E 2000 "Hello" 0d 00
    
    F xxxx yyyy bytes       Fill memory, range required
                            'bytes' : see E command
                            
    G (xxxx)                go execute at current PC or (xxxx)
    
    H xxxx yyyy bytes       hunt (search) for vals. display addesses
                            'bytes' : see E command
    
    I                       trace into (F1)   } current PC
    
    J (xxxx)                call sub at PC or xxxx, provides returnaddress into monitor
    
    L "name" dev (xxxx)     load file. if (xxxx), use it as loadaddress, else use fileheader.
                            displays IO status and highest loadaddress (+1) if finished
                            
    O                       trace over (F8)   } current PC
    
    P xxxx                  short for R PC xxxx
    
    Q                       unhook BRK vector, quit, cold start BASIC
    
    R                       regdisp. for indirect- or indexed modes the effective address is shown after the instruction.
    R AXYPSF val            reg change val (R A 44 sets A to 44    R P 5678 sets PC to 5678)
    
    S "name" dev xxxx yyyy  save file, from xxxx upto and including yyyy
    
    T xxxx                  trace until xxxx
    
    U (xxxx)                Unassemble (disassemble) 6502 code starting at the
                            current PC or at optional address xxxx
    
    X                       toggle between simulated/executed trace
    
    @ devnr                 Dir for device xx
    
    W (?) (xxxx) (-)cc      set watch for To command (in simulated mode)
                            W          clears watch
                            W ?        query watch
                            W xxxx     set watch on loc xxxx, signals any change
                            W xxxx cc  set watch on loc xxxx, signals contents == cc
                            W xxxx -cc set watch on loc xxxx, signals contents != cc
