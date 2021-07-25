; ********************************************************
; *                                                      *
; * Z80 Home Build Development                           *
; * Steve Bradford                                       *
; * 01/05/2021                                           *
; *                                                      *
; * format a disk image                                  *
; *                                                      *
; * 8pformat.asm                                         *
; *                                                      *
; ********************************************************
;
; Write $e5 to each directory track/sector
; Write $00 to each data track/sector.
; Uses BIOS calls
;
BDOS        equ $0005
CNTRLC      equ $03
;
; define BDOS functions
;
C_READ      equ $01         ; console input
C_WRITE     equ $02         ; console output
C_WRITESTR  equ $09         ; output string, '$' terminated, to console output
C_READSTR   equ $0a         ; buffered console input
DRV_SET     equ $0e         ; select drive
F_OPEN      equ $0f         ; open file
F_CLOSE     equ $10         ; close file
F_WRITE     equ $15         ; write record
F_MAKE      equ $16         ; create file
DRV_GET     equ $19         ; get current drive
F_DMAOFF    equ $1a         ; set DMA address
DRV_DPB     equ $1f         ; get DPB address
DRV_RESET   equ $25         ; selectively reset a disk drive
;
;
            org	0100h

    ld sp, stack            ; set programme stack space

    ld hl, str_signon
    call PRINT_STR

    ; ask which drive to format
.ask
    ld hl, str_which_drive
    call PRINT_STR
.getloop
    call GETC               ; expect A thru D
    cp $0d                  
    jp z, .ask              ; got a CR so reprint line
    cp CNTRLC
    jp z, done
    cp 'A'
    jp c, .do_bs            ; < 'A' so try again
    cp 'E'
    jp c, .got              ; < 'Q' so got a drive letter
    cp 'a'
    jp c, .do_bs
    cp 'e'
    jp nc, .do_bs

    jp .got

.do_bs
    ld a, $08
    call PRINT_CHAR         ; print backspace
    jp .getloop

.got:
    sub 'A'                 ; if A = 0 thru 15 then valid drive
    cp 4 ;16                   ; allowing for P:
    jp c, .entered_drive    ; must be uppercase

    sub $20

.entered_drive:
    ld (wanted_drive), a    ; save selected drive number 0..3
    call PRINT_NEWLINE

    ;
    ; save BIOS entry points
    ;
    ld a, (2)               ; get the BIOS start high byte so we can calculate the BIOS jump vectors
    ld (settrk+2), a
    ld (setsec+2), a
    ld (setdma+2), a
    ld (write+2), a

    ld c, DRV_GET
    call BDOS
    ld (drive), a	        ; save current drive
    
    ; can't be the same as we are logged on to
    ld a, (wanted_drive)    ; get selected drive
    ld hl, drive
    cp (hl)
    jp nz, .cont

    ld hl, str_select_error
    call PRINT_STR
    jp done

.cont
    ld a, (wanted_drive)
    ld e, a
    ld c, DRV_SET           
    call BDOS               ; select wanted disk drive
    cp 0
    jp nz, select_fail      ; call failed

    ld c, DRV_DPB
    call BDOS               ; get DPB for wanted disk drive
    ld (DPBaddress), hl     ; returns HL = DPB address
    jp .cont2

select_fail:
    ld hl, str_select_fail
    call PRINT_STR
    jp done

.cont2:
    ;
    ; Disk Parameter Block (DPB)
    ; W spt Sectors Per Track
    ; B bsh Block Shift
    ; B blm Block Mask
    ; B exm Extent Mask
    ; W dsm Number of blocks - 1
    ; W drm Number of directory entries - 1
    ; B al0
    ; B al1
    ; W cks Checksum
    ; W off Offset
    ;
    call PRINT_NEWLINE
    ld hl, str_format
    call PRINT_STR
    ld a, (wanted_drive)
    add a, 'A'
    call PRINT_CHAR
    ld a, ':'
    call PRINT_CHAR
    call PRINT_NEWLINE
    call PRINT_NEWLINE
    ld hl, str_header
    call PRINT_STR

    ;
    ; print spt, tracks, disk size
    ;
    ld hl, (DPBaddress)     ; spt
    ld (spt), hl

    ld a, (hl)
    call PRINT_HEX
    inc hl
    ld a, (hl)
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)              ; bsh
    ld (bsh), a
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)              ; blm
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)              ; exm
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)              ; dsm-hi
    ld (dsm), a
    call PRINT_HEX
    inc hl
    ld a, (hl)              ; dsm-lo
    ld (dsm+1), a
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)
    call PRINT_HEX
    inc hl
    ld a, (hl)
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)
    call PRINT_HEX
    inc hl
    ld a, (hl)
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR

    inc hl
    ld a, (hl)
    call PRINT_HEX
    inc hl
    ld a, (hl)
    call PRINT_HEX
    ld a, ' '
    call PRINT_CHAR

    call PRINT_NEWLINE
    call PRINT_NEWLINE


    ; calculate disk drives track count
    ; could do this the hard way, but lets hard code it
    ; drives A: & B: have 77 tracks
    ; drives C: & D: have 1024 tracks
    ld a, (wanted_drive)
    cp 2                    ; is it drive 0 or 1?
    jr c, drivesAB

    ld bc, 1024             ; total tracks for 8MB drive
    jp trackcount
drivesAB:
    ld bc, 77               ; total tracks for floppy
trackcount:
    ld (totaltracks), bc

    ld hl, str_total_tracks
    call PRINT_STR
    ld a, (totaltracks+1)
    call PRINT_HEX
    ld a, (totaltracks)
    call PRINT_HEX
    ld a, 'h'
    call PRINT_CHAR
    call PRINT_NEWLINE

    ld hl, str_disk_size
    call PRINT_STR
    
    ; calculate disk size
    ; (dsm + 1) * (128 << bsh)
    ld a, (bsh)             ; bsh 
    ld de, (dsm)            ; dsm
    inc de                  ; dsm + 1
    
    ; as we only need the disk size to be in KB, rather than multiply then later divide,
    ; just multiply by 1 for a bsh of three, and multiply by 2 for a bsh of 4              
    sub 3
    ld b, 0
    inc a
    ld c, a
    call DE_Times_BC        ; returns 32bit answer in DEHL
    call B2D24              ; print decimal number
    call PRINT_STR
    ld a, ' '
    call PRINT_CHAR
    ld a, 'K'
    call PRINT_CHAR
    call PRINT_NEWLINE
    call PRINT_NEWLINE

    ld hl, str_question     ; ask question
    call PRINT_STR
    call GETC
    cp 'y'
    jp z, format_go
    cp 'Y'
    jp z, format_go
    jp done

format_go:
    ld hl, str_clear_line   ; overwrite this message
    call PRINT_STR

    ;
    ; start the format
    ;
    ld bc, buffer
    call setdma             ; call BIOS setdma
    ld bc, 0                ; tracks start at 0
    ld (track), bc
track_loop:	
    ld bc, 0	            ; sectors start at 0
    ld (sector), bc

    ;
    ; print track
    ;
    ld hl, str_formatting
    call PRINT_STR
   
    ld a, (track+1)
    call PRINT_HEX
    ld a, (track)
    call PRINT_HEX
    ld a, 'h'
    call PRINT_CHAR
    ld a, 13               ; CR to overwrite line
    call PRINT_CHAR
    ld bc, (track)
    call settrk             ; call BIOS settrk
    
    ld bc, 0                ; start at sector 0
    ld hl, (spt)            ; sectors per track
    dec hl                  ; because sectors start at 0
    
sector_loop:	
    push bc
    call setsec             ; call BIOS setsec
    call write              ; call BIOS write
    pop bc
    ld a, c
    cp l                    ; will either be 26 or 64, so only need to compare lsb                
    jp z, next_track

    inc bc                  
    jp sector_loop          ; next sector

next_track:	
    ld hl, (totaltracks)
    dec hl                  ; tracks start at 0, so take one off
    dec hl                  ; take another off for the 16bit compare to work with carry flag
    or a                    ; 16bit compare, so clear carry flag first
    ld de, (track)
    sbc hl, de
    jp c, complete          ; format completed

    inc	de
    ld (track), de
    jp track_loop           ; next track

complete:
    call PRINT_NEWLINE
    ld hl, str_complete
    call PRINT_STR

done:	
    ;ld sp, (CPMSP)
    jp 0                    ; return to CP/M with a warmboot
    ;ret
;
; multiplication function
; multiply two 16bit registers
; returns a 32bit sum
; ref: http://z80-heaven.wikidot.com/math#toc4
;
DE_Times_BC:
;Inputs:
;     DE and BC are factors
;Outputs:
;     A is 0
;     BC is not changed
;     DEHL is the product
;
    ld hl, 0
    ld a, 16
Mul_Loop_1:
    add hl, hl
    rl e
	rl d
    jr nc, $+6
    add hl, bc
    jr nc, $+3
    inc de
    dec a
    jr nz, Mul_Loop_1

    ret

str_signon:
    db "Format: v1.0 June 2021, Steve Bradford\r\n"
    db "Z80 Playground [8bitStack.co.uk]\r\n\n"
    db '$'
str_question:
    db "Are you really sure? [Y]: $"
str_clear_line:
    db "\r                             \r$"
str_newline:
    db "\r\n$"
str_which_drive:
    db "Select Drive to Format (cannot be the same as you are using): $"
str_select_error:
    db "You cannot format the drive you are logged in to\r\n$"
str_format:
    db "Drive $"
str_formatting:
    db "Formatting: T: $"
;str_formatting_sector:
 ;   db "h, S: $"
str_complete:
    db "Formatting Completed\r\n$"
str_total_tracks:
    db "Total tracks: $"
str_disk_size:
    db "Disk size: $"
str_select_fail:
    db "Disk select failed\r\n$"
str_header:
    db "SPT  BSH  BLM  EXM  DSM  DRM  AL0  AL1  CHS  OFF\r\n$"
;
;
; general BDOS functions
;
; function GETC
GETC:
    ld c, C_READ
    call BDOS
    ret
; end function


; function PRINT_CHAR
PRINT_CHAR:
    push hl
    push de
    push bc
    
    ld e, a
    ld c, C_WRITE
    call BDOS

    pop bc
    pop de
    pop hl
    ret
; end function


; function PRINT_NEWLINE
PRINT_NEWLINE:
    ;push hl
    ld hl, str_newline
    call PRINT_STR
    ;pop hl
    ret
; end function


; function PRINT_STR
PRINT_STR:
    push hl
    push de
    push bc
    
    ex de, hl
    ld c, C_WRITESTR
    call BDOS

    pop bc
    pop de
    pop hl
    ret
; end function

; function PRINT_HEX
; print a hexadecimal byte
;
; entry:
;   A byte to print
; 
PRINT_HEX:  
            PUSH    AF
            RRCA                         
            RRCA 
            RRCA 
            RRCA                        ; get high nibble
            CALL    PASS1               ; 
            POP     AF
PASS1:      AND     $0F
            ADD     A,$30
            CP      $3A
            JR      C,NUM               ; A < $3a - 0-9
            ADD     A,$07               ; else A-F
NUM:
            CALL    PRINT_CHAR
            RET
; end function
;


; Combined routine for conversion of different sized binary numbers into
; directly printable ASCII(Z)-string
; Input value in registers, number size and -related to that- registers to fill
; is selected by calling the correct entry:
;
;  entry  inputregister(s)  decimal value 0 to:
;   B2D8             A                    255  (3 digits)
;   B2D16           HL                  65535   5   "
;   B2D24         E:HL               16777215   8   "
;   B2D32        DE:HL             4294967295  10   "
;   B2D48     BC:DE:HL        281474976710655  15   "
;   B2D64  IX:BC:DE:HL   18446744073709551615  20   "
;
; The resulting string is placed into a small buffer attached to this routine,
; this buffer needs no initialization and can be modified as desired.
; The number is aligned to the right, and leading 0's are replaced with spaces.
; On exit HL points to the first digit, (B)C = number of decimals
; This way any re-alignment / postprocessing is made easy.
; Changes: AF,BC,DE,HL,IX
; P.S. some examples below

; by Alwin Henseler


B2D8:    LD H,0
         LD L,A
B2D16:   LD E,0
B2D24:   LD D,0
B2D32:   LD BC,0
B2D48:   LD IX,0          ; zero all non-used bits
B2D64:   
         LD (B2DINV),HL
         LD (B2DINV+2),DE
         LD (B2DINV+4),BC
         LD (B2DINV+6),IX ; place full 64-bit input value in buffer
         LD HL,B2DBUF
         LD DE,B2DBUF+1
         LD (HL)," "
B2DFILC: EQU $-1            ; address of fill-character
         LD BC,18
         LDIR               ; fill 1st 19 bytes of buffer with spaces
         LD (B2DEND-1),BC ;set BCD value to "0" & place terminating 0
         LD E,1             ; no. of bytes in BCD value
         LD HL,B2DINV+8     ; (address MSB input)+1
         LD BC,#0909
         XOR A
B2DSKP0: DEC B
         JR Z,B2DSIZ        ; all 0: continue with postprocessing
         DEC HL
         OR (HL)            ; find first byte <>0
         JR Z,B2DSKP0
B2DFND1: DEC C
         RLA
         JR NC,B2DFND1      ; determine no. of most significant 1-bit
         RRA
         LD D,A             ; byte from binary input value
B2DLUS2: PUSH HL
         PUSH BC
B2DLUS1: LD HL,B2DEND-1     ; address LSB of BCD value
         LD B,E             ; current length of BCD value in bytes
         RL D               ; highest bit from input value -> carry
B2DLUS0: LD A,(HL)
         ADC A,A
         DAA
         LD (HL),A          ; double 1 BCD byte from intermediate result
         DEC HL
         DJNZ B2DLUS0       ; and go on to double entire BCD value (+carry!)
         JR NC,B2DNXT
         INC E              ; carry at MSB -> BCD value grew 1 byte larger
         LD (HL),1          ; initialize new MSB of BCD value
B2DNXT:  DEC C
         JR NZ,B2DLUS1      ; repeat for remaining bits from 1 input byte
         POP BC             ; no. of remaining bytes in input value
         LD C,8             ; reset bit-counter
         POP HL             ; pointer to byte from input value
         DEC HL
         LD D,(HL)          ; get next group of 8 bits
         DJNZ B2DLUS2       ; and repeat until last byte from input value
B2DSIZ:  LD HL,B2DEND       ; address of terminating 0
         LD C,E             ; size of BCD value in bytes
         OR A
         SBC HL,BC          ; calculate address of MSB BCD
         LD D,H
         LD E,L
         SBC HL,BC
         EX DE,HL           ; HL=address BCD value, DE=start of decimal value
         LD B,C             ; no. of bytes BCD
         SLA C              ; no. of bytes decimal (possibly 1 too high)
         LD A,"0"
         RLD                ; shift bits 4-7 of (HL) into bit 0-3 of A
         CP "0"             ; (HL) was > 9h?
         JR NZ,B2DEXPH      ; if yes, start with recording high digit
         DEC C              ; correct number of decimals
         INC DE             ; correct start address
         JR B2DEXPL         ; continue with converting low digit
B2DEXP:  RLD                ; shift high digit (HL) into low digit of A
B2DEXPH: LD (DE),A          ; record resulting ASCII-code
         INC DE
B2DEXPL: RLD
         LD (DE),A
         INC DE
         INC HL             ; next BCD-byte
         DJNZ B2DEXP        ; and go on to convert each BCD-byte into 2 ASCII
         SBC HL,BC          ; return with HL pointing to 1st decimal
         RET

B2DINV:  DS 8               ; space for 64-bit input value (LSB first)
B2DBUF:  DS 20              ; space for 20 decimal digits
B2DEND:  DS 2, '$';S 1      ; space for terminating 0


; BIOS entry points
settrk: 
    db $c3, $1e, $f6        ; $f6 will be overwritten to match system BIOS
    ret
setsec:
    db $c3, $21, $f6
    ret
setdma:
    db $c3, $24, $f6
    ret
write:
    db $c3, $2a, $f6
    ret
; end BIOS entry points
wanted_drive:
    db 0
spt:
    dw 1
bsh:
    db 0
dsm:
    dw 1
drive:	
    db 0
sector:	
    dw 1
track:	
    dw 1
totaltracks:
    dw 1
DPBaddress:
    ds 2
buffer:
    ds 150, $e5      	    ; one CP/M record filled with 0E5h
    dw 32	                ; stack space
stack:
    end