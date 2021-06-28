;
;
;
NewLine:    DB      $0d, $0a, 0         ; CR, LF, NULL
cls:        DB      $1b, '[', '2', 'J', 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     
; function GETC
GET_CHAR:
            IN      A, (UALSR)          ; read Line Status Register           
            BIT     0, A                ; bit set when Rx Holding register 
            JP      Z, GET_CHAR
            IN      A, (UARX)

            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     
; function PRINT_STR
; Print null terminated ASCII string
; entry:
;   HL points to start of string
;
PRINT_STR:  PUSH    AF
            PUSH    HL

PRINT_STR1: LD      A,(HL)              ; get character
            CP      0                   ; test for end of string
            JP      Z,PRINT_STR_END     ; return

            CALL    PRINT_CHAR
            INC     HL 
            JP      PRINT_STR1          ; repeat

PRINT_STR_END:
            POP     HL
            POP     AF

            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function PRINT_CHAR
; Print a single ASCII character
;
; entry:
;   A ASCII character to print
;
PRINT_CHAR: 
            PUSH    AF
.loop1:     IN      A,(UALSR)           ; read Line Status Register           
            BIT     5,A                 ; bit set when Transmit Holding register and shift register have emptied
            JP      Z,.loop1            ; loop until ready

            POP     AF
            OUT     (UATX),A            ; write character to UART
            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function PRINT_SPACE
; Print a single space character
; entry:
;
PRINT_SPACE:
    push af
    ld a, ' '
    call PRINT_CHAR
    pop af
    
    ret
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
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
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function PRINT_16BIT
; print 16bit in hex
;
; entry:
;   hl = address to print
PRINT_16BIT:
    push hl
    
    ld a, (hl)
	call PRINT_HEX
	inc hl
	ld a, (hl)
	call PRINT_HEX

    pop hl
    ret
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function PRINT_REGS
; print registers
;
; entry:
;
PRINT_REGS:
    push af
    push hl
    push de
    push bc
    
    ld hl, str_regs
    call PRINT_STR

    call PRINT_HEX      ; print A
	call PRINT_SPACE
	call PRINT_SPACE
    pop bc
    ;push bc
	ld a, b
	call PRINT_HEX      ; print BC
	ld a, c
	call PRINT_HEX
	call PRINT_SPACE
    pop de
	ld a, d
	call PRINT_HEX      ; print DE
	ld a, e
	call PRINT_HEX
	call PRINT_SPACE
    pop hl
	ld a, h
	call PRINT_HEX      ; print HL
	ld a, l
	call PRINT_HEX
    call PRINT_NEWLINE

    ;pop hl
    pop af
    ret
str_regs:
	db "\r\nA    BC   DE   HL\r\n", 0
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function CLS
; clear screen
;
; entry:
;
CLS:        LD      HL,cls
            CALL    PRINT_STR

            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function PRINT_NEWLINE
; print a newline
;
; entry:
;
PRINT_NEWLINE: 
            PUSH    HL
            LD      HL,NewLine
            CALL    PRINT_STR

            POP     HL
            
            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function ROM_OFF
; disable ROM
;
; entry:
;   none
;
rom_off:    
            ; rom LED off + disable ROM
	        in a, (UAMCR)
	        or %00001000
	        out (UAMCR), a
            ld a, ROM_DISABLE
            ld (V_ROMSTATE), a             ; save ROM enabled/disabled state
            
            ret
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function ROM_ON
; enable ROM
;
; entry:
;   none
;
rom_on:     
            ; rom LED on + enable ROM
	        in a, (UAMCR)
	        and %11110111
	        out (UAMCR), a
            ld a, ROM_ENABLE
            ld (V_ROMSTATE), a             ; save ROM enabled/disabled state
            
            ret
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function ROM_TOGGLE
rom_toggle:
            in a, (ROM_CNTL)
            bit 3, a
            jr z, _rom_toggle_off

            call rom_on
            
            rst $08
_rom_toggle_off:
            call rom_off
            
            rst $08
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function STRLEN
;
; entry:
;   HL poins to null terminated string
;
; exit:
;   A returns length
;
; A destroyed
STRLEN:
            push hl
            push bc

            ld b, 0
_strlen_loop:                      ; get string length of entered string
            ld a, (hl)
            cp 0
            jr z, _strlen_end
            inc hl
            inc b
            jr _strlen_loop
_strlen_end:
            ld a, b
            pop bc
            pop hl
            ret
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function CHECKOPTIONS
; compare string for cmd args
;
; entry:
;   hl string pointer
;   de string pointer to compare against
;
; exit:
;   a returns 0 on success, 255 on error
;
; A, B destroyed
CHECKOPTIONS:
            push de         ; string to match
            push hl         ; argv string
            call STRLEN
            ld b, a
_do:
            ;ld a, (hl)
            ;cp 'a'
            ;jr c, _already_lowercase ; A >= 'a'
            ;add a, $20
_already_lowercase:
            ld a, (de)
            cp (hl)         ; compare A with (DE)
            jr nz, _no_match

            inc de
            inc hl
            djnz _do        ; loop until no match

            ld a, 0
            pop hl
            pop de
            ret
_no_match:
            ld a, 255
            pop hl
            pop de
            ret
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function DE_TIMES_BC
; 32bit multiplication
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
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function B2D8 + various entry points
;
; 32bit multiplication
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
B2DFILC: EQU $-1         ; address of fill-character
         LD BC,18
         LDIR            ; fill 1st 19 bytes of buffer with spaces
         LD (B2DEND-1),BC ;set BCD value to "0" & place terminating 0
         LD E,1          ; no. of bytes in BCD value
         LD HL,B2DINV+8  ; (address MSB input)+1
         LD BC,#0909
         XOR A
B2DSKP0: DEC B
         JR Z,B2DSIZ     ; all 0: continue with postprocessing
         DEC HL
         OR (HL)         ; find first byte <>0
         JR Z,B2DSKP0
B2DFND1: DEC C
         RLA
         JR NC,B2DFND1   ; determine no. of most significant 1-bit
         RRA
         LD D,A          ; byte from binary input value
B2DLUS2: PUSH HL
         PUSH BC
B2DLUS1: LD HL,B2DEND-1  ; address LSB of BCD value
         LD B,E          ; current length of BCD value in bytes
         RL D            ; highest bit from input value -> carry
B2DLUS0: LD A,(HL)
         ADC A,A
         DAA
         LD (HL),A       ; double 1 BCD byte from intermediate result
         DEC HL
         DJNZ B2DLUS0    ; and go on to double entire BCD value (+carry!)
         JR NC,B2DNXT
         INC E           ; carry at MSB -> BCD value grew 1 byte larger
         LD (HL),1       ; initialize new MSB of BCD value
B2DNXT:  DEC C
         JR NZ,B2DLUS1   ; repeat for remaining bits from 1 input byte
         POP BC          ; no. of remaining bytes in input value
         LD C,8          ; reset bit-counter
         POP HL          ; pointer to byte from input value
         DEC HL
         LD D,(HL)       ; get next group of 8 bits
         DJNZ B2DLUS2    ; and repeat until last byte from input value
B2DSIZ:  LD HL,B2DEND    ; address of terminating 0
         LD C,E          ; size of BCD value in bytes
         OR A
         SBC HL,BC       ; calculate address of MSB BCD
         LD D,H
         LD E,L
         SBC HL,BC
         EX DE,HL        ; HL=address BCD value, DE=start of decimal value
         LD B,C          ; no. of bytes BCD
         SLA C           ; no. of bytes decimal (possibly 1 too high)
         LD A,"0"
         RLD             ; shift bits 4-7 of (HL) into bit 0-3 of A
         CP "0"          ; (HL) was > 9h?
         JR NZ,B2DEXPH   ; if yes, start with recording high digit
         DEC C           ; correct number of decimals
         INC DE          ; correct start address
         JR B2DEXPL      ; continue with converting low digit
B2DEXP:  RLD             ; shift high digit (HL) into low digit of A
B2DEXPH: LD (DE),A       ; record resulting ASCII-code
         INC DE
B2DEXPL: RLD
         LD (DE),A
         INC DE
         INC HL          ; next BCD-byte
         DJNZ B2DEXP     ; and go on to convert each BCD-byte into 2 ASCII
         SBC HL,BC       ; return with HL pointing to 1st decimal
         RET

B2DINV:  DS 8            ; space for 64-bit input value (LSB first)
B2DBUF:  DS 20           ; space for 20 decimal digits
B2DEND:  DS 2, '$';S 1            ; space for terminating 0
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

