;
; filetransfer.asm
;
; function TFER
; read FAT files and write them to CP/M
;
; idea is to read a FAT file in chunks of say 10KB, and write to RAM.
; Use the CP/M built-in SAVE function to create the CP/M file
;
; Files to be transferred, will be placed in folder TFER on USB memory stick
;
; syntax
;   tfer <from> <to>
;       <from> FAT filename
;       <to> CP/M filename
;
CDRIVE      equ $4          ; current disk drive
BDOS        equ $5          ; BDOS jump vector
BOOT        equ $0          ; CP/M warm boot vector
FCB         equ $5c         ; default system FCB for transient programmes
FCBdst      equ $6c         ; second filename
DMA         equ $80         ; default dma buffer (128 bytes)
FCBcr	    equ FCB+32      ; Current record
;
; define BDOS functions
;
C_READ      equ $01         ; console input
C_WRITE     equ $02         ; console output
C_WRITESTR  equ $09         ; output string, '$' terminated, to console output
C_READSTR   equ $0a         ; buffered console input
;
F_OPEN      equ $0f         ; open file
F_CLOSE     equ $10         ; close file
F_WRITE     equ $15         ; write record
F_MAKE      equ $16         ; create file
F_DMAOFF    equ $1a         ; set DMA address
;
; control keys
;
CNTRL_C     equ 3
;
;
;
            org $100        ; CP/M programmes are loaded and run from address $100


tfer:
    ld	(CPMSP), sp         ; save CP/M stack pointer
    ld sp, stack            ; set programme stack space

    ld hl, str_signon
    call PRINT_STR

    ; list TFER directory
    ld hl, tfer_home
    call open_file          ; open file is now TFER
    cp ERR_OPEN_DIR
    jr z, read_dir

openFail:
    push hl
    ld hl, str_open_fail
    call PRINT_STR
    call PRINT_HEX
    call PRINT_NEWLINE
    pop hl
    call PRINT_STR
    call PRINT_NEWLINE
    jp exit

read_dir:
    ld a, 0
    ld de, TMPBUFFER
    call dir
    cp 0
    jr nz, cmd_error3

done:
    call PRINT_NEWLINE
    call PRINT_NEWLINE

    ld hl, str_prompt_src
    call PRINT_STR
    ld hl, filename_src
    call getc               ; src filename
    ;ld hl, filename_src
    ;call CONVFST
    ;jp c, cmd_error2

    ;ld hl, str_prompt_dst
    ;call PRINT_STR
    ;ld hl, filename_dst
    ;call getc               ; dst filename

    ; attempt to open CP/M file
	ld de, FCB		        ; create new file
	ld a, 0		            ; Start at block 0
	ld (FCBcr), a
	ld c, F_OPEN
	jp BDOS		        ; returns A = 255 if error in opening
	inc a
	jp z, cmd_error2

	




cmd_no_error:


; read FAT file

; write CP/M file
WriteSector:
 	ld de, TMPBUFFER		; Set DMA address to the packet data buff
	ld c, F_DMAOFF
	call BDOS
	ld de, FCB		        ; File Description Block
	ld c, F_WRITE
	call BDOS		        ; Returns A=0 if ok, A=1 if EOF
    cp 0
	jp nz, writeFail
    jr closeFile
writeFail:
    ld hl, str_write_fail
    call PRINT_STR

; close FAT file

; close CP/M file    
closeFile:
	ld de, FCB		        ; Close the file
	ld c, F_CLOSE
	call BDOS
    
    jr exit                 ; return back to CP/M


cmd_error:
    ld  hl, str_syntax
    ;call PRINT_STR
    jr exit                 ; return back to CP/M

cmd_error2:
    ld hl, str_invalid_filename
    ;call PRINT_STR
    jr exit

cmd_error3:
    ld hl, str_dir_fail
    
    ;jr exit

exit:
    call PRINT_STR
    ld	sp, (CPMSP)
	jp 0 ;ret
; end function TFER
;

;
; function getc
; hl = temp buffer pointer
getc:                       ; read input
    
.loop:
    push hl
    ld c, C_READ
    call BDOS
    cp 13                   ; is char a CR?
    jr z, .cont

    cp CNTRL_C
    jp z, 0                 ; abort program and warm BOOT

    pop hl
    ld (hl), a
    inc hl

    jr .loop

.cont:
    ;dec hl
    ld (hl), 0              ; mark end of string by replacing CR with null

    ; TODO check entered string for valid filename format 8.3
    ret
; end function
;





;
; the following functions have been copied from CP/M
;

;
;   Check character at (DE) for legal command input. Note that the
; zero flag is set if the character is a delimiter.
;
CHECK:	
    LD	A,(DE)
	OR	A
	RET	Z
	CP	' '		;control characters are not legal here.
	ret	C ;,SYNERR
	RET	Z		;check for valid delimiter.
	;CP	'='
	;RET	Z
	;CP	'_'
	;RET	Z
	CP	'.'
	RET	Z
	CP	':'
	RET	Z
	;CP	';'
	;RET	Z
	;CP	'<'
	;RET	Z
	;CP	'>'
	;RET	Z
	RET	
;
;   Get the next non-blank character from (DE).
;
NONBLANK: 
    LD	A,(DE)
	OR	A		;string ends with a null.
	RET	Z
	CP	' '
	RET	NZ
	INC	DE
	JP	NONBLANK
;
;   Add (HL)=(HL)+(A)
;
ADDHL:	
    ADD	A,L
	LD	L,A
	RET	NC		;take care of any carry.
	INC	H
	RET	

;
;   Convert the first name in (FCB).
;
CONVFST:
    LD	A,0
;
;   Format a file name (convert * to '?', etc.). On return,
; (A)=0 is an unambigeous name was specified. Enter with (A) equal to
; the position within the fcb for the name (either 0 or 16).
;
    ex de, hl ; save filename string
CONVERT:
    LD	HL,FCB
	CALL	ADDHL
	PUSH	HL
	PUSH	HL
	XOR	A
	;LD	(CHGDRV),A	;initialize drive change flag.
    ex de, hl ; getback filename string pointer
	;LD	HL,(INPOINT)	;set (HL) as pointer into input line.
	EX	DE,HL
	CALL	NONBLANK	;get next non-blank character.
	EX	DE,HL
	;LD	(NAMEPNT),HL	;save pointer here for any error message.
	EX	DE,HL
	POP	HL
	LD	A,(DE)		;get first character.
	OR	A
	JP	Z,CONVRT1
	SBC	A,'A'-1		;might be a drive name, convert to binary.
	LD	B,A		;and save.
	INC	DE		;check next character for a ':'.
	LD	A,(DE)
	CP	':'
	JP	Z,CONVRT2
	DEC	DE		;nope, move pointer back to the start of the line.
CONVRT1:
    LD	A,(CDRIVE)
	LD	(HL),A
	JP	CONVRT3
CONVRT2:LD	A,B
	;LD	(CHGDRV),A	;set change in drives flag.
	LD	(HL),B
	INC	DE
;
;   Convert the basic file name.
;
CONVRT3:
    LD	B,08H
CONVRT4:
    CALL	CHECK
    ret c ; invalid input
	JP	Z,CONVRT8
	INC	HL
	;CP	'*'		;note that an '*' will fill the remaining
	;JP	NZ,CONVRT5	;field with '?'.
	;LD	(HL),'?'
	;JP	CONVRT6
CONVRT5:
    LD	(HL),A
	INC	DE
CONVRT6:
    DEC	B
	JP	NZ,CONVRT4
CONVRT7:
    CALL	CHECK		;get next delimiter.
    ret c ; invalid input
	JP	Z,GETEXT
	INC	DE
	JP	CONVRT7
CONVRT8:INC	HL		;blank fill the file name.
	LD	(HL),' '
	DEC	B
	JP	NZ,CONVRT8
;
;   Get the extension and convert it.
;
GETEXT:	LD	B,03H
	CP	'.'
	JP	NZ,GETEXT5
	INC	DE
GETEXT1:
    CALL	CHECK
    ret c ; invalid input
	JP	Z,GETEXT5
	INC	HL
	CP	'*'
	JP	NZ,GETEXT2
	LD	(HL),'?'
	JP	GETEXT3
GETEXT2:LD	(HL),A
	INC	DE
GETEXT3:DEC	B
	JP	NZ,GETEXT1
GETEXT4:
    CALL	CHECK
    ret c ; invalid input
	JP	Z,GETEXT6
	INC	DE
	JP	GETEXT4
GETEXT5:INC	HL
	LD	(HL),' '
	DEC	B
	JP	NZ,GETEXT5
GETEXT6:LD	B,3
GETEXT7:INC	HL
	LD	(HL),0
	DEC	B
	JP	NZ,GETEXT7
	EX	DE,HL
	;LD	(INPOINT),HL	;save input line pointer.
	POP	HL
;
;   Check to see if this is an ambigeous file name specification.
; Set the (A) register to non zero if it is.
;
	LD	BC,11		;set name length.
GETEXT8:INC	HL
	LD	A,(HL)
	CP	'?'		;any question marks?
	JP	NZ,GETEXT9
	INC	B		;count them.
GETEXT9:DEC	C
	JP	NZ,GETEXT8
	LD	A,B
	OR	A
	RET	









str_syntax:
    db "Syntax: tfer <src> <dst>", 13, 10
    db "Example: tfer one.com d:two.com", 13, 10
    db '$'                  ; BDOS end of string marker
str_invalid_filename:
    db "Invalid filename\r\n$"
str_dir_fail:
    db "Dir read failed\r\n$"
str_write_fail:
    db "CP/M Write failed\r\n$"
str_open_fail:
    db "Open failed on $"
str_signon:
    db "File Transfer: v1.0 June 2021, Steve Bradford\r\n"
    db "Z80 Playground [8bitStack.co.uk]\r\n\n"
    db '$'

str_prompt_src:
    db "Enter source filename [8.3] to TransFER> $"
str_prompt_dst:
    db "Enter destination filename [8.3] to create> $"
;
;
; general BDOS functions
;
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
    ld hl, str_newline
    call PRINT_STR
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
PRINT_HEX:  ;PUSH    AF
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

            ;POP     AF
            RET
; end function
;

str_newline:
    db 13, 10, '$'
;tfer_root:   
;    db "/CPM", 0, '$'
tfer_home:
    db "/TFER", 0, '$'
filename_src:
    ds 20, 0
filename_dst:
    ds 20, 0

; File Control block_loop (FCB) - 36 bytes
; DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL CR R0 R1 R2
FCBcopy:
    db 0                    ; 1  DR
    db '           '        ; 11 F1-F8, T1-T3
    db 0, 0, 0, 0, 0        ; 4  EX, S1, S2, RC
    db '                '   ; 16 AL
    db 0                    ; 1  CR
    db 0, 0, 0              ; 3  R0, R1, R2
;
CPMSP:
    dw 1
TMPBUFFER:
    ds 128, 0
;
;
    include "memorystick_small.asm"
;
    ds 64, 0                ; 64 bytes reserved for programme stack (32 words)
stack:
;
    end
