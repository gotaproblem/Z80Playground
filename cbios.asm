; ********************************************************
; *                                                      *
; * Z80 Home Build Development                           *
; * Steve Bradford                                       *
; * 01/05/2021                                           *
; *                                                      *
; * CBIOS                                                *
; * ref.                                                 *
; * skeletal CBIOS for CP/M 2.0 alteration               *
; *                                                      *
; * cbios.asm                                            *
; *                                                      *
; * History                                              *
; * v1.0   June      2021	Initial release              *
; * v1.1   October   2021	Corrected hdd vector bytes   *
; *                         Signon message change        *
; * v1.2   January   2022   Fixed sectran ()             *
; *                                                      *
; ********************************************************
;
; *************************************
; * Hardware                Ports     *
; * SC16C550 UART           $08       *
; * USB CH376S Disk Module  $10       *
; *                                   *
; * 64KB RAM                          *
; * 32KB ROM                          *
; *                                   *
; * Control IO                        *
; *   ROM Enable            $0C bit 3 *
; *   USER LED              $0C bit 0 *
; *   DISK LED              $0C bit 2 *
; *   CH376S data           $10       *
; *   CH376S command        $11       *
; *                                   *
; *************************************

; build options
DEBUG: DEFL 0

		include "vars.asm"
;
;
; Custom BIOS (CBIOS)
; 63K System - best that can be done (need to free up over 512 bytes to make a 64K System)
; A 63K System has enough space for 2x fdd, 3x hdd
;
ccp:	equ	$e000  			; base of ccp ($e000 = 63K System)
bdos:	equ	$e806  			; bdos entry
bios:	equ	$f600  			; base of bios
cdisk:	equ	4				; current disk number 0=a,... 15=p
iobyte:	equ	3				; i/o byte address
disks:	equ	4				; number of disks in the system ( 2x fdd, 2x hdd)
;
;
		org	bios			; origin of this program
CBIOS_START: equ $
;
;	jump vector for individual subroutines
;
	JP	boot	;cold start
wboote:	
	JP	wboot	;warm start
	JP	const	;console status
	JP	conin	;console character in
	JP	conout	;console character out
	JP	list	;list character out
	JP	punch	;punch character out
	JP	reader	;reader character out
	JP	home	;move head to home position
	JP	seldsk	;select disk
	JP	settrk	;set track number
	JP	setsec	;set sector number
	JP	setdma	;set dma address
	JP	read	;read disk
	JP	write	;write disk
	JP	listst	;return list status
	JP	sectran	;sector translate
;
; custom calls
;	JP	msh		;monitor shell
;
;	fixed data tables for five-drives 
;   standard ibm-compatible 8" disks use skew of 6 (drives A & B)
;
dpbase:	
;	disk Parameter header for disk 00
	defw	trans, 0000h
	defw	0000h, 0000h
	defw	dirbf, dpblk1
	defw	0000, all00
;	disk parameter header for disk 01
	defw	trans, 0000h
	defw	0000h, 0000h
	defw	dirbf, dpblk1
	defw	0000, all01
;	disk parameter header for disk 02
	defw	0000h, 0000h
	defw	0000h, 0000h
	defw	dirbf, dpblk3
	defw	0000, all02
;	disk parameter header for disk 03
	defw	0000h, 0000h
	defw	0000h, 0000h
	defw	dirbf, dpblk3
	defw	0000, all03
;	disk parameter header for disk 04
;	defw	0000h, 0000h
;	defw	0000h, 0000h
;	defw	dirbf, dpblk2
;	defw	0000, all04
;
;	sector translate vector
trans:	
	defm	 0,  6, 12, 18	;sectors  1,  2,  3,  4
	defm	24,  4, 10, 16	;sectors  5,  6,  7,  8
	defm	22,  2,  8, 14	;sectors  9, 10, 11, 12
	defm	20,  1,  7, 13	;sectors 13, 14, 15, 16
	defm	19, 25,  5, 11	;sectors 17, 18, 19, 20
	defm	17, 23,  3,  9	;sectors 21, 22, 23, 24
	defm	15, 21			;sectors 25, 26
;
dpblk1:	;disk parameter block for disks A & B 250KB - skew 6, 75 + 2 tracks = IBM SSSD 8" 3740.
	defw	26		; SPT sectors per track
	defm	3		; BSH block shift factor
	defm	7		; BLM block mask
	defm	0		; EXM null mask
	defw	243		; DSM disk size-1
	defw	63		; DRM directory max
	defm	192		; ALV alloc 0			$c0 = %11000000 = ((dir max * 32) / block size) = 2 bits
	defm	0		; ALV alloc 1			$00 = %00000000
	defw	0		; CKS check size
	defw	2		; OFF track offset
;
;dpblk2:	;disk parameter block for 8MB hdd - 1024 tracks bs = 2K
;	defw	64		;sectors per track
;	defm	4		;block shift factor = block size of 2048 bytes
;	defm	15		;block mask
;	defm	0		;null mask
;	defw	4095	;disk size-1 (4096 * 2048)
;	defw	1023	;directory max
;	defm	255		;alloc 0			$ff = %11111111 = ((dir max * 32) / block size) = 16 bits
;	defm	255		;alloc 1			$ff = %11111111
;	defw	0		;check size
;	defw	0		;track offset
;
dpblk3:	;disk parameter block for 8MB hdd - 1024 tracks bs = 4K
	defw	64		;sectors per track
	defm	5		;block shift factor = block size of 4096 bytes
	defm	31		;block mask
	defm	1		;extent mask
	defw	2047	;disk size-1 (in blocks = 2048 * 4096)
	defw	1023	;directory max
	defm	255		;alloc 0			$ff = %11111111 = ((dir max * 32) / block size) = 8 bits
	defm	0		;alloc 1			$00 = %00000000
	defw	0		;check size
	defw	0		;track offset
;
;	end of fixed tables



;
;	BIOS functions
boot:	
	; initialise CH376S module and mount disk A
	ld hl, str_cpm_msg1
	call PRINT_STR

	call ch376s_init

boot_real:
	ld hl, signon
	call PRINT_STR

	ld hl, cpm2_driveA		; starting disk image file name = "/A.DSK"
	ld de, cpm_driveX
	ld bc, 7				; length of disk image file name + 1
	ldir					; make a copy of the disk image file name

	ld a, 93h
	ld (iobyte), a			; set the iobyte
	ld a, 1					; setting drive to 1 (B:) triggers initialise
	ld (cdisk), a			; 
	ld (cdrive), a
	jp gocpm				; initialize and go to cp/m
;
wboot:
	ld	sp, $80				; use space below buffer for stack
	ld a, (cdrive)
	ld c, a					; select previous disk
	call seldsk
	call home	    		; go to track 0
;

; load CP/M from ROM (CCP only)
; this could be put on drive A's boot tracks
	in a, (UAMCR)
	and %11110111
	out (UAMCR), a			; enable ROM
							; this address needs to match main.asm CBIOS load point in ROM
	ld hl, $1000			; copy ROM CCP to RAM
	ld de, ccp
	ld bc, $0800			; size of CCP
	ldir

	in a, (UAMCR)
	or %00001000
	out (UAMCR), a			; disable ROM
;

gocpm:
	ld 	a, $c3				; c3 is a jmp instruction
	ld	(0), a				; for jmp to wboot
	ld	hl, wboote			; wboot entry point
	ld	(1), hl				; set address field for jmp at 0
;
	ld	(5), a				; for jmp to bdos
	ld	hl, bdos			; bdos entry point
	ld	(6), hl				; address field of Jump at 5 to bdos
;
	ld	bc, $80				; default dma address is 0080h
	call setdma
;
							; as this is only used at a cold boot, we are always using drive A
    ld 	c, 0				; send to the ccp
	jp	ccp		     		; go to cp/m for further processing
;
;
const:	;console status, return 0ffh if character ready, 00h if not
	in a, (UALSR)			; get status from Line Status Register
	bit 0, a				; zero flag set to true if bit 0 is 0 (bit 0 = Receive Data Ready)
	jp z, no_char
	ld a, 0ffh				; char ready	
	ret
no_char:
    ld a, 00h				; no char
	ret
;
conin:	;console character into register a
	in a, (UALSR)			; get status from Line Status Register
	bit 0, a				; zero flag set to true if bit 0 is 0 (bit 0 = Receive Data Ready)
	jp z, conin				; loop until char ready
	in a, (UARX)			; Get the incoming char
							; Xmodem will not work if strip parity is used
	and $7f		   		 	; strip parity bit
	ret
;
conout:	;console character output from register c
	in	a,(UALSR)   		; check UART is ready to send.
    bit 5,a         		; zero flag set to true if bit 5 is 0
	jp	z,conout			; loop until port ready

	ld	a,c		    		; get the char
	out	(UATX),a			; out to port
	ret
;
list:	;list character from register c
	ld 	a, c	  			; character to register a
	ret		  	    		; null subroutine
;
listst:	;return list status (0 if not ready, 1 if ready)
	xor	a	 	    		; 0 is always ok to return
	ret
;
punch:	;punch	character from	register C
	in	a,(UALSR)   		; check UART is ready to send.
    bit 5,a         		; zero flag set to true if bit 5 is 0
	jp	z, punch			; loop until port ready
	
	ld	a, c	    		; get the char
	out	(UATX), a			; out to port
	ret
;
;
reader:	; read character into register a from reader device
		; raw data
	in a, (UALSR)			; get status from Line Status Register
	bit 0, a				; zero flag set to true if bit 0 is 0 (bit 0 = Receive Data Ready)
	jp z, reader			; loop until char ready

	in a, (UARX)			; Get the incoming char
	ret
;
;
;	i/o drivers for the disk follow
;
home:	; move to track 0 of current drive
        ; translate this call into a settrk call with Parameter 00
	ld     bc, 0			; select track 0
	call   settrk
	ret			    		; we will move to 0 on first read/write
;
;
seldsk:	;select drive given by register c	
	ld hl, 0000h			; error return code
	ld a, c
	cp disks				; must be between 0 and 'disks'
	ret nc					; no carry if >= 'disks'

	; optimise by checking current drive against wanted drive
	;ld a, (cdrive)
	;ld a, (cdisk)
	;cp c
	;jp z, seldsk_optimise	; current and wanted drives are the same so no need to perform the following
	
	ld a, c
	ld (cdisk), a			; selected disk is valid, so save it
	ld (cdrive), a

	ld hl, cpm_driveX
	add a, 'A'				; add real disk number to filename (eg. disk 0 = /A.DSK)
	inc hl
	ld (hl), a				; ammend current disk image filename	

;	disk number is in the proper range
;	compute proper disk parameter header (DPH) address
	ld 	l, c				; L = disk number 0, 1, 2, 3, 4
	ld 	h, 0				; high order zero
	add	hl, hl				; *2
	add	hl, hl				; *4
	add	hl, hl				; *8
	add	hl, hl				; *16 (size of each header)
	ld	de, dpbase
	add	hl, de				; hl = dpbase (diskno*16)
	ld (cdph), hl			; save current disk drives dph address

	ld de, 10		
	add hl, de				; add offset to point to dpb address
	
	ld c, (hl)
	inc hl
	ld b, (hl)
	ld (cdpb), bc			; save current disk drives dpb address		
	
	ld h, b
	ld l, c
	ld c, (hl)
	inc hl
	ld b, (hl)				; bc = spt

	ld (cspt), bc			; save curent disk drives spt value

	ld hl, cpm_driveX
	call open_file			; open selected disks disk image
	jp z, seldsk_optimise

	ld hl, str_dir_fail3
	call PRINT_STR
	ld hl, 0000h
	ret

seldsk_optimise:
	ld hl, (cdph)					
	ret						; return hl = address of dph
;
;
settrk:	;set track given by registers bc
	ld (track), bc
	ret
;
;
setsec:	;set sector given by registers bc
	;ld hl, prev_sector
	;ld bc, (sector)
	;ld (hl), c
	;inc hl
	;ld (hl), b
	ld (sector), bc
	ret
;
;
sectran:
	;translate the sector given by bc using the
	;translate table given by de
	;EX DE, HL				; hl=.trans
	;ADD	HL, BC				; hl=.trans (sector)

	;ld a, (cdisk)
	;cp 2
	;ret nc					; only the first two drives use skew in this CP/M implementation
	
	;LD l, (hl)				; l=trans (sector)
	;LD h, 0					; hl=trans (sector)	
	;ret			    		; with value in hl

	ld l, c
	ld h, b
	ld a, d
	or e
	ret z					; DE = 0, so no translation, HL = BC

	ex de, hl
	add hl, bc
	ld l, (hl)
	ld h, 0

	ret
;
;
setdma:	;setdma address given by registers b and c
	ld l, c					; low order address
	ld h, b					; high order address
	ld (dmaad), hl			; save the address
	ret
;
; Read one CP/M sector from disk.
; Return a 0 in A if the operation completes properly, and 1 if a read error occurs.
;
read:
	;ld a, (cdisk)
	;ld c, a
	;call seldsk				; if we don't do this, get seek errors on same drive writes
	call drive_seek			; calculate disk image file offset
	jr nz, read_fail

	call read_from_file		; read 128 bytes
	;cp 0
	;jr nz, read_fail

	ld a, 0
	ret

read_fail:
	ld a, 1
    ret

drive_seek:
	ld bc, (cspt)			; current drive's sectors per track
	; calculate number of cpm records (128 byte records)
	; cpmrec = (spt * track) + sector
	ld de, (track)	
	call DE_Times_BC		; track * spt (de * bc) returns answer in dehl

	;push de				; debug - check to see if DE is being used - expect not
	;ld a, d
	;or e
	;cp 0
	;jp z, ds_cont

	;push hl
	;ld hl, str_ds_debug
	;call PRINT_STR
	;pop hl

;ds_cont:
	;pop de

	ld bc, (sector)
	add hl, bc				; add sector to hl - might need to be a 32bit add

	; DEHL = (spt * track) + sector
	; DEHL is number of cpm records (128 bytes each)
	; seek offset needs to be in bytes, so multiply by 128

	; now need to multiply by 128 to get byte offset
	;ex de, hl
	;ld bc, 128				; de * 128
	;call DE_Times_BC		; result in dehl

	; for an 8MB disk, the max number of CP/M records is 65536 = 0xFFFF
	; so, DE should always be 0 at this point
	; use DE to shift in to
	; rotate left 7 (multiply by 128)
	; DE = high 16bits, HL = low 16bits
	;ld d, 0
	;ld e, h					; copy H to E
	;srl e					; shift right once

	;ld a, h
	;rrc a
	;and $80
	;ld b, a
	;ld a, l
	;srl a
	;or b
	;ld b, a

	;ld a, l
	;rrc a
	;and $80
	;ld l, a
	;ld h, b

	ld bc, 128
	ld d, h
	ld e, l
	call DE_Times_BC
	
_seek:
	call move_to_file_pointer ; altered to use dehl
	cp USB_INT_SUCCESS
	ret z					; zero flag set for okay

if DEBUG
	push af
	ld hl, str_seek_fail
	call PRINT_STR
	pop af
	push af
	call PRINT_HEX
	call PRINT_NEWLINE
	pop af

endif
	ld a, 1					; clear zero flag
	ret
; end drive_seek
str_seek_fail:
	db "BIOS: Seek error ", 0


; Write one CP/M record to disk.
; Return a 0 in A if the operation completes properly, and 1 if a write error occurs.
;
write:
	;ld a, (cdisk)
	;ld c, a
	call drive_seek			; calculate disk image file offset
	jr nz, write_fail

	call write_to_file		; write 128 bytes
	;cp 0
	;jr nz, write_fail

	ld a, 0
	ret

write_fail:
	ld a, 1
    ret
;
;
;msh:
;monitor_jump:
	;ld a, ROM_ENABLE
	;out (ROM_CNTL), a
	;call rom_on
	
	;rst $08
	ret
;
str_write_fail:
	db "Write failed\r\n", 0

str_dir_fail: 
	db "Failed to open directory /CPM\r\n", 0
;
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

str_cpm_msg1: 	db 'Configure CH376 module...',13,10,0
;str_cpm_msg2: 	db 'Check CH376 module exists...',13,10,0
;str_cpm_msg3: 	db 'Get CH376 module version...',13,10,0
str_dir_fail3: 	db "Failed to open disk image ",0
;str_cmd_cpm_mounted2:
;			 db " mounted\r\n", 0
NewLine:		db $0d, $0a, 0
signon:
				db 27, '[2J'                    ; clear screen
    			db 27, '[H'                     ; cursor home
    			db 27, '[0m'                    ; clear attributes
    			db 27, '[?25h'                  ; Show cursor   
				db "Z80 Playground [8bitsack.co.uk]", 13, 10   
	;if CBIOS_LENGTH <= $600
	;			db '64K CP/M v2.2 [CCP: $E400  BDOS: $EC00  BIOS: $FA00]', 13, 10
	;else
	;	if CBIOS_LENGTH <= $A00
				db '63K CP/M v2.2 [CCP: $E000  BDOS: $E800  BIOS: $F600]', 13, 10
	;	endif
	;endif
				db "CP/M Copyright Caldera Inc., 1996", 13, 10
				db "CBIOS v1.2 for Z80 Playground, Steve Bradford, 2022", 13, 10
				;
				; ref: Programmers CPM Handbook -figure 7-8
				;
				; BIOS 		BIOS 	DDT 	MOVCPM 	CCP 	BOOS 
				; Length 	Base 	Offset 	'nn'	Base 	Base
				;
				; 600 		FAOO 	2580 	64 		E400	EC00
				; AOO 		F600 	2980 	63 		E000	E800
				; EOO 		F200 	2080 	62		DC00	E400
				; 1200 		EEOO 	3180 	61 		D800	E000
				; 1600 		EAOO 	3580 	60 		D400	DC00
				; 1AOO 		E600 	3980 	59 		D000	D800
				; 1EOO 		E200 	3080 	58 		CC00	D400
				; 2200 		DEOO 	4180 	57 		C800	D000
				; 2600 		DAOO 	4580 	56 		C400	CC00
				; 						ETC.
				;
    			db 0							; end of string marker
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
; function PRINT_CHAR
; Print a single ASCII character
; entry:
;   A ASCII character to print
;
PRINT_CHAR: PUSH    AF
            ;CP      $7E                 
            ;RET     C                  	; A > $7e

.loop1:     IN      A, (UALSR)          ; read Line Status Register           
            BIT     5, A                ; bit set when Transmit Holding register and shift register have emptied
            JP      Z, .loop1           ; loop until ready
            POP     AF
            
            OUT     (UATX), A           ; write character to UART

            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     
; function PRINT_STR
; 	Print null terminated ASCII string
; entry:
;   HL points to string
;
; all registers preserved
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


if DEBUG
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
endif


	include "ch376s_driver.asm"
;
;
;	the remainder of the cbios is reserved uninitialized
;	data area, and does not need to be a Part of the
;	system	memory image (the space must be available,
;	however, between"begdat" and"enddat").
;
begdat:	equ	$	 			; beginning of data area
;prev_sector:
;		defs	2
track:	defs	2			; two bytes for expansion
sector:	defs	2			; two bytes for expansion
dmaad:	defs	2			; direct memory address
cdrive: defs	1			; current drive
cdpb:	defs	2			; current disks dpb address
cdph:	defs	2			; current disks dph address
cspt:	defs	2			; current disks sector per track
;
;	scratch ram area for bdos use

dirbf:	defs	128	 		; scratch directory area
all00:	defs	31	 		; allocation vector 0
all01:	defs	31	 		; allocation vector 1		one bit per block (disk size = 244 / 8 = 31 bytes)
all02:	defs	256	 		; allocation vector 2		one bit per block (disk size = 2048 / 8 = 256 bytes)
all03:	defs	256	 		; allocation vector 3		one bit per block (disk size = 2048 / 8 = 256 bytes)
;all04:	defs	255	 		; allocation vector 4
;chk00:	defs	16			; check vector 0
;chk01:	defs	16			; check vector 1
;chk02:	defs	16	 		; check vector 2
;chk03:	defs	16	 		; check vector 3
;
enddat:	equ	$	 			; end of data area
datsiz:	equ	$-begdat;		; size of data area
CBIOS_LENGTH: equ $-CBIOS_START

	end