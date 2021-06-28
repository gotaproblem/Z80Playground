;	ref.
;	skeletal CBIOS for first level of CP/M 2.0 alteration
;
; *************************************
; * CBIOS                             *
; * Z80 Playground                    *
; * Steve Bradford                    *
; * 16/06/2021                        *
; *                                   *
; *                                   *
; *                                   *
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

DEBUG: DEFL 0

;
; 16C550 Dual UART Registers
;
; General Register Set
; only available when LCR is set to $00 (bit 7 = 0)
UIER:       EQU     1               	; Interrupt Enable Register friendly name
UFCR:       EQU     2               	; FIFO Control Register friendly name
ULCR:       EQU     3               	; Line Control Register friendly name
UMCR:       EQU     4               	; Modem Control Register friendly name
ULSR:       EQU     5               	; Line Status Register friendly name
UMSR        EQU     6               	; Modem Status Register friendly name
USPR:       EQU     7               	; Scratchpad Register friendly name


; Baud Rate Register Set
; only available when LCR is set to $80 (bit 7 = 1)
UDLL:       EQU     0                   ; LSB of Divisor Latch (rw)
UDLM:       EQU     1                   ; MSB of Divisor Latch (rw)

; Enhanced Register Set
; only accessible when LCR is set to $bf
UFR2:       EQU     2                   ; Enhanced Feature register (rw)
UFR4:       EQU     4                   ; XON1 word (rw)
UFR5:       EQU     5                   ; XON2 word (rw)
UFR6:       EQU     6                   ; XOFF1 word (rw)
UFR7:       EQU     7                   ; XOFF2 word (rw)


NP81:       EQU     $03                 ; UART No Parity, 8 data bits, 1 stop bit


; UARTs IO Port Address
UARTA:      EQU     $08                ; mapped to IO Port 


; UART Register Address offsets
UAIER:      EQU     UARTA + UIER
UALCR:      EQU     UARTA + ULCR
UALSR:      EQU     UARTA + ULSR
UAMCR:		EQU		UARTA + UMCR
UAMSR:		EQU		UARTA + UMSR
UAFCR:		EQU		UARTA + UFCR
UASPR:      EQU     UARTA + USPR
UADLL:      EQU     UARTA + UDLL
UADLM:      EQU     UARTA + UDLM
UATX:       EQU     UARTA + 0
UARX:       EQU     UARTA + 0
;
ENABLE      EQU     $01
DISABLE     EQU     $00
ROM_DISABLE EQU     1
ROM_ENABLE  EQU     0
MEM_CNTL    EQU     UAMCR 
ROM_CNTL    EQU     MEM_CNTL; enable or disable ROM
ULED1       EQU		UAMCR  	; playground bit 2
;
;
;
; Custom BIOS (CBIOS)
;
ccp:	equ	$dc00 			; base of ccp ($dc00 = 56k system)
bdos:	equ	$e406 			; bdos entry
bios:	equ	$f200 			; base of bios
cdisk:	equ	0004h			; current disk number 0=a,... l5=p
iobyte:	equ	0003h			; i/o byte
disks:	equ	04h				; number of disks in the system
;
		org	bios			; origin of this program
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
;	fixed data tables for four-drives 
;   standard ibm-compatible 8" disks use skew of 6 (drives A & B)
;
;	disk Parameter header for disk 00
dpbase:	
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
	defw	dirbf, dpblk2
	defw	0000, all02
;	disk parameter header for disk 03
	defw	0000h, 0000h
	defw	0000h, 0000h
	defw	dirbf, dpblk2
	defw	0000, all03
;
;	sector translate vector
trans:	
	defm	 1,  7, 13, 19	;sectors  1,  2,  3,  4
	defm	25,  5, 11, 17	;sectors  5,  6,  7,  6
	defm	23,  3,  9, 15	;sectors  9, 10, 11, 12
	defm	21,  2,  8, 14	;sectors 13, 14, 15, 16
	defm	20, 26,  6, 12	;sectors 17, 18, 19, 20
	defm	18, 24,  4, 10	;sectors 21, 22, 23, 24
	defm	16, 22			;sectors 25, 26
;
dpblk1:	;disk parameter block for disks A & B 250KB - skew 6, 75 + 2 tracks = IBM PC SSSD 8".
	defw	26		;sectors per track
	defm	3		;block shift factor
	defm	7		;block mask
	defm	0		;null mask
	defw	243		;disk size-1
	defw	63		;directory max
	defm	192		;alloc 0
	defm	0		;alloc 1
	defw	0		;check size
	defw	2		;track offset
;
dpblk2:	;disk parameter block for 8MB - 1024 tracks
	defw	64		;sectors per track
	defm	4		;block shift factor = block size of 2048 bytes
	defm	15		;block mask
	defm	0		;null mask
	defw	4095	;disk size-1
	defw	1023	;directory max
	defm	255		;alloc 0
	defm	255		;alloc 1
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
 
    call check_module_exists
	ld hl, str_cpm_msg3
    call PRINT_STR
    
    call get_module_version

	call configure_memorystick
	ld hl, str_cpm_msg2
    call PRINT_STR

	;ld hl, cpm_fileroot     ; open root directory /CPM
    ;call open_file
	;cp ERR_OPEN_DIR			; this is actually good
	;jr nz, dir_open_failed

	;ld hl, cpm_driveA
	;call open_file
	;cp USB_INT_SUCCESS		; file opened okay
	;jr z, boot_real

disk_image_open_failed:
	;ld hl, str_disk_image_open_fail
	;call PRINT_STR

	;halt

dir_open_failed:
	;ld hl, str_dir_fail
	;call PRINT_STR
	
	;halt					; TODO - better to return to monitor

boot_real:
	ld hl, signon
	call PRINT_STR

	ld hl, cpm_driveA		; starting disk image filename = "/A.DSK"
	ld de, cpm_driveX
	ld bc, 7
	ldir					; make a copy of the disk image filename

	xor	a		     		; zero in the accum
	ld	(iobyte), a			; clear the iobyte
	ld a, 1					; setting drive to 1 (B:) triggers initialise
	ld	(cdisk), a			; 
	jp	gocpm				; initialize and go to cp/m
;
wboot:
	ld	sp, $80				; use space below buffer for stack
	ld a, (cdisk)
	ld c, a
	;ld 	c, 0				; select disk 0
	call seldsk
	call home	    		; go to track 00, sector 1
;

; load CP/M from ROM (CCP only)
; this could be put on drive A's boot tracks
	in a, (UAMCR)
	and %11110111
	out (UAMCR), a
	ld a, ROM_ENABLE

	ld hl, $1000			; this address needs to match main.asm CBIOS load point in ROM
	ld de, ccp
	ld bc, $0800
	ldir

	in a, (UAMCR)
	or %00001000
	out (UAMCR), a
	ld a, ROM_DISABLE
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
	ld	bc, $80				; default dma address is 80h
	call setdma
;
							; as this is only done at a cold boot, we are always using drive A
	;ld	a, (cdisk)			; get current disk number
    ld 	c, 0;a				; send to the ccp
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
	LD     bc, 0			; select track 0
	call   settrk
	ld	   bc, 1
	call   setsec
	ret			    		; we will move to 00 on first read/write
;
;
seldsk:	;select drive given by register c	
	ld hl, 0000h			; error return code
	ld a, c
	cp disks				; must be between 0 and 3
	ret nc					; no carry if >= 4

	; optimise by only doing the following if a change of disk
	;ld a, (cdisk)
	;cp c
	;jp z, seldsk_optimise
	
	;call close_file

	ld (cdisk), a			; selected disk is valid

	ld hl, cpm_driveX
	add a, 'A'				; add real disk number to filename (eg. /A.DSK becomes /B.DSK)
	inc hl
	ld (hl), a				; ammend current disk image filename	

;	disk number is in the proper range
;	compute proper disk parameter header (DPH) address
	LD 	l, c				; l=disk number 0, 1, 2, 3
	LD 	h, 0				; high order zero
	ADD	HL,HL				; *2
	ADD	HL,HL				; *4
	ADD	HL,HL				; *8
	ADD	HL,HL				; *16 (size of each header)
	LD	DE, dpbase
	ADD	HL,DE				; hl = dpbase (diskno*16)
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




	;ld hl, cpm_fileroot     ; open root directory /CPM
    ;call open_file

	;ld hl, cpm_driveX
	;call open_file


	

seldsk_optimise:
	ld hl, (cdph)					
	ret						; return hl = address of dph
;
;
settrk:	;set track given by register c - TODO should this be bc?
	ld (track), bc
	ret
;
;
setsec:	;set sector given by register c
	ld (sector), bc
	ret
;
;
sectran:
	;translate the sector given by bc using the
	;translate table given by de
	EX DE, HL				; hl=.trans
	ADD	HL, BC				; hl=.trans (sector)

	ld a, (cdisk)
	cp 2
	ret nc					; only the first two drives use skew in this CP/M implementation
	
	LD l, (hl)				; l=trans (sector)
	LD h, 0					; hl=trans (sector)	
	ret			    		; with value in hl
;
;
setdma:	;setdma address given by registers b and c
	ld l, c					; low order address
	ld h, b					; high order address
	ld (dmaad), hl			; save the address
	ret
;
; Read one CP/M sector from disk.
; Return a 00h in register a if the operation completes properly, and 0lh if an error occurs during the read.
;
;
read:
	;ld hl, cpm_fileroot     ; open root directory /CPM
    ;call open_file

	ld hl, cpm_driveX
	call open_file
	cp USB_INT_SUCCESS		; file opened okay
	jr z, read_cont

if DEBUG
	call PRINT_HEX			; print error code
	call PRINT_NEWLINE
	ld hl, str_dir_fail3
	call PRINT_STR
	ld hl, cpm_driveX		; print current disk image filename
	call PRINT_STR
	call PRINT_NEWLINE
endif
	jr read_fail

read_cont:
if DEBUG
	ld hl, str_read_t
	call PRINT_STR
	ld de, (track)
	ld a, d
	call PRINT_HEX
	ld a, e
	call PRINT_HEX
	ld hl, str_read_s
	call PRINT_STR
	ld de, (sector)
	ld a, d
	call PRINT_HEX
	ld a, e
	call PRINT_HEX
	call PRINT_NEWLINE
endif
	call drive_seek			; calculate disk image file offset
	
	ld hl, (dmaad)
	call read_from_file		; read 128 bytes
	cp 0
	jr nz, read_fail

	call close_file
	ld a, 0
	ret

read_fail:
	call close_file
if DEBUG
    ld hl, str_read_fail
	call PRINT_STR
endif
	ld a, 1
    ret

str_read_fail:
	db "Read failed\r\n", 0 ;, track ", 0
str_read_t:
	db "Read - track ", 0
str_read_s:
	db ", sector ", 0

drive_seek:
	ld bc, (cspt)			; cspt set by setsec
	
	; calculate number of cpm records (128 byte records)
	; cpmrec = (spt * track) + sector

	; need to subtract 1 from sector number for the following to work
	; i.e. sector number needs to start from 0

	ld de, (track)
	call DE_Times_BC		; track * spt (de * bc) returns answer in dehl
	
	ld bc, (sector)
	dec bc					; sector - 1
	add hl, bc				; add sector to hl
	
	; hl is number of cpm records (128 bytes each)
	; seek offset needs to be in bytes, multiply by 128

	; dehl = (spt * track) + (sector - 1)
	; now need to multiply by 128 to get byte offset
	ex de, hl
	ld bc, 128				; de * 128
	call DE_Times_BC		; result in dehl

	call move_to_file_pointer ; altered to use dehl
	cp USB_INT_SUCCESS
	jr nz, disk_seek_fail

	ret
; end drive_seek

disk_seek_fail:
	ld hl, str_seek_fail
	call PRINT_STR

	ret

str_seek_fail:
	db "Seek failed \r\n", 0
str_seek:
	db "Seeking\r\n", 0


; Write one CP/M record to disk.
; Return a 00h in register a if the operation completes properly, and 0lh if an error occurs during the write.
;
write:
	;ld hl, cpm_fileroot     ; open root directory /CPM
    ;call open_file
	ld hl, cpm_driveX
	call open_file
	cp USB_INT_SUCCESS
	jr z, write_cont
	
if DEBUG
	call PRINT_HEX			; print error code
	call PRINT_NEWLINE
	ld hl, str_dir_fail3
	call PRINT_STR
	ld hl, cpm_driveX		; get saved disk image filename
	call PRINT_STR
	call PRINT_NEWLINE
endif
	jr write_fail

write_cont:
	call drive_seek			; calculate disk image file offset

	ld hl, (dmaad)
	call write_to_file		; write 128 bytes
	cp 0
	jr nz, write_fail

	call close_file
	ld a, 0
	ret

write_fail:
if DEBUG
    ld hl, str_write_fail
	call PRINT_STR
endif
	call close_file
	ld a, 1
    ret
;
;
;
msh:
monitor_jump:
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
str_disk_image_open_fail:
	db "Failed to open A.DSK\r\n", 0
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
str_cpm_msg2: 	db 'Check CH376 module exists...',13,10,0
str_cpm_msg3: 	db 'Get CH376 module version...',13,10,0
str_dir_fail3: 	db "Failed to open disk image ",0
;str_cmd_cpm_mounted2:
;			 db " mounted\r\n", 0
NewLine:		db $0d, $0a, 0
signon:
				db 27,'[2J'                     ; clear screen
    			db 27,'[H'                      ; cursor home
    			db 27,'[0m'                     ; clear attributes
    			db 27,'[?25h'                   ; Show cursor   
				db "Z80 Playground [8bitsack.co.uk]", 13, 10   
    			db '64K CP/M v2.2 [CBIOS v1.0 for Z80 Playground]', 13, 10
				db "CP/M Copyright Caldera Inc., 1996", 13, 10
				db "CBIOS Steve Bradford, 2021", 13, 10
    			db 13, 10
    			db 0
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



	include "memorystick.asm"
;
;
;	the remainder of the cbios is reserved uninitialized
;	data area, and does not need to be a Part of the
;	system	memory image (the space must be available,
;	however, between"begdat" and"enddat").
;
track:	defs	2			; two bytes for expansion
sector:	defs	2			; two bytes for expansion
dmaad:	defs	2			; direct memory address
cdpb:	defs	2			; current disks dpb address
cdph:	defs	2			; current disks dph address
cspt:	defs	2			; current disks sector per track
;
;	scratch ram area for bdos use
begdat:	equ	$	 			; beginning of data area
dirbf:	defs	128	 		; scratch directory area
all00:	defs	31	 		; allocation vector 0
all01:	defs	31	 		; allocation vector 1
all02:	defs	255	 		; allocation vector 2
all03:	defs	255	 		; allocation vector 3
chk00:	defs	16			; check vector 0
chk01:	defs	16			; check vector 1
;chk02:	defs	16	 		; check vector 2
;chk03:	defs	16	 		; check vector 3
;
enddat:	equ	$	 			; end of data area
datsiz:	equ	$-begdat;		; size of data area

	end