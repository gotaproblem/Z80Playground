;*****************************************************************************
; XR - Xmodem Receive for Z80 CP/M 2.2 using CON:
; Copyright 2017 Mats Engstrom, SmallRoomLabs
;
; Licensed under the MIT license
;*****************************************************************************
;
;	Common entry points and locations
;
BOOT:		EQU	$0000		; Warm boot/Reset vector
BDOS:		EQU $0005		; BDOS function vector
DFCB:		EQU	$5c			; Default File Control Block
DFCBcr:		EQU DFCB+32 	; Current record



			include "vars.asm"


;
; BDOS function codes
;
;WBOOT:	 	EQU	0	; System Reset
;GETCON: 	EQU	1	; Console Input A<char
;OUTCON: 	EQU	2	; Console Output E=char
;GETRDR: 	EQU	3	; Reader Input A<char
;PUNCH:	 	EQU	4	; Punch Output E=char
;LIST:	 	EQU	5	; List Output E=char
;DIRCIO: 	EQU	6	; Direct Console I/O E=char/FE/FF A<char
;GETIOB: 	EQU	7	; Get I/O Byte A<value
;SETIOB: 	EQU	8	; Set I/O Byte E=value
;PRTSTR: 	EQU	9	; Print $ String DE=addr
;RDBUFF: 	EQU	10	; Read Console Buffer DE=addr
;GETCSTS:	EQU	11	; Get Console Status A<status (00empty FFdata)
;GETVER: 	EQU	12	; Return Version Number HL<version
;RSTDSK: 	EQU	13	; Reset Disk System
;SETDSK: 	EQU	14	; Select Disk E=diskno
;OPENFIL:	EQU	15	; Open File DE=FCBaddr A<handle (FFerr)
CLOSEFIL:	EQU	16	; Close File DE=FCBaddr A<handle (FFerr)
;GETFST: 	EQU	17	; Search for First DE=FCBaddr A<handle (FFerr)
;GETNXT: E	QU	18	; Search for Next A<handle (FFerr)
DELFILE: 	EQU	19	; Delete File DE=FCBaddr A<handle (FFerr)
;READSEQ:	EQU	20	; Read Sequential DE=FCBaddr A<status (00ok)
WRTSEQ:  	EQU	21	; Write Sequential DE=FCBaddr A<status (00ok)
FCREATE: 	EQU	22	; Make File  DE=FCBaddr A<handle (FFerr)
;RENFILE:	EQU	23	; Rename File DE=FCBaddr A<handle (FFerr)
;GETLOG: 	EQU	24	; Return Log-in Vector HL<bitmap
;GETCRNT:	EQU	25	; Return Current Disk A<diskno
PUTDMA:		EQU	26	; Set DMA Address DE=addr
;GETALOC:	EQU	27	; Get Addr (ALLOC) HL<addr
;WRTPRTD:	EQU	28	; Write Protect Current Disk
;GETROV: 	EQU	29	; Get Read-Only Vector HL<bitmap
;SETATTR:	EQU	30	; Set File Attributes DE=FCBaddr A<handle
;GETPARM:	EQU	31	; Get Addr (DISKPARMS) A<DPBaddr
;GETUSER:	EQU	32	; Set/Get User Code E=code (FFget) A<value
;RDRANDOM:	EQU	33	; Read Random DE=FCBaddr A<status
;WTRANDOM:	EQU	34	; Write Random DE=FCBaddr A<status
;FILESIZE:	EQU	35	; Compute File Size DE=FCBaddr
;SETRAN: 	EQU	36	; Set Random Record DE=FCBaddr
;LOGOFF: 	EQU	37	; Reset Drive DE=drivevector
;WTSPECL:	EQU	40	; Write Random with Zero Fill DE=FCBaddr A<status

;
; ASCII codes
;
SOH:		EQU	1;'A'-40h	; ^A CTRL-A
EOT:		EQU	4;'D'-40h	; ^D = End of Transmission
ACK:		EQU	6;'F'-40h	; ^F = Positive Acknowledgement
NAK:		EQU	21;'U'-40h	; ^U = Negative Acknowledgement
CAN:		EQU	24;'X'-40h	; ^X = Cancel
ETB:		EQU 23; End of Transmission Block
C_:			EQU 'C'; Indicate use of CRC method


;
; Start of code
;
			ORG 0100h

	ld sp, stack

	di
	
	in a, (UAMCR)
	and %11111110
	out (UAMCR), a			; turn ULED1 off

	ld HL, str_signon		; Print a greeting
	call PrintString0

	ld A, (DFCB+1)			; Check if we got a filename
	cp ' '
	jp Z, NoFileName

	call DeleteFile			; delete file if already exists

	ld DE, DFCB				; Then create new file
	ld A, 0					; Start at block 0
	ld (DFCBcr), A
	ld C, FCREATE
	call BDOS				; Returns A in 255 if error opening
	inc A
	jp Z, FailCreateFile

	ld A, 1					; The first packet is number 1
	ld (pktNo), A
	ld A, 255-1				; Also store the 1-complement of it
	ld (pktNo1c), A

GetNewPacket:		
	ld A, 40				; We retry 40 times before giving up
	ld (retrycnt), A

NPloop:
	ld A, 3					; 3 Seconds of timeout before each new block
	call GetCharTmo
	jp NC, NotPacketTimeout

	ld HL, retrycnt			; Reached max number of retries?
	dec (HL)
	jp Z, Failure			; Yes, print message and exit

	ld C, NAK				; Send a NAK to the uploader
	call CONOUT
	jp NPloop

NotPacketTimeout:
	cp	EOT					; Did uploader say we're finished?
	jp	Z, Done				; Yes, then we're done

	cp 	CAN					; Uploader wants to abort transfer?
	jp 	Z, Cancelled		; Yes, then we're also done

	cp ETB					; End of Transmission Block
	jp Z, Done

	cp	SOH					; Did we get a start-of-new-packet?
	jp	NZ, NPloop			; No, go back and try again

	ld	HL, packet			; Save the received char into the...
	ld	(HL),A				; ...packet buffer and...
	inc 	HL				; ...point to the next location
	ld 	B, 131				; Get 131 more characters for a full packet
GetRestOfPacket:
	push 	BC
	ld 	A, 1				; timeout value in seconds between characters
	call GetCharTmo
	jp c, transmission_error
	pop 	BC
	ld	(HL), A				; ...packet buffer and...
	inc 	HL				; ...point to the next location
	djnz GetRestOfPacket
	
	ld	HL, packet+3		; Calculate checksum from 128 bytes of data
	ld	B, 128
	ld	A, 0
csloop:	
	add	A, (HL)				; Just add up the bytes
	inc	HL
	djnz csloop

	xor	(HL)				; HL points to the received checksum so
	jp	NZ, Failure			; by xoring it to our sum we check for equality

	ld	A, (pktNo)			; Check if agreement of packet numbers
	ld	C, A
	ld	A, (packet+1)
	cp	C
	jp	NZ, Failure

	ld	A, (pktNo1c)		; Check if agreement of 1-compl packet numbers
	ld	C, A
	ld	A, (packet+2)
	cp	C
	jp	NZ, Failure

	ld	DE, packet+3		; Reset DMA address to the packet data buff
	ld 	C, PUTDMA
	call BDOS
	ld  DE, DFCB			; File Description Block
	ld 	C, WRTSEQ
	call BDOS				; Returns A=0 if ok
	cp	0
	jp	NZ, FailWrite

	ld	HL, pktNo			; Update the packet counters
	inc (HL)
	ld	HL, pktNo1c
	dec	(HL)

	ld 	C, ACK				; Tell uploader that we're happy with the
	call CONOUT				; packet and go back and fetch some more
	jp	GetNewPacket

transmission_error:
	pop BC					; sync stack
	ld c, NAK
	call CONOUT
	jp GetNewPacket

DeleteFile:
	ld  DE, DFCB			; Delete file first
	ld 	C, DELFILE			;
	CALL BDOS				; Returns A=255 if error, but we don't care
	ret

CloseFile:
	ld  DE, DFCB			; Close the file
	ld 	C, CLOSEFIL
	call BDOS
	ret

Done:
	call CloseFile
	ld	C, ACK				; Tell uploader we're done
	call CONOUT
	ld 	HL, msgSucces1		; Print success message and filename
	call PrintString0
	call PrintFilename
	ld 	HL,msgSucces2
	call PrintString0
	jp Exit

FailCreateFile:
	ld	HL, msgFailCre
	call PrintString0
	call PrintFilename
	ld	HL, msgCRLF
	jp Exit

FailWrite:
	ld	HL, msgFailWrt
	jp	Die

NoFileName:
	ld 	HL, msgNoFile
	call PrintString0
	jp	Exit

Failure:
	ld 	HL, msgFailure
	jp	Die

Cancelled:
	ld 	HL, msgCancel
	jp	Die

Die:
	call 	PrintString0	; Prints message and exits from program
	call	CloseFile
	call	DeleteFile
Exit:
	;ld	SP,(oldSP)
	jp 0					; warm boot after
	;ret


;
; Waits for up to A seconds for a character to become available and
; returns it in A without echo and Carry clear. If timeout then Carry
; it set.
;
GetCharTmo:
	ld b, a					; x seconds timeout
	ld c, 0
timer3: 
	push bc
	ld b, 3					; 3 = 1 second timeout
timer2:						; seconds timer
	ld de, 32051			; 32051 * 10.4us = 0.3333304s
timer1:						; fine timer = number of clock cycles * CPU fq period
							; 10MHz clock = 100ns per tick
							; 4MHz clock = 250ns per tick
	call CONST				; 46 ticks
	cp $ff					; char available?
	jp z, GotChar			; yes, then exit loop

	dec de
	ld a, d
	or e
	jp nz, timer1

	djnz timer2

	pop bc
	djnz timer3

	scf 					; Set carry = signals timeout
	ret

GotChar:
	pop	bc					; sync stack
	call CONIN
	or a 					; Clear Carry = signals success
	ret


;
; Print message pointed top HL until 0
;
PrintString0:
	ld	A,(HL)
	or	A					; Check if got zero?
	ret	Z					; If zero return to caller
	ld 	C,A
	call	CONOUT			; else print the character
	inc	HL
	jp	PrintString0


;
; Prints the 'B' bytes long string pointed to by HL, but no spaces
;
PrintNoSpaceB:
	push	BC
	ld	A,(HL)				; Get character pointed to by HL
	ld	C,A
	cp	' '					; Don't print spaces
	call	NZ,CONOUT
	pop	BC
	inc	HL					; Advance to next character
	djnz	PrintNoSpaceB	; Loop until B=0
	ret
;
;
;
PrintFilename:
	ld	A,(DFCB)			; Print the drive
	or	A					; If Default drive,then...
	jp	Z,PFnoDrive			; ...don't print the drive name
	add	A,'@'				; The drives are numbered 1-16...
	ld	C,A					; ...so we need to offset to get A..P
	call	CONOUT

	ld	C,':'				; Print colon after the drive name
	call	CONOUT

PFnoDrive:
	ld	HL,DFCB+1	; Start of filename in File Control Block
	ld	B,8					; First part is 8 characters
	call	PrintNoSpaceB

	ld	C,'.'				; Print the dot between filname & extension
	call	CONOUT

	ld 	B,3					; Then print the extension
	call	PrintNoSpaceB
	ret

;
; BIOS and BDOS console routines interfere with the data in several ways
; so create our own console IO here
;
CONIN:
.loop:      
	IN A, (UALSR)           ; read Line Status Register      
	ld b, a

	and $1e					; look for errors in Rx
	cp 0
	jr nz, rx_error

	ld a, b     
	BIT 0, A                 ; bit set when Rx Holding register 
	JP Z, .loop

	IN A, (UARX)
	RET
rx_error:
	in a, (UAMCR)
	or %00000001
	out (UAMCR), a			; turn ULED1 on if Rx error
	ret

CONOUT: 
.loop1:     
	IN A, (UALSR)           ; read Line Status Register           
	BIT 5, A                 ; bit set when Transmit Holding register and shift register have emptied
	JP Z, .loop1            ; loop until ready
	
	ld a, c
	OUT (UATX), A            ; write character to UART
	RET

CONST:
	IN A, (UALSR)           ; read Line Status Register           
	BIT 0, A                ; bit set when Rx Holding register 
	jp z, .no_char

	ld a, $FF				; else have a char ready to read
	ret

.no_char:
	ld a, 0
	ret
;
; Message strings
;
str_signon:
    db "Xmodem Receive: v1.1 October 2021, Steve Bradford\r\n"
    db "Z80 Playground [8bitStack.co.uk]\r\n\n"
    db 0
msgFailWrt:	DB	CR,LF,'Failed writing to disk',CR,LF,0
msgFailure:	DB	CR,LF,'Transmission failed',CR,LF,0
msgCancel: 	DB	CR,LF,'Transmission cancelled',CR,LF,0
msgSucces1:	DB	CR,LF,'File ',0
msgSucces2:	DB	' received successfully',CR,LF,0
msgFailCre:	DB	'Failed creating file named ',0
msgNoFile: 	DB	'Filename expected',CR,LF,0
msgCRLF:   	DB	CR,LF,0

;
; Variables
;
oldSP:	 	DS	2	; The orginal SP to be restored before exiting
retrycnt:	DS 	1	; Counter for retries before giving up
chksum:	 	DS	1	; For claculating the ckecksum of the packet
pktNo:		DS 	1 	; Current packet Number
pktNo1c: 	DS 	1 	; Current packet Number 1-complemented
packet:	 	DS 	1	; SOH
	 		DS	1	; PacketN
	 		DS	1	; -PacketNo,
	 		DS	128	; data*128,
	 		DS	1 	; chksum
;CRC16:	DW 1

	 		DS 	64	; stack space 32 addresses
stack: 	EQU $

			END