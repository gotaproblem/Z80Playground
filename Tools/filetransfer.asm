;
; filetransfer.asm
;
; function TFER
; read FAT files and write them to CP/M
;
;
; Files to be transferred, must be placed in folder /TFER on USB memory drive
;
; syntax
;   tfer
;
CDRIVE      equ $4          ; current disk drive
BDOS        equ $5          ; BDOS jump vector
BOOT        equ $0          ; CP/M warm boot vector
FCB         equ $5c         ; default system FCB for transient programmes

; define BDOS functions
;
C_READ      equ $01         ; console input
C_WRITE     equ $02         ; console output
C_WRITESTR  equ $09         ; output string, '$' terminated, to console output
C_READSTR   equ $0a         ; buffered console input
;
DRV_SET     equ $0e         ; select drive
F_OPEN      equ $0f         ; open file
F_CLOSE     equ $10         ; close file
F_DELETE:   equ $13         ; delete file
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
    ld sp, stack            ; set programme stack space

    ld hl, str_signon
    call PRINT_STR

    ; list /TFER directories contents
    ld hl, tfer_home
    call open_file          ; open directory /TFER
    cp ERR_OPEN_DIR
    jp nz, openFail

    ld hl, str_read_dir
    call PRINT_STR

    ld hl, STAR_DOT_STAR    ; select all files
    call open_file
    ld b, 0                 ; file counter
    ld de, FILELIST         ; buffer for file list - 1k holds 85 filenames
    jp read_dir

openFail:
    ld hl, str_open_fail
    call PRINT_STR
    jp exit

str_open_fail:
    db "Open failed on /TFER$"
read_dir:
    push bc                 ; save file count
    push de                 ; save filelist offset
    
    ld a, RD_USB_DATA0
    call send_command_byte
    call read_data_byte     ; returns number of bytes to read
    cp 32                   ; should have at least 32 bytes?
    jp c, done              ; no, then finished
    
    call read_data_bytes_into_buffer ; read them into buffer

    ld hl, disk_buffer
    ld a, (hl)
    cp '.'
    jp z, carry_on          ; if filename starts with a dot then skip (./ ../)

    cp '_'                  ; MacOS problem. I seem to get duplicate files on a USB copy, prefixed with an underscore
    jp z, carry_on          ; if filename starts with an underscore then skip

valid_file:
    pop de                  ; get filelist offset
    ld hl, disk_buffer
    ld bc, 11
    ldir                    ; copy filename [8+3] from disk_buffer

    pop bc
    inc b                   ; increment file counter
    
    ld a, 0
    ld (de), a              ; string termination
    inc de

    push bc                 ; save file count
    push de                 ; update filelist position
    
    ld hl, str_tab
    call PRINT_STR          ; indent file list
    ld h, d
    ld l, e
    ld bc, 12
    sbc hl, bc              ; point HL to previous filename
    call PRINT_FILENAME     ; print filename
    call PRINT_NEWLINE
carry_on:
    ld a, FILE_ENUM_GO      ; look for next directory entry
    call send_command_byte
    call read_status_byte
    pop de
    pop bc
    cp USB_INT_DISK_READ    ; there is one, so move to next entry
    jp z, read_dir

    ;cp ERR_MISS_FILE        ; end of directory
    jp done_cont

done:
    pop de                  ; sync stack
    pop bc                  ; sync stack
    ld a, b
    cp 0
    jp nz, done_cont

    ld hl, str_tfer_empty
    call PRINT_STR
    jp exit

done_cont:
    push bc                 ; save file count
    ld hl, str_copy
    call PRINT_STR
    
    call getc               ; check for copy confirmation
    jp c, exit              ; has CONTROL-C been entered?
   
    cp 'Y'
    jp z, do_copy

    cp 'y'
    jp z, do_copy

    jp exit

do_copy:
    pop bc                  ; B should equal number of files
    call PRINT_NEWLINE
    call clearFCB
    ld de, FILELIST         ; start of filelist

copy_loop:
    push bc                 ; save loop counter = number of files to go
    push de                 ; save filelist pointer offset (current filename)

    ld hl, filename_buffer
    ld a, 0
    ld (hl), a
    ld de, filename_buffer+1
    ld bc, 19
    ldir                    ; clear filename_buffer every pass

    ; open FAT file - filelist

    ld hl, tfer_home
    call open_file          ; open /TFER

    pop hl                  ; HL = DE = FILELIST position
    push hl                 ; actually saving DE for later
    call build_filename     ; returns corrected filename (8.3) in filename_buffer

    ; TODO if no extension, remove dot

    ld hl, filename_buffer
    call open_file          ; should open /TFER/xxxxxxxx.xxx
    cp USB_INT_SUCCESS
    jp nz, file_open_FAT_fail

    ld hl, filename_buffer
    call PRINT_FILENAME
    call PRINT_SPACE
    call file_size          ; print FAT files size

    ; open / create CP/M file
                            ; if CP/M file already exists, then delete it
                            ; and create new file
    pop de                  ; get filelist pointer offset
    push de
    call buildFCB           ; create CP/M FCB for the new file

    ; read FAT file / write CP/M file loop
    ld bc, $0000
    ld (FAToffset), bc      ; CP/M records count
rwloop:    
    ; read FAT file
    ld hl, tfer_home
    call open_file          ; open /TFER
    ld hl, filename_buffer
    call open_file          ; open FAT file

    call FATseek            ; seek to file offset

    ld hl, (FAToffset)
    inc hl
    ld (FAToffset), hl      ; save number of 128 byte records

    ld de, dmaBuffer
    call read_from_file     ; read 128 bytes
    jp z, WriteSector
    
    cp USB_INT_SUCCESS+1
    jp z, lastRead          ; reached EOF
    
    call PRINT_HEX          ; else error
    call PRINT_NEWLINE
    jp FATread_fail

    ; write CP/M file
WriteSector:
 	ld de, dmaBuffer		; Set CP/M DMA address to the FAT file buffer
	ld c, F_DMAOFF
	call BDOS

	ld de, FCB	            ; File Description Block
	ld c, F_WRITE
	call BDOS		        ; Returns A=0 if ok, A=1 if EOF
                            ; don't care about returned status

    jp rwloop               ; more data
    ; end read / write loop

lastRead:
    ld de, FCB		        ; close CP/M file
	ld c, F_CLOSE
	call BDOS

    pop de                  ; FILELIST offset pointer
    ld hl, 11
    adc hl, de              ; point to next filename
    ld d, h
    ld e, l                 ; de = de + 11
    
    pop bc                  ; get file count
    dec b
    ld a, b
    cp 0
    jp nz, copy_loop        ; another file to copy
    ; end file copy loop

    jp exit1                ; all finished

;
; Error traps
;
FATread_fail:
    ld hl, str_FATread_fail
    call PRINT_STR
    ld hl, filename_buffer
    call PRINT_FILENAME
    call PRINT_NEWLINE
    pop de                  ; stack sync
    pop bc                  ; stack sync
    call closeFile

    jp exit1

str_FATread_fail:
    db "file read error on $"


;
; function closeFile
; close CP/M file    
closeFile:
	ld de, FCB		        ; Close the file
	ld c, F_CLOSE
	call BDOS
    
    ;jp exit1                 ; return back to CP/M
    ret
; end function
;


;
; function file_open_FAT_file
;
file_open_FAT_fail:
    ld hl, str_FATopen_fail
    call PRINT_STR
    pop de
    ex de, hl
    call PRINT_FILENAME
    call PRINT_NEWLINE
    pop bc                  ; stack sync
    jp exit1

str_FATopen_fail:
    db "Failed to open file $"
exit:
    pop bc
exit1:
    ;ld a, (CDRIVE)               ; get current drive number
    ;ld e, a
    ;ld c, DRV_SET           ; restore logged in drive
    ;call BDOS
    ;ld	sp, (CPMSP)
    ;ret
    jp 0
; end function
;


;
; function FATseek
; move to open FAT file position
;
; on exit
;   BC, DE, HL destroyed
;
;
FATseek:
	ld bc, (FAToffset)		; 
	ld de, 128				; BC * 128
	call DE_Times_BC		; result in dehl

	call move_to_file_pointer
	cp USB_INT_SUCCESS
	ret z

	ld hl, str_seek_fail
	call PRINT_STR
	ret
; end function
;
str_seek_fail:
	db "Seek failed \r\n", 0


;
; function move_to_file_pointer
; Set the BYTE_LOCATE file position in the currently open file.
; Value is passed in dehl, hl = low word, de = high word
move_to_file_pointer:
    ld a, BYTE_LOCATE
    call send_command_byte
    ld a, l
    call send_data_byte
    ld a, h
    call send_data_byte
    ld a, e
    call send_data_byte
    ld a, d
    call send_data_byte
    call read_status_byte
    
    ret
; function end
;


;
; function DE_times_BC
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
;


;
; function build_filename
; on entry
;   HL points to FILELIST offset for filename
;
; on exit
;   DE points to corrected filename stored in filename_buffer
;
; A, B, DE, HL destroyed
;
build_filename:
    ; remove spaces from filename and add '.'
    ld b, 11
    ld de, filename_buffer
nextChar:
    ld a, b
    cp 3                    ; add a dot for file extension
    jp nz, notExt

    ld a, '.'
    ld (de), a
    inc de
notExt:
    ld a, (hl)
    cp ' '
    jp z, ignoreSpace

    ld (de), a
    inc de
ignoreSpace:
    inc hl
    djnz nextChar           ; continue to build filename

    ret
; end function
;


;
; function file_size
; on exit
;   DEHL = 32 bit file size = HL low 16bits, DE high 16bits
;
;   A destroyed
;
file_size:
    push bc
    push de
    push hl
    ld a, GET_FILE_SIZE     ; get FAT files file size
    call send_command_byte
    ld a, $68
    call send_data_byte

    call read_data_byte     ; byte 0 = low byte
    ld l, a
    
    call read_data_byte     ; byte 1
    ld h, a
    
    call read_data_byte     ; byte 2
    ld e, a
    
    call read_data_byte     ; byte 3 = high byte
    ld d, a
    
    call B2D32              ; DEHL = 32bit number to convert
                            ; HL = null terminated ASCII string to print
    call PRINT_STR
    ld hl, str_file_size_bytes
    call PRINT_STR
    pop hl
    pop de
    pop bc
    ret
; end function
;
str_file_size_bytes:
    db " Bytes\r\n$"


;
; function getc
; hl = temp buffer pointer
getc:                       ; read input
    ld c, C_READ
    call BDOS

    cp CNTRL_C
    jp z, .cc               ; abort program and warm BOOT

    ; TODO check entered string for valid filename format 8.3
    ret

.cc:
    or a
    scf
    ret
; end function
;



str_dir_fail:
    db "Dir read failed\r\n$"
str_read_dir:
    db "TransFER Directory:\r\n$"
str_tfer_empty:
    db "   Empty\r\n\n$"
str_tab:
    db "   $"
str_copy:
    db "Copy all files [Y/N]: $"
str_write_fail:
    db "CP/M Write failed\r\n$"
str_signon:
    db "File Transfer: v1.0 June 2021, Steve Bradford\r\n"
    db "Z80 Playground [8bitStack.co.uk]\r\n\n"
    db '$'

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
;


;
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
;


;
; function PRINT_SPACE
PRINT_SPACE:
    ld a, ' '
    call PRINT_CHAR
    ret
; end function
;


;
; function PRINT_FILENAME
;
; on entry
;   HL = pointer to filename to print
;
PRINT_FILENAME:
    push hl
    push de
    push bc

.print_fn_loop:
    ld a, (hl)
    cp 0
    push af
    call PRINT_CHAR
    inc hl
    pop af
    jp nz, .print_fn_loop

    pop bc
    pop de
    pop hl
    ret
; end function
;


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
; end function
;





; memorystick_low_level.asm
;--------------------;
; LOW LEVEL ROUTINES ;
;--------------------;

create_file:
    ; pass in DE = pointer to filename
    push de
    ld a, SET_FILE_NAME
    call send_command_byte
    pop hl
    call send_data_string

    ld a, FILE_CREATE
    call send_command_byte

    call read_status_byte
    cp USB_INT_SUCCESS
    ret

open_file:
    ; Tells the module to use the filename from the filename_buffer.
    ; Returns z=true if ok to proceed.
    ; Pass in hl -> directory string, e.g. "/folder"
    ld a, SET_FILE_NAME
    call send_command_byte
    call send_data_string
    ld a, FILE_OPEN
    call send_command_byte
    call read_status_byte
    ;cp USB_INT_SUCCESS
    ret

close_file:
    ld a, FILE_CLOSE
    call send_command_byte
    ld a, 1                             ; 1 = update file size if necessary
    call send_data_byte
    call read_status_byte
    ret


read_from_file:
    ; Ask to read 128 bytes from the current file into the dma_address area pointed to by DE.
    ; Returns Zero flag set for success, clear for fail.
    push de
    ld a, BYTE_READ
    call send_command_byte
    ld a, 128                           ; Request 128 bytes
    call send_data_byte
    ld a, 0
    call send_data_byte
    call read_status_byte

read_from_file1:
    cp USB_INT_DISK_READ                    ; This means "go ahead and read"
    jr z, read_from_file3
    cp USB_INT_SUCCESS                      ; Bizarrely this means we are finished
    ;jp z, read_from_file_cannot
    ;jr read_from_file_cannot
    jp read_from_file_cannot

read_from_file3:
    ld a, RD_USB_DATA0                      ; Find out how many bytes are available to read
    call send_command_byte
    call read_data_byte                     ; A = number of bytes available to read

    ; If there are less than 128 bytes to read, fill the buffer with 0s first
    cp 128
    jr nc, read_from_file_128
    pop hl
    push hl
    push af
    ld b, 128
read_from_file_padding:
    ld (hl), 0
    inc hl
    djnz read_from_file_padding
    pop af

read_from_file_128:
    pop hl
    call read_data_bytes_into_hl        ; Read this block of data
    push hl
    ld a, BYTE_RD_GO
    call send_command_byte
    ld a, GET_STATUS
    call send_command_byte
    call read_data_byte
    pop hl
    ; All done, so return ZERO for success
    cp a                                ; set zero flag for success
    ret

 read_from_file_cannot:
    pop de
    or 1                                ; clear zero flag
    ret




;
; function buildFCB
; build FCB for new CP/M file
; 
; on entry
;   DE points to null terminated filename
;
; BC, DE, HL preserved
;
buildFCB:
    push bc
    push de                 ; filename
    push hl                 
    ex de, hl               ; we need HL = filename
    ld de, FCB              ; point to our FCB

    ; DR field = drive number
    ld a, 3                 ; use CP/M D: drive
    inc a                   ; DR numbers start at 1
    ld (de), a
    
    ; copy filename in to FCB
    ; 11 bytes = filename with extension (no dot)
    ld bc, 11
    inc de
    ldir
   
    ld c, F_DELETE
    ld de, FCB
    call BDOS               ; delete file if already exists
    
    ld de, FCB
    call set_random_pointer_in_fcb

    ld de, FCB+$20          ; CR set to 0 when opening file
    ld (de), a

    ld hl, FCB
    call set_file_size_in_fcb

    ; Clear all 16 disk allocation bytes. TODO: Actually, fill with sensible values
    ld de, FCB
    ld hl, 16
    add hl, de
    ex de, hl
    ld b, 16+4
    ld a, 0
clear_allocation_loop:
    ld (de), a
    inc de
    djnz clear_allocation_loop 

    ld c, F_MAKE
    ld de, FCB
    call BDOS               ; create file
    cp $ff
    jp nz, buildFCBexit     
                            ; create failed
    ld hl, str_create_failed
    call PRINT_STR
    call PRINT_FILENAME
    call PRINT_NEWLINE

buildFCBexit:
    pop hl
    pop de
    pop bc
    ret
; end function buildFCB
;
str_create_failed:
    db "Failed to create file ", $

;
; function clearFCB
; clear FCB
;
; on entry
;   -
;
; on exit
;   BC, DE, HL preserved
;
clearFCB:
    push bc
    push de
    push hl
    
    ld hl, FCB
    ld (hl), 0
    ld bc, 35
    ld de, FCB+1
    ldir                    ; clear FCB

    pop hl
    pop de
    pop bc
    ret
; end function clearFCB
;


send_data_byte:
    out (mem_stick_data_port), a
    call wait_til_not_busy
    ret
    
send_data_string:
    ; The string is pointed to by HL
    ld a, (hl)
    cp 0
    jr z, send_data_string_done
    push af
    push hl
    call send_data_byte
    pop hl
    pop af
    inc hl
    jp send_data_string
send_data_string_done:
    ld a, 0
    call send_data_byte
    ret

send_command_byte:
    out (mem_stick_command_port), a
    call wait_til_not_busy
    ret
    
read_command_byte:
    in a, (mem_stick_command_port)
    ret
    
read_data_byte:
    in a, (mem_stick_data_port)
    ret

read_data_bytes_into_buffer:
    ; The number of bytes should be in A.
    ; Read that many bytes into the buffer.
    ; The value of A is retained.
    ld hl, disk_buffer
read_data_bytes_into_hl:
    ; This entry point will read A bytes into the area pointed to by HL.
    ; On exit HL will point to the location after where the bytes were added.
    push af
    ld b, a
    ld c, mem_stick_data_port
 read_data_bytes_into_buffer1:
    inir                    ; A rare use of In, Increase & Repeat!!!
    pop af
    ret


short_pause:
	ld bc,100
pause0:
	dec bc
	ld a,b
	or c
	jp nz,pause0
	ret


wait_til_not_busy:
    ld bc, 60000            ; retry max 60000 times!!!
wait_til_not_busy1:
    push bc
    call read_command_byte
    and %00010000
    jp nz, wait_til_not_busy2
    pop bc
    ret
wait_til_not_busy2:
    call short_pause
    pop bc
    dec bc
    ld a, b
    or c
    jr nz, wait_til_not_busy1
;    ld hl, str_USB_TIMEOUT
;    call PRINT_STR
    
    ret
;str_USB_TIMEOUT:
;    db '[USB TIMEOUT]\r\n', $

read_status_byte:
    ld a, GET_STATUS
    call send_command_byte
    call read_data_byte
    ret


set_random_pointer_in_fcb:
    ; pass in de -> fcb
    ; Pass hl = random pointer value
    ; Random pointer goes to fcb + 33 & 34. fcb + 35 gets 0.
    ; preserve de
    push de
    ex de, hl
    ld bc, 33
    add hl, bc
    ld (hl), e
    inc hl
    ld (hl), d
    inc hl
    ld (hl), 0
    ex de, hl
    pop de
    ret


set_file_size_in_fcb:
    ; Pass HL -> FCB (Note that this is an unusual way to pass it in)
    ; Pass file pointer (in 128-byte records) in bcde.
    ; Preserves hl

    ; The following details are from http://www.primrosebank.net/computers/cpm/cpm_software_mfs.htm
    ; RC = record counter, goes from 0 to $80. $80 means full, and represents 128*128=16K.
    ; EX = 0 for files < 16K, otherwise 1 - 31 for Extents of 16K each.
    ; S2 = high byte for the EXc ounter, so if EX wants to be bigger than 31, overflow it into here.

    ; Split bcde into S2, EX & RC.
    ; To do this:
    ; RC = e & %0111 1111               (i.e. a number 0..127)
    ; Divide bcde by 128                (Shift right 7 bits, or shift left 1 bit then right 8)
    ; EX = e & %0001 1111               (i.e. it has a max of 31)
    ; Shift left 3 places
    ; S2 = d

    ; RC = e & %0111 1111
    push hl
    ld a, e
    and %01111111                       ; RC is in A

    sla e                               ; Shift all left by 1 bit
    rl d
    rl c
    rl b

    ld e, d                             ; Shift all right by 8 bits
    ld d, c
    ld c, b
    ld b, 0                             ; We've effectively shifted right by 7 bits

    ld bc, 15                           ; ex is as FCB+12, s2 is at FCB+14, rc is at FCB + 15
    add hl, bc                          ; hl -> FCB.RC
    ld (hl), a                          ; RC is now stored in FCB

    dec hl                              
    dec hl                              
    dec hl                              ; hl -> FCB.EX
    ld a, e
    and %00011111                       ; EX is in A
    ld (hl), a

    sla e                               ; Shift all left by 1 bit
    rl d
    rl c
    rl b
    sla e                               ; Shift all left by 1 bit
    rl d
    rl c
    rl b
    sla e                               ; Shift all left by 1 bit
    rl d
    rl c
    rl b

    inc hl
    ld a, 0
    ld (hl), 0                          ; Blank out the mystery byte called "unused"
    inc hl                              ; hl -> FCB.S2

    ld a, d
    and %00011111                       ; S2 is in A
    ld (hl), a

    pop hl
    ret


; Print A to the screen as an ASCII character, preserving all registers.
print_a:
    push af                         ; Store A for a bit
print_a1:
    in a,(uart_LSR)                 ; check UART is ready to send.
    bit 5,a                         ; zero flag set to true if bit 5 is 0
    jp z, print_a1                  ; non-zero = ready for next char.

    pop af                          ; UART IS READY, GET OLD "A" BACK
    out (uart_tx_rx),a              ; AND SEND IT OUT
	ret
	
space:
	ld a,32
	call print_a
	ret



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
B2D64:   LD (B2DINV),HL
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
         LD BC,$0909
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
B2DEND:  DS 1            ; space for terminating 0
MARKER:  DB '$'



mem_stick_data_port equ 16
mem_stick_command_port equ 17


GET_FILE_SIZE equ $0C

GET_STATUS equ $22
RD_USB_DATA0 equ $27
SET_FILE_NAME equ $2F
FILE_OPEN equ $32
FILE_ENUM_GO equ $33
FILE_CREATE equ $34
FILE_CLOSE equ $36
BYTE_LOCATE equ $39
BYTE_READ equ $3A
BYTE_RD_GO equ $3B
DISK_CAPACITY equ $3E
DISK_QUERY equ $3F


; Statuses
USB_INT_SUCCESS equ $14
USB_INT_CONNECT equ $15
USB_INT_DISCONNECT equ $16
USB_INT_BUF_OVER equ $17
USB_INT_USB_READY equ $18
USB_INT_DISK_READ equ $1D
USB_INT_DISK_WRITE equ $1E
USB_INT_DISK_ERR equ $1F
YES_OPEN_DIR equ $41
ERR_OPEN_DIR equ $41
ERR_MISS_FILE equ $42
ERR_FOUND_NAME equ $43
ERR_DISK_DISCON equ $82
ERR_LARGE_SECTOR equ $84
ERR_TYPE_ERROR equ $92
ERR_BPB_ERROR equ $A1
ERR_DISK_FULL equ $B1
ERR_FDT_OVER equ $B2
ERR_FILE_CLOSE equ $B4


; Here are the port numbers for various UART registers:
uart_tx_rx 		equ    	8
uart_LSR 		equ     13


STAR_DOT_STAR:
    db '*', 0, '$'


filename_buffer: 
    ds 20
disk_buffer: 
    ds 36

str_newline:
    db 13, 10, '$', 0
tfer_home:
    db "/TFER", 0
FAToffset:
    ds 2, 0
;
;CPMSP:
;    dw 1
FILELIST:
    ds 1024, 0              ; file list - enough space to store 85 filenames
dmaBuffer:
    ds 128, 0    
    ds 64, 0                ; 64 bytes reserved for programme stack (32 words)
stack:
;
    end
