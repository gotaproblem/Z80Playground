; *******************************************************************************
; *                                                                             *
; * Z80 Home Build Development                                                  *
; * Steve Bradford                                                              *
; * 01/05/2021                                                                  *
; *                                                                             *
; * filetransfer.asm => 8PTFER.COM                                              *
; *                                                                             *
; * Syntax                                                                      *
; *     A> 8PTFER </R | /W> <[d:]filename.sfx>                                    * 
; *                                                                             *
; * Examples                                                                    *
; *     A> 8PTFER                                                               *
; *         Displays /TFER directory contents and prompts to copy all FAT files *
; *         to CP/M drive D:                                                    *
; *     A> 8PTFER /r [d:]filename.sfx                                           *
; *         Read filename.sfx from /TFER and write it to supplied [d:] CP/M drive
; *     A> 8PTFER /w [d:]filename.sfx                                           *
; *         Write CP/M [d:]filename.sfx to /TFER                                *
; *                                                                             * 
; * Files to be transferred must be placed in /TFER on the USB Memory Stick.    *
; *                                                                             *
; * A maximum of 85 files can be transferred.                                   *
; * 1KB buffer (FILELIST) can hold 85 8.3 + NULL filenames. Simply increase     *
; * this buffer size if needed.                                                 *
; *                                                                             *
; * History                                                                     *
; * 1.0     June      2021  Initial release                                     *
; * 1.1     September 2021  Added message for empty /TFER directory             *
; * 1.2     October   2021  Tidied up filename listing                          *
; * 1.3     November  2021  Added options to read/write files from/to CP/M/FAT  *
; * 1.4     November  2021  Fixed write to same logged drive                    *
; *                                                                             *
; * Known Issues / Bugs                                                         *
; * Nov 2021                                                                    *
; * Attempting to write a CP/M file to the logged in drive results in a File -  *
; * Create error. ***Fixed in v1.4***                                           *
; *                                                                             *
; *******************************************************************************
;
; build options
DEBUG       DEFL    0
OLD_READ    DEFL    0
OLDWR       DEFL    0



BOOT        equ 0          ; CP/M warm boot vector
CDRIVE      equ 4          ; current disk drive
BDOS        equ 5          ; BDOS jump vector
FCB         equ 5ch         ; default system FCB for transient programmes
FCB2        equ 6ch         ; 2nd system FCB - passed arguments need to be referenced
;
;
; define BDOS functions
;
C_READ      equ 01          ; console input
C_WRITE     equ 02          ; console output
C_WRITESTR  equ 09          ; output string, '$' terminated, to console output
C_READSTR   equ 0ah         ; buffered console input
RST_DISK    equ 0dh         ; reset disk system
DRV_SET     equ 0eh         ; select drive
F_OPEN      equ 0fh         ; open file
F_CLOSE     equ 10h         ; close file
F_SFIRST    equ 11h         ; search for first
F_DELETE    equ 13h         ; delete file
F_READ      equ 14h         ; read record
F_WRITE     equ 15h         ; write record
F_MAKE      equ 16h         ; create file
F_DMAOFF    equ 1ah         ; set DMA address
F_MULTISEC  equ 2ch         ; set number of records to read/write at once
;
;
; control keys
;
CNTRL_C     equ 3
;


;RAMbuff     equ 2000h       ; start address of RAM buffer
RAMbuffSz   equ 8000h       ; use 32KB of RAM as file transfer buffer


FALSE       equ 0
TRUE        equ !FALSE

;
; Programme start
;
            ORG 100h        ; CP/M programmes are loaded and run from address $0100
;
tfer:
    ld (CPMSP), sp          ; save CP/M stack pointer
    ld sp, stack            ; set programme stack space

    ld hl, str_signon
    call PRINT_STR

    ; decipher argument list
    ld a, (FCB+1)
    cp ' '
    jp z, noArgs

    cp '/'
    jp nz, syntaxerror

    ld a, (FCB+2)
    cp 'R'
    jp z, read

    cp 'W'
    jp z, write

syntaxerror:
    ; syntax error
    ld hl, str_syntax
    call PRINT_STR
    jp exit1

str_syntax:
    db "Syntax error - invalid option\r\n"
    db "8PTFER </R | /W> <[d:]filename.sfx>\r\n$"

;
; Read option
; 8PTFER /R [d:]filename.sfx
;
; Read a FAT file and write it to CP/M
; FAT filename MUST = CP/M file format (8.3)
;
read:
    call read_do
    jp exit1

read_do:
    call clear_fn           ; clear filename_buffer
    ld a, (FCB2)
    ld (ARGDRIVE), a        ; save the drive number

    ld hl, FCB2+1
    ld a, (hl) 
    cp ' '
    jp z, syntaxerror2      ; expected filename, but is empty

    call build_filename     ; returns filename_buffer = FAT filename eg. abc.def
    call open_FAT           ; open FAT file specified in filename_buffer
    cp ERR_MISS_FILE
    jp z, missingfile

                            ; FAT file exists
    ld hl, str_read         ; "  Read "
    call PRINT_STR
    ld hl, filename_buffer  ; '$' terminated
    call PRINT_STR
    call PRINT_SPACE
    call file_size          ; print FAT files size
    
    ld a, TRUE
    ld (firstPass), a
    ld a, FALSE
    ld (endPass), a

    ld hl, 0000h
    ld (FAToffset), hl      ; CP/M records counter
    ld hl, RAMbuff
    ld (DMAoffset), hl
    ld a, 0
    ld (rdrecs), a          ; count number of 128 byte records

    jp rdFATloop

if DEBUG
str_nextpass:
    db "  Next Pass\r\n$"
endif

nextPass:
    ;
if DEBUG
    ld hl, str_nextpass
    call PRINT_STR
endif
    call open_FAT

    ld hl, RAMbuff
    ld (DMAoffset), hl      ; reset DMA pointer back to RAM buffer start

    ld a, 0
    ld (rdrecs), a          ; reset record count

rdFATloop:
    call FATseek            ; seek to file offset = (FAToffset * 128)

    ld hl, (DMAoffset)      ; buffer pointer
    ld (dmaad), hl 
    call read_from_file     ; read 128 bytes from FAT file in to buffer
    cp USB_INT_SUCCESS+1
    push af                 ; save zero flag = EOF

    ld a, (rdrecs)
    inc a
    ld (rdrecs), a          ; rdrecs must be at least 1 (0 = 256)

    pop af
    jp z, rd_done           ; break loop on EOF

    ld bc, 128
    ld hl, (DMAoffset)
    add hl, bc
    ld (DMAoffset), hl   

    ld hl, (FAToffset)
    inc hl
    ld (FAToffset), hl      ; save FAT file offset (number of 128 byte reads)

    ld a, (rdrecs)
    cp 0                    ; 32K / 128 = 256, so should = 0
    jp nz, rdFATloop        ; break loop on end of RAM buffer (buffer full)

    jp rd_done2             ; file too big for buffer, more to do

rd_done:
    ; EOF reached
if DEBUG
    ld hl, str_eof
    call PRINT_STR
endif
    ld a, TRUE
    ld (endPass), a         ; flag EOF

rd_done2:
    ; buffer is full and there's more
    ld a, (firstPass)
    cp TRUE
    jp nz, opened           ; CP/M file should be opened already

not_opened:
    ; this is the first pass through, so CP/M file is not open yet
if DEBUG
    ld hl, str_notopened
    call PRINT_STR
endif
    ld a, FALSE
    ld (firstPass), a       ; first pass is now done

    ld hl, FCB2+1
    ld de, cpmFilename
    ld bc, 11
    ldir                    ; copy filename argument (CP/M format)
    call buildFCB

    ld de, FCBnew
    ld c, F_OPEN
    call BDOS

opened: 
    ; 
if DEBUG
    ld hl, str_opened
    call PRINT_STR
endif
    ld hl, RAMbuff
    ld (DMAoffset), hl      ; point to beginning of RAM buffer

    ld e, 1
    ld c, F_MULTISEC
    call BDOS

    ; select a drive not being used
    ld a, (ARGDRIVE)
    dec a
    ld e, a
    ld a, 3
    xor e
    ld c, DRV_SET
    call BDOS

    ld a, (rdrecs)          ; write out CP/M file. Can be a max of 0 (256) records or 32K
    ld b, a
    ld c, 0

if DEBUG
    ld hl, str_wrloop
    call PRINT_STR
endif

CPMwriteloop: 
    ; write RAM buffer to CP/M file
    push bc                 ; number of 128 byte records to write

    ld de, (DMAoffset)
    ld c, F_DMAOFF
    call BDOS

    ld de, FCBnew           ; File Description Block
	ld c, F_WRITE
	call BDOS		        ; Returns A=0 if ok, A=1 if EOF

    ld bc, 128
    ld hl, (DMAoffset)
    add hl, bc
    ld (DMAoffset), hl

    pop bc                  ; get loop count in B
    djnz CPMwriteloop       ; loop until no more 128 byte records

    ld a, (endPass)
    cp TRUE
    jp z, rd_copy_end

    jp nextPass             ; should only be jumping if file > 32KB

if DEBUG
str_wrloop:
    db "   Entering Write Loop\r\n$"
endif

rd_copy_end:
    ; complete CP/M file write by closing it
if DEBUG
    ld hl, str_copyend
    call PRINT_STR
endif

    ld de, FCBnew	        ; close CP/M file
	ld c, F_CLOSE
	call BDOS

    ld hl, str_cp1
    call PRINT_STR

    ld a, (ARGDRIVE)
    cp 0
    jp nz, dletter

    ld a, (CDRIVE)
    inc a

dletter:
    add a, 40h              ; drive number to letter (1 = A:)
    call PRINT_CHAR
    ld hl, str_cp2
    call PRINT_STR
    call PRINT_NEWLINE

    ; read of FAT file and write of CP/M file has completed
    ;jp exit1

    ret

str_read:
    db "  Read $"

if DEBUG
str_eof:
    db "  EOF\r\n$"
str_notopened:
    db "  NOT OPENED\r\n$"
str_opened:
    db "  OPENED\r\n$"
str_copyend:
    db "  Closing\r\n$"
endif
endPass     db  FALSE
firstPass   db  TRUE


str_cp1:
    db "  Copied to $"
str_cp2:
    db ":$"

read1error:
    ld hl, str_read1_fail
    call PRINT_STR
    ;jp exit
    ret

str_read1_fail:
    db "Read option failed\r\n$"

missingfile:
    ld hl, str_missingfile
    call PRINT_STR
    ld hl, filename_buffer
    call PRINT_STR
    call PRINT_NEWLINE
    ;jp exit
    ret

str_missingfile:
    db "File does not exist - $"



;------------------------------------------------------------------------------


;
; Write option
; 8PTFER /W [d:]filename.sfx
;
; Write a CP/M file to USB Memory Drive
;
write:
    call write_do
    jp exit1

write_do:
    call clear_fn
    ld a, (FCB2)
    ld (ARGDRIVE), a        ; save the drive number

    ld a, (FCB2+1)
    cp ' '
    jp z, syntaxerror2      ; expected filename, but is empty

    ld hl, FCB2+1
    call build_filename     ; returns filename_buffer = FAT filename ie. abc.def

    ; open / create CP/M file
    ld hl, FCB2+1
    ld de, cpmFilename
    ld bc, 11
    ldir                    ; copy filename argument (CP/M format)

    call clearFCB
    ld de, FCBnew
    ld a, (ARGDRIVE)        ; use supplied drive
    ld (de), a
    inc de                  ; DE = FCB+1, HL = cpmFilename
    ; copy filename in to new FCB
    ; 11 bytes = filename with extension (no dot)
    
    ld hl, cpmFilename
    ld bc, 11
    ldir

    ld a, 0
    ld de, FCBnew+20h       ; CR set to 0 when opening file
    ld (de), a
    ld de, FCBnew+12
    ld (de), a
    inc de
    ld (de), a
    inc de
    ld (de), a

    ; check CP/M file exists
    ld de, FCBnew
    ld c, F_OPEN
    call BDOS

    cp 0ffh                 ; -1 CP/M file does not exist
    jp nz, wr_continue      ; CP/M file exists

    ld hl, str_cpm_open_fail
    call PRINT_STR

    ret

str_cpm_open_fail:
    db "CP/M file does not exist\r\n$"

wr_continue:
    ld hl, tfer_home
    call open_file          ; open /TFER
    ld de, filename_buffer
    call create_file        ; create FAT file as defined in filename_buffer
    
    ld de, RAMbuff		    ; point CP/M DMA address to the FAT file buffer
	ld c, F_DMAOFF
	call BDOS

    ;ld hl, 0                
    ;ld (FAToffset), hl      ; seek to end of file

    ld hl, RAMbuff          ; common buffer, will only use 128 bytes
    ld (dmaad), hl

rloop:
    ; read CP/M file
    ld de, FCBnew
    ld c, F_READ
    call BDOS               ; read 1 CP/M record (128 bytes)

    cp 0
    jp z, wrFAT             ; read good, write to FAT file

    cp 1
    jp z, wr_complete       ; CP/M EOF, so finish

    jp readerror

wrFAT:
    call open_FAT
    jp nz, str_open_failed
    
    ;call FATseek
    ld hl, 0ffffh
    ld de, 0ffffh
    call move_to_file_pointer 
	


    call write_to_file      ; write 128 bytes to FAT file

    ;ld hl, (FAToffset)
    ;inc hl                  ; increment CP/M file record offset
    ;ld (FAToffset), hl

    jp rloop

str_open_failed:
    db "Open failed\r\n$"

readerror:
    ld hl, str_read_error
    call PRINT_STR

    ;jp exit                 ; end of /w option
    ret

str_read_error:
    db "Copy failed - read error from CP/M file\r\n$"

wr_complete:
    ld hl, str_indent
    call PRINT_STR
    ld hl, filename_buffer
    call PRINT_FILENAME

    call PRINT_SPACE
    call file_size          ; print the open FAT file size

    ld hl, str_FATwrite_complete
    call PRINT_STR

    ld de, FCBnew	        ; close CP/M file
	ld c, F_CLOSE
	call BDOS

    call close_file
    ;jp exit1
    ret

str_indent:
    db "  $"
str_FATwrite_complete:
    db "  Written to USB storage.\r\n$"

syntaxerror2:
    ld hl, str_syntax2
    call PRINT_STR
    ;jp exit1
    ret

str_syntax2:
    db "Syntax error - missing filename\r\n"
    db "8PTFER </R | /W> <[d:]filename.sfx>\r\n$"


;
; function
; 8PTFER
;
; No arguments to 8PTFER
; list all files in the /TFER directory and prompt to copy all
; to CP/M drive D:
;
noArgs:
    call noArgs_do
    jp exit1

noArgs_do:
    ; list /TFER directories contents
    ld hl, tfer_home
    call open_file          ; open directory /TFER
    cp ERR_OPEN_DIR
    jp nz, openFail

    ld hl, str_read_dir
    call PRINT_STR

    ld a, 4                 ; for batch tfer, set default drive to D:
    ld (ARGDRIVE), a

    ld hl, STAR_DOT_STAR    ; select all files
    call open_file

    ld a, 0
    ld (filecount), a
    ld (columncount), a
    ld hl, FILELIST
    ld (flptr), hl
    jp read_dir

openFail:
    ld hl, str_open_fail
    call PRINT_STR
    ;jp exit
    ret

str_open_fail:
    db "Open failed on /TFER\r\n$"

filecount:  db 0
flptr:      dw 1

read_dir:
    ld a, CH_CMD_RD_USB_DATA0
    call send_command
    call read_data          ; returns number of bytes to read
    cp 32                   ; should have 32 bytes for a directory entry
    jp nz, done             ; no, then finished
    
    call rd_inbuff          ; read FAT directory entry into disk_buffer

    ld hl, disk_buffer
    ld a, (hl)
    cp '.'
    jp z, carry_on          ; if filename starts with a dot then skip (./ ../)

    cp '_'                  ; MacOS problem. I seem to get duplicate files on a USB copy, prefixed with an underscore
    jp z, carry_on          ; if filename starts with an underscore then skip
            
    ld de, (flptr)          ; get filelist ptr
    ld hl, disk_buffer
    ld bc, 11
    ldir                    ; copy filename [8+3] from disk_buffer
    
    ld a, 0 ;'$'
    ld (de), a              ; add filename delimeter

    ld hl, str_tab
    call PRINT_STR          ; indent file list
    ld hl, (flptr)
    call PRINT_FILENAME     ; print filename
    
    ld bc, 12
    add hl, bc
    ld (flptr), hl          ; update file pointer
    ld a, (filecount)
    inc a
    ld (filecount), a       ; update file counter

    ; v1.2 print /TFER files in 5 columns per line
    ld a, (columncount)
    cp 4
    jp z, _newline

    inc a
    ld (columncount), a
    jp carry_on

_newline:
    ld a, 0
    ld (columncount), a
    call PRINT_NEWLINE

carry_on:
    ld a, CH_CMD_FILE_ENUM_GO  ; look for next directory entry
    call send_command
    call read_status
    cp ERR_MISS_FILE        ; end of directory?
    jp nz, read_dir         ; read next entry

    jp done_cont

done:
    ld a, (filecount)
    cp 0
    jp nz, done_cont

    ld hl, str_tfer_empty
    call PRINT_STR
    ;jp exit
    ret

done_cont:
    ;ld a, b                 ; v1.1 - print "empty"if no files to transfer
    ld a, (filecount)
    cp 0                    ; empty /TFER directory
    jp nz, have_files

    ld hl, str_empty_tfer
    call PRINT_STR
    ;jp exit
    ret
    
have_files:
    call PRINT_NEWLINE
    call ask
    call getc               ; check for copy confirmation
    ;jp c, exit              ; has CONTROL-C been entered?
    ret c
   
    cp 'Y'
    jp z, do_copy

    cp 'y'
    jp z, do_copy

    ;jp exit
    ret

ask:
    call PRINT_NEWLINE
    ld hl, str_copy1
    call PRINT_STR          ; "Copy "
    ld a, (filecount)
    ld h, 0
    ld l, a
    call DispHL             ; print file count
    ld hl, str_copy2
    call PRINT_STR          ; " file"
    ld a, (filecount)
    cp 2
    jp c, .ask_cont

    ld a, 's'
    call PRINT_CHAR         ; append 's' to 'file'

    ld hl, str_copy4
    call PRINT_STR          ; " to D:"

.ask_cont
    ld hl, str_copy3
    call PRINT_STR          ; " [Y/N]: "
    ret


;Number in hl to decimal ASCII
;Thanks to z80 Bits
;inputs:	hl = number to ASCII
;example: hl=300 outputs '00300'
;destroys: af, bc, hl, de used
DispHL:
	;ld	bc,-10000
	;call	Num1
	;ld	bc,-1000
	;call	Num1
	;ld	bc,-100
	;call	Num1
	ld	bc,-10
	call	Num1
	ld	c,-1
Num1:	ld	a,'0'-1
Num2:	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc
	call PRINT_CHAR
	ret 





do_copy:
    call PRINT_NEWLINE
    call PRINT_NEWLINE
    
    ld de, FILELIST         ; start of filelist
    ld a, (filecount)
    ld b, a
    ld c, 0

copy_loop:
    push bc                 ; save filecount
    push de                 ; save filelist pointer offset (current filename)

    ; build FCB2 before calling read
    ld hl, FCB2
    ld a, 4                 ; for batch tfer, set default drive to D:
    ld (hl), a
    ld a, 0
    ld hl, FCB2+12          ; EX
    ld (hl), a
    inc hl                  ; S1
    ld (hl), a
    inc hl                  ; S2
    ld (hl), a
    inc hl                  ; RC
    ld (hl), a

    pop hl                  ; filelist pointer
    push hl
    ld de, FCB2+1
    ld bc, 11
    ldir                    ; copy filelist filename to FCB

    call read_do            ; all the hard work... GO

    pop hl                  ; get filelist pointer DE -> HL
    ld bc, 12
    adc hl, bc              ; increment filelist pointer
    ld d, h
    ld e, l

    pop bc                  ; get file count
    djnz copy_loop          ; another file to copy

    ret
; end function




;
; programme end nicely
;
;exit:
 ;   pop bc

exit1:
    ld c, RST_DISK          ; restore logged in drive
    call BDOS
    ld	sp, (CPMSP)

    ret
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
	ld bc, (FAToffset)		; sector number offset relative to start of file
	ld de, 128				; BC * 128
    call DE_Times_BC        ; result in DEHL

if DEBUG
    push de
    push hl

    ld a, d
    call PRINT_HEX
    ld a, e
    call PRINT_HEX
    ld a, h
    call PRINT_HEX
    ld a, l
    call PRINT_HEX
    call PRINT_NEWLINE

    pop hl
    pop de
endif

	call move_to_file_pointer ; move file pointer using DEHL (hi->lo)
	cp USB_INT_SUCCESS
	ret z

    push af
	ld hl, str_seek_error
	call PRINT_STR

    pop af
    call PRINT_HEX
    call PRINT_NEWLINE
	ret
; end function
;
str_seek_error:
	db "Seek error ", 0



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
; function DEHL_Div_C
; multiply two 16bit registers
; returns a 32bit sum
; ref: http://z80-heaven.wikidot.com/math#toc16
;
DEHL_Div_C:
;Inputs:
;     DEHL is a 32 bit value where DE is the upper 16 bits
;     C is the value to divide DEHL by
;Outputs:
;    A is the remainder
;    B is 0
;    C is not changed
;    DEHL is the result of the division
;
     ld b,32
     xor a
       add hl,hl
       rl e
       rl d
       rla
       cp c
       jr c,$+4
         inc l
         sub c
       djnz $-11
     ret
; function end
;

;
; function build_filename
; CP/M filename converted to FAT filename eg. abc     .xyz = abc.xyz
;
; on entry
;   HL points to FILELIST offset for filename
;
; A, B, DE, HL destroyed
;
build_filename:    
    ld b, 8
    ld de, filename_buffer

nextChar:
    ld a, (hl)
    cp ' '
    jr z, doNothing

    ld (de), a
    inc de

doNothing:
    inc hl
    djnz nextChar

doExt:
    ld a, '.'
    ld (de), a              ; add full-stop for extension
    inc de
    ld b, 3                 ; now do extension

nextExt:
    ld a, (hl)
    ld (de), a
    inc de
    inc hl
    djnz nextExt

    ret
; end function
;


;
; function file_size
; on exit
;   DEHL = 32 bit file size = DE high 16bits, HL low 16bits
;
;   A destroyed
;
file_size:
    push bc
    push de
    push hl

    ld a, CH_CMD_GET_FILE_SIZE ; get FAT files file size
    call send_command
    ld a, 68h
    call send_data

    call read_data          ; byte 0 = low byte
    ld hl, fileSize32bit
    ld (hl), a
    ld l, a
    push hl                 ; saving L

    call read_data          ; byte 1
    ld hl, fileSize32bit+1
    ld (hl), a
    pop hl
    ld h, a
    push hl                 ; saving HL
    
    call read_data          ; byte 2
    ld hl, fileSize32bit+2
    ld (hl), a
    ld e, a
    
    call read_data          ; byte 3 = high byte
    ld hl, fileSize32bit+3
    ld (hl), a
    ld d, a

    pop hl
    ld (fsH), de
    ld (fsL), hl
    
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

fsH         ds  2, 0
fsL         ds  2, 0
RAMspace    ds  2, 0
remainH     ds  2, 0
remainL     ds  2, 0
passes      ds  2, 0
rdrecs      db  0
maxPasses   ds  2, 0        ; file size / 128



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
    db " : $"
str_copy1:
    db "Copy $"
str_copy2:
    db " file$"
str_copy3:
    db " [Y/N]: $"
str_copy4:
    db " to D:$"
str_empty_tfer:
    db "Empty\r\n$"
str_write_fail:
    db "CP/M Write failed\r\n$"
str_signon:
    db "File Transfer: v1.4 November 2021, Steve Bradford\r\n"
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



; function
; print CP/M filename - prints padding too
;
; on entry
;   HL = pointer to filename to print
;
PRINT_FILENAME:
    push hl
    push de
    push bc

    ld b, 8
    call .printfn           ; print filename
    push hl

    ld a, '.'
    call PRINT_CHAR

    pop hl
    ld b, 3
    call .printfn           ; print extension

    pop bc
    pop de
    pop hl

    ret

.printfn:
    ld a, (hl)
    call PRINT_CHAR
    inc hl
    djnz .printfn

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
PASS1:      AND     0Fh
    ADD     A,30h
    CP      3Ah
    JR      C,NUM               ; A < $3a - 0-9
    ADD     A,07h               ; else A-F
NUM:
    CALL    PRINT_CHAR

    RET
; end function




    INCLUDE "ch376s_driver.asm"



; function
; build FCB for new CP/M file
; 
; on entry
;   DE points to null terminated CP/M filename
;
; on exit
;   BC, DE, HL preserved
;
buildFCB:
    push bc
    push de                 ; CP/M filename
    push hl                 

    call clearFCB
    ld de, FCBnew           ; point to our new FCB

    ; DR field = drive number
    ld a, (ARGDRIVE)        ; use supplied drive   
    cp 0
    jr nz, skip_change

    ld a, (CDRIVE)          ; CDRIVE is 0 - n where 0 = A:
    inc a                   ; FCB needs drive to start at 1 for A:
    ld (ARGDRIVE), a

skip_change:
    ld (de), a
    inc de                  ; DE = FCB+1, HL = cpmFilename

    ; copy filename in to new FCB
    ; 11 bytes = filename with extension (no dot)
    ld hl, cpmFilename
    ld bc, 11
    ldir

    ld a, 0
    ld de, FCBnew+20h       ; CR set to 0 when opening file
    ld (de), a

    ld de, FCBnew+00ch      ; EX set to 0 when opening
    ld (de), a
    inc de
    ld (de), a              ; S1 set to 0
    inc de
    ld (de), a              ; S2 set to 0
    inc de     
    ld (de), a              ; RC set to 0

                            ; BUG: can not write to the logged drive
                            ; eg. D>8PTFER /R XYZ.COM will fail to create file
                            ; likewise D>8PTFER /R D:XYZ.COM will also fail
                            ; WORKAROUND: change logged drive to something else
    ld a, (ARGDRIVE)
    ld b, a
    dec b
    ld a, (CDRIVE)
    cp b
    jr nz, skip_change_ldrive

    ld a, (CDRIVE)          
    xor 3
    ld c, DRV_SET
    ld e, a
    call BDOS               ; set logged drive to something else

skip_change_ldrive:
    ld c, F_SFIRST          ; search for first
    ld de, FCBnew
    call BDOS
    cp 0ffh
    jr z, doesnt_exist

    ld c, F_DELETE
    ld de, FCBnew
    call BDOS               ; delete file if already exists

doesnt_exist:
    ld c, F_MAKE
    ld de, FCBnew
    call BDOS               ; create file
    push af
    cp 0ffh                 ; directory is full
    jp nz, buildFCBexit     
                            ; create failed
    ld hl, str_create_failed
    call PRINT_STR
    call PRINT_NEWLINE

buildFCBexit:
    pop af                  ; retain result of MAKE
    pop hl
    pop de
    pop bc

    ret
; end function


str_create_failed:
    db "Failed to create file $"



; function
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
    
    ld hl, FCBnew
    ld (hl), 0
    ld bc, 35
    ld de, FCBnew+1
    ldir                    ; clear FCB

    pop hl
    pop de
    pop bc

    ret
; end function


; function
; read A bytes in to disk_buffer
rd_inbuff:
    ; The number of bytes should be in A.
    ; Read that many bytes into the buffer.
    ; The value of A is retained.
    ld hl, disk_buffer
rd_bytes_hl:
    ; This entry point will read A bytes into the area pointed to by HL.
    ; On exit HL will point to the location after where the bytes were added.
    ld b, a
    ld c, mem_stick_data_port
    inir 

    ret
; end function


; function 
; clear filename buffer
clear_fn:
    ld hl, filename_buffer
    ld a, 0
    ld (hl), a
    ld de, filename_buffer+1
    ld bc, 11
    ldir                    ; clear filename_buffer

    ret
; end function


; function
; open FAT file
open_FAT:
    ld hl, tfer_home
    call open_file          ; open /TFER
    ld hl, filename_buffer
    call open_file          ; open FAT file as defined in filename_buffer

    ret
; end function





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
         LD BC,0909h
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


ERR_OPEN_DIR equ 41h
ERR_MISS_FILE equ 42h



CPMSP:
    dw 1                    ; save CP/M stack pointer
STAR_DOT_STAR:
    db '*', 0, '$'
ARGDRIVE:
    ds 1                    ; drive letter (number) argument
columncount:
    ds 1
filename_buffer:            ; FAT filename space
    ds 12, 0
    db '$'
cpmFilename:                ; CP/M filename space
    ds 12, 0
    db '$'
disk_buffer: 
    ds 36
fileSize32bit:
    ds 4, 0                 ; file size in bytes
str_newline:
    db 13, 10, '$', 0
tfer_home:
    db "/TFER", 0
FAToffset:
    ds 2, 0
DMAoffset:
    ds 2, 0
dmaad:                      ; needed for integration of ch376s_driver.asm
    ds 2, 0
FCBnew:
    ds 36, 0                ; FCB allocation
FILELIST:
    ds 1024, 0              ; file list - enough space to store 85 filenames
;dmaBuffer:
;    ds 128, 0    
    ds 64, 0                ; 64 bytes reserved for programme stack
stack:                      ; also marks programme end
;
RAMbuff     equ $           ; start address of RAM buffer
    END
