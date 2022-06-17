; ********************************************************
; *                                                      *
; * Z80 Home Build Development                           *
; * Steve Bradford                                       *
; * 01/05/2021                                           *
; *                                                      *
; * low level interface to CH376S module                 *
; *                                                      *
; * ch376s_driver.asm                                    *
; *                                                      *
; ********************************************************
;
 

; -----------------------------------------------------------------------------
; Constant definitions
; -----------------------------------------------------------------------------

;--- CH376 port to Z80 ports mapping

CH_DATA_PORT:           equ $10
CH_COMMAND_PORT:        equ $11    


;--- Commands
CH_CMD_RESET_ALL:       equ 05h
CH_CMD_CHECK_EXIST:     equ 06h
CH_CMD_SET_RETRY:       equ 0Bh
CH_CMD_GET_FILE_SIZE    equ 0Ch
CH_CMD_DELAY_100US:     equ 0Fh
CH_CMD_SET_USB_ADDR:    equ 13h
CH_CMD_SET_USB_MODE:    equ 15h
CH_CMD_TEST_CONNECT:    equ 16h
CH_CMD_ABORT_NAK:       equ 17h
CH_CMD_GET_STATUS:      equ 22h
CH_CMD_RD_USB_DATA0:    equ 27h
CH_CMD_WR_HOST_DATA:    equ 2Ch
CH_CMD_WR_REQ_DATA:     equ 2Dh
CH_CMD_SET_FILE_NAME:   equ $2f
CH_CMD_DISK_CONNECT:    equ $30
CH_CMD_DISK_MOUNT:      equ $31
CH_CMD_FILE_OPEN:       equ $32
CH_CMD_FILE_ENUM_GO     equ 33h
CH_CMD_FILE_CREATE:     equ $34
CH_CMD_FILE_ERASE:      equ $35
CH_CMD_FILE_CLOSE:      equ $36
CH_CMD_BYTE_LOCATE:     equ $39
CH_CMD_BYTE_READ:       equ $3a
CH_CMD_BYTE_RD_GO:      equ $3b
CH_CMD_BYTE_WRITE:      equ $3c
CH_CMD_BYTE_WR_GO:      equ $3d
CH_CMD_DISK_CAPACITY:   equ $3e
CH_CMD_DISK_QUERY:      equ $3f

;--- Status codes
CH_ST_INT_SUCCESS:      equ 14h
CH_ST_INT_CONNECT:      equ 15h
CH_ST_INT_DISCONNECT:   equ 16h
CH_ST_INT_BUF_OVER:     equ 17h
CH_ST_RET_SUCCESS:      equ 51h
CH_ST_RET_ABORT:        equ 5Fh


USB_INT_SUCCESS:        equ $14
USB_INT_CONNECT:        equ $15
USB_INT_DISK_READ       equ $1D
USB_INT_DISK_WRITE      equ $1E
USB_INT_DISK_ERR        equ $1F



eos					equ		00h					; End of string
cr					equ		0dh
lf					equ		0ah

;
; build options
;

;
; select CPU clock speed
CPU_4MHZ    DEFL    0
CPU_10MHZ   DEFL    1




;----------------------------------------------------------------
; pause in 0.1ms increments
;
; on entry BC = pause value in 0.01ms increments
;
; BC, A destroyed
hw_pause:
if CPU_10MHZ
    ld a, 2                 ; 10us
else
    ld a, 8
endif
_hwloop:                    ; 50 ticks
    nop
    nop                     
    dec a                   
    jr nz, _hwloop 
    dec bc
    ld a, b
    or c
    jr nz, hw_pause
    
    ret
;-----------------------------------------------------------------


;----------------------------------------------------------------
; wait for CH376S chip to signal ready
;
; on entry -
;
; BC, A destroyed
wait:
    ld bc, 50000            ; retry max 500ms
wait1:
    push bc
    call read_command
    and %00010000           ; bit 4 busy flag
    jp nz, wait2

    and %00100000           ; bit 5 fault flag
    jp nz, ch376_fault

    pop bc
    ret

wait2:
    ld bc, 1
    call hw_pause           ; wait 10us
    pop bc
    dec bc
    ld a, b
    or c
    jr nz, wait1

    call wait               ; try again
    ret

ch376_fault:
    push hl
    ld hl, str_faulted
    call PRINT_STR
    pop hl
    pop bc
    ret

str_faulted: db "[CH376S FAULTED]", cr, lf, eos
;-----------------------------------------------------------------


;----------------------------------------------------------------
; send command to CH376S
;
; on entry BC = pause value in 0.1ms increments
;
; BC, A destroyed
send_command:
    out (CH_COMMAND_PORT), a
    call wait
    ret
;-----------------------------------------------------------------


;----------------------------------------------------------------
; read data from CH376S
;
; on entry -
;
; returns data byte in A
read_data:
    in a, (CH_DATA_PORT)
    ret
;-----------------------------------------------------------------


;----------------------------------------------------------------
; read status from CH376S
;
; on entry -
;
; returns data byte in A
read_status:
    ld a, CH_CMD_GET_STATUS
    call send_command
    call read_data
    ret
;-----------------------------------------------------------------


; ----------------------------------------------------------------
; send data in A
;
; Output:
;
send_data:
    out (CH_DATA_PORT), a
    call wait
    ret
;-----------------------------------------------------------------


; ----------------------------------------------------------------
; read command in A
;
; Output:
;
read_command:
    in a, (CH_COMMAND_PORT)
    ret
;-----------------------------------------------------------------

; ----------------------------------------------------------------
; initialise CH376S hardware and check operational
;
; Output: Cy = 0 if hardware is operational, 1 if it's not
;
ch376s_init:
    ld a, CH_CMD_RESET_ALL
    call send_command       ; reset CH376S hardware
    ld bc, 3500
    call hw_pause           ; wait 35ms for hardware to settle

    ld a, CH_CMD_CHECK_EXIST
    call send_command
    ld a, 123               ; send an arbitrary number
    call send_data
    call read_data
    cp 255-123              ; The result is 255 minus what we sent in
    jp nz, ch367s_check_failed

    ld a, CH_CMD_SET_USB_MODE
    call send_command
    ld a, 6
    call send_data
    call read_status
    cp USB_INT_CONNECT
    jp nz, ch376s_mode_failed

    ld a, CH_CMD_DISK_CONNECT
    call send_command
    call read_status
    jp nz, ch376s_connect_failed

    ld a, CH_CMD_DISK_MOUNT
    call send_command
    call read_status
    jp nz, ch376s_mount_failed

    or a                    ; resets carry flag
    ret

ch376s_mount_failed:
    ld hl, str_mount_failed
    call PRINT_STR
    scf                     
    ret

ch376s_connect_failed:
    ld hl, str_connect_failed
    call PRINT_STR
    scf
    ret
    
ch367s_check_failed:
    ld hl, str_check_failed
    call PRINT_STR
    scf
    ret

ch376s_mode_failed:
    ld hl, str_mode_failed
    call PRINT_STR
    scf
    ret

str_mode_failed:
    db "[ERROR: No USB Disk?]\r\n", cr, lf, eos
str_check_failed:
    db "[CH376S MODULE NOT FOUND]\r\n", cr, lf, eos
str_connect_failed:
    db "[ERROR connecting to USB Disk]\r\n", cr, lf, eos
str_mount_failed:
    db "ERROR mounting USB Disk", cr, lf, eos

;-----------------------------------------------------------------



; HL points to null terminated string
send_data_string:
    ld a, (hl)
    cp 0
    jr z, send_data_string_done

    call send_data
    inc hl
    jp send_data_string

send_data_string_done:
    call send_data
    ret




; HL = filename pointer
open_file:
    ld a, CH_CMD_SET_FILE_NAME
    call send_command
    call send_data_string
    ld a, CH_CMD_FILE_OPEN
    call send_command
    call read_status
    cp USB_INT_SUCCESS
    ret


; DE = pointer to filename
create_file:
    push de
    ld a, CH_CMD_SET_FILE_NAME
    call send_command
    pop hl
    call send_data_string
    ld a, CH_CMD_FILE_CREATE
    call send_command
    call read_status
    cp USB_INT_SUCCESS

    ret


; DE = pointer to filename
delete_file:
    push de
    ld a, CH_CMD_SET_FILE_NAME
    call send_command
    pop hl
    call send_data_string
    ld a, CH_CMD_FILE_ERASE
    call send_command
    call read_status
    cp USB_INT_SUCCESS

    ret

;
; read 128 bytes from the current file into buffer address pointed to in HL
;
read_from_file:
    ld a, CH_CMD_BYTE_READ
    call send_command
    ld a, 128               ; Request 128 bytes (one CP/M sector)
    call send_data
    ld a, 0
    call send_data
    call read_status
    cp USB_INT_DISK_READ    ; go ahead and read
    jr z, read_from_file3

    cp USB_INT_SUCCESS      ; this means we are finished
    jp z, read_eof

    ld a, 1                 ; else return error condition
    ret

read_from_file3:
    ld a, CH_CMD_RD_USB_DATA0 
    call send_command       ; how many bytes are available to read?
    call read_data          ; A tells us
    
                            ; quickest way to get the data
    ld hl, (dmaad)
    ld b, a
    ld c, CH_DATA_PORT
    inir                    ; IO loop - data is written to buffer pointed to in HL

    ld a, CH_CMD_BYTE_RD_GO
    call send_command
    call read_status
    cp a
    ret

read_eof:    
    ld a, USB_INT_SUCCESS+1
    ret                     ; return good status


;
; Set the BYTE_LOCATE file position in the currently open file.
; Value is passed in DEHL, DE = high word, HL = low word
move_to_file_pointer:
    ld a, CH_CMD_BYTE_LOCATE
    call send_command
    ld a, l
    call send_data
    ld a, h
    call send_data
    ld a, e
    call send_data
    ld a, d
    call send_data
    call read_status
                   
    ret 

if OLDWR
wrbytes: db 0
;----------------------------------
; CP/M WRITE TO FILE
; writes 128 bytes from current location pointed to by HL, to the open file
write_to_file:   
    ld a, 128
    ld (wrbytes), a

write_start:
    ld a, CH_CMD_BYTE_WRITE
    call send_command

    ; Send number of bytes we are about to write, as 16 bit number, low first
    ;ld a, 128
    ;ld hl, wrbytes
    ld a, (wrbytes)
    call send_data
    ld a, 0
    call send_data

;write_loop
    call read_status
    cp USB_INT_SUCCESS
    jr z, write_finished    ; all done

    cp USB_INT_DISK_WRITE
    jr z, write_loop

    jp write_error          ; error

write_loop:
    ; Ask if we can send some bytes
    ld a, CH_CMD_WR_REQ_DATA
    call send_command
    call read_data          ; returns allowed byte count
    ;cp 128
    ;jp nz, write_partial
    push af
    ld hl, str_want
    call PRINT_STR
    ld a, (wrbytes)
    call PRINT_HEX
    call PRINT_NEWLINE

    ld hl, str_write_partial
    call PRINT_STR
    pop af
    push af
    call PRINT_HEX
    call PRINT_NEWLINE

    pop af
    ld b, a                 ; should be 128
    ld c, CH_DATA_PORT
    ld hl, (dmaad)
    otir

    ld b, a                 ; bytes sent
    ld a, (wrbytes)
    sub b                   ; did all go?
    ld (wrbytes), a
    cp 0
    jp z, write_completed

    jp write_start

write_completed:
    ld a, CH_CMD_BYTE_WR_GO
    call send_command       ; finish write operation
    call close_file         ; close FAT file

write_finished:
    ld a, 0
    ret

write_partial:
    push af
    ld hl, str_write_partial
    call PRINT_STR
    pop af
    call PRINT_HEX
    call PRINT_NEWLINE

    ld a, 0
    ret

write_error:
    push af
    ld hl, str_write_error
    call PRINT_STR
    pop af
    call PRINT_HEX
    call PRINT_NEWLINE

    ld a, 1
    ret

str_write_error:
    db "Write error $"
str_write_partial:
    db "Got $"
str_want:
    db "Want $"
endif








; ----------------------------------
; CP/M WRITE TO FILE
; writes 128 bytes from current location pointed to by HL, to the open file
write_to_file:
    ld hl, (dmaad)
    push hl
    ld a, CH_CMD_BYTE_WRITE
    call send_command

    ; Send number of bytes we are about to write, as 16 bit number, low first
    ld a, 128
    call send_data
    ld a, 0
    call send_data

    pop hl                              ; hl -> the data

write_loop
    call read_status
    cp USB_INT_DISK_WRITE
    jr nz, write_finished

    push hl
    ; Ask if we can send some bytes
    ld a, CH_CMD_WR_REQ_DATA
    call send_command
    call read_data
    pop hl
    cp 0
    jr z, write_finished

    ld b, a
block_loop:
    ld a, (hl)
    push hl
    push bc
    call send_data
    pop bc
    pop hl
    inc hl
    djnz block_loop

    push hl
    ld a, CH_CMD_BYTE_WR_GO
    call send_command
    pop hl
    jp write_loop

write_finished:
    ret







close_file:
    ld a, CH_CMD_FILE_CLOSE
    call send_command
    ld a, 1 ;0                 ; 1 = update FAT file size if necessary
    call send_data
    ;call read_status
    ret


cpm2_driveA: db "/A.DSK", 0, 0, 0, 0, 0, 0, 0, 0, 0
cpm3_driveA: db "/CPM3.DSK", 0, 0, 0, 0, 0, 0
cpm_driveX: ds 15, 0