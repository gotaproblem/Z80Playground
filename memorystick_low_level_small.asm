; memorystick_low_level_small.asm
;--------------------;
; LOW LEVEL ROUTINES ;
;--------------------;
DEBUG DEFL 0
NEW_READ DEFL 0






;----------------------------------------------------------------
; pause in 0.1ms increments (4MHz CPU A=32 / 10MHz CPU A=20)
hw_pause:
    ld a, 20                ; 8 = 100us
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

;-----------------------------------------------------------------
connect_to_disk:
    ld a, DISK_CONNECT  ; expect status of INT_SUCCESS ($14)
    call send_command_byte
    ld a, GET_STATUS
    call read_status_byte
    ret z

    ld hl, msg5
    call PRINT_STR
    
    ret
msg5: db 'ERROR connecting to USB Disk.',13,10,0
;-----------------------------------------------------------------
mount_disk:
    ld a, DISK_MOUNT
    call send_command_byte
    ld a, GET_STATUS
    call read_status_byte
    ret z

    ld hl, msg6
    call PRINT_STR
    
    ret
msg6: db 'ERROR mounting USB Disk.',13,10,0
;-----------------------------------------------------------------
read_disk_signature:
    ld a, RD_USB_DATA0
    call send_command_byte
    call read_data_byte                 ; A = length of bytes to now read
    cp 36
    jr nz, could_not_read_disk_sig

    ; Ignore the first 8 bytes
    ld b, 8
 read_disk_signature1:
    push bc
    call read_data_byte_silent
    pop bc
    djnz read_disk_signature1   

    ; Display the next 8 bytes (Manufacturer)
    ld b, 8
 read_disk_signature2:
    push bc
    call read_data_byte_silent
    call PRINT_CHAR ;print_a
    pop bc
    djnz read_disk_signature2   
    call PRINT_NEWLINE

    ; Display the next 16 bytes (Model)
    ld b, 16
 read_disk_signature3:
    push bc
    call read_data_byte_silent
    call PRINT_CHAR ;print_a
    pop bc
    djnz read_disk_signature3

    ld a, ' '
    call PRINT_CHAR ;print_a

    ; Display the next 4 bytes (Version)
    ld b, 4
 read_disk_signature4:
    push bc
    call read_data_byte_silent
    call PRINT_CHAR ;print_a
    pop bc
    djnz read_disk_signature4   
    call PRINT_NEWLINE ;newline
    ret

could_not_read_disk_sig:
    ld hl, msg7
    call PRINT_STR
    
    ret
msg7: db 'ERROR reading disk sig.',13,10,0
;connect_to_usb_drive:
    ; Connects us up to the USB Drive.
    ; Returns Zero flag = true if we can connect ok.
    ;call reset_ch376_module 
    ;call set_usb_host_mode
    ;cp USB_INT_CONNECT
    ;ret

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
;      push hl
;      call message
;      db 'open_file:[',0
;  open_file1:
;      ld a, (hl)
;      cp 0
;      jr z, open_file2
;      call print_a
;      inc hl
;      jr open_file1

;  open_file2:
;      ld a, ']'
;      call print_a
;      call newline
;      pop hl

    ld a, SET_FILE_NAME
    call send_command_byte
    call send_data_string
    ld a, FILE_OPEN
    call send_command_byte
    call read_status_byte
    cp USB_INT_SUCCESS
    ret

close_file:
    ld a, FILE_CLOSE
    call send_command_byte
                                        ; set to 0 as we don't want to play with file sizes
    ld a, 0                             ; 0 = leave file size alone, 1 = update file size if necessary
    call send_data_byte
    call read_status_byte
    ret

create_directory:
    ; Tells the module to use the filename from the filename_buffer to create a directory of that name.
    ; Returns z=true if ok to proceed.
    ld hl, filename_buffer
; create_directory1:
;     ld a, (hl)
;     cp 0
;     jr z, create_directory2
;     inc hl
;     call print_a
;     jr create_directory1

create_directory2:

    ld a, SET_FILE_NAME
    call send_command_byte
    ld hl, filename_buffer
    call send_data_string
    ld a, DIR_CREATE
    call send_command_byte
    call read_status_byte
    cp USB_INT_SUCCESS
    ret





if NEW_READ
read_from_file:
    ; Ask to read 128 bytes from the current file into the dma_address area pointed to by HL.
    ; Returns Zero flag set for success, clear for fail.
    push hl                 ; save buffer address

    ld a, BYTE_READ
    call send_command_byte
    ld a, 128               ; Request 128 bytes (one CP/M record)
    call send_data_byte
    ld a, 0
    call send_data_byte

    call read_status_byte  
if DEBUG 
    ;push af

    ;call PRINT_HEX
    ;call PRINT_NEWLINE

    ;pop af
endif
    cp USB_INT_DISK_READ    ; expected - this means "go ahead and read"
    jr z, read_from_file3
    cp USB_INT_SUCCESS      ; this means we are finished - EOF perhaps
    jr z, read_close1

    ;jr read_from_file_cannot; else error

read_from_file2:
    ld a, BYTE_RD_GO                      
    call send_command_byte
    call read_status_byte   ; returns $1d
    cp USB_INT_SUCCESS 
    jr read_from_file_cannot; error
if DEBUG    
    ;call PRINT_HEX
    ;call PRINT_NEWLINE
endif
read_from_file3:
    ld a, RD_USB_DATA0      ; Find out how many bytes are available to read
    call send_command_byte
    call read_data_byte     ; returns A = number of bytes available to read
if DEBUG    
    ;push af                 ; save length
    ;call PRINT_HEX
    ;call PRINT_NEWLINE

    ;pop af                  ; expect 128 ($80)
endif
    cp $80
    jr nz, read_from_file2

    pop hl                  ; buffer address
    call read_data_bytes_into_hl        ; Read this block of data
   
    ;call close_file
    ld a, 0 
    ret

read_close1:
    ;call close_file
    pop hl                  ; sync stack
    ld a, 0
    ret
 read_from_file_cannot:
if DEBUG
    ;call PRINT_HEX
    ;call PRINT_NEWLINE
endif
    pop hl
    ld a, 1                 ; error
    ret



else




read_from_file:
    ; Ask to read 128 bytes from the current file into the dma_address area pointed to by DE.
    ; Returns Zero flag set for success, clear for fail.
    push hl                 ; save buffer address

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
    jp z, read_from_file_cannot
    jr read_from_file_cannot

read_from_file3:
    ld a, RD_USB_DATA0                      ; Find out how many bytes are available to read
    call send_command_byte
    call read_data_byte                     ; A = number of bytes available to read

    ; If there are less than 128 bytes to read, fill the buffer with 0s first
    ;cp 128
    ;jr nc, read_from_file_128
    ;pop hl
    ;push hl
    ;push af
    ;ld b, 128
;read_from_file_padding:
    ;ld (hl), 0
    ;inc hl
    ;djnz read_from_file_padding
    ;pop af

;read_from_file_128:
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
    ;cp a                                ; set zero flag for success
    ld a, 0
    ret

 read_from_file_cannot:
    pop de
    ;or 1                                ; clear zero flag
    ld a, 1
    ret
endif












copy_filename_to_buffer:
    ; Enter with hl->zero-terminated-filename-string
    ; Copies this to filename_buffer
    ld de, filename_buffer
copy_filename_to_buffer1:
    ld a, (hl)
    ld (de), a
    inc hl
    inc de
    cp 0
    ret z
    jr copy_filename_to_buffer1

send_data_byte:
    ; push af
    ; call message
    ; db 'send_data_byte ', 0
    ; pop af
    ; push af
    ; call show_a_as_hex
    ; call newline
    ; pop af
    out (mem_stick_data_port), a
    call wait_til_not_busy
    ret
    
send_data_string:
    ; The string is pointed to by HL
    ;push hl
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
    ;pop hl
    ret

send_command_byte:
    ; push af
    ; call message
    ; db 'send_command_byte ',0
    ; pop af
    ; push af
    ; call show_a_as_hex
    ; call newline
    ; pop af
    out (mem_stick_command_port), a
    ;call short_pause ;tiny_pause
    call wait_til_not_busy
    ret
    
read_command_byte:
    ;call wait_til_not_busy
    ; call message
    ; db 'read_command_byte: ', 0
    in a, (mem_stick_command_port)
    ; push af
    ; call show_a_as_hex
    ; call newline
    ; pop af
    ret
    
read_data_byte:
    ;call wait_til_not_busy
    ; call message
    ; db 'read_data_byte: ', 0
    in a, (mem_stick_data_port)
    ; push af
    ; call show_a_as_hex
    ; call newline
    ; pop af
    ret
    
read_data_byte_silent:
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
    ;ld c, mem_stick_data_port
 read_data_bytes_into_buffer1:
    ;inir                    ; A rare use of In, Increase & Repeat!!!
    push bc
    ;call short_pause
    in a, (mem_stick_data_port)
    ld (hl), a
    inc hl
    pop bc
    djnz read_data_bytes_into_buffer1

    pop af
    ret
    
wait_til_not_busy:
    ; call message
    ; db 'waiting...', 13, 10, 0
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

    ;push hl
    ld hl, msg8
    call PRINT_STR
    ;pop hl

    ret
msg8: db '[USB TIMEOUT]', 13, 10, 0
read_status_byte:
    ld a, GET_STATUS
    call send_command_byte
    call read_data_byte
    ret

; show_status:
;     call read_status_byte
;     push af
;     call report_on_status
;     pop af
;     ret                     ; The status is returned in A

; report_on_status:
;     cp USB_INT_SUCCESS
;     jr nz, ros1
;     call message
;     db 'USB_INT_SUCCESS',13,10,0
;     ret
; ros1:
;     cp USB_INT_CONNECT
;     jr nz, ros2
;     call message
;     db 'USB_INT_CONNECT',13,10,0
;     ret
; ros2:
;     cp USB_INT_DISCONNECT
;     jr nz, ros3
;     call message
;     db 'USB_INT_DISCONNECT',13,10,0
;     ret
; ros3:
;     cp USB_INT_BUF_OVER
;     jr nz, ros4
;     call message
;     db 'USB_INT_BUF_OVER',13,10,0
;     ret
; ros4:
;     cp USB_INT_USB_READY
;     jr nz, ros5
;     call message
;     db 'USB_INT_USB_READY',13,10,0
;     ret
; ros5:
;     cp USB_INT_DISK_READ
;     jr nz, ros6
;     call message
;     db 'USB_INT_DISK_READ',13,10,0
;     ret
; ros6:
;     cp USB_INT_DISK_WRITE
;     jr nz, ros7
;     call message
;     db 'USB_INT_DISK_WRITE',13,10,0
;     ret
; ros7:
;     cp USB_INT_DISK_ERR
;     jr nz, ros8
;     call message
;     db 'USB_INT_DISK_ERR',13,10,0
;     ret
; ros8:
;     cp YES_OPEN_DIR
;     jr nz, ros9
;     call message
;     db 'YES_OPEN_DIR',13,10,0
;     ret
; ros9:
;     cp ERR_MISS_FILE
;     jr nz, ros10
;     call message
;     db 'ERR_MISS_FILE',13,10,0
;     ret
; ros10:
;     cp ERR_FOUND_NAME
;     jr nz, ros11
;     call message
;     db 'ERR_FOUND_NAME',13,10,0
;     ret
; ros11:
;     cp ERR_DISK_DISCON
;     jr nz, ros12
;     call message
;     db 'ERR_DISK_DISCON',13,10,0
;     ret
; ros12:
;     cp ERR_LARGE_SECTOR
;     jr nz, ros13
;     call message
;     db 'ERR_LARGE_SECTOR',13,10,0
;     ret
; ros13:
;     cp ERR_TYPE_ERROR
;     jr nz, ros14
;     call message
;     db 'ERR_TYPE_ERROR',13,10,0
;     ret
; ros14:
;     cp ERR_BPB_ERROR
;     jr nz, ros15
;     call message
;     db 'ERR_BPB_ERROR',13,10,0
;     ret
; ros15:
;     cp ERR_DISK_FULL
;     jr nz, ros16
;     call message
;     db 'ERR_DISK_FULL',13,10,0
;     ret
; ros16:
;     cp ERR_FDT_OVER
;     jr nz, ros17
;     call message
;     db 'ERR_FDT_OVER',13,10,0
;     ret
; ros17:
;     cp ERR_FILE_CLOSE
;     jr nz, ros18
;     call message
;     db 'ERR_FILE_CLOSE',13,10,0
;     ret
; ros18:
;     call message
;     db 'UNKNOWN STATUS: ',0
;     call show_a_as_hex
;     call newline
;     ret

long_pause:
	ld bc,65000
    jr pause0
medium_pause:
	ld bc,45000
    jr pause0
short_pause:
	ld bc,200;100
pause0:
	dec bc
	ld a,b
	or c
	jp nz,pause0
	ret



mem_stick_data_port equ 16 ; $f0 ; was 16
mem_stick_command_port equ 17 ; $f1 ; was 17

GET_IC_VER equ $01
SET_BAUDRATE equ $02
RESET_ALL equ $05
CHECK_EXIST equ $06
GET_FILE_SIZE equ $0C
SET_USB_MODE equ $15
GET_STATUS equ $22
RD_USB_DATA0 equ $27
WR_USB_DATA equ $2C
WR_REQ_DATA equ $2D
WR_OFS_DATA equ $2E
SET_FILE_NAME equ $2F
DISK_CONNECT equ $30
DISK_MOUNT equ $31
FILE_OPEN equ $32
FILE_ENUM_GO equ $33
FILE_CREATE equ $34
FILE_ERASE equ $35
FILE_CLOSE equ $36
DIR_INFO_READ equ $37
DIR_INFO_SAVE equ $38
BYTE_LOCATE equ $39
BYTE_READ equ $3A
BYTE_RD_GO equ $3B
BYTE_WRITE equ $3C
BYTE_WR_GO equ $3D
DISK_CAPACITY equ $3E
DISK_QUERY equ $3F
DIR_CREATE equ $40


; Statuses
USB_INT_SUCCESS equ $14
USB_INT_CONNECT equ $15
USB_INT_DISCONNECT equ $16
USB_INT_BUF_OVER equ $17
USB_INT_USB_READY equ $18
USB_INT_DISK_READ equ $1D
USB_INT_DISK_WRITE equ $1E
USB_INT_DISK_ERR equ $1F
ERR_OPEN_DIR equ $41
YES_OPEN_DIR equ $41
ERR_MISS_FILE equ $42
ERR_FOUND_NAME equ $43
ERR_DISK_DISCON equ $82
ERR_LARGE_SECTOR equ $84
ERR_TYPE_ERROR equ $92
ERR_BPB_ERROR equ $A1
ERR_DISK_FULL equ $B1
ERR_FDT_OVER equ $B2
ERR_FILE_CLOSE equ $B4

ROOT_DIRECTORY:
    db '*',0

SLASH:
    db '/',0

;SAVE_FILENAME:
;    db 'TESTING',0

;TARGET_FILENAME:
;    db '/TARGET2.TXT',0

;NO_EXTENSION:
;    db '   ',0

;TXT_EXTENSION:
;    db 'TXT',0
