; ********************************************************
; *                                                      *
; * Z80 Home Build Development                           *
; * Steve Bradford                                       *
; * 01/05/2021                                           *
; *                                                      *
; * Z80 library functions                                *
; *                                                      *
; * commands.asm                                         *
; *                                                      *
; ********************************************************
;
str_commands:	DB		" Available Commands are: ", CR, LF
				DB		"    help - show this list", CR, LF
				DB		"    reset - Reset Cold/Warm", CR, LF
				DB		"    peek - view memory contents", CR, LF
				DB		"    fill - fill bytes in memory", CR, LF
				DB		"    move - move data in memory", CR, LF
				DB		"    load - load data to address", CR, LF
                DB		"    rom - enable/disable ROM", CR, LF
				DB		"    cpm2 - load CP/M v2.2 Operating System", CR, LF
				DB		"    cpm3 - load CP/M v3.1 Operating System", CR, LF
				DB		"    test - check Hardware", CR, LF
				DB		"    halt - halt CPU", CR, LF
				DB		"    toggle - toggle User LED1", CR, LF
                DB      EOS

; TABLE OF COMMANDS, holds pointer to string, and address
str_cmd_peek 	DB		"peek", EOS
str_cmd_rst		DB		"reset", EOS
str_cmd_help	DB		"help", EOS
str_cmd_fill    DB      "fill", EOS
str_cmd_move    DB      "move", EOS
str_cmd_load    DB      "load", EOS
str_cmd_rom     DB      "rom", EOS
str_cmd_cpm2    DB      "cpm2", EOS
str_cmd_cpm3    DB      "cpm3", EOS
str_cmd_test	DB		"test", EOS
str_cmd_halt	DB		"halt", EOS
str_cmd_toggle	DB		"toggle", EOS



CMD_TABLE:	    DW		str_cmd_help,       cmd_help
				DW		str_cmd_peek,       cmd_peek	    ; peek command
				DW		str_cmd_rst,        cmd_reset	    ; reset command
				DW		str_cmd_test,       cmd_test	    ; test command
				DW		str_cmd_fill,       cmd_fill	    ; fill command
                DW		str_cmd_move,       cmd_move	    ; move command
                DW		str_cmd_load,       cmd_load	    ; load command
                DW		str_cmd_rom,        cmd_rom	    	; rom command
				DW		str_cmd_cpm2,       cmd_cpm2	   	; cpm2 command
				DW		str_cmd_cpm3,       cmd_cpm3    	; cpm3 command
				DW		str_cmd_halt,       cmd_halt	    ; halt command
				DW		str_cmd_toggle,     cmd_toggle	    ; toggle command

CMD_TABLE_ENTRIES:	EQU	(($ - CMD_TABLE) / (2*2))	; calculate size in bytes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_help - prints the help info for the user				
cmd_help:		ld		hl, str_commands
				call	PRINT_STR
				ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_peek - parses command line for a start and end address and then calls the dump_memory function to dump the data
cmd_peek:		ld		a, (V_ARGC)				    ; load number of arguments
				cp		2
                jr      z, two_args                 ; 2 arguments?
                cp      1
				jr		nz, cmd_peek_err		    ; must be 0 args, print error message

two_args:    	ld		hl, (V_ARGV)				; first argument
				call	str_parse_word
				jr		nc, cmd_peek_err
                push	de 

                ld      a, (V_ARGC)                 ; recheck for arg count
                cp      1
                jr     	z, arg2_2                   ; only one argument, so HL and DE will be the same
				                         
arg2:			ld		hl, (V_ARGV + 2)			; second argument
    			call	str_parse_word
				jr		nc, cmd_peek_err2
			
arg2_2:			pop		hl                          ; retrieve first hex word to HL
				call	peek_memory                 ; DE now has second hex word
				ret

cmd_peek_err2:  pop     de                          ; sync stack
cmd_peek_err:	ld		hl, str_cmd_peek_err
				call	PRINT_STR
				ret


str_cmd_peek_err DB	"Usage: peek <START> [END]", CR, LF, EOS

; dump_memory: prints a nice table of the memory contents starting at address HL 
; and ending at address DE (rounded up to closest multiple of 16)
; affects: none
peek_memory:
				push	hl						; save registers
				push	de
				push	af
				ld		a, l					; align start address to 16 bytes
				and		$f0
				ld		l, a
				inc		de						; multiple of 16? (print extra line)
row_loop:		push 	hl
				ld 		a, l					; print header every 256 bytes
				cp 		0
				call 	z, draw_header
				pop 	hl
				ld		a, h
				call	PRINT_HEX				; print the starting address
				ld 		a, l
				call	PRINT_HEX
				ld		a, SPACE
				call	PRINT_CHAR			    ; print two spaces
				call	PRINT_CHAR
				push	hl						; save row start address
				ld		b, 16					; 16 bytes to display 
byte_loop:		ld		a, (hl)					; load byte at (HL)
				call	PRINT_HEX;				; print it
				
				ld		a, SPACE				; print a space
				call 	PRINT_CHAR
				inc		hl						; 
				djnz	byte_loop				; loop for 16
				
				ld		a, '|'					; print a '|'
				call	PRINT_CHAR
				ld		b, 16					; 16 bytes to display
				pop		hl						; get the row start address
ascii_loop:		ld		a, (hl)					;
				cp		$20						; is this a displayable character? 
				jp		c, dot					; A < ' '
				cp		$7F
				jp		c, ascii_loop_1			; A < $7F
dot:
				ld		a, '.'					; no - print '.' instead
ascii_loop_1:	call	PRINT_CHAR				; print character
				inc		hl						; HL now points to the next byte in memory
				djnz	ascii_loop				; do this B times (16)
				
				ld		a, '|'					; print a '|'
				call	PRINT_CHAR
				call	PRINT_NEWLINE			; end of row

				ld 		a, l
				or 		h
				jp 		z, peek_finish			; if HL = 0 then reached end of RAM

				push	hl						; save hl
				and		a						; reset carry flag
				sbc		hl, de					; do HL - DE, carry indicates DE > HL
				pop		hl						; restore hl since the above modifies it
				jp		c, row_loop				; if DE > HL, do next row
				
peek_finish:
				pop		af						; restore registers
				pop		de
				pop		hl
				ret
draw_header:
				ld		hl, peek_memory_header	; print header
				call	PRINT_STR
				ret
				
peek_memory_header:
				DB		"\r\n      00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F", CR, LF, CR, LF, EOS
				

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_reset	- does a cold or warm reset
cmd_reset:
				ld		hl, cmd_reset_str
				call	PRINT_STR
				
				call	GET_CHAR
				call	to_upper

				cp		'Y'						; cold reset?
				jr		nz, cmd_reset_1			; no - skip to reset
				
                rst     $00
cmd_reset_1:	
                rst		$08	                    ; warm boot			
cmd_reset_str:	DB		"Reset: Cold reset? (Y/N): ", EOS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
; cmd_jump - jumps to specific address and continues excecution from there
cmd_jump:		; make sure there is exactly 1 argument
				ld		a, (V_ARGC)
				cp		1				
				jr		nz, cmd_jump_error
				
				; read in argument
				ld		hl, (V_ARGV)
				call	str_parse_word
				jr		nc, cmd_jump_error
				
				push	de						; store address on stack...
				ret								; ...wich makes this call the specified function
cmd_jump_error:
				ld		hl,(str_cmd_jump_err)
				call	PRINT_STR

				ret
str_cmd_jump_err: 	DB		"Usage: jump <ADDR>", CR, LF, EOS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_continue - exits out of monitor mode and continues on
;cmd_continue:	pop		hl		; exit out of excecute_command
;				jp 		monitor_leave	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_test
cmd_test:		
				;call	TEST1
				;call	USB_DISK_INIT

				;call	USB_DISK_MOUNT			; disk connect then disk mount

				;call 	USB_DISK_CAPACITY		; get disk capacity

				;call	USB_DISK_QUERY			; get disk space

				;call	USB_DISK_FILE

				ret

;filename: DB "/CPM/DISKS/A", EOS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_fill - fill memory with pattern		
; > fill 8000 80ff 12		
cmd_fill:		; make sure there are 3 arguments
				ld		a, (V_ARGC)
				cp		3				
				jr		nz, cmd_fill_err
				
				ld		hl, (V_ARGV)			; first argument
				call	str_parse_word
				jr		nc, cmd_fill_err
				push	de                      ; save first hex word

				ld		hl, (V_ARGV + 2)		; second argument
				call	str_parse_word
				jr		nc, cmd_fill_err2
                push    de

                ld		hl, (V_ARGV + 4)		; third argument
				call	str_parse_byte
				jr		nc, cmd_fill_err2

                pop     de
				pop		hl                      
				call	fill_memory             ; HL = <START> DE = <END> A = <PATTERN>
				ret

cmd_fill_err2:  pop     de                      ; sync stack
cmd_fill_err:	ld		hl, str_cmd_fill_err
				call	PRINT_STR
				ret							    

str_cmd_fill_err DB	"Usage: fill <START> <END> <PATTERN>", CR, LF, EOS

fill_memory:    push    af
                
                ld      a, d
                sub     h
                ld      b, a
                ld      a, e
                sub     l
                ld      c, a
                
                ld      d, h
                ld      e, l
                inc     de
                
                pop     af
                ld      (hl), a
                ldir

                ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_move - move a block of memory		
; > move <FROM> <TO> <SIZE>
cmd_move:		; make sure there are 3 arguments
				ld		a, (V_ARGC)
				cp		3				
				jr		nz, cmd_move_err
				
				ld		hl, (V_ARGV)			; first argument
				call	str_parse_word
				jr		nc, cmd_move_err
				push	de                      ; save first hex word

				ld		hl, (V_ARGV + 2)		; second argument
				call	str_parse_word
				jr		nc, cmd_move_err2
                push    de

                ld		hl, (V_ARGV + 4)		; third argument
				call	str_parse_word
				jr		nc, cmd_move_err3
                push    de
                
                pop     bc
                pop     de
				pop		hl                      
				call	move_memory             ; HL = <FROM> DE = <TO> BC = <SIZE>
				ret

cmd_move_err3:  pop     de                      ; sync stack
cmd_move_err2:  pop     de                      ; sync stack
cmd_move_err:	ld		hl, str_cmd_move_err
				call	PRINT_STR
				ret							    
str_cmd_move_err DB	"Usage: move <FROM> <TO> <SIZE>", CR, LF, EOS

;
; extra checks needed
; 1. no overlap in addresses
;
move_memory:    push    bc
                push    hl                      ; no not a mistake, later we want de and hl the same
                push    hl
                
                ldir                            ; move memory

                pop     hl
                pop     de
                pop     bc
                
                inc     de
                ld      a, $00
                ld      (hl), a
                ldir                            ; fill <FROM> with zero's

                ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_load - load data to memory		
; > load <TO>
cmd_load:		; make sure there are 2 arguments
				ld		a, (V_ARGC)
				cp		2				
				jr		nz, cmd_load_err

                call	load                    ; HL = <FROM> DE = <TO> BC = <SIZE>
				ret

cmd_load_err:	ld		hl, str_cmd_load_err
				call	PRINT_STR
				ret							    

str_cmd_load_err DB	"Usage: load <TO>", CR, LF, EOS

load:
                ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_rom - disable or enable ROM		
; > rom <DISABLE | ENABLE>
cmd_rom:		; make sure there is 1 argument
				ld		a, (V_ARGC)
				cp		1				
				jp		nz, cmd_rom_err
	
                ld		hl, (V_ARGV)            ; compare against valid string
                ld      de, str_cmd_rom_d
                call    CHECKOPTIONS            ; check for disable option
				cp      0
				jr		nz, cmd_rom_next
	
                ld      a, ROM_DISABLE
                jr		cmd_rom_do           	; option valid
cmd_rom_next:
                ld		hl, (V_ARGV)            ; compare against valid string
                ld      de, str_cmd_rom_e
                call    CHECKOPTIONS            ; check for enable option
				cp		0
				jr		nz, cmd_rom_err			; option error

                ld      a, ROM_ENABLE
cmd_rom_do:
                call	rom                     ; C reflects ROM request (0=Enable, 1=Disable)
				ret

cmd_rom_err:	ld		hl, str_cmd_rom_err
				call	PRINT_STR
				ld		a, $ff					; error
				ret							    

str_cmd_rom_err DB	"Usage: rom <disable | enable>", CR, LF, EOS
str_cmd_rom_d   DB  "disable", EOS
str_cmd_rom_e   DB  "enable", EOS


rom:            
				ld      (V_ROMSTATE), a			; save ROM enabled/disabled state
				cp		ROM_ENABLE
				jr 		nz, _rom_disable
_rom_enable:
				call rom_on
				rst		$08
				ret
_rom_disable:
				call rom_off
                rst     $08
				ret
				
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_halt - HALT CPU		
; 
cmd_halt:		halt
				ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_toggle - toggle User LED1	(bit 0)	
; 
cmd_toggle:		in a, (ULED1)
				bit 0, a
				jr z, led_toggle_on
led_toggle_off:
				and %11111110
				jr cmd_toggle_end
led_toggle_on:
				or %00000001
cmd_toggle_end:
				out (ULED1), a
				ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_cpm2 - load CP/M 2.2
; 
cmd_cpm2:
	; copy CP/M (CCP + BDOS) to RAM
	ld hl, CPM_BIN
	ld de, $e000 ;$dc00						; alter this if CP/M and/or BIOS changes
	ld bc, CPM_LENGTH
	ldir

	; copy CBIOS to RAM
	ld hl, CBIOS_BIN
	ld de, $f600 ;$f200						; alter this if CP/M and/or BIOS changes
	ld bc, CBIOS_LENGTH
	ldir

	; initiate page 0
	ld hl, page0
	ld de, $0000
	ld bc, 8
	ldir

	; run CP/M
	call rom_off							; disable ROM - LED state shows CP/M is loaded in to memory

	jp 0f600h ;$f200						; alter this if CP/M and/or BIOS changes

;cmd_cpm_end:
;	ret
;
page0:
	db $c3
bios_base:
	db $03
	db $f6 ;$f2								; alter this if CP/M and/or BIOS changes
iobyte:
	db $00
disk_user:
	db $00
bdos_base:
	db $c3
	db $06
	db $e8 ;$e4								; alter this if CP/M and/or BIOS changes
;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cmd_cpm3 - load CP/M Plus	
; 
; read boot disk (A:) track 0 sector 2 thru track 1 sector 26
; write contents to RAM $0100
; NOTE first two tracks (0 and 1) do not use a skew
cmd_cpm3:
	ld hl, str_cpm3_msg1
	call PRINT_STR

	call ch376s_init					; initialise CH376S module
										; mount disk A
	ld hl, cpm3_driveA					; boot disk image filename = "/CPM3.DSK"
	call open_file
	jp nz, cmd_cpm3_open_failed


	ld b, 25+26                         ; remainder of track 0 sectors + all of track 1
    ld hl, 100h                         ; writing data to TPA
    ld (dmaad), hl 
	ld hl, 1
    ld (sector), hl						; skip first sector
ldrloop:
    push bc								; save loop count

    call seek							; seek to file position
    call read_from_file					; read 128 bytes (one sector) and write to dma buffer
	cp 0
	jp nz, ldr_read_failed				; read failed

    ld hl, (dmaad)				
    ld de, 128
    add hl, de
    ld (dmaad), hl                      ; add 128 bytes to dma buffer (TPA)
    ld hl, (sector)
    inc hl
    ld (sector), hl						; increment sector count for seek
    pop bc
    djnz ldrloop                        ; next sector
	
	; copy 'disable' code to disable ROM in to RAM
	ld hl, bootmesrc
	ld de, bootme
	ld bc, bootmeend-bootmesrc			; size of code in bytes
	ldir								; copy instructions to RAM

	jp bootme							; execute ROM disable and jump to CP/M 3 loader
	
bootme 		equ 8004h					
bootmesrc 	equ $
			db 	0dbh, 0ch				; IN   A, (12)
			db 	0f6h, 08h				; OR   %00001000
			db 	0d3h, 0ch				; OUT  (12), A
			db 	0c3h, 00h, 01h			; JP   0100h
bootmeend	equ $	

ldr_read_failed:
    pop bc                              ; sync stack
    ld hl, str_cpm3_read_failed
    call PRINT_STR
    halt



cmd_cpm3_end:
	ret

cmd_cpm3_read_failed:
	ld hl, str_cpm3_read_failed
	call PRINT_STR
	ret

str_cpm3_read_failed:
	db "CP/M 3 Cold Boot Loader: Read failed", CR, LF, EOS

cmd_cpm3_open_failed:
	ld hl, str_cpm3_open_failed
	call PRINT_STR
	ret

str_cpm3_open_failed:
	db "CP/M 3 Cold Boot Loader: Open /CPM3.DSK failed", CR, LF, EOS

str_cpm3_msg1: 	
	db "Configure CH376 module...", CR, LF, EOS


;
; function seek
;
seek:	
	ld de, 0
	ld hl, (sector)						; for this loader, sectors are consecutive up to sector 51
	add hl, hl							; * 2
	add hl, hl							; * 4
	add hl, hl							; * 8
	add hl, hl							; * 16
	add hl, hl							; * 32
	add hl, hl							; * 64
	add hl, hl							; * 128

_seek:
	call move_to_file_pointer 			; altered to use dehl
	cp USB_INT_SUCCESS
	ret z

	ld hl, str_seek_fail
	call PRINT_STR
	ret
; end drive_seek

str_seek_fail:
	db "Cold Boot Loader - Seek failed ", CR, LF, EOS

; IMPORTANT
; r/w variables can not be in ROM space
dmaad 	equ 8000h								; needed for ch376s driver
sector 	equ 8002h
