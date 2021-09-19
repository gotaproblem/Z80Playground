; ********************************************************
; *                                                      *
; * Z80 Home Build Development                           *
; * Steve Bradford                                       *
; * 01/05/2021                                           *
; *                                                      *
; * modified to run on Z80 Playground by 8bitStack.co.uk *
; *                                                      *
; * main.asm                                             *
; *                                                      *
; ********************************************************
;

; build options

            Include "vars.asm"



            ORG     $0000
COLD:       JP      INIT_HW ; monitor cold boot

            ORG     $0008
WARM:       JP      MAIN    ; monitor warm boot

            ORG     $0100
Enabled:    DB      "Enabled ", EOS
Disabled:   DB      "Disabled", EOS
RomState:   DB      "32K ROM is ", EOS
SignOn:    
            ;DB      $1b, '[2J', $1b, '[H'
            DB      "Monitor v1.1 August 2021, Steve Bradford\r\n"
            DB      "Z80 Playground [8bitStack.co.uk]\r\n"
            DB      0
Prompt:     DB      "MON> ", 0

;*******************************************************************
;*        MAIN PROGRAM                                             *
;*******************************************************************

            
            Include "initialise.asm"
            Include "lib.asm"
            ;Include "tests.asm"
            Include "monitor.asm"
            Include "commands.asm"
            include "ch376s_driver.asm"
;
; COLD BOOT
;
INIT_HW:    DI                          ; Disable interrupts

PAUSE:                                  ; allow UART to settle after a reset
            LD     B, 50                ; approximately 50ms
PAUSELOOP1: LD     A, 196               ; 10 MHz clock ~ 1ms
PAUSELOOP2: NOP                         ; loop cycles 51
            NOP
            NOP
            NOP
            NOP
            NOP
            DEC    A   
            JP     NZ, PAUSELOOP2

            DJNZ   PAUSELOOP1
COPY_ROM:
            LD      HL, $0000           ; copy ROM to RAM
            LD      DE, $0000
            LD      BC, $8000         
            LDIR
CLEAR_RAM:
            LD      HL, $8000           ; clear top 32K RAM
            LD      DE, $8001
            LD      BC, $7FFF
            LD      (HL), $00           ; fill with zero's
            LDIR

            ld      sp, STACKTOP        ; Set stack pointer under CP/M
            call    rom_on     
;
; WARM BOOT
;
MAIN:
            LD      A, 8                ; select baud rate, 0 = max speed
            CALL    INIT_UART           ; have to re-initialise the UART after a rst
            CALL    CLS
            CALL    PRINT_NEWLINE
            LD      HL, RomState
            CALL    PRINT_STR
            LD      A, (V_ROMSTATE)
            CP      ROM_ENABLE
            JR      NZ, ROMisDisabled

            CALL    rom_on
            LD      HL, Enabled
            CALL    PRINT_STR
            JR      CONT      

ROMisDisabled:
            CALL    rom_off
            LD      HL, Disabled
            CALL    PRINT_STR
CONT:       CALL    PRINT_NEWLINE
            LD      HL, SignOn
            CALL    PRINT_STR           ; display sign on message
            CALL    MONITOR 

            RST     $00                 ; don't expect to get here, but just in case, cold boot
;
;
; CP/M & CBIOS included within ROM
			org $1000					; just to make it easy to find
CPM_BIN:
            incbin "cpm22.bin"
CPM_LENGTH  equ $-CPM_BIN

			org $3000					; just to make it easy to find
CBIOS_BIN:
            incbin "cbios.bin"
CBIOS_LENGTH equ $-CBIOS_BIN
;
;
            ;print CPM_LENGTH
            ;print CBIOS_LENGTH
PROGRAMME_END:
            END
