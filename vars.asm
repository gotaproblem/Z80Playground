; ********************************************************
; *                                                      *
; * Z80 Home Build Development                           *
; * Steve Bradford                                       *
; * 01/05/2021                                           *
; *                                                      *
; * declerations                                         *
; *                                                      *
; * vars.asm                                             *
; *                                                      *
; ********************************************************
;

;
; 16C550 Dual UART Registers
;
; General Register Set
; only available when LCR is set to $00 (bit 7 = 0)
UIER:       EQU     1               	; Interrupt Enable Register friendly name
UISR:		EQU		2					; Interrupt Status Register friendly name
UFCR:       EQU     2               	; FIFO Control Register friendly name
ULCR:       EQU     3               	; Line Control Register friendly name
UMCR:       EQU     4               	; Modem Control Register friendly name
ULSR:       EQU     5               	; Line Status Register friendly name
UMSR        EQU     6               	; Modem Status Register friendly name
USPR:       EQU     7              		; Scratchpad Register friendly name

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
FLOW:		EQU		$22					; Enable Auto Flow Control (RTS/CTS)

; UART A IO Port Addres
UARTA:      EQU     $08                 ; mapped to IO Port 


; UARTA Register Address offsets
UAIER:      EQU     UARTA + UIER
UAISR:		EQU		UARTA + UISR
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


; 16C550 Baud Rate defines
; Baud rate is a function of (clock speed / 16) / chosen baud rate
; example: (7.3728 MHz / 16) / 9600 = 48
;
BAUD_MAX    EQU     1
;BAUD_1200   EQU     384
BAUD_2400   EQU     192
BAUD_4800   EQU     96
BAUD_9600   EQU     48
BAUD_19200  EQU     24
BAUD_38400  EQU     12
BAUD_57600  EQU     8
BAUD_115200 EQU     4


; ROM Monitor Memory Map
; 0xFFFF
; 0xFF00 0.25K Variable space - see below
; 0xFEFF
; 0xFE00 0.25K Stack
; 0xFDFF
; 0x8000 31.5K RAM
; 0x7FFF
; 0x0000 32k ROM - ROM can be switched out to increase RAM
;
; Variable Space
;

VAR_ADDR    EQU     $CE00               ; 256 bytes
STACKTOP    EQU     $CFFF				; sits under CP/M
STACKBOTTOM EQU     $CF00               ; 256 bytes of stack
RAMTOP      EQU     VAR_ADDR - 1     	; ref. Memory Map

VAR_SPACE   EQU     $0100

;
; define variable space (256 bytes available)
;
V_ROMSTATE  EQU     VAR_ADDR + $00
V_USER_LED1 EQU		V_ROMSTATE + $01
V_VARSPACE  EQU     V_USER_LED1 + $01
V_BOOTFLAG  EQU     V_VARSPACE + $01
V_ARGC      EQU     V_BOOTFLAG + $01    ; 1 byte number of arguments
V_ARGV      EQU     V_ARGC + $01        ; (16 * 2 bytes) address of each argument
V_CMD_BUFFER EQU    V_ARGV + $01 + 32   ; 128 bytes
V_CPM		EQU		V_CMD_BUFFER + $01 + 128
;
;        
EOS:        EQU     $00					; signify end of string with a NULL (0)
CR:         EQU     $0d
LF:         EQU     $0a
SPACE:      EQU     $20
BS:         EQU     $08
ESC:        EQU     $1B
;
;
ROM_DISABLE EQU     1
ROM_ENABLE  EQU     0
MEM_CNTL    EQU     UAMCR
ROM_CNTL    EQU     MEM_CNTL            ; enable or disable ROM

ENABLE      EQU     $01
DISABLE     EQU     $00

ULED1       EQU		UAMCR

; Z80 Playground control bits (uses UART MCR register)
; bit 0 = User LED (0)off/(1)on
; bit 2 = Disk LED (0)off/(1)on
; bit 3 = ROM (0)enable/(1)disable