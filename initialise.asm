

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; function INIT_UART
; Initialise UART hardware
; entry:
;   A baud rate to configure
;
INIT_UART:  PUSH     AF
            ;
            ; test for presence of UART by writing to UART scratchpad register
            ; HALT if not
            ;
            ;LD      A,$55               ; pattern 1
            ;OUT     (UASPR),A
            ;IN      A,(UASPR)           ; read pattern back
            ;CP      $55                 
            ;JP      NZ,Terminate

            LD      A,00
            OUT     (UAIER),A           ; clear interrupt register

            LD      A,$80
            OUT     (UALCR),A           ; select Divisor Latch Register
UART_OK:    POP     AF

            CP      1
            JP      Z,UART1200
            CP      2
            JP      Z,UART2400
            CP      3
            JP      Z,UART4800
            CP      4
            JP      Z,UART9600
            CP      5
            JP      Z,UART19K2
            CP      6
            JP      Z,UART38K4
            CP      7
            JP      Z,UART57K6
            CP      8
            JP      Z,UART115K2

; if nothing is defined, default to max speed
            JP      UART460K8

            HALT

;
; configure UART registers for selected baud rate
UART1200:   
            LD      A,BAUD_1200                ; 
            JP      INITRET             ;

UART2400:   
            LD      A,BAUD_2400 
            JP      INITRET  

UART4800:      
            LD      A,BAUD_4800       
            JP      INITRET 

UART9600:   
            LD     A,BAUD_9600
            JP     INITRET 

UART19K2:   
            LD     A,BAUD_19200  
            JP     INITRET 

UART38K4:    
            LD     A,BAUD_38400
            JP     INITRET  

UART57K6:   
            LD     A,BAUD_57600
            JP     INITRET
            
UART115K2:  
            LD     A,BAUD_115200
            JP     INITRET

UART460K8:
            LD     A,BAUD_MAX

INITRET:    
            OUT     (UADLL),A 
            LD      A,0
            OUT     (UADLM),A 
            LD      A,NP81              ; deselect Divisor Latch and set ...
            OUT     (UALCR),A           ; 8 bit data, 1 stop bit, no parity
            
            ld      a, $01		        ; enable FIFO with trigger level of 1
	        out     (UAFCR), a			

            LD      A,FLOW              ; enable auto flow control, bit 5 = 1, bit 1 = 1
            OUT     (UAMCR),A
            RET
; function end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

