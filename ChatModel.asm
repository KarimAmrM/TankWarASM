DisplayString macro string
mov ah,9
mov dx,offset string
int 21h
endm DisplayString 
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MoveCursor macro x,y
             mov           dl, x        	;Column
	         mov           dh, y          	;Row
	         mov           bh, 0           	;Display page
	         mov           ah, 02h         	;SetCursorPosition
	         int           10h
endm MoveCursor
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ClearScreen macro
    mov           ax,0600h
	mov           bh,07
	mov           cx,0
	mov           dx,184FH
	int           10h
endm ClearScreen
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawLine Macro Y,Character
	     MoveCursor    0,Y
	     mov           ah,9
	     mov           al,1
	     mov           bh,0
	     mov           al,Character
	     mov           cx,80d
	     mov           bl,0fH
	     int           10h
ENDM DrawLine
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RemoveValueBuffer MACRO
    LOCAL NO_value
        MOV AX,100H
        INT 16h
        CMP AX,100H
        JZ NO_value
        MOV AH,0
        INT 16H
        NO_value:nop      
ENDM RemoveValueBuffer

ResetMessage Macro msg,SizeString
    LOCAL Inerment
	mov si,offset msg
	mov cx,SizeString
 Inerment:	
	mov [si],'$'
	inc si
	dec cx
	jnz Inerment
ENDM ResetMessage
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
.MODEL small
.386
.STACK 64
.DATA
	player1         db "player1$"
	player2         db "player2$"
	letterToSend    db ?
	msgToSend       db 60,?,60 dup('$')
    MsgToReceive    db 60,?,60 dup('$')
    letterToReceive db ?
    confirmReceive  db 10

.CODE
InitializeSerialPort PROC	NEAR
	                            mov   dx,3fbh                                         	; Line Control Register
	                            mov   al,10000000b                                    	;Set Divisor Latch Access Bit
	                            out   dx,al                                           	;Out it

	                            mov   dx,3f8h                                         	;Set LSB byte of the Baud Rate Divisor Latch register.
	                            mov   al,0ch
	                            out   dx,al

	                            mov   dx,3f9h                                         	;Set MSB byte of the Baud Rate Divisor Latch register.
	                            mov   al,00h
	                            out   dx,al

	                            mov   dx,3fbh                                         	;Set port configuration
	                            mov   al,00011011b
	                            out   dx, al
	                            RET
InitializeSerialPort ENDP
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;sends value in AH
SendValueThroughSerial PROC	NEAR
	                            push  dx
	                            push  ax
	;Check that Transmitter Holding Register is Empty
	                            mov   dx , 3FDH                                       	; Line Status Register
	                            In    al , dx                                         	;Read Line Status
	                            test  al , 00100000b
	                            JNZ   EmptyLineRegister                               	;Not empty
	                            pop   ax
	                            pop   dx
	                            RET
	EmptyLineRegister:          
	;If empty put the VALUE in Transmit data register
	                            mov   dx , 3F8H                                       	; Transmit data register
	                            mov   al, ah
	                            out   dx, al
	                            pop   ax
	                            pop   dx
	                            RET
SendValueThroughSerial ENDP
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; receives a byte from serial stored in AH, and the AL is used a flag (0 means there is a value, 1 means no value was sent)
ReceiveValueFromSerial PROC	NEAR
	;Check that Data is Ready
	                            push  dx
	                            mov   dx , 3FDH                                       	; Line Status Register
	                            in    al , dx
	                            test  al , 1
	                            JNZ   SerialInput                                     	;Not Ready
	                            mov   al, 1
	                            pop   dx
	                            RET                                                   	;if 1 return
	SerialInput:                
	;If Ready read the VALUE in Receive data register
	                            mov   dx , 03F8H
	                            in    al , dx
	                            mov   ah, al
	                            mov   al, 0
	                            pop   dx
	                            RET
ReceiveValueFromSerial ENDP
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MAIN PROC 
         mov           ax,@data
	     mov           ds,ax
         CALL          InitializeSerialPort
	     ClearScreen

         DrawLine 12,'-'

	     MoveCursor    0,0     	;area where player receives msg
	     DisplayString player1

	     MoveCursor    0,13
	     DisplayString player2
         
 CheckKey:         
         MOV AH,1
         INT 16H
         CMP ah,1ch
         JZ BeginWriting
         CMP AH,3DH
         JZ EndChat
         RemoveValueBuffer
         jmp CheckRecieved

 BeginWriting:
           RemoveValueBuffer
           MoveCursor    7,15 
	       mov  ah,0AH
	       mov  dx,offset msgToSend
	       int  21h
            
           mov  si,offset msgToSend+1
	       mov  CL,BYTE PTR [si]         ;Send size of string
           mov  AH,CL
            mov dx , 3FDH		; Line Status Register
AGAIN1:  	In al , dx 			;Read Line Status
  	     	AND al , 00100000b
      		JZ AGAIN1

           Call SendValueThroughSerial
	       lea  si,msgToSend
	       add  si,2
	    SendNextLetter:  
        	mov dx , 3FDH		; Line Status Register
AGAIN:  	In al , dx 			;Read Line Status
  	     	AND al , 00100000b
      		JZ AGAIN

	       mov  AH,[SI]
           Call SendValueThroughSerial
           INC  SI
           DEC  CL   
           JNZ  SendNextLetter

            DrawLine 15," "
            MoveCursor 7,14
            DisplayString msgToSend+2
            ResetMessage msgToSend+2,60


           jmp CheckKey

CheckRecieved:
              CALL ReceiveValueFromSerial
              CMP AL,1
              JZ CheckKey
              LEA SI,MsgToReceive+1
              mov cl,ah
              INC SI
              MoveCursor 7,1
ReceiveNextLetter:
             mov dx , 3FDH		; Line Status Register
	         CHK1:	in al , dx 
  		     AND al , 1
  		     JZ CHK1

              CALL ReceiveValueFromSerial
              mov dl,ah
              MOV AH,2
              int 21h

              DEC CL
              JNZ ReceiveNextLetter
              MoveCursor 7,1
              
              DisplayString MsgToReceive+2
              JMP CheckKey

            

EndChat:
         ClearScreen
         MoveCursor 0,0
         MOV AH,4CH
         INT 21h
MAIN ENDP
END MAin