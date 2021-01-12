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
Send MACRO letter
LOCAL AGAINS
        
        ;Check that Transmitter Holding Register is Empty

        mov dx , 3FDH ; Line Status Register
        AGAINS: In al , dx ;Read Line Status
        test al , 00100000b
        JZ AGAINS ;Not empty

        ;If empty put the VALUE in Transmit data register
        mov dx , 3F8H ; Transmit data register
        mov al,letter
        out dx , al

ENDM Send
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ConfigureComms MACRO 
        mov dx,3fbh ; Line Control Register
        mov al,10000000b ;Set Divisor Latch Access Bit
        out dx,al ;Out it

        mov dx,3f8h
        mov al,0ch
        out dx,al

        mov dx,3f9h
        mov al,00h
        out dx,al
        mov  dx,3fbh
        mov  al,00011011b
        out  dx,al
ENDM ConfigureComms
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

Receive MACRO 
LOCAL chkR
LOCAL   againR
     
;Check that Data is Ready

mov dx , 3FDH ; Line Status Register

    chkR:
        in al,dx
        and al,1
        jz chkR

        mov dx , 03F8H
        in al , dx
        mov letterToReceive,al

        mov dx,3FDH
        againR:  
        in al,dx
        and al,00100000b
        jz againR
        mov dx,3f8h
        mov al,confirmReceive
        out dx,al

ENDM Receive
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
.MODEL small
.STACK 64
.DATA
	player1         db "player1$"
	player2         db "player2$"
	letterToSend    db ?
	msgToSend       db 60,?,60 dup('$')
    MsgToReceive    db 60 dup('$')
    letterToReceive db ?
    confirmReceive  db 10

.CODE
Main proc near
	     mov           ax,@data
	     mov           ds,ax
        
  
	    ClearScreen
        DrawLine 12,'-'

	     MoveCursor    0,0     	;area where player receives msg
	     DisplayString player1

	     MoveCursor    0,13
	     DisplayString player2

	     ;MoveCursor    7,15    	;where user types his message
	     ;;DisplayString player1


    chatting:
           MoveCursor 7,15


	       mov  ah,0AH
	       mov  dx,offset msgToSend
	       int  21h
           
           RemoveValueBuffer

 		   mov ah,1
           int 16H
           cmp al,1ch
           je SendMessage

    SendMessage:
         DrawLine 14,' '
            DrawLine 15,' '
            MoveCursor 7,14
            DisplayString msgToSend+2
            ResetMessage msgToSend+2,60
         jmp chatting





main endp
end main