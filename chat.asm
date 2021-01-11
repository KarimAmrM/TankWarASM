ClearScreen macro
    mov           ax,0600h
	mov           bh,07
	mov           cx,0
	mov           dx,184FH
	int           10h
endm ClearScreen
.MODEL small
.STACK 64
.DATA
   inputString1 DB 'Please Enter your name:','$'
.CODE
Main proc near

mov ah,0h
mov al,13h 
INT 10h 

mov ah,6 ; function 6
mov al,1 ; scroll by 1 line
mov bh,0FH ; normal video attribute
mov ch,0 ; upper left Y
mov cl,0 ; upper left X
mov dh,12 ; lower right Y
mov dl,79 ; lower right X
int 10h
        
        mov  dl, 0   ;Column
        mov  dh, 0   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 0  ;SetCursorPosition
        int  10h

        mov al, 1
	mov bh, 0
	mov bl, 0011_1011b
	mov cx, msg1end - offset msg1 ; calculate message size. 
	mov dl, 10
	mov dh, 7
	push cs
	pop es
	mov bp, offset msg1
	mov ah, 13h
	int 10h


main endp
end main