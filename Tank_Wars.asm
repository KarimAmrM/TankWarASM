DrawVerticalLine MACRO X,Y,L,Colour ;Draws a single vertical line from x,y to x,y+l
LOCAL Draw
MOV CX,X
MOV DX,Y
MOV BX,Y
ADD BX,L
MOV AH,0CH
MOV AL,Colour


Draw:INt 10h
INC DX
CMP DX,BX
JNZ Draw
ENDM DrawVerticalLine

DrawHorizontalLine MACRO X,Y,L,Colour ;Darws a single horizontal line from x,y to x+l,z
LOCAL Draw
MOV CX,X
MOV DX,Y
MOV BX,X
ADD BX,L

MOV AH,0CH
MOV AL,Colour

Draw:INt 10h
INC CX
CMP CX,BX
JNZ Draw 
ENDM DrawHorizontalLine

DrawTank MACRO 
    ;Cx Holds the x values for drawing
    ;Dx Holds the y values for drawing


    DrawVerticalLine X_Posi1,Y_posi,28d,02h ;draw first vertical line
    MOV CX,X_Posi1 ;mirrors the vertical line by adding the tank's length
    ADD CX,28d
    DrawVerticalLine CX,Y_posi,28d,02h
    DrawHorizontalLine X_Posi1,Y_posi,28d,02h ; draw first horizontal line
    MOV DX,Y_Posi    ;mirrors the horizontal line by adding the tank's length
    ADD DX,28d
    DrawHorizontalLine X_Posi1,DX,29D,02h

    MOV AX,28d
    MOV BX,04D
    DIV BL
    MOV AH,0
    MOV DX,Y_Posi
    ADD DX,AX
    MOV CX,X_posi1
    ADD CX,AX
    mov x,cx
    mov y,dx
    DrawVerticalLine x,y,14D,02h 

    MOV AX,28d
    MOV BX,04D
    DIV BL
    MOV AH,0
    MOV DX,Y_Posi
    ADD DX,AX
    MOV CX,X_posi1
    ADD CX,AX
    mov x,cx
    mov y,dx
    DrawHorizontalLine x,y,15D,02h 

    MOV AX,28d
    MOV BX,04D
    DIV BL
    MOV AH,0
    MOV DX,Y_Posi
    ADD DX,AX
    MOV CX,X_posi1
    ADD CX,AX
    ADD DX,14D
    mov x,cx
    mov y,dx
    DrawHorizontalLine x,y,14D,02h 

    MOV AX,28d
    MOV BX,04D
    DIV BL
    MOV AH,0
    MOV DX,Y_Posi
    ADD DX,AX
    MOV CX,X_posi1
    ADD CX,AX
    ADD CX,14D
    inc DX
    mov x,cx
    mov y,dx
    DrawVerticalLine x,y,14D,02h 

ENDM DrawTank

.MODEL Medium
.STACK 64
.DATA
Tank1 LAbel Byte
X_Posi1 dw 12
Y_posi  dw 12
x dw ?
y dw ?
Helath  db 3
.code
MAIN proc FAR
mov ax,@data
mov Ds,ax
MOV AH,0
MOV AL,13H
int 10H

mov al,0
mov CX,00
mov DX,0FFFFh
mov ah,6
mov bh,0Eh
int 10h
DrawTank

mov AH,0Ch
int 21h

MAIN ENDP
END MAIN

