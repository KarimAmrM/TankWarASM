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

DrawSquare MACRO X,Y,L,Colour
    DrawVerticalLine X,Y,L,Colour ;draw first vertical line
    MOV CX,X ;mirrors the vertical line by adding the tank's length
    ADD CX,L
    DrawVerticalLine CX,Y,L,Colour
    DrawHorizontalLine X,Y,L,Colour ; draw first horizontal line
    MOV DX,Y    ;mirrors the horizontal line by adding the tank's length
    ADD DX,L
    DrawHorizontalLine X,DX,L+1,Colour
ENDM DrawSquare

DrawRectangel MACRO X,Y,W,L,Colour
    DrawVerticalLine X,Y,W,Colour ;draw first vertical line
    MOV CX,X ;mirrors the vertical line by adding the tank's length
    ADD CX,L
    DrawVerticalLine CX,Y,W,Colour
    DrawHorizontalLine X,Y,L,Colour ; draw first horizontal line
    MOV DX,Y    ;mirrors the horizontal line by adding the tank's length
    ADD DX,W
    DrawHorizontalLine X,DX,L+1,Colour
ENDM DrawRectangel

DrawTank MACRO 
    ;Cx Holds the x values for drawing
    ;Dx Holds the y values for drawing
    DrawSquare X_Posi1,Y_Posi,28D,02h
    MOV AX,28d
    MOV BX,04D
    DIV BL
    MOV AH,0
    MOV DI,Y_Posi
    ADD DI,AX
    MOV SI,X_posi1
    ADD SI,AX
    DrawSquare SI,DI,14D,02H
    
    MOV AX,28d
    MOV BX,02D
    DIV BL
    MOV AH,0
    MOV DI,Y_Posi
    ADD DI,AX
    MOV SI,X_posi1
    ADD SI,AX
    DEC DI
    DrawRectangel SI,DI,3,28D,07H


ENDM DrawTank



.MODEL Medium
.STACK 64
.DATA
Tank1 LAbel Byte
X_Posi1 dw 100
Y_posi  dw 100
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
labeltest:
DrawTank
DrawRectangel 00,00,5,7,02
jmp labeltest
mov AH,0Ch
int 21h

MAIN ENDP
END MAIN

