DrawVerticalLine MACRO X,Y,L,Colour
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

DrawHorizontalLine MACRO X,Y,L,Colour
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
    
    DrawVerticalLine X_Posi1,Y_posi,25D,02h
    MOV AX,X_Posi1
    ADD AX,25D
    DrawVerticalLine AX,Y_posi,25D,02h
    DrawHorizontalLine X_Posi1,Y_posi,25D,02h
    MOV AX,Y_Posi
    ADD AX,25D
    DrawHorizontalLine X_Posi1,AX,26D,02h




ENDM DrawTank

.MODEL Medium
.STACK 64
.DATA
Tank1 LAbel Byte
X_Posi1 dw 12
Y_posi dw 12
Helath db 3
.code
MAIN proc FAR
mov ax,@data
mov Ds,ax
MOV AH,0
MOV AL,13H
int 10H

DrawTank

;DrawVerticalLine 16,0,200,03h
hlt

MAIN ENDP
END MAIN