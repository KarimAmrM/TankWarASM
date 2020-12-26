DrawVerticalLine MACRO X,Y,L,Colour
LOCAL Draw
MOV AH,0CH
MOV AL,Colour
MOV CX,X
MOV DX,Y
MOV BX,Y
ADD BX,L

Draw:INt 10h
INC DX
CMP DX,BX
JNZ Draw

ENDM DrawVerticalLine

DrawHorizontalLine MACRO X,Y,L,Colour
LOCAL Draw
MOV AH,0CH
MOV AL,Colour
MOV CX,X
MOV DX,Y
MOV BX,X
ADD BX,L

Draw:INt 10h
INC CX
CMP CX,BX
JNZ Draw

ENDM DrawHorizontalLine

.MODEL Medium
.STACK 64
.DATA
.code
MAIN proc FAR

MOV AH,0
MOV AL,13H
int 10H

DrawHorizontalLine 15,20,200,02h

;DrawVerticalLine 16,0,200,03h
hlt

MAIN ENDP
END MAIN