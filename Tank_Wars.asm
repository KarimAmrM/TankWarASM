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

.MODEL Medium
.STACK 64
.DATA
.code
MAIN proc FAR

MOV AH,0
MOV AL,13H
int 10H

DrawVerticalLine 15,0,200,0Bh

;DrawVerticalLine 16,0,200,03h
hlt

MAIN ENDP
END MAIN