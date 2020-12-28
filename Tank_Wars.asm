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
    MOV AX,L
    INC AX
    MOV LengthRec,AX
    DrawHorizontalLine X,DX,LengthRec,Colour
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

Move_Tank1 MACRO
        MOV AH,1
        INT 16h
        CMP AH,48H
        JZ Move_up
        CMP AH,50H
        JZ Move_Down
        CMP AH,4BH
        JZ Move_Left
        CMP AH,4DH
        JZ Move_Right

        JMP No_Movement
Move_up:
        MOV AX,Y_Posi
        CMP AX,0
        JZ Read_Value
        DEC AX
        MOV Y_Posi,AX
        JMP Read_Value
Move_Down:
        MOV AX,Y_Posi
        CMP AX,171
        JZ Read_Value
        INC AX
        MOV Y_Posi,AX
        JMP Read_Value
Move_Left:
        MOV AX,X_posi1
        CMP AX,0
        JZ Read_Value
        DEC AX
        MOV X_posi1,AX
        JMP Read_Value
Move_Right:
        MOV AX,X_posi1
        CMP AX,277
        JZ Read_Value
        INC AX
        MOV X_posi1,AX
        JMP Read_Value
Read_Value:
        RemoveValueBuffer
        mov al,0
	    mov CX,00
	    mov DX,0FFFFh
	    mov ah,6
	    mov bh,0Eh 
	    int 10h
No_Movement:nop

ENDM Move_Tank1

RemoveValueBuffer MACRO
    LOCAL NO_value
        MOV AH,1
        INT 16h
        JZ NO_value
        MOV AH,0
        INT 16H
        NO_value:nop
        
        
ENDM RemoveValueBuffer

DrawObstacles Macro
LOCAL Draw
LOCAL Increment
        MOV SI, OFFSET Obstacles
        MOV DI,[SI]
        ADD SI,2
Draw :
        MOV AX,[SI]+8
        CMP AX,0
        JZ Increment
        DrawRectangel [SI],[SI]+2,[SI]+4,[SI]+6,01
Increment:
        ADD SI , 10D
        DEC DI
        JNZ Draw 

ENDM DrawObstacles


collisionDetection Macro xt,xo,Lt,Lo

        LOCAL secondCheck
        LOCAL False
        LOCAL true
        LOCAL terminate
        
                MOV Ax,xt               ;we check to see if the object at xo,yo with length Lo collided with a tank, to get a collision the object has to be in the x range of the tank and its y range
                cmp xo,AX               
                JL secondCheck          ;if(xo<xt) go to second condition  
                ADD AX,Lt                     
                cmp xo,AX               ;if(xo>x+lt) then there's no collision and we go to false label to set collision to 0                   
                JG False                
                JMP True
                
                secondCheck:            ;the other check 
                mov ax,xo               
                add ax,Lo
                cmp xt,Ax               ;if(xt>xo+Lo) then there's no collision and we go to false label to set collision to 0
                JG False
                JMP True

                False:
                mov al,0
                JMP terminate
                
                True:
                mov al,1
                jmp terminate
                
                terminate: nop
ENDM collisionDetection

.MODEL Medium
.STACK 64
.DATA
	        Tank1 Label Byte
	X_Posi1 dw    100
	Y_posi  dw    100
	x       dw    ?
	y       dw    ?
Helath  db 3
Obstacles DW 1,50,50,10,5,1 ;nObstacles, x , y , width, length of obstacles , DrawStatus 1: to be drawn 0: Destroyed
LengthRec DW ?
.code
MAIN proc FAR

	          mov           ax,@data
	          mov           Ds,ax
	          MOV           AH,0
	          MOV           AL,13H
	          int           10H

	          mov           al,0
	          mov           CX,00
	          mov           DX,0FFFFh
	          mov           ah,6
	          mov           bh,0Eh
	          int           10h

	labeltest:
                
                DrawObstacles        
	        DrawTank
	        Move_Tank1
                RemoveValueBuffer
	          jmp           labeltest
	          mov           AH,0Ch
	          int           21h
                  

MAIN ENDP
END MAIN


