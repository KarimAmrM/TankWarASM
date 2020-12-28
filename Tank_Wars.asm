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





RemoveValueBuffer MACRO
    LOCAL NO_value
        MOV AH,1
        INT 16h
        JZ NO_value
        MOV AH,0
        INT 16H
        NO_value:nop
        
        
ENDM RemoveValueBuffer

collisionDetection Macro xt,xo,Lt1,Lo
        LOCAL secondCheck
        LOCAL False
        LOCAL True
        LOCAL terminate
        
                MOV Ax,xt               ;we check to see if the object at xo,yo with length Lo collided with a tank, to get a collision the object has to be in the x range of the tank and its y range
                cmp AX,xo               
                JG secondCheck          ;if(xo<xt) go to second condition  
                ADD AX,Lt1                     
                cmp AX,xo               ;if(xo>x+lt) then there's no collision and we go to false label to set collision to 0                   
                JL False                
                JMP True
                
        secondCheck:            ;the other check 
                mov ax,xo               
                add ax,Lo
                cmp Ax,xt               ;if(xt>xo+Lo) then there's no collision and we go to false label to set collision to 0
                JL False
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
        Tank_length dw    28d
	x       dw    ?
	y       dw    ?
        yObs    dw    ?
        xObs    dw    ?
        lobs    dw    ?
        wobs     dw    ?
Obstacles DW 2,50,50,10,5,1,20,50,10,5,1 ;nObstacles, x , y , width, length of obstacles , DrawStatus 1: to be drawn 0: Destroyed
LengthRec DW ?
.code
delay       proc 
            mov     cx, 002H
         delRep: push    cx
            mov     cx, 0D090H
         delDec: dec     cx
            jnz     delDec
            pop     cx
            dec     cx
            jnz     delRep
            ret
delay       endp

Move_Tank1 proc
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
        No_Movement:    ret
Move_Tank1 ENDp

DrawTank1 proc 
                       mov                si,2
	               MOv                Dx,Y_posi
	firstloop:     
	               MOV                CX,X_Posi1
	               inc                CX
	               DrawHorizontalLine CX,DX,26D,00
	               MOV                CX,X_Posi1
	               inc                CX
	               add                dx ,Tank_length
	               DrawHorizontalLine CX,DX,26D,00
	               sub                dx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               inc                dx
	               dec                SI
	               jnz                firstloop

	               dec                dx

	               mov                si,4
	secondloop:    
	               mov                Cx,X_Posi1
	               inc                dx
	               DrawHorizontalLine CX,DX,1,00
	               DrawHorizontalLine CX,DX,26,01
	               DrawHorizontalLine CX,DX,1,00
	               mov                Cx,X_Posi1
	               add                dx,Tank_length
	               DrawHorizontalLine CX,DX,1,00
	               DrawHorizontalLine CX,DX,26,01
	               DrawHorizontalLine CX,DX,1,00
	               sub                dx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jz                 beginthirdloop
	               jmp                secondloop

	beginthirdloop:mov                si,2
	thirdloop:     
	               mov                cx,X_Posi1
	               inc                dx
	               DrawHorizontalLine CX,DX,28,00
	               mov                cx,X_Posi1
	               add                dx,Tank_length
	               DrawHorizontalLine CX,DX,28,00
	               sub                dx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                thirdloop

	               mov                si,4
	fourthloop:    
	               mov                cx,X_Posi1
	               inc                dx
	               inc                cx
	               DrawHorizontalLine cx,dx,5,00
	               DrawHorizontalLine cx,dx,15,01
	               DrawHorizontalLine cx,dx,3,00
	               mov                cx,X_Posi1
	               add                dx,Tank_length
	               inc                cx
	               DrawHorizontalLine cx,dx,5,00
	               DrawHorizontalLine cx,dx,15,01
	               DrawHorizontalLine cx,dx,3,00
	               sub                dx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jz                 beginfifthloop
	               jmp                fourthloop

	beginfifthloop:mov                si , 5
	fifthloop:     mov                cx,X_Posi1
	               inc                dx
	               add                cx,2
	               DrawHorizontalLine cx,dx,28,00
	               dec                SI
	               jnz                fifthloop
        mov Tank_length,28d
        ret
DrawTank1 endp

DrawObstacles proc

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
                ret
DrawObstacles endp
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
                Call Move_Tank1
                Call DrawObstacles
                Call DrawTank1
	        
                

                ;process input : update tank coordinates , health , score , bullets
                ;update : change data , collision
                ; render : draw 

                        
                jmp labeltest      

	          mov           AH,0Ch
	          int           21h
                  

MAIN ENDP
END MAIN


