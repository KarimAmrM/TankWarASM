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
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RemoveValueBuffer MACRO
    LOCAL NO_value
        MOV AH,1
        INT 16h
        JZ NO_value
        MOV AH,0
        INT 16H
        NO_value:nop
        
        
ENDM RemoveValueBuffer
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
collisionDetection Macro x1,x2,L1,L2
        LOCAL secondCheck
        LOCAL False
        LOCAL True
        LOCAL terminate
        
                MOV Ax,x1               ;we check to see if the object at xo,yo with length Lo collided with a tank, to get a collision the object has to be in the x range of the tank and its y range
                cmp AX,x2               
                JG secondCheck          ;if(xo<xt) go to second condition  
                ADD AX,L1                     
                cmp AX,x2               ;if(xo>x+lt) then there's no collision and we go to false label to set collision to 0                   
                JL False                
                JMP True
                
        secondCheck:            ;the other check 
                mov ax,x2               
                add ax,L2
                cmp Ax,x1               ;if(xt>xo+Lo) then there's no collision and we go to false label to set collision to 0
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
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawTankDown MACRO Xpos,Ypos,Colour,BackgroundColour,TankColour
        LOCAL firstloopTDown
        LOCAL secondloopTDown
        LOCAL thirdloopTDown
        LOCAL fourthloopTDown
        LOCAL fifthloopTDown       
                        mov                si,2
                       MOV                CX,Xpos
	firstloopTDown:     
	               MOV                Dx,Ypos
	               DrawVerticalLine CX,DX,1,BackgroundColour
	               DrawVerticalLine CX,DX,26D,TankColour
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               MOV                Dx,Ypos
	               add                Cx ,Tank_length
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               DrawVerticalLine CX,DX,26D,TankColour
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               sub                Cx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               inc                CX
	               dec                SI
	               jNz                firstloopTDown

                       dec                CX
	               mov                si,4
	secondloopTDown:    
	               mov                Dx,Ypos
	               inc                Cx
	               DrawVerticalLine CX,DX,1,TankColour
	               DrawVerticalLine CX,DX,26,Colour
	               DrawVerticalLine CX,DX,1,TankColour
	               mov                Dx,Ypos
	               add                CX,Tank_length
	               DrawVerticalLine CX,DX,1,TankColour
	               DrawVerticalLine CX,DX,26,Colour
	               DrawVerticalLine CX,DX,1,TankColour
	               sub                CX ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                secondloopTDown

                       mov                si,2
	thirdloopTDown:     
	               mov                DX,Ypos
	               inc                Cx
	               DrawVerticalLine CX,DX,28,TankColour
	               mov                Dx,Ypos
	               add                Cx,Tank_length
	               DrawVerticalLine CX,DX,28,TankColour
	               sub                CX,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                thirdloopTDown

	               mov                si,4
	fourthloopTDown:    
	               mov                DX,Ypos
	               inc                Cx
	               DrawVerticalLine CX,DX,1,BackgroundColour
	               DrawVerticalLine cx,dx,3,TankColour
	               DrawVerticalLine cx,dx,15,Colour
	               DrawVerticalLine cx,dx,5,TankColour
                       DrawVerticalLine CX,DX,4,BackgroundColour
	               mov                DX,Ypos
	               add                Cx,Tank_length
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               DrawVerticalLine cx,dx,3,TankColour
	               DrawVerticalLine cx,dx,15,Colour
	               DrawVerticalLine cx,dx,5,TankColour
                       DrawVerticalLine CX,DX,4,BackgroundColour
	               sub                Cx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                 fourthloopTDown

                       mov                si , 5
	fifthloopTDown:   
                       mov                DX,Ypos
	               inc                CX
	               DrawVerticalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTDown
                       mov Tank_length,28d 
ENDM DrawTankDown
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawTankUP MACRO Xpos,Ypos,Colour,BackgroundColour,TankColour
        LOCAL firstloopTup
        LOCAL secondloopTup
        LOCAL thirdloopTup
        LOCAL fourthloopTup
        LOCAL fifthloopTup       
                        mov                si,2
                       MOV                CX,Xpos
	firstloopTup:     
	               MOV                Dx,Ypos
	               DrawVerticalLine CX,DX,1,BackgroundColour
	               DrawVerticalLine CX,DX,26D,TankColour
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               MOV                Dx,Ypos
	               add                Cx ,Tank_length
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               DrawVerticalLine CX,DX,26D,TankColour
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               sub                Cx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               inc                CX
	               dec                SI
	               jNz                firstloopTup

                       dec                CX
	               mov                si,4
	secondloopTup:    
	               mov                Dx,Ypos
	               inc                Cx
	               DrawVerticalLine CX,DX,1,TankColour
	               DrawVerticalLine CX,DX,26,Colour
	               DrawVerticalLine CX,DX,1,TankColour
	               mov                Dx,Ypos
	               add                CX,Tank_length
	               DrawVerticalLine CX,DX,1,TankColour
	               DrawVerticalLine CX,DX,26,Colour
	               DrawVerticalLine CX,DX,1,TankColour
	               sub                CX ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                secondloopTup

                       mov                si,2
	thirdloopTup:     
	               mov                DX,Ypos
	               inc                Cx
	               DrawVerticalLine CX,DX,28,TankColour
	               mov                Dx,Ypos
	               add                Cx,Tank_length
	               DrawVerticalLine CX,DX,28,TankColour
	               sub                CX,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                thirdloopTup

	               mov                si,4
	fourthloopTup:    
	               mov                DX,Ypos
	               inc                Cx
	               DrawVerticalLine CX,DX,4,BackgroundColour
	               DrawVerticalLine cx,dx,5,TankColour
	               DrawVerticalLine cx,dx,15,Colour
	               DrawVerticalLine cx,dx,3,TankColour
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               mov                DX,Ypos
	               add                Cx,Tank_length
                       DrawVerticalLine CX,DX,4,BackgroundColour
	               DrawVerticalLine cx,dx,5,TankColour
	               DrawVerticalLine cx,dx,15,Colour
	               DrawVerticalLine cx,dx,3,TankColour
                       DrawVerticalLine CX,DX,1,BackgroundColour
	               sub                Cx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                 fourthloopTup

                       mov                si , 5
	fifthloopTup:   
                       mov                DX,Ypos
	               inc                CX
	               DrawVerticalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTup
                       mov Tank_length,28d 
ENDM DrawTankUP
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawTankRight MACRO Xpos,Ypos,Colour,BackgroundColour,TankColour
        LOCAL firstloopTR
        LOCAL secondloopTR
        LOCAL thirdloopTR
        LOCAL fourthloopTR
        LOCAL fifthloopTR

                       mov                si,2
	               MOV                Dx,Ypos
	firstloopTR:     
	               MOV                CX,Xpos
	               DrawHorizontalLine CX,DX,1,BackgroundColour
	               DrawHorizontalLine CX,DX,26D,TankColour
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               MOV                CX,Xpos
	               add                dx ,Tank_length
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               DrawHorizontalLine CX,DX,26D,TankColour
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               sub                dx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               inc                dx
	               dec                SI
	               jnz                firstloopTR  
	               
                       dec                dx
	               mov                si,4
	secondloopTR:    
	               mov                Cx,Xpos
	               inc                dx
	               DrawHorizontalLine CX,DX,1,TankColour
	               DrawHorizontalLine CX,DX,26,Colour
	               DrawHorizontalLine CX,DX,1,TankColour
	               mov                Cx,Xpos
	               add                dx,Tank_length
	               DrawHorizontalLine CX,DX,1,TankColour
	               DrawHorizontalLine CX,DX,26,Colour
	               DrawHorizontalLine CX,DX,1,TankColour
	               sub                dx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                secondloopTR


	               mov                si,2
	thirdloopTR:     
	               mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine CX,DX,28,TankColour
	               mov                cx,Xpos
	               add                dx,Tank_length
	               DrawHorizontalLine CX,DX,28,TankColour
	               sub                dx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                thirdloopTR

	               mov                si,4
	fourthloopTR:    
	               mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine CX,DX,1,BackgroundColour
	               DrawHorizontalLine cx,dx,3,TankColour
	               DrawHorizontalLine cx,dx,15,Colour
	               DrawHorizontalLine cx,dx,5,TankColour
                       DrawHorizontalLine CX,DX,4,BackgroundColour
	               mov                cx,Xpos
	               add                dx,Tank_length
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               DrawHorizontalLine cx,dx,3,TankColour
	               DrawHorizontalLine cx,dx,15,Colour
	               DrawHorizontalLine cx,dx,5,TankColour
                       DrawHorizontalLine CX,DX,4,BackgroundColour
	               sub                dx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                fourthloopTR

                       mov                si , 5
	fifthloopTR:    
                       mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTR
                       mov Tank_length,28d
ENDM DrawTankRight
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawTankLeft MACRO Xpos,Ypos,Colour,BackgroundColour,TankColour
        LOCAL firstloopTL
        LOCAL secondloopTL
        LOCAL thirdloopTL
        LOCAL fourthloopTL
        LOCAL fifthloopTL

                       mov                si,2
	               MOV                Dx,Ypos
	firstloopTL:     
	               MOV                CX,Xpos
	               DrawHorizontalLine CX,DX,1,BackgroundColour
	               DrawHorizontalLine CX,DX,26D,TankColour
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               MOV                CX,Xpos
	               add                dx ,Tank_length
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               DrawHorizontalLine CX,DX,26D,TankColour
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               sub                dx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               inc                dx
	               dec                SI
	               jnz                firstloopTL  
	               
                       dec                dx
	               mov                si,4
	secondloopTL:    
	               mov                Cx,Xpos
	               inc                dx
	               DrawHorizontalLine CX,DX,1,TankColour
	               DrawHorizontalLine CX,DX,26,Colour
	               DrawHorizontalLine CX,DX,1,TankColour
	               mov                Cx,Xpos
	               add                dx,Tank_length
	               DrawHorizontalLine CX,DX,1,TankColour
	               DrawHorizontalLine CX,DX,26,Colour
	               DrawHorizontalLine CX,DX,1,TankColour
	               sub                dx ,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                secondloopTL


	               mov                si,2
	thirdloopTL:     
	               mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine CX,DX,28,TankColour
	               mov                cx,Xpos
	               add                dx,Tank_length
	               DrawHorizontalLine CX,DX,28,TankColour
	               sub                dx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                thirdloopTL

	               mov                si,4
	fourthloopTL:    
	               mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine CX,DX,4,BackgroundColour
	               DrawHorizontalLine cx,dx,5,TankColour
	               DrawHorizontalLine cx,dx,15,Colour
	               DrawHorizontalLine cx,dx,3,TankColour
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               mov                cx,Xpos
	               add                dx,Tank_length
                       DrawHorizontalLine CX,DX,4,BackgroundColour
	               DrawHorizontalLine cx,dx,5,TankColour
	               DrawHorizontalLine cx,dx,15,Colour
	               DrawHorizontalLine cx,dx,3,TankColour
                       DrawHorizontalLine CX,DX,1,BackgroundColour
	               sub                dx,Tank_length
	               mov                ax,Tank_length
	               sub                ax,2
	               mov                Tank_length,ax
	               dec                SI
	               jnz                fourthloopTL

                       mov                si , 5
	fifthloopTL:    
                       mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTL
                       mov                Tank_length,28d
ENDM DrawTankLeft
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawFilledRectangle MACRO Xpos,Ypos,LengthR,WidthR,Colour
        LOCAL Draw
        PUSHA
        MOV CX,Xpos
        MOV DX,Ypos
        MOV DI,Xpos
        ADD DI,LengthR
Draw:
        DrawVerticalLine CX,DX,WidthR,Colour
        MOV DX,Ypos
        INC CX
        CMP CX,DI
        JNZ Draw
        POPA
ENDM DrawFilledRectangle
.MODEL Medium
.386
.STACK 64
.DATA
	Tank1 Label Word
	Tank1_Xpos  dw    150
	Tank1_Ypos  dw    60
        Tank1_Status dw   "R"
	Tank2 Label Word
	Tank2_Xpos  dw    180
	Tank2_Ypos  dw    60
        Tank2_Status dw   "L"
        Tank_length dw    28d
	x       dw    ?
	y       dw    ?
        yObs    dw    ?
        xObs    dw    ?
        lobs    dw    ?
        wobs    dw    ?
        Bullets1 dw  20,80 dup(0)  
        Bullets2 dw  20,80 dup(0)                       ;xB [] , yB[+2] , DrawStatus[+4], Direction [+6]
        Obstacles DW 4,50,50,10,5,1,20,50,10,5,1,250,0,20,6,1,200,50,10,20,1 ;nObstacles, x , y[+2] , width[+4], length[+6] of obstacles , DrawStatus [+8],: to be drawn 0: Destroyed
        LengthRec DW  ?
        nBullets1 dw  ?
        nBullets2 dw  ?
        nObstacles dw ? 
.code
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
delay       proc 
            mov cx,00h
            mov dx,2120h
            mov ah,86h
            int 15h
            ret
delay       endp
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tank1Action proc
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
		CMP AH,39H
                JZ Fire_Tank1
                JMP No_Movement
        Move_up:
                MOV Tank1_Status,'UP'
                MOV AX,Tank1_Ypos
                CMP AX,0
                JZ Read_Value
                MOV DX,Tank1_Ypos
                ADD DX,28d
                push ax
                DrawHorizontalLine Tank1_Xpos,DX,28D,0Eh
                pop ax
                DEC AX
                MOV Tank1_Ypos,AX
                JMP Read_Value
        Move_Down:
                MOV Tank1_Status,'DO'
                MOV AX,Tank1_Ypos
                CMP AX,171
                JZ Read_Value
                MOV DX,Tank1_Ypos
                push ax
                DrawHorizontalLine Tank1_Xpos,DX,28D,0Eh
                pop ax
                INC AX
                MOV Tank1_Ypos,AX
                JMP Read_Value
        Move_Left:
                MOV Tank1_Status,'L'
                MOV AX,Tank1_Xpos
                CMP AX,0
                JZ Read_Value
                MOV CX,Tank1_Xpos
                add cx,28D
                push ax
                DrawVerticalLine Cx,Tank1_Ypos,28,0Eh
                 dec cx
                DrawVerticalLine Cx,Tank1_Ypos,28,0Eh
                pop ax
                DEC AX
                MOV Tank1_Xpos,AX
                JMP Read_Value
        Move_Right:
                MOV Tank1_Status,'R'
                MOV AX,Tank1_Xpos
                CMP AX,277
                JZ Read_Value
                MOV CX,Tank1_Xpos
                push ax
                DrawVerticalLine Cx,Tank1_Ypos,28,0Eh
                pop ax
                INC AX
                MOV Tank1_Xpos,AX
                JMP Read_Value
		Fire_Tank1:
		MOV SI , OFFSET Bullets1
		MOv DI , [SI]
		ADD SI , 2
	 StoreBullets1:
		MOV AX,[SI]+4
		CMP AX,0
		JNZ Occupied1
		MOV CX,Tank1_Xpos
		ADD CX,28D
		MOV [SI],CX
		MOV DX,Tank1_Ypos
		ADD DX,13D
		MOV [SI]+2,DX
		MOV [SI]+4,1
		jmp Read_Value
	Occupied1:
		ADD SI,6D
		DEC DI
		JNZ StoreBullets1
        Read_Value:
                RemoveValueBuffer
        No_Movement:    ret
 Tank1Action ENDp
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tank2Action proc
	              MOV                AH,1
	              INT                16h
	              CMP                AL,'w'
	              JZ                 Move_up2
	              CMP                AL,'s'
	              JZ                 Move_Down2
	              CMP                AL,'a'
	              JZ                 Move_Left2
	              CMP                AL,'d'
	              JZ                 Move_Right2
	              CMP                AL,'e'
	              JZ                 Fire_Tank2
	              JMP                No_Movement2
	Move_up2:     
                      MOV                Tank2_Status,'UP' 
	              MOV                AX,Tank2_Ypos
	              CMP                AX,0
	              JZ                 Read_Value2
	              MOV                DX,Tank2_Ypos
	              ADD                DX,28d
	              push               ax
	              DrawHorizontalLine Tank2_Xpos,DX,28D,0Eh
	              pop                ax
	              DEC                AX
	              MOV                Tank2_Ypos,AX
	              JMP                Read_Value2
	Move_Down2:
                      MOV                Tank2_Status,'DO'    
	              MOV                AX,Tank2_Ypos
	              CMP                AX,171
	              JZ                 Read_Value2
	              MOV                DX,Tank2_Ypos
	              push               ax
	              DrawHorizontalLine Tank2_Xpos,DX,28D,0Eh
	              pop                ax
	              INC                AX
	              MOV                Tank2_Ypos,AX
	              JMP                Read_Value2
	Move_Left2:    
                      MOV                Tank2_Status,'L' 
	              MOV                AX,Tank2_Xpos
	              CMP                AX,0
	              JZ                 Read_Value2
	              MOV                CX,Tank2_Xpos
	              add                cx,28D
	              push               ax
	              DrawVerticalLine   Cx,Tank2_Ypos,28,0Eh
	              dec                cx
	              DrawVerticalLine   Cx,Tank2_Ypos,28,0Eh
	              pop                ax
	              DEC                AX
	              MOV                Tank2_Xpos,AX
	              JMP                Read_Value2
	Move_Right2:
                      MOV                Tank2_Status,'R'    
	              MOV                AX,Tank2_Xpos
	              CMP                AX,277
	              JZ                 Read_Value2
	              MOV                CX,Tank2_Xpos
	              push               ax
	              DrawVerticalLine   Cx,Tank2_Ypos,28,0Eh
	              pop                ax
	              INC                AX
	              MOV                Tank2_Xpos,AX
	              JMP                Read_Value2
	Fire_Tank2:   
	              MOV                SI , OFFSET Bullets2
	              MOv                DI , [SI]
	              ADD                SI , 2
	StoreBullets2:
	              MOV                AX,[SI]+4
	              CMP                AX,0
	              JNZ                Occupied2
	              MOV                CX,Tank2_Xpos
                      add                cx,1
	              MOV                [SI],CX
	              MOV                DX,Tank2_Ypos
	              ADD                DX,13D
	              MOV                [SI]+2,DX
	              MOV                [SI]+4,1
	              jmp                Read_Value2
	Occupied2:    
	              ADD                SI,6D
	              DEC                DI
	              JNZ                StoreBullets2
	Read_Value2:   
	              RemoveValueBuffer
	No_Movement2:  ret
Tank2Action ENDP
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawTank1 proc 
          MOV AX,Tank1_Status
          CMP AX,'R'
          Jz right
          CMP AX,'L'
          JZ left
          CMP AX,'UP'
          JZ up
          CMP AX,'DO'
          JZ down
          up:
          DrawTankUP Tank1_Xpos,Tank1_Ypos,01H,0Eh,0
          JMP EndDrawTank1
          down:
          DrawTankDown Tank1_Xpos,Tank1_Ypos,01H,0Eh,0
          JMP EndDrawTank1
          left:
          DrawTankLeft Tank1_Xpos,Tank1_Ypos,01H,0Eh,0
          JMP EndDrawTank1
          right:
          DrawTankRight Tank1_Xpos,Tank1_Ypos,01H,0Eh,0
          EndDrawTank1:ret
DrawTank1 endp
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawTank2 proc
          MOV AX,Tank2_Status
          CMP AX,'R'
          Jz right2
          CMP AX,'L'
          JZ left2
          CMP AX,'UP'
          JZ up2
          CMP AX,'DO'
          JZ down2
          up2:
          DrawTankUP Tank2_Xpos,Tank2_Ypos,04H,0Eh,0
          JMP EndDrawTank2
          down2:
          DrawTankDown Tank2_Xpos,Tank2_Ypos,04H,0Eh,0
          JMP EndDrawTank2
          left2:
          DrawTankLeft Tank2_Xpos,Tank2_Ypos,04H,0Eh,0
          JMP EndDrawTank2
          right2:
          DrawTankRight Tank2_Xpos,Tank2_Ypos,04H,0Eh,0
          EndDrawTank2:ret
        ret
DrawTank2 endp
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawBullets proc
;Bullets from first tank
MOV SI,offset Bullets1
MOV DI,[SI]
add SI,02h
        Drawbullet1: 
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementBullets1
                    MOV CX,[SI]  
                    sub cx,4
                    MOV DX,[SI]+2
                    DrawHorizontalLine Cx,DX,4,0Eh
                    DrawHorizontalLine Cx,DX,4,01
                    MOV CX,[SI]
                    sub cx,4
                    inc dx
                    DrawHorizontalLine Cx,DX,4,0Eh
                    DrawHorizontalLine Cx,DX,4,01
        IncrementBullets1:
                          ADD SI,6 ;kemo rkz ya m3rs 8
                          dec DI
                          jnz Drawbullet1   

MOV SI,offset Bullets2
MOV DI,[SI]
add SI,02h
        Drawbullet2: 
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementBullets2
                    MOV CX,[SI]  
                    add cx,4
                    MOV DX,[SI]+2
                    DrawHorizontalLine Cx,DX,4,0Eh
                    sub cx,8
                    DrawHorizontalLine Cx,DX,4,04h
                    MOV CX,[SI]
                    add cx,4
                    inc dx
                    DrawHorizontalLine Cx,DX,4,0Eh
                    sub cx,8
                    DrawHorizontalLine Cx,DX,4,04H
        IncrementBullets2:
                          ADD SI,6 ;kemo rkz ya m3rs 8
                          dec DI
                          jnz Drawbullet2  
                          


DrawBullets endp;
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;-------------------------------------------------------------------------------------------------------------------------------------------------------------
MoveBullets proc
MOV SI,offset Bullets1
MOV DI,[SI]
add si,02h
        Movebullets1: 
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementMove
                    MOV CX,[SI]    
                    INC CX
                    CMP CX,316
                    jz setzero
                    MOV [SI],CX
                    jmp IncrementMove
        setzero:   
                    MOV [SI]+4,0h
                    mov CX,[si]
                    dec CX
                    DrawHorizontalLine cx,[SI]+2,5,0Eh
                    mov CX,[si]
                    dec CX
                    mov dx,[si]+2
                    inc dx
                    DrawHorizontalLine cx,dx,5,0Eh
        IncrementMove:
                    ADD SI,6
                    dec DI
                    jnz Movebullets1


MOV SI,offset Bullets2
MOV DI,[SI]
add si,02h
        Movebullets2: 
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementMove2
                    MOV CX,[SI]    
                    dec CX
                    CMP CX,0
                    jz setzero2
                    MOV [SI],CX
                    jmp IncrementMove2
        setzero2:   
                    MOV [SI]+4,0h
                    DrawHorizontalLine [si],[SI]+2,5,0Eh
                    mov dx,[si]+2
                    inc dx
                    DrawHorizontalLine [SI],dx,5,0Eh
        IncrementMove2:
                    ADD SI,6
                    dec DI
                    jnz Movebullets2            
               ret

MoveBullets ENDP

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Collision proc 
MOV  SI, OFFSET Obstacles
mov DI,[SI]
ADD SI,2
Tank1Obst:      
                 cmp [si]+8,0
                 jz IncrementObstacles1
              

                 collisionDetection Tank1_Xpos,[SI],Tank_length,[SI]+6  ;tank and obs
                 mov bl,al
                 collisionDetection Tank1_Ypos,[SI]+2,Tank_length,[SI]+4  ;tank and obs
                 and al,bl
                 jz IncrementObstacles1
                 mov [SI]+8,0
                 DrawRectangel [SI],[SI]+2,[SI]+4,[SI]+6,0Eh
IncrementObstacles1:add si,10D
                 dec DI
                 jnz Tank1Obst
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        MOV  SI, OFFSET Obstacles
        mov DI,[SI]
        ADD SI,2
Tank2Obst:      
                 cmp [si]+8,0
                 jz IncrementObstacles2
              

                 collisionDetection Tank2_Xpos,[SI],Tank_length,[SI]+6  ;tank and obs
                 mov bl,al
                 collisionDetection Tank2_Ypos,[SI]+2,Tank_length,[SI]+4  ;tank and obs
                 and al,bl
                 jz IncrementObstacles2
                 mov [SI]+8,0
                 DrawRectangel [SI],[SI]+2,[SI]+4,[SI]+6,0Eh
IncrementObstacles2:add si,10D
                 dec DI
                 jnz Tank2Obst
 ;--------------------------------------------------------------------------------------------------------------------------------------------------------------------                

                 Mov si,offset Bullets2
                 Mov di,[si]
                 
                 add si,2
Bullets2Tank1:
                 cmp [si]+4,0
                 jz IncBullets2
                 collisionDetection Tank1_Xpos,[SI],Tank_length,4  ;bullets from second tank and tank1
                 mov bl,al
                 collisionDetection Tank1_Ypos,[SI]+2,Tank_length,1  
                 and al,bl
                 jz IncBullets2
                 mov [SI]+4,0
                 mov cx,[si]
                 dec cx
                 mov [si],cx
                 DrawHorizontalLine [si],[SI]+2,7,0Eh
                 mov dx,[si]+2
                 inc dx
                 DrawHorizontalLine [SI],dx,7,0Eh
IncBullets2:
                 add si,6
                 dec DI
                 jnz Bullets2Tank1
                 
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
                Mov si,offset Bullets1
                Mov di,[si]
                add si,2
Bullets1Tank2:
                 cmp [si]+4,0
                 jz IncBullets1
                 collisionDetection Tank2_Xpos,[SI],Tank_length,4  ;bullets from first tank and tank2
                 mov bl,al
                 collisionDetection Tank2_Ypos,[SI]+2,Tank_length,1  
                 and al,bl
                 jz IncBullets1
                 mov [SI]+4,0
                 mov cx,[si]
                 sub cx,2
                 mov [si],cx
                 DrawHorizontalLine [si],[SI]+2,4,0Eh
                 mov dx,[si]+2
                 inc dx
                 DrawHorizontalLine [SI],dx,4,0Eh
IncBullets1:
                 add si,6
                 dec DI
                 jnz Bullets1Tank2



;-------------------------------------------------------------------------------------------------------------------------------------------------------------------- 

                mov si,offset Bullets1
                mov di,[si]
                mov nBullets1,di
                add si,2
                push si

                mov di,offset Obstacles
                mov si,[di]
                mov nObstacles,si
                add di,2
                pop si

 Bullets1Obst:
               cmp[si+4],0
               jz nextBullet
               cmp [DI+8],0
               JZ nextObstacle
               collisionDetection [si],[di],4,[di+6]
               mov bl,al
               collisionDetection [si+2],[di+2],1,[di+4]
               and al,bl
               jz nextObstacle ;jump if no collision
               mov [si+4],0
               mov [di+8],0
               mov cx,[si]
               sub cx,2
               mov [si],cx
               DrawHorizontalLine [si],[SI]+2,4,0Eh
               mov dx,[si]+2
               inc dx
               DrawHorizontalLine [SI],dx,4,0Eh

               DrawRectangel [di],[di]+2,[di]+4,[di]+6,0Eh
        nextBullet:
                add si,6
                mov di,offset Obstacles
                MOV AX,[DI]
                mov nObstacles,AX
                add di,2
                mov cx,nBullets1
                dec Cx
                mov nBullets1,cx
                jnz Bullets1Obst
                jmp CollisionOf2ndBullets
                
        nextObstacle:
                add di,10
                mov cx,nObstacles
                dec Cx
                mov nObstacles,cx
                jnz Bullets1Obst
                jmp nextBullet

        
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
         CollisionOf2ndBullets:
                mov si,offset Bullets2
                mov di,[si]
                mov nBullets2,di
                add si,2
                push si

                mov di,offset Obstacles
                mov si,[di]
                mov nObstacles,si
                add di,2
                pop si

 Bullets2Obst:
               cmp[si+4],0
               jz nextBullet2
               cmp [DI+8],0
               JZ nextObstacle2
               collisionDetection [si],[di],4,[di+6]
               mov bl,al
               collisionDetection [si+2],[di+2],1,[di+4]
               and al,bl
               jz nextObstacle2 ;jump if no collision
               mov [si+4],0
               mov [di+8],0
               mov cx,[si]
               dec cx
               mov [si],cx
               DrawHorizontalLine [si],[SI]+2,7,0Eh
               mov dx,[si]+2
               inc dx
               DrawHorizontalLine [SI],dx,7,0Eh

               DrawRectangel [di],[di]+2,[di]+4,[di]+6,0Eh
               jmp nextBullet2
        nextBullet2:
                add si,6
                mov di,offset Obstacles
                MOV AX,[DI]
                mov nObstacles,AX
                ADD DI,2
                mov cx,nBullets2
                dec Cx
                mov nBullets2,cx
                jnz Bullets2Obst
                jmp endl
                
        nextObstacle2:
                add di,10
                mov cx,nObstacles
                dec Cx
                mov nObstacles,cx
                jnz Bullets2Obst
                jmp nextBullet2






endl:
ret
Collision ENDP
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

MAIN proc FAR

	          mov           ax,@data
	          mov           Ds,ax
	          MOV           AH,0
	          MOV           AL,13H
	          int           10H

	          mov           al,0
	          mov           CX,00
	          mov           DX,1827h
	          mov           ah,6
	          mov           bh,0Eh
	          int           10h


                      
	labeltest:
                
              
                call delay

                Call Tank1Action
                call Tank2Action
                Call MoveBullets
                call Collision
                Call DrawObstacles
                call DrawBullets
                Call DrawTank1
                Call DrawTank2
                ;DrawTankDown 80,80,03,0Eh,00H
                ;DrawFilledRectangle 0,0,1000,20000,00h

                jmp labeltest  
MAIN ENDP
END MAIN





