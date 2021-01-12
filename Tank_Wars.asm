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
        CMP AX,100H
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
                JLE False
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

                       mov                si , 4
	fifthloopTDown:   
                       mov                DX,Ypos
	               inc                CX
	               DrawVerticalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTDown
                       mov Tank_length,27d 
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

                       mov                si , 4
	fifthloopTup:   
                       mov                DX,Ypos
	               inc                CX
	               DrawVerticalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTup
                       mov Tank_length,27d 
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

                       mov                si , 4
	fifthloopTR:    
                       mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTR
                       mov Tank_length,27d
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

                       mov                si , 4
	fifthloopTL:    
                       mov                cx,Xpos
	               inc                dx
	               DrawHorizontalLine cx,dx,28,TankColour
	               dec                SI
	               jnz                fifthloopTL
                       mov                Tank_length,27d
ENDM DrawTankLeft
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawFilledRectangle MACRO Xpos,Ypos,LengthR,WidthR,Colour
        LOCAL Draw
        MOV CX,Xpos
        MOV DX,Ypos
        mov y,DX
        MOV BX,Xpos
        ADD BX,LengthR
Draw:
        push BX
        DrawVerticalLine CX,DX,WidthR,Colour
        MOV DX,y
        INC CX
        POP BX
        CMP CX,BX
        JNZ Draw
ENDM DrawFilledRectangle
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
clearBullets MACRO 
        local ClearUpBullet1
        local ClearDownBullet1
        local ClearRightBullet1
        local ClearLeftBullet1
        local endClear1

                mov ax,[si+6]
                cmp ax,'UP'
                jz ClearUpBullet1
                cmp ax,'DO'
                jz ClearDownBullet1
                cmp ax,'L'
                jz ClearLeftBullet1
                cmp ax,'R'
                jz ClearRightBullet1
                jmp endClear1
        ClearUpBullet1:
                mov cx,[si]
                DrawVerticalLine cx,[si+2],Bullet_Size,ScreenColour
                inc cx
                DrawVerticalLine cx,[si+2],Bullet_Size,ScreenColour
                jmp endClear1
        ClearDownBullet1:
                mov cx,[si]
                DrawVerticalLine [si],[si+2],3,ScreenColour
                inc cx
                DrawVerticalLine CX,[si+2],3,ScreenColour
                jmp endClear1
        ClearLeftBullet1:
                 mov cx,[si]
                 dec cx
                 mov [si],cx
                 DrawHorizontalLine [si],[SI]+2,7,ScreenColour
                 mov dx,[si]+2
                 inc dx
                 DrawHorizontalLine [SI],dx,7,ScreenColour
                 jmp endClear1
        ClearRightBullet1:
                 mov cx,[si]
                 sub cx,2
                 DrawHorizontalLine CX,[SI]+2,4,ScreenColour
                 mov cx,[si]
                 sub cx,2
                 mov dx,[si]+2
                 inc dx
                 DrawHorizontalLine CX,dx,4,ScreenColour
                 jmp endClear1
endClear1: mov [si+6],0  

ENDM clearBullets
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RandomNumber MACRO max

xor ax,ax                   
mov ax,seed            
xor dx,dx            
mov bx,max
       
div bx               
inc dx              
mov ax,dx  

MOV DX,seed
ADD DX,AX
MOV seed,DX

ENDM RandomNumber
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Error macro
       mov ax,0600h
       mov bh,07
       mov cx,0
       mov dx,184FH                       ;clear screen
       int 10h   


        mov  dl, 10   ;Column
        mov  dh, 09   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h  

       mov ah, 9
       mov dx, offset InputString2
       int 21h  
endm Error             
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
storeString macro string
 mov ah,0AH
     mov dx,offset string
     int 21h                               ;store input string
endm storeString
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DisplayNumber macro NumberToDisplay
local Divide1
local Divide2
        mov si, offset NumberToDisplay
        mov ax, [si]
        mov bx, 10d
        mov cx, 0
    Divide1:
        mov bx, 10d
        cwd
        mov dx, 0
        div bx
        push dx
        inc cx
        cmp ax, 0
        jnz Divide1
    Divide2:
        pop dx
        add dx, '0'
        MOV AH,2
        INT 21H
        loop Divide2
endm DisplayNumber
;=============================================================================================================================================================================================================================      
.MODEL MEDIUM
.386
.STACK 64
.DATA
        InputString1 DB 'Please Enter your name:','$'
        InputString2 DB 'Please Enter your name only alphabets in the first please','$'
        InputString3 DB 'Press any key to continue','$'
        InDATA1 db 15,?,15 dup('$')
        InDATA2 db 15,?,15 dup('$')
	menu_Message1    db '* To start chatting press F1','$'
	menu_Message2    db '* To start game press F2','$'
	menu_Message3    db '* To end the program press ESC','$'
	notification_bar db '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -','$'
        keyWorking    	 db "sh3'ala$"    
	Tank1 Label Word
	Tank1_Xpos  dw    150
	Tank1_Ypos  dw    60
        Tank1_Status dw   "R"
        Tank1_Health dw   99
	Tank2 Label Word
	Tank2_Xpos  dw    180
	Tank2_Ypos  dw    60
        Tank2_Status dw   "L"
        Tank2_Health dw   99
        Tank_length  dw    27d
        Bullet_Size  dw     2D
        ScreenColour db    2AH
        ObstacleColour db   00H
        GameEndFlag   db    00H
        seed    dw    ?
        extra   dw    ?
	x       dw    ?
	y       dw    ?
        yObs    dw    ?
        xObs    dw    ?
        lobs    dw    ?
        wobs    dw    ?
        Bullets1 dw  20,80 dup(0)  
        Bullets2 dw  20,80 dup(0)                       ;xB [] , yB[+2] , DrawStatus[+4], Direction [+6]
        Obstacles DW 40,200 dup(0) ;nObstacles, x , y[+2] , width[+4], length[+6] of obstacles , DrawStatus [+8],: to be drawn 0: Destroyed
        WidthObst DW 20
        LengthObst DW 10    
        LengthRec DW  ?
        nBullets1 dw  ?
        nBullets2 dw  ?
        nObstacles dw ? 
        Tank1WinText  DB "Tank 1 Wins $"
        Tank2WinText  DB "Tank 2 Wins $"
        Tank1Score    DB "Tank 1 Health:$"
        Tank2Score    DB "Tank 2 Health:$"
        MainMenuFlag  DB 0   
        keyboardBufferScan DB 48H,50H,4BH,4DH,39H,3EH,'$'
        keyboardBufferAscii DB 'w','a','s','d','e','$'
.code
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RecieveNames proc
             ClearScreen
             jmp input1
             error1:
             Error InputString2
             input1:
             MoveCursor 20,12
             DisplayString InputString1
             storeString InDATA1
             MoveCursor 20,15
             DisplayString InputString3
            mov si,offset InDATA1+2
            check1:MOV  al,'Z'                	; Setting AL with the ascii of Z 
	        CMP  [si],al               ; Check if the ascii is less than ascii of Z
	        JG   check2             	

            MOV  al,'A'                 ; Setting AL with the ascii of A
            CMP  [si],al                ; Check if the ascii is greater than ascii of A
            JL   error1
         
            jmp true1                   ;here we know its upperrcase letter
          
    check2:MOV  al,'z'                	; Setting AL with the ascii of z    here we want to know if its a lowercase letter or not 
	       CMP  [si],al               	; Check if the ascii is less than ascii of z
	       JG   error1             	

           MOV  al,'a'                   ; Setting AL with the ascii of a
           CMP  [si],al                  ; Check if the ascii is greater than ascii of a
           JL   error1

           jmp true1                     ; here we knew its a lowercase letter


             
      true1: mov ah,0
             int 16h                               ;wait for key    
             ClearScreen

jmp input2

    error2:
    ClearScreen
    MoveCursor 10,09
    DisplayString InputString2
    input2:
    MoveCursor 20,12
    DisplayString InputString1
    storeString InDATA2
    MoveCursor 20,15
    DisplayString InputString3
    mov si,offset InDATA2+2

     check3:MOV  al,'Z'                	; Setting AL with the ascii of Z 
	        CMP  [si],al               	; Check if the ascii is less than ascii of Z
	        JG   check4             	

            MOV  al,'A'                 ; Setting AL with the ascii of A
            CMP  [si],al                ; Check if the ascii is greater than ascii of A
            JL   error2
         
            jmp true2                  ;here we kknow its an uppercase letter
          
    check4:MOV  al,'z'                	; Setting AL with the ascii of z    here we want to know if its a lowercase letter or not 
	       CMP  [si],al               	; Check if the ascii is less than ascii of z
	       JG   error2            	

           MOV  al,'a'                   ; Setting AL with the ascii of a
           CMP  [si],al                  ; Check if the ascii is greater than ascii of a
           JL   error2

           jmp true2                    ;here we know its lowercase letter


             
      true2: mov ah,0
             int 16h                               ;wait for key  
             ClearScreen
        RET              
RecieveNames ENDP
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Mainmenu Proc
                 MoveCursor    24,8
	         DisplayString menu_Message1

	         MoveCursor    24,10
	         DisplayString menu_Message2

	         MoveCursor    24,12
	         DisplayString menu_Message3

	         MoveCursor    0,20
	         DisplayString notification_bar
                 RET
Mainmenu ENDP
LoadObstacles   proc
        mov si,offset Obstacles
        mov di,[si]     ;Holds number of obstacles
        add si,2        ; si: x , si+2 : y
  FillObstacles:
        
        mov [si+8],1
        mov AX,LengthObst
        mov [si+6],AX
  getAnotherX:

        RandomNumber 319
        mov bx,ax
        add bx,[si+6]
        cmp bx,319
        jg getAnotherX ;obstacle in range add it to array
        mov [si],ax
     
        mov AX,WidthObst
        mov [si+4],AX
   getAnotherY:
        RandomNumber 175
        mov bx,ax
        add bx,[si+4]
        cmp bx,175
        jg getAnotherY
        mov [si+2],ax
        add si,10
        dec di
        cmp di,0 
        jnz FillObstacles
        ret
LoadObstacles   endp
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
                MOV SI , OFFSET Bullets1
		MOv DI , [SI]
		ADD SI , 2
                MOV AH,1
                INT 16h
                CMP AH,48H
                JZ Move_Up
                CMP AH,50H
                JZ Move_Down
                CMP AH,4BH
                JZ Move_Left
                CMP AH,4DH
                JZ Move_Right
		CMP AH,39H
                JZ Fire_Tank1
                JMP No_Movement
        
               
        Move_Up:
                MOV Tank1_Status,'UP'
                MOV AX,Tank1_Ypos
                CMP AX,0
                JZ Read_Value
                MOV DX,Tank1_Ypos
                ADD DX,26d
                MOV y,DX
                push ax
                DrawFilledRectangle Tank1_Xpos,y,29D,02H,ScreenColour
                pop ax
                SUB AX,2
                MOV Tank1_Ypos,AX
                JMP Read_Value
        
        

        Move_Down:        
                MOV Tank1_Status,'DO'
                MOV AX,Tank1_Ypos
                CMP AX,146
                JZ Read_Value
                MOV DX,Tank1_Ypos
                DEC DX
                MOV y,DX
                push ax
                DrawFilledRectangle Tank1_Xpos,y,29D,04H,ScreenColour
                pop ax
                ADD AX,2
                MOV Tank1_Ypos,AX
                JMP Read_Value
        
        
        Move_Left:        
                MOV Tank1_Status,'L'
                MOV AX,Tank1_Xpos
                CMP AX,0
                JZ Read_Value
                MOV CX,Tank1_Xpos
                add cx,26D
                MOV X,CX
                push ax
                DrawFilledRectangle X,Tank1_Ypos,3,28D,ScreenColour
                pop ax
                SUB AX,2
                MOV Tank1_Xpos,AX
                JMP Read_Value
        
                
        
        Move_Right:
                MOV Tank1_Status,'R'
                MOV AX,Tank1_Xpos
                CMP AX,292
                JZ Read_Value
                MOV CX,Tank1_Xpos
                DEC Cx
                MOV X,CX
                push ax
                DrawFilledRectangle X,Tank1_Ypos,4,28D,ScreenColour
                pop ax
                add AX,2
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
                Mov ax,Tank1_Status
                cmp ax,'UP'
                jz fireUp1
                cmp ax,'DO'
                jz fireDown1
                cmp ax,'L'
                jz fireLeft1
                cmp ax,'R'
                jz fireRight1   
        fireUp1:
                MOV [si+6],ax
                mov cx,Tank1_Xpos
                mov dx,Tank1_Ypos
                add cx,13D
                mov [si],cx
                mov [si+2],dx
                mov [si+4],1
                jmp Read_Value
        fireDown1:
                MOV [si+6],ax
                mov dx,Tank1_Ypos
                add dx,28D
                mov [si+2],dx
                mov cx,Tank1_Xpos
                add cx,13D
                mov [si],cx
                MOV [SI]+4,1
                jmp Read_Value
        fireRight1:
                MOV [si+6],ax
		MOV CX,Tank1_Xpos
		ADD      CX,28D
		MOV     [SI],CX
		MOV     DX,Tank1_Ypos
		ADD     DX,13D
		MOV     [SI]+2,DX
		MOV     [SI]+4,1
		jmp     Read_Value
        fireLeft1:
                MOV                [si+6],ax
                MOV                CX,Tank1_Xpos
	        MOV                [SI],CX
	        MOV                DX,Tank1_Ypos
	        ADD                DX,13D
	        MOV                [SI]+2,DX
	        MOV                [SI]+4,1
                jmp                Read_Value
	Occupied1:
		ADD SI,8        
		DEC DI
		JNZ StoreBullets1
        Read_Value:
                RemoveValueBuffer
        No_Movement:    ret
Tank1Action ENDP
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
	Move_Up2:
                MOV Tank2_Status,'UP'
                MOV AX,Tank2_Ypos
                CMP AX,0
                JZ Read_Value2
                MOV DX,Tank2_Ypos
                ADD DX,26d
                MOV y,DX
                push ax
                DrawFilledRectangle Tank2_Xpos,y,29D,02H,ScreenColour
                pop ax
                SUB AX,2
                MOV Tank2_Ypos,AX
                JMP Read_Value2
        
        

        Move_Down2:        
                MOV Tank2_Status,'DO'
                MOV AX,Tank2_Ypos
                CMP AX,146
                JZ Read_Value2
                MOV DX,Tank2_Ypos
                DEC DX
                MOV y,DX
                push ax
                DrawFilledRectangle Tank2_Xpos,y,29D,04H,ScreenColour
                pop ax
                ADD AX,2
                MOV Tank2_Ypos,AX
                JMP Read_Value2
        
        
        Move_Left2:        
                MOV Tank2_Status,'L'
                MOV AX,Tank2_Xpos
                CMP AX,0
                JZ Read_Value2
                MOV CX,Tank2_Xpos
                add cx,26D
                MOV X,CX
                push ax
                DrawFilledRectangle X,Tank2_Ypos,3,28D,ScreenColour
                pop ax
                SUB AX,2
                MOV Tank2_Xpos,AX
                JMP Read_Value2
        
                
        
        Move_Right2:
                MOV Tank2_Status,'R'
                MOV AX,Tank2_Xpos
                CMP AX,292
                JZ Read_Value2
                MOV CX,Tank2_Xpos
                DEC Cx
                MOV X,CX
                push ax
                DrawFilledRectangle X,Tank2_Ypos,4,28D,ScreenColour
                pop ax
                add AX,2
                MOV Tank2_Xpos,AX
                JMP Read_Value2



	Fire_Tank2:
		MOV SI , OFFSET Bullets2
		MOv DI , [SI]
		ADD SI , 2
	StoreBullets2:
		MOV AX,[SI]+4
		CMP AX,0
		JNZ Occupied2
                Mov ax,Tank2_Status
                cmp ax,'UP'
                jz fireUp2
                cmp ax,'DO'
                jz fireDown2
                cmp ax,'L'
                jz fireLeft2
                cmp ax,'R'
                jz fireRight2
        fireUp2:
                MOV [si+6],ax
                mov cx,Tank2_Xpos
                mov dx,Tank2_Ypos
                add cx,13D
                mov [si],cx
                mov [si+2],dx
                mov [si+4],1
                jmp Read_Value2
        fireDown2:
                MOV [si+6],ax
                mov dx,Tank2_Ypos
                add dx,28D
                mov [si+2],dx
                mov cx,Tank2_Xpos
                add cx,13D
                mov [si],cx
                MOV [SI]+4,1
                jmp Read_Value2
        fireRight2:
                MOV [si+6],ax
		MOV CX,Tank2_Xpos
		ADD      CX,28D
		MOV     [SI],CX
		MOV     DX,Tank2_Ypos
		ADD     DX,13D
		MOV     [SI]+2,DX
		MOV     [SI]+4,1
		jmp     Read_Value2
        fireLeft2:
                MOV                [si+6],ax
                MOV                CX,Tank2_Xpos
	        MOV                [SI],CX
	        MOV                DX,Tank2_Ypos
	        ADD                DX,13D
	        MOV                [SI]+2,DX
	        MOV                [SI]+4,1
                jmp                Read_Value2
	Occupied2:    
	              ADD                SI,8
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
          DrawTankUP Tank1_Xpos,Tank1_Ypos,01H,ScreenColour,0
          JMP EndDrawTank1
          down:
          DrawTankDown Tank1_Xpos,Tank1_Ypos,01H,ScreenColour,0
          JMP EndDrawTank1
          left:
          DrawTankLeft Tank1_Xpos,Tank1_Ypos,01H,ScreenColour,0
          JMP EndDrawTank1
          right:
          DrawTankRight Tank1_Xpos,Tank1_Ypos,01H,ScreenColour,0
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
          DrawTankUP Tank2_Xpos,Tank2_Ypos,04H,ScreenColour,0
          JMP EndDrawTank2
          down2:
          DrawTankDown Tank2_Xpos,Tank2_Ypos,04H,ScreenColour,0
          JMP EndDrawTank2
          left2:
          DrawTankLeft Tank2_Xpos,Tank2_Ypos,04H,ScreenColour,0
          JMP EndDrawTank2
          right2:
          DrawTankRight Tank2_Xpos,Tank2_Ypos,04H,ScreenColour,0
          EndDrawTank2:ret
        ret
DrawTank2 endp
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawObstacles proc
                pusha
                MOV SI, OFFSET Obstacles
                MOV DI,[SI]
                ADD SI,2
        Draw :
                MOV AX,[SI]+8
                CMP AX,0
                JZ Increment
                DrawFilledRectangle [SI],[SI]+2,[SI]+6,[SI]+4,ObstacleColour
        Increment:
                ADD SI , 10D
                DEC DI
                JNZ Draw 
                popa
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
                    mov ax,[si+6]
                    cmp ax,'UP'
                    jz FiredUp1
                    cmp ax,'DO'
                    jz FiredDown1
                    cmp ax,'L'
                    jz FiredLeft1
                    cmp ax,'R'
                    jz FiredRight1
                    jmp IncrementBullets1
        FiredUp1:   
                mov cx,[si]
                mov dx,[si+2]
                sub dx,2
                DrawVerticalLine cx,dx,Bullet_Size,01
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                mov dx,[si+2]
                sub dx,2
                inc cx 
                DrawVerticalLine cx,dx,Bullet_Size,01
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
               
                jmp IncrementBullets1
        FiredDown1:
                mov cx,[si]
                mov dx,[si+2]
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                DrawVerticalLine cx,dx,Bullet_Size,01d
                inc cx
                mov dx,[si+2]
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                DrawVerticalLine cx,dx,Bullet_Size,01d
                  jmp IncrementBullets1
        FiredLeft1:
                    MOV CX,[SI]  
                    MOV DX,[SI]+2
                    DrawHorizontalLine Cx,DX,Bullet_Size,01h
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    MOV CX,[SI]
                    INC DX
                    DrawHorizontalLine Cx,DX,Bullet_Size,01h
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    jmp IncrementBullets1
        FiredRight1:
                    MOV CX,[SI]  
                    sub cx,2
                    MOV DX,[SI]+2
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    DrawHorizontalLine Cx,DX,Bullet_Size,01
                    MOV CX,[SI]
                    sub cx,2
                    inc dx
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    DrawHorizontalLine Cx,DX,Bullet_Size,01
        IncrementBullets1:
                          ADD SI,8 
                          dec DI
                          jnz Drawbullet1   

MOV SI,offset Bullets2
MOV DI,[SI]
add SI,02h
        Drawbullet2: 
                   MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementBullets2
                    mov ax,[si+6]
                    cmp ax,'UP'
                    jz FiredUp2
                    cmp ax,'DO'
                    jz FiredDown2
                    cmp ax,'L'
                    jz FiredLeft2
                    cmp ax,'R'
                    jz FiredRight2
                    jmp IncrementBullets2
        
        FiredUp2:   
                mov cx,[si]
                mov dx,[si+2]
                sub dx,2
                DrawVerticalLine cx,dx,Bullet_Size,04
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                mov dx,[si+2]
                sub dx,2
                inc cx 
                 DrawVerticalLine cx,dx,Bullet_Size,04
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                jmp IncrementBullets2
        FiredDown2:
                mov cx,[si]
                mov dx,[si+2]
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                DrawVerticalLine cx,dx,Bullet_Size,04d
                inc cx
                mov dx,[si+2]
                DrawVerticalLine cx,dx,Bullet_Size,ScreenColour
                DrawVerticalLine cx,dx,Bullet_Size,04d
                jmp IncrementBullets2
        FiredLeft2:
                    MOV CX,[SI]  
                    MOV DX,[SI]+2
                    DrawHorizontalLine Cx,DX,Bullet_Size,04
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    MOV CX,[SI]
                    INC DX
                    DrawHorizontalLine Cx,DX,Bullet_Size,04h
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    jmp IncrementBullets2
        FiredRight2:
                    MOV CX,[SI]  
                    sub cx,2
                    MOV DX,[SI]+2
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    DrawHorizontalLine Cx,DX,Bullet_Size,04
                    MOV CX,[SI]
                    sub cx,2
                    inc dx
                    DrawHorizontalLine Cx,DX,Bullet_Size,ScreenColour
                    DrawHorizontalLine Cx,DX,Bullet_Size,04
        IncrementBullets2:
                          ADD SI,8
                          dec DI
                          jnz Drawbullet2  
                          


DrawBullets endp;
;-------------------------------------------------------------------------------------------------------------------------------------------------------------
MoveBullets proc
MOV SI,offset Bullets1
MOV DI,[SI]
add si,02h
        Movebullets1: 
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementMove
                    mov bx,[si+6]
                    cmp bx,'UP'
                    jz moveUp1
                    cmp bx,'DO'
                    jz moveDown1
                    cmp bx,'L'
                    jz moveLeft1
                    cmp bx,'R'
                    jz moveRight1
                    jmp IncrementMove
        moveUp1:        
                mov dx,[si+2]
                cmp dx,0
                jz setzero
                dec dx
                mov [si+2],dx
                jmp IncrementMove
        moveDown1:
                mov dx,[si+2]
                cmp dx,172
                jge setzero
                inc dx
                mov [si+2],dx
                jmp IncrementMove
        moveLeft1:
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementMove
                    MOV CX,[SI]    
                    CMP CX,0
                    jz setzero
                    dec CX
                    MOV [SI],CX
                    jmp IncrementMove       
        moveRight1:
                    MOV CX,[SI]    
                    INC CX
                    CMP CX,318
                    JGE setzero
                    MOV [SI],CX
                    jmp IncrementMove
        setzero:   
                    MOV [SI]+4,0h
                    clearBullets  
                    
                    
        IncrementMove:
                    ADD SI,8
                    dec DI
                    jnz Movebullets1


MOV SI,offset Bullets2
MOV DI,[SI]
add si,02h
        Movebullets2: 
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementMove2
                    mov bx,[si+6]
                    cmp bx,'UP'
                    jz moveUp2
                    cmp bx,'DO'
                    jz moveDown2
                    cmp bx,'L'
                    jz moveLeft2
                    cmp bx,'R'
                    jz moveRight2
                    jmp IncrementMove2

        moveUp2:        
                mov dx,[si+2]
                cmp dx,0
                jz setzero2
                dec dx
                mov [si+2],dx
                jmp IncrementMove2
        moveDown2:
                mov dx,[si+2]
                cmp dx,172
                jge setzero2
                inc dx
                mov [si+2],dx
                jmp IncrementMove2
        moveLeft2:
                    MOV AX,[SI]+4
                    CMP AX,0
                    JZ IncrementMove2
                    MOV CX,[SI]   
                    CMP CX,0
                    jz setzero2
                    dec CX
                    MOV [SI],CX
                    jmp IncrementMove2   
        moveRight2:
                    MOV CX,[SI]    
                    INC CX
                    CMP CX,318
                    JGE setzero2
                    MOV [SI],CX
                    jmp IncrementMove2  
        setzero2:   
                    MOV [SI]+4,0h
                    clearBullets
        IncrementMove2:
                    ADD SI,8
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
                 DrawFilledRectangle [SI],[SI]+2,[SI]+6,[SI]+4,ScreenColour
                 Call DrawObstacles
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
                 DrawFilledRectangle [SI],[SI]+2,[SI]+6,[SI]+4,ScreenColour
                 Call DrawObstacles
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
                 collisionDetection Tank1_Xpos,[SI],Tank_length,Bullet_Size  ;bullets from second tank and tank1
                 mov bl,al
                 collisionDetection Tank1_Ypos,[SI]+2,Tank_length,Bullet_Size  
                 and al,bl
                 jz IncBullets2
                 mov [SI]+4,0
                 clearBullets
                 MOV AX,Tank1_Health
                 DEC AX
                 CMP AX,0
                 JZ ZeroHealthT1
                 MOV Tank1_Health,AX
                 JMP IncBullets2
ZeroHealthT1:    MOV GameEndFlag,2
                 RET

                

IncBullets2:
                 add si,8
                 dec DI
                 jnz Bullets2Tank1
                 
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
                Mov si,offset Bullets1
                Mov di,[si]
                add si,2
Bullets1Tank2:
                 cmp [si]+4,0
                 jz IncBullets1
                 collisionDetection Tank2_Xpos,[SI],Tank_length,Bullet_Size  ;bullets from first tank and tank2
                 mov bl,al
                 collisionDetection Tank2_Ypos,[SI]+2,Tank_length,Bullet_Size  
                 and al,bl
                 jz IncBullets1
                 mov [SI]+4,0
                 clearBullets
                 MOV AX,Tank2_Health
                 DEC AX
                 CMP AX,0
                 JZ ZeroHealthT2
                 MOV Tank2_Health,AX
                 JMP IncBullets1
ZeroHealthT2:    MOV GameEndFlag,1
                 RET

IncBullets1:
                 add si,8
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
               collisionDetection [si],[di],Bullet_Size,[di+6]
               mov bl,al
               collisionDetection [si+2],[di+2],Bullet_Size,[di+4]
               and al,bl
               jz nextObstacle ;jump if no collision
               mov [si+4],0
               mov [di+8],0
               clearBullets

               DrawFilledRectangle [di],[di]+2,[di]+6,[di]+4,ScreenColour
               Call DrawObstacles
        nextBullet:
                add si,8
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
               collisionDetection [si],[di],Bullet_Size,[di+6]
               mov bl,al
               collisionDetection [si+2],[di+2],Bullet_Size,[di+4]
               and al,bl
               jz nextObstacle2 ;jump if no collision
               mov [si+4],0
               mov [di+8],0
               clearBullets
               DrawFilledRectangle [di],[di]+2,[di]+6,[di]+4,ScreenColour
               Call DrawObstacles
        nextBullet2:
                add si,8
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
LoadingScreen proc
mov           al,0
mov           CX,00
mov           DX,1727h
mov           ah,6
mov           bh,00
int           10h

MOV           CX,50D ;Frame
mov           DX,25D
DrawFilledRectangle CX,DX,5D,145D,24H
DrawFilledRectangle CX,DX,210D,5D,24H
DrawFilledRectangle CX,DX,5D,145D,24H
MOV           CX,55D
mov           DX,165D
DrawFilledRectangle CX,DX,210D,5D,24H


MOV           CX,60D ;yellow left T
mov           DX,35D
DrawFilledRectangle CX,DX,10D,5D,0EH
DrawFilledRectangle CX,DX,5D,45D,0EH
MOV           CX,60D
mov           DX,45D
DrawFilledRectangle CX,DX,5D,25D,0EH
MOV           CX,60D
mov           DX,75D
DrawFilledRectangle CX,DX,10D,5D,0EH


MOV           CX,80D ;T
mov           DX,35D
DrawFilledRectangle CX,DX,50D,10D,0BH
MOV           CX,100D
mov           DX,45D
DrawFilledRectangle CX,DX,10D,30D,0BH
MOV           CX,95D
mov           DX,75D
DrawFilledRectangle CX,DX,20D,5D,0BH


MOV           CX,80D ;Blue left T
mov           DX,50D
DrawFilledRectangle CX,DX,5D,25D,20H
DrawFilledRectangle CX,DX,10D,5D,20H
SUB CX,5D
ADD DX,10D
DrawFilledRectangle CX,DX,5D,10D,20H
MOV           CX,80D
mov           DX,75D
DrawFilledRectangle CX,DX,10D,5D,20H


MOV           CX,115D ;Blue Right T
mov           DX,50D
DrawFilledRectangle CX,DX,10D,5D,20H
DrawFilledRectangle CX,DX,5D,25D,20H
SUB CX,15D
MOV DX,60D
DrawFilledRectangle CX,DX,5D,10D,20H
MOV DX,75D
DrawFilledRectangle CX,DX,10D,5D,20H

MOV           CX,135D ; A
mov           DX,35D
DrawFilledRectangle CX,DX,30D,45D,0BH
MOV           CX,145D
mov           DX,45D
DrawFilledRectangle CX,DX,10D,10D,00H
MOV           CX,145D
mov           DX,65D
DrawFilledRectangle CX,DX,10D,15D,00H

MOV           CX,170D ; n
mov           DX,35D
DrawFilledRectangle CX,DX,35D,45D,0BH
MOV           CX,180D
mov           DX,45D
DrawFilledRectangle CX,DX,15D,35D,00H
MOV           CX,185D
mov           DX,50D
DrawFilledRectangle CX,DX,5D,30D,20H

MOV           CX,210D ; k
mov           DX,35D
DrawFilledRectangle CX,DX,30D,45D,0BH
MOV           CX,220D 
mov           DX,35D
DrawFilledRectangle CX,DX,5D,15D,00H
SUB           CX,5D
mov           DX,65D
DrawFilledRectangle CX,DX,5D,20D,00H
MOV           CX,230D 
mov           DX,50D
DrawFilledRectangle CX,DX,5D,10D,00H
MOV           CX,235D 
mov           DX,40D
DrawFilledRectangle CX,DX,5D,35D,00H

MOV           CX,240D ; Yellow Right K
mov           DX,45D
DrawFilledRectangle CX,DX,10D,5D,0EH
ADD DX,10D
SUB CX,10D
DrawFilledRectangle CX,DX,10D,5D,0EH
ADD DX,10D
SUB CX,10D
DrawFilledRectangle CX,DX,10D,5D,0EH
MOV           CX,245D 
mov           DX,35D
DrawFilledRectangle CX,DX,5D,45D,0EH
DrawFilledRectangle CX,DX,10D,5D,0EH
ADD DX,10D
SUB CX,5D
DrawFilledRectangle CX,DX,5D,25D,0EH
MOV           CX,250D 
mov           DX,75D
DrawFilledRectangle CX,DX,10D,5D,0EH

MOV CX,60D                              ;yellow left and right
MOV DX,85D
DrawFilledRectangle CX,DX,10D,10D,0EH   ;left
MOV CX,250D                             ;right
DrawFilledRectangle CX,DX,10D,10D,0EH
MOV CX,60D                              ;left
MOV DX,100D
DrawFilledRectangle CX,DX,15D,25D,0EH
MOV CX,60D
ADD DX,5D
DrawFilledRectangle CX,DX,5D,5D,00H
ADD DX,10D
DrawFilledRectangle CX,DX,5D,10D,00H
SUB DX,10D
DrawFilledRectangle CX,DX,5D,5D,00H
MOV CX,245D                              ;Right
MOV DX,100D
DrawFilledRectangle CX,DX,15D,25D,0EH
MOV CX,245D
ADD DX,5D
DrawFilledRectangle CX,DX,5D,5D,00H
ADD DX,10D
DrawFilledRectangle CX,DX,5D,10D,00H
SUB DX,10D
DrawFilledRectangle CX,DX,5D,5D,00H
MOV CX,60D                              ;left
MOV DX,130D
DrawFilledRectangle CX,DX,5D,30D,0EH
ADD CX,5D
DrawFilledRectangle CX,DX,5D,30D,0EH
SUB CX,15D
ADD DX,15D
DrawFilledRectangle CX,DX,15D,5D,00H
SUB CX,10D
SUB DX,5D
DrawFilledRectangle CX,DX,5D,15D,0EH
MOV CX,245D                               ;Right
MOV DX,130D
DrawFilledRectangle CX,DX,5D,30D,0EH
ADD CX,5D
DrawFilledRectangle CX,DX,5D,30D,0EH
SUB CX,15D
ADD DX,15D
DrawFilledRectangle CX,DX,15D,5D,00H
SUB CX,10D
SUB DX,5D
DrawFilledRectangle CX,DX,5D,15D,0EH

MOV CX,75D                              ;I
MOV DX,85D
DrawFilledRectangle CX,DX,170D,10D,0BH
MOV CX,75D                              
MOV DX,90D
DrawFilledRectangle CX,DX,5D,5D,00H
MOV CX,240D                              
MOV DX,90D
DrawFilledRectangle CX,DX,5D,5D,00H
MOV CX,145D                              
MOV DX,95D
DrawFilledRectangle CX,DX,30D,5D,0BH
MOV CX,150D                              
MOV DX,100D
DrawFilledRectangle CX,DX,20D,5D,0BH
MOV CX,155D
MOV DX,105D
DrawFilledRectangle CX,DX,10D,35D,0BH
MOV CX,150D
MOV DX,140D
DrawFilledRectangle CX,DX,20D,5D,0BH
MOV CX,145D
MOV DX,145D
DrawFilledRectangle CX,DX,30D,5D,0BH
MOV CX,110D
MOV DX,150D
DrawFilledRectangle CX,DX,105D,5D,0BH
MOV CX,105D
MOV DX,155D
DrawFilledRectangle CX,DX,115D,5D,0BH

MOV CX,80D                              ; Pink left P
MOV DX,100D
DrawFilledRectangle CX,DX,5D,25D,24H
MOV CX,80D
MOV DX,130D
DrawFilledRectangle CX,DX,5D,30D,24H
MOV CX,85D
MOV DX,120D
DrawFilledRectangle CX,DX,15D,5D,24H
MOV CX,85D
MOV DX,130D
DrawFilledRectangle CX,DX,15D,5D,24H
MOV CX,90D
MOV DX,100D
DrawFilledRectangle CX,DX,5D,15D,24H
MOV CX,90D
MOV DX,140D
DrawFilledRectangle CX,DX,5D,20D,24H
MOV CX,95D
MOV DX,100D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV CX,95D
MOV DX,110D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV CX,95D
MOV DX,140D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV CX,95D
MOV DX,155D
DrawFilledRectangle CX,DX,5D,5D,24H


MOV CX,105D                             ;P
MOV DX,100D
DrawFilledRectangle CX,DX,30D,45D,0BH
MOV CX,115D
MOV DX,110D
DrawFilledRectangle CX,DX,10D,10D,00H
MOV CX,115D
MOV DX,130D
DrawFilledRectangle CX,DX,20D,15D,00H
MOV CX,120D
MOV DX,135D
DrawFilledRectangle CX,DX,5D,10D,24H
ADD CX,5D
DrawFilledRectangle CX,DX,5D,10D,24H
ADD CX,5D
MOV DX,105D
DrawFilledRectangle CX,DX,5D,35D,24H
ADD DX,5D
DrawFilledRectangle CX,DX,5D,5D,24H
SUB CX,5D
ADD DX,10D
DrawFilledRectangle CX,DX,5D,5D,24H
SUB CX,5D
ADD DX,10D
DrawFilledRectangle CX,DX,5D,5D,24H

MOV CX,170D                             ;Pink Left T
MOV DX,110D
DrawFilledRectangle CX,DX,5D,25D,24H
MOV DX,120D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV DX,115D
DrawFilledRectangle CX,DX,5D,15D,24H
SUB CX,5D
MOV DX,135D
DrawFilledRectangle CX,DX,5D,10D,24H
MOV DX,115D
DrawFilledRectangle CX,DX,10D,5D,24H
SUB CX,10D
MOV DX,125D
DrawFilledRectangle CX,DX,10D,5D,24H
SUB CX,10D
MOV DX,140D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV DX,130D
DrawFilledRectangle CX,DX,5D,5D,24H

MOV CX,180D                             ;T
MOV DX,100D
DrawFilledRectangle CX,DX,50D,10D,0BH
MOV CX,200D
MOV DX,110D
DrawFilledRectangle CX,DX,10D,30D,0BH
MOV CX,195D
MOV DX,140D
DrawFilledRectangle CX,DX,20D,5D,0BH

MOV CX,215D                             ;Pink Right T
MOV DX,115D
DrawFilledRectangle CX,DX,15D,5D,24H
MOV CX,215D
MOV DX,125D
DrawFilledRectangle CX,DX,15D,5D,24H
MOV CX,215D
MOV DX,130D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV CX,220D
MOV DX,140D
DrawFilledRectangle CX,DX,10D,5D,24H
MOV CX,225D
MOV DX,135D
DrawFilledRectangle CX,DX,5D,5D,24H
MOV CX,225D
MOV DX,120D
DrawFilledRectangle CX,DX,10D,5D,24H
MOV DX,100D
DrawFilledRectangle CX,DX,5D,60D,24H
MOV DX,135D
SUB CX,5D
DrawFilledRectangle CX,DX,5D,5D,0H
MOV DX,150D
SUB CX,15D
DrawFilledRectangle CX,DX,15D,5D,24H
ret
LoadingScreen ENDP
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
DrawHealthBar Proc
                MOV AX,Tank1_Health
                CMP AX,0
                JZ DrawHT2
                MOV BX,AX
                SUB AX,99
                MOV DL,-1
                IMUL DL
                MOV extra,AX
                MOV AX,BX 
                push Ax
                DrawRectangel 24,181,13,102,0Fh
                pop AX
                mov cx,26D
        DrawFilledHT1:
                push AX
                DrawFilledRectangle CX,183,1,10,01
                POP AX
                DEC AX
                JNZ DrawFilledHT1

                MOV AX,extra
                CMP AX,0
                JZ DrawHT2
        DrawEmptyT1:
                Push AX
                DrawFilledRectangle CX,183,1,10,00
                POP AX
                DEC AX
                JNZ DrawEmptyT1



        DrawHT2:     
                MOV AX,Tank2_Health
                CMP AX,0
                JZ EndDraw
                MOV BX,AX
                SUB AX,99
                MOV DL,-1
                IMUL DL
                MOV extra,AX
                MOV AX,BX 
                push Ax
                DrawRectangel 198,181,13,102,0Fh
                pop AX
                mov cx,200D
        DrawFilledHT2:
                push AX
                DrawFilledRectangle CX,183,1,10,04
                POP AX
                DEC AX
                JNZ DrawFilledHT2

                MOV AX,extra
                CMP AX,0
                JZ EndDraw
        DrawEmptyT2:
                Push AX
                DrawFilledRectangle CX,183,1,10,00
                POP AX
                DEC AX
                JNZ DrawEmptyT2
        EndDraw:
        RET
DrawHealthBar endp
;------------------ ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
EraseBullets proc
	                 MOV                 SI,OFFSET Bullets1
	                 MOV                 DI,[SI]
	                 ADD                 SI,2
	EraseBullets1:     
                         MOV                 [SI+4],0
                         ADD                 SI,8
                         DEC                 DI
                         JNZ                 EraseBullets1

         	         MOV                 SI,OFFSET Bullets2
	                 MOV                 DI,[SI]
	                 ADD                 SI,2
	EraseBullets2:     
                         MOV                 [SI+4],0
                         ADD                 SI,8
                         DEC                 DI
                         JNZ                 EraseBullets2                
                         RET
EraseBullets ENDP
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
StartGame proc
        MOV           AH,0
	MOV           AL,13H
	INT           10H
        call          LoadingScreen
        MOV           DI,100
Wait5sec:
        call          delay
        DEC           DI 
        JNZ           Wait5sec
        MOV           ah,02h
        INT           1ah
        MOV           seed,dx
        call          LoadObstacles
        
        Call          EraseBullets

        mov           al,0
	mov           CX,00
	mov           DX,1527h
	mov           ah,6
	mov           bh,ScreenColour
	INT           10h   

        MOV           Tank1_Health,99
        MOV           Tank1_Xpos,0D
        MOV           Tank1_Ypos,0D
        MOV           Tank1_Status,'R'
        MOV           Tank2_Health,99
        MOV           Tank2_Xpos,292D
        MOV           Tank2_Ypos,146D
        MOV           Tank2_Status,'L'
        MOV           GameEndFlag,0
        MOV           MainMenuFlag,0
        RET
StartGame ENDP
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CheckGameEnd proc
        MOV     AL,2
        CMP     AL,GameEndFlag
        JZ      Tank2Wins
        
        MOV     AL,1
        CMP     AL,GameEndFlag
        JZ      Tank1Wins
        
        MOV     AH,1
        INT     16h
        CMP     AH,3Eh  
        JZ      EndGameNoWinners
        
        JMP     ContinueGame

Tank2Wins:
        MOV AH,0
        MOV AL,3
        INT 10h 

        MoveCursor 33,0
        MOV AH,9
        MOV DX,OFFSET Tank2WinText
        INT 21h
        
        
        MOV AH,9
        MOV DX,OFFSET InDATA2+2
        INT 21h

        MOV MainMenuFlag,1

        RET
Tank1Wins:
        MOV AH,0
        MOV AL,3
        INT 10h 

        MoveCursor 33,0
        MOV AH,9
        MOV DX,OFFSET Tank1WinText
        INT 21h
        
        MOV AH,9
        MOV DX,OFFSET InDATA1+2
        INT 21h

        MOV MainMenuFlag,1

        RET
EndGameNoWinners:
        MOV AH,0
        MOV AL,3
        INT 10h 
        RemoveValueBuffer

        MoveCursor 12,0
        MOV AH,9
        MOV DX,OFFSET Tank1Score
        INT 21h

        DisplayNumber Tank1_Health
        
        MoveCursor 12,1
        MOV AH,9
        MOV DX,OFFSET InDATA1+2
        INT 21h

        MoveCursor 52,0
        MOV AH,9
        MOV DX,OFFSET Tank2Score
        INT 21h

        DisplayNumber Tank2_Health
       
        MoveCursor 52,1
        MOV AH,9
        MOV DX,OFFSET InDATA2+2
        INT 21h

        MOV MainMenuFlag,1

        RET
ContinueGame:
        RET
CheckGameEnd ENDP
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ClearBufferValues proc
StartAgain:     MOV AH,1
                INT 16H
                CMP AH,1
                JNZ IntializeValuesScan
                CMP AL,0
                JZ EmptyBuffer
IntializeValuesScan: MOV BX,0
                MOV SI,OFFSET keyboardBufferScan
                DEC SI
ClearValuesScan:
                INC SI
                CMP [SI],'$'
                JZ  IntializeValuesAscii
                CMP AH,[SI]
                JNZ ClearValuesScan
                MOV BX,1
                JMP ClearValuesScan
IntializeValuesAscii:
                MOV SI,OFFSET keyboardBufferAscii
                DEC SI
ClearValuesAscii:
                INC SI
                CMP [SI],'$'
                JZ  CheckExist
                CMP AL,[SI]
                JNZ ClearValuesAscii
                MOV BX,1
                JMP ClearValuesAscii
CheckExist:     CMP BX,1
                JZ EmptyBuffer
                RemoveValueBuffer
                JMP StartAgain
EmptyBuffer:RET
ClearBufferValues ENDP
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MAIN proc FAR

	        mov           ax,@data
	        mov           Ds,ax
                CALL RecieveNames
        Menu:   CALL Mainmenu
        	get_char:
	         mov           ah,08h
	         int           21h
	         mov           ah,1
	         int           16h

	         cmp           al,3bh          	;F1 compare
	         cmp           al,3ch          	;F2 compare
                 JZ            PLay
	         mov           bl,01bh         	;ESC compare
	         cmp           al,bl
                 JZ            EndGame
	         Jmp           get_char      
        Play:   CALL StartGame
                CALL DrawObstacles
	GameLoop:
                call delay
                call ClearBufferValues
                Call Tank1Action
                call Tank2Action
                Call MoveBullets
                call Collision
                Call DrawHealthBar
                call DrawBullets
                Call DrawTank1
                Call DrawTank2
                Call CheckGameEnd
                
                MOV AL,1
                CMP AL,MainMenuFlag
                JZ Menu
                jmp GameLoop 
EndGame:
                MoveCursor 0,0
                ClearScreen
                mov AH,4CH
                INT 21h 
MAIN ENDP
END MAIN