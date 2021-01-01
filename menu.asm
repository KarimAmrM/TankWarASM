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

storeString macro string
 mov ah,0AH
     mov dx,offset string
     int 21h                               ;store input string
endm storeString



DisplayString macro string
mov ah,9
mov dx,offset string
int 21h
endm DisplayString 

MoveCursor macro x,y
             mov           dl, x        	;Column
	         mov           dh, y          	;Row
	         mov           bh, 0           	;Display page
	         mov           ah, 02h         	;SetCursorPosition
	         int           10h
endm MoveCursor

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
.CODE
Main proc near
	         mov           AX,@DATA
	         mov           DS,AX
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
                      
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;main menu
	         MoveCursor    24,8
	         DisplayString menu_Message1

	         MoveCursor    24,10
	         DisplayString menu_Message2

	         MoveCursor    24,12
	         DisplayString menu_Message3

	         MoveCursor    0,20
	         DisplayString notification_bar

	get_char:
	         mov           ah,08h
	         int           21h
	         mov           ah,1
	         int           16h

	         cmp           al,3bh          	;F1 compare
	         Je            test

	         cmp           al,3ch          	;F2 compare
	         Je            test

	         mov           bl,01bh         	;ESC compare
	         cmp           al,bl
	         Je            test
	         Jmp           get_char
	test:    
	         ClearScreen
	         MoveCursor    60,10
	         DisplayString keyWorking

	lbl:     jmp           lbl


                  
main endp
end main



