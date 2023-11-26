[org 0x0100]
jmp start


opn1: db 'PLEASE PRESS ENTER TO START THE GAME',0
opn4: db 'PLEASE PRESS R TO RESTART THE GAME',0

score: db '--SCORE--'

opn2: db 'GAME OVER',0
opn3: db 'YOUR SCORE IS   ',0

rul1: db '1- KILL THE ENEMIES WITH BIRDS TO MOVE TO NEXT STAGE  ',0
rul2: db '2- 10 BIRDS PER STAGE, IF YOU DONT MOVE THE BIRD IN 30 SECONDS,1 BIRD DIES  ',0
rul3: db '3- IF YOU DONT KILL ALL ENEMIES WITH 10 BIRDS, GAME ENDS  ',0
rul4: db '4- PRESS ECS TO SKIP THE STAGE, TOTAL STAGES ARE 4  ',0

scorevalue : dw 0

time: db 'TIME: 0 :  '

tickcount: dw 0
seconds: dw 0
minutes: dw 0
htime: dw 0
oldsir:	dd 0

units: dw 0
tens:	dw 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;THIS SECTION DISPLAYS THE TIMER USING INTERUPTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;IF BIRD IS NOT MOVED UNTIL FIRST 30 SECONDS, A LIFE OF BIRD DECREASES;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

dominus:
call minuslife
jmp tiloop

;;;;;;;;;;;;;;;;;;;;;;;;

timer: 	pusha
	mov ax, [htime]
	cmp ax, 0
	je tiloop
	mov dx, 0
	mov bx, 29
	div bx
	cmp dx, 0
	je cgek2
	jmp tiloop
cgek2:
	cmp word [cs:tickcount], 17
	je dominus

tiloop:
	mov ax,0xb800
	mov es,ax

	push word 280
	mov ax,time
	push ax
	push word 11

	call printstr2

	inc word [cs:tickcount]; increment tick count
   	cmp word [cs:tickcount],18
	je incre
	jne ee

incre:	
	inc word [seconds]
	inc word [htime]
	mov word [cs:tickcount],0

	cmp word [seconds],60
	je min
	jne ee

min:	inc word [minutes]
	mov word [seconds],0


ee:	push word 294
	push word [minutes]
	call printnum ; print tick count	

	push word 298
	push word [seconds]
	call printnum ; print tick count

	mov al, 0x20
	out 0x20, al ; end of interrupt
	popa
	iret ; return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;HOOK THE TIMER INTERUPTS;;;;;;;;;;;;;;;;;;;;;;;;

runtime: pusha

	xor ax, ax
	mov es, ax ; point es to IVT base

	mov ax,[es:8*4]
	mov [oldsir],ax

	mov ax,[es:8*4+2]
	mov [oldsir+2],ax

	cli ; disable interrupts
	mov word [es:8*4], timer; store offset at n*4
	mov [es:8*4+2], cs ; store segment at n*4+2
	sti ; enable interrupts
	mov dx, start ; end of resident portion
	add dx, 15 ; round up to next para
	mov cl, 4
	shr dx, cl ; number of paras

	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;UNHOOK THE TIMER INTERUPT;;;;;;;;;;;;;;;;;;;;;;;;

endtime:	pusha

		xor ax,ax
	mov es,ax
	cli
	mov ax,[oldsir]
	mov [es:8*4],ax

	mov ax,[oldsir+2]
	mov [es:8*4+2],ax
	sti

		popa
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;THIS SECTION DISPLAYS THE OPENING PAGE USING INT 10 VIDEO MODE;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;BIRD IS DISPLAYED BY COMBINATION OF DIFFERENT SHAPES;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;LETTERS ARE PRINTED BY USING RECTANGLE SUBRUTINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


backgnd: PUSHA

	mov ax,0xA000
	mov es,ax	

	mov di,0
	mov cx,64000
	
l1:	mov word [es:di], 40
	add di,1
	loop l1

	push word 24
	push word 26
	push word 22440
	call eyes

	push word 24
	push word 26
	push word 29480
	call eyes

	push word 24
	push word 26
	push word 36520
	call eyes

	push word 24
	push word 26
	push word 22660
	call eyes

	push word 24
	push word 26
	push word 29700
	call eyes

	push word 24
	push word 26
	push word 36740
	call eyes


	POPA
	ret

load1:	
	pusha
	push word 16
	push word 47
	push word 22444
	call eyes

	push word 16
	push word 26
	push word 29484
	call eyes

	push word 16
	push word 26
	push word 36524
	call eyes

	push word 16
	push word 47
	push word 22664
	call eyes

	push word 16
	push word 26
	push word 29704
	call eyes

	push word 16
	push word 26
	push word 36744
	call eyes

	popa 
	ret

load2:	
	pusha
	push word 16
	push word 26
	push word 22444
	call eyes

	push word 16
	push word 47
	push word 29484
	call eyes

	push word 16
	push word 26
	push word 36524
	call eyes

	push word 16
	push word 26
	push word 22664
	call eyes

	push word 16
	push word 47
	push word 29704
	call eyes

	push word 16
	push word 26
	push word 36744
	call eyes

	popa 
	ret

load3:	
	pusha
	push word 16
	push word 26
	push word 22444
	call eyes

	push word 16
	push word 26
	push word 29484
	call eyes

	push word 16
	push word 47
	push word 36524
	call eyes

	push word 16
	push word 26
	push word 22664
	call eyes

	push word 16
	push word 26
	push word 29704
	call eyes

	push word 16
	push word 47
	push word 36744
	call eyes

	popa 
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sleep2:		push cx
		push bx

		mov cx, 0xFFFF
	dela:	loop dela

		pop bx
		pop cx

		ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sleepx:	pusha
	
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2
	call sleep2

	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
load:	pusha

	call sleepx
	call load1
	call sleepx
	call load2
	call sleepx
	call load3
	
	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	

printbird :	push ax
		push bx
		push cx
		push dx
		push di
		push si	

	mov ax,0xA000
	mov es,ax

	push word 18
	push word 4
	push word 18675
	call sphere

	push word 18
	push word 4
	push word 18725
	call sphere

	push word 15
	push word 0
	push word 18685
	call sphere

	push word 15
	push word 0
	push word 18720
	call sphere

	push word 13
	push word 15
	push word 19007
	call sphere

	push word 13
	push word 15
	push word 19042
	call sphere

	push word 18
	push word 0
	push word 24135
	call eyes

	push word 16
	push word 0
	push word 24165
	call eyes

	push word 52
	push word 0
	push word 35972
	call chon

	push word 44
	push word 43
	push word 35976
	call chon

	push word 60
	push word 0
	push word 35648
	call chonch

	push word 52
	push word 43
	push word 35652
	call chonch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	mov di,19357
	mov dx,0
	mov cx,25

l2:	mov word [es:di], 0
	add di,1
	inc dx
	cmp dx,3
	jne l2

	add di,317
	mov dx,0
	loop l2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	mov ax,19358
	mov di,ax	
	mov cx,60
	mov dx,cx
	sub di,dx
	add cx,dx
	mov bx,0

l3:	mov word [es:di], 0
	add di,1
	loop l3

	add di,320
	sub di,dx
	sub dx,5
	sub di,dx

	add bx,1
	mov cx,dx
	add cx,dx
	cmp bx,12
	jne l3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov di,ax
	sub di,321	
	mov cx,60
	mov dx,cx
	mov bx,0

l4:	mov word [es:di], 0
	sub di,1
	loop l4

	sub di,320
	sub dx,4
	add di,dx

	add bx,1
	mov cx,dx
	dec cx
	cmp bx,15
	jne l4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov di,ax
	sub di,320	
	mov cx,60
	mov dx,cx
	mov bx,0

l5:	mov word [es:di], 0
	add di,1
	loop l5

	sub di,320
	sub dx,4
	sub di,dx

	add bx,1
	mov cx,dx
	dec cx
	cmp bx,15
	jne l5

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax

	ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


sphere: push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	shl dx,1
	mov cx,dx
	mov bx,0

lo33:	mov word [es:di], ax
	add di,1
	loop lo33

	add si,319
	mov di,si
	add bx,1
	add dx,2
	mov cx,dx
	cmp bx,[bp+8]
	jne lo33

	mov cx,dx
	mov bx,0

lo7:	mov word [es:di], ax
	add di,1
	loop lo7

	add si,320
	mov di,si
	mov cx,dx
	add bx,1
	cmp bx,[bp+8]
	jne lo7

	mov cx,dx
	mov bx,0

lo4:	mov word [es:di], ax
	add di,1
	loop lo4

	add si,321
	mov di,si
	add bx,1
	sub dx,2
	mov cx,dx
	cmp bx,[bp+8]
	jne lo4


	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

eyes:	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,dx
	mov bx,0

loo3:	mov word [es:di], ax
	add di,1
	loop loo3

	add si,321
	mov di,si
	add bx,1
	sub dx,2
	mov cx,dx
	cmp dx,0
	jne loo3

	mov di,[bp+4]
	mov si,di	
	mov dx,[bp+8]
	mov cx,dx
	mov bx,0

loo4:	mov word [es:di], ax
	add di,1
	loop loo4

	sub si,319
	mov di,si
	add bx,1
	sub dx,2
	mov cx,dx
	cmp dx,0
	jne loo4


	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chonch:	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,dx
	mov bx,0

looo3:	mov word [es:di], ax
	add di,1
	loop looo3

	add si,322
	mov di,si
	add bx,1
	sub dx,4
	mov cx,dx
	cmp dx,0
	jne looo3

	mov di,[bp+4]
	mov si,di	
	mov dx,[bp+8]
	mov cx,dx
	mov bx,0

looo4:	mov word [es:di], ax
	add di,1
	loop looo4

	sub si,319
	mov di,si
	add bx,1
	sub dx,2
	mov cx,dx
	cmp dx,0
	jne looo4


	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

chon:	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,dx
	mov bx,0

loooo3:	mov word [es:di], ax
	add di,1
	loop loooo3

	add si,321
	mov di,si
	add bx,1
	sub dx,2
	mov cx,dx
	cmp dx,0
	jne loooo3

	mov di,[bp+4]
	mov si,di	
	mov dx,[bp+8]
	mov cx,dx
	mov bx,0

loooo4:	mov word [es:di], ax
	add di,1
	loop loooo4

	sub si,318
	mov di,si
	add bx,1
	sub dx,4
	mov cx,dx
	cmp dx,0
	jne loooo4


	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


rect:	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov ax,0xA000
	mov es,ax	

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,[bp+10]
	mov bx,0

loo9:	call sleep
	mov word [es:di], ax
	add di,1
	add bx,1
	cmp bx,dx
	jne loo9

	add si,320
	mov di,si
	mov bx,0
	loop loo9

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

x2:	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov ax,0xA000
	mov es,ax	

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,[bp+10]
	mov bx,0

loooo9:	call sleep
	mov word [es:di], ax
	add di,1
	add bx,1
	cmp bx,dx
	jne loooo9

	add si,319
	mov di,si
	mov bx,0
	loop loooo9

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

x1:	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push di
	push si

	mov ax,0xA000
	mov es,ax	

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,[bp+10]
	mov bx,0

looo9:	call sleep
	mov word [es:di], ax
	add di,1
	add bx,1
	cmp bx,dx
	jne looo9

	add si,321
	mov di,si
	mov bx,0
	loop looo9

	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

	ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphA:	push bp
	mov bp,sp

	PUSHA

	push word 14 ;height
	push word 3 ;width
	push word 15
	mov ax,[bp+4]
	add ax,3200
	push ax
	call rect 
	
	push word 10
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,10
	push ax
	call x1

	push word 10
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,10
	push ax
	call x2

	push word 1
	push word 20
	push word 15
	mov ax,[bp+4]
	add ax,3200
	push ax
	call rect 

	push word 14
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,3220
	push ax
	call rect 


	POPA
	pop bp
	ret 2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphN:	push bp
	mov bp,sp

	PUSHA

	push word 20 ;height
	push word 3 ;width
	push word 15
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 20
	push word 2
	push word 15
	mov ax,[bp+4]
	add ax,2
	push ax
	call x1

	push word 20
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,20
	push ax
	call rect 

	POPA
	pop bp
	ret 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphG:	push bp
	mov bp,sp

	PUSHA

	push word 20 ;height
	push word 3 ;width
	push word 15
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 3
	push word 18
	push word 15
	mov ax,[bp+4]
	add ax,2
	push ax
	call rect

	push word 3
	push word 18
	push word 15
	mov ax,[bp+4]
	add ax,6400
	push ax
	call rect 

	push word 13
	push word 2
	push word 15
	mov ax,[bp+4]
	add ax,3858
	push ax
	call rect 

	push word 2
	push word 10
	push word 15
	mov ax,[bp+4]
	add ax,3854
	push ax
	call rect 

	POPA
	pop bp
	ret 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphR:	push bp
	mov bp,sp

	PUSHA

	push word 17
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,3200
	push ax
	call x1

	push word 26 ;height
	push word 3 ;width
	push word 15
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 2
	push word 17
	push word 15
	mov ax,[bp+4]
	push ax
	call rect

	push word 2
	push word 17
	push word 15
	mov ax,[bp+4]
	add ax,3200
	push ax
	call rect 

	push word 12
	push word 2
	push word 15
	mov ax,[bp+4]
	add ax,17
	push ax
	call rect 

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


aphY:	push bp
	mov bp,sp

	PUSHA

	push word 10
	push word 3
	push word 15
	mov ax,[bp+4]
	push ax
	call x1

	push word 10
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,20
	push ax
	call x2

	push word 16
	push word 3
	push word 15
	mov ax,[bp+4]
	add ax,3210
	push ax
	call rect 

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphB:	push bp
	mov bp,sp

	PUSHA

	push word 30
	push word 4
	push word 44
	mov ax,[bp+4]
	push ax
	call rect

	

	push word 3
	push word 22
	push word 44
	mov ax,[bp+4]
	push ax
	call rect

	push word 3
	push word 22
	push word 44
	mov ax,[bp+4]
	add ax,4800
	push ax
	call rect 
	
	push word 3
	push word 22
	push word 44
	mov ax,[bp+4]
	add ax,9600
	push ax
	call rect

	push word 7
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,20
	push ax
	call x1
	
	push word 8
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,2267
	push ax
	call x2

	push word 8
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,4820
	push ax
	call x1
	
	push word 10
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,7388
	push ax
	call x2

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphI:	push bp
	mov bp,sp

	PUSHA

	push word 34
	push word 4
	push word 44
	mov ax,[bp+4]
	push ax
	call rect

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphR1:	push bp
	mov bp,sp

	PUSHA

	push word 20
	push word 3
	push word 44
	mov ax,[bp+4]
	add ax,4800
	push ax
	call x1

	push word 34 ;height
	push word 4 ;width
	push word 44
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 3
	push word 24
	push word 44
	mov ax,[bp+4]
	push ax
	call rect

	push word 3
	push word 24
	push word 44
	mov ax,[bp+4]
	add ax,4800
	push ax
	call rect 

	push word 15
	push word 3
	push word 44
	mov ax,[bp+4]
	add ax,21
	push ax
	call rect 

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphD:	push bp
	mov bp,sp

	PUSHA

	push word 30 ;height
	push word 4 ;width
	push word 44
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 3
	push word 17
	push word 44
	mov ax,[bp+4]
	push ax
	call rect

	push word 3
	push word 15
	push word 44
	mov ax,[bp+4]
	add ax,9600
	push ax
	call rect 

	push word 15
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,15
	push ax
	call x1

	push word 18
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,4830
	push ax
	call x2


	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


aphS:	push bp
	mov bp,sp

	PUSHA

	push word 10
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,10
	push ax
	call x2

	push word 10
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,10
	push ax
	call x1

	push word 20
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,3200
	push ax
	call x1

	push word 10
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,9600
	push ax
	call x1

	push word 10
	push word 4
	push word 44
	mov ax,[bp+4]
	add ax,9620
	push ax
	call x2


	POPA
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sleepp:		push cx
		push bx

		mov cx, 0xFFFF
	delayy:	loop delay

		pop bx
		pop cx

		ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


sleep:		push cx
		push bx

		mov cx, 0x1FFF
	delay:	loop delay

		pop bx
		pop cx

		ret 

open1:	pusha
	
		call backgnd
		call printbird

		push word 3920
		call aphA

		push word 4600
		call aphN

		push word 4320
		call aphG
	
		push word 4040
		call aphR

		push word 4070
		call aphY

		push word 45510
		call aphB

		push word 45555
		call aphI

		push word 45580
		call aphR1
		
		push word 45625
		call aphD

		push word 44715
		call aphS

		call load
		call load
		call load

		call clrscr

		popa
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;THIS SECTION DISPLAYS THE INSTRUCTION PAGE BY DEFAULT VIDEO MODE;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;GRASS AND TREE SUBROUTINES ARE CALLED IN IT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;INSTRUCTIONS ARE DISPLAYED ON IT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

open2:
	pusha

	mov ax, 0xb800
	mov es, ax
	mov cx, 2000
	mov ax, 0x1020
	mov di, 0
	cld
	rep stosw

	push word 22
	push word 10
	push word 0x9E
	push opn1
	call printstr

	push word 2
	push word 4
	push word 0x1E
	push rul1
	call printstr

	push word 2
	push word 5
	push word 0x1E
	push rul2
	call printstr

	push word 2
	push word 6
	push word 0x1E
	push rul3
	call printstr

	push word 2
	push word 7
	push word 0x1E
	push rul4
	call printstr

	

	call grass
	call trees
	call build

	popa
	ret

printstr: 

	push bp 
 	mov bp, sp 
 	push es 
 	push ax 
 	push cx 
 	push si 
 	push di 
 	push ds 
 	pop es 			; load ds in es 
 	mov di, [bp+4] 		; point di to string 
 	mov cx, 0xffff 		; load maximum number in cx 
 	xor al, al 		; load a zero in al 
 	repne scasb 		; find zero in the string 
 	mov ax, 0xffff 		; load maximum number in ax 
 	sub ax, cx 		; find change in cx 
 	dec ax 			; exclude null from length 
 	jz exit 		; no printing if string is empty
 	mov cx, ax 		; load string length in cx 
 	mov ax, 0xb800 
 	mov es, ax 		; point es to video base 
 	mov al, 80 		; load al with columns per row 
 	mul byte [bp+8] 	; multiply with y position 
 	add ax, [bp+10] 	; add x position 
 	shl ax, 1 		; turn into byte offset 
 	mov di,ax 		; point di to required location 
 	mov si, [bp+4] 		; point si to string 
 	mov ah, [bp+6] 		; load attribute in ah 
 	cld 			; auto increment mode 
	nextchar: 
		lodsb 		; load next char in al 
 		stosw 		; print char/attribute pair 
 		loop nextchar 	; repeat for the whole string 
	exit: 
		pop di 
 		pop si 
 		pop cx 
 		pop ax 
 		pop es 
 		pop bp 
 		ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

grass:	 	pusha
		mov ax, 0xb800
		mov es, ax
		mov di, 4000

	l11:	mov word [es:di], 0x2F5E
		sub di,2
		cmp di,3678
		jne l11

  		popa
  		ret 		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

printTriangle:

	push bp
	mov bp, sp
	sub sp, 4
	push ax
	push bx
	push cx
	push dx
	
	mov ax, 0xb800
	mov es, ax

	mov ax, [bp + 6]	;top in ax
	mov bx, 160
	mul bx
	mov [bp - 2], ax	;top in [bp - 2]
	
	mov ax, [bp + 4]	;left in ax
	shl ax, 1
	mov [bp - 4], ax	;left in [bp - 4]

	mov di, word [bp - 2]	
	add di, word [bp - 4]	;top of triangle
	mov ax, [bp + 10]
	mov bx, 1
	mov cx, 1

	L1:
		rep stosw
		add di, 160
		shl bx, 1
		sub di, bx
		sub di, 2
		shr bx, 1
		add bx, 2
		mov cx, bx
		sub [bp + 8], word 1
		mov dx, [bp + 8]
		cmp dx, 0
		jne L1
		
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	pop bp
	ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

rectan:	push bp
	mov bp,sp
	pusha

	mov ax,0xb800
	mov es,ax	

	mov di,[bp+4]
	mov si,di	
	mov ax,[bp+6]
	mov dx,[bp+8]
	mov cx,[bp+10]
	mov bx,0

lll9:	mov word [es:di], ax
	add di,2
	add bx,1
	cmp bx,dx
	jne lll9

	add si,160
	mov di,si
	mov bx,0
	loop lll9

	popa
	pop bp

	ret 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

build2:
	push bp
	mov bp,sp

	pusha

	mov ax,0xb800
	mov es,ax

	push word 10
	push word 8
	mov ax,[bp+4]
	push ax
	mov di,2124
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+964],0x7020
	mov word[es:di+1284],0x7020
	mov word[es:di+330],0x7020
	mov word[es:di+650],0x7020
	mov word[es:di+970],0x7020
	mov word[es:di+1290],0x7020
	
	push word 6
	push word 6
	mov ax,[bp+4]
	push ax
	mov di,2782
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+328],0x7020
	mov word[es:di+648],0x7020
	
	push word 12
	push word 6
	mov ax,[bp+4]
	push ax
	mov di, 1838
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+964],0x7020
	mov word[es:di+1284],0x7020
	mov word[es:di+328],0x7020
	mov word[es:di+648],0x7020
	mov word[es:di+968],0x7020
	mov word[es:di+1288],0x7020
	mov word[es:di+1604],0x7020
	mov word[es:di+1608],0x7020

	

	popa
	pop bp


	ret 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

build:
	pusha

	mov ax,0xb800
	mov es,ax

	push word 10
	push word 8
	push word 0x0720
	mov di,2184
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+964],0x7020
	mov word[es:di+1284],0x7020
	mov word[es:di+330],0x7020
	mov word[es:di+650],0x7020
	mov word[es:di+970],0x7020
	mov word[es:di+1290],0x7020
	
	push word 6
	push word 6
	push word 0x0720
	mov di,2842
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+328],0x7020
	mov word[es:di+648],0x7020
	
	push word 12
	push word 6
	push word 0x0720
	mov di, 1898
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+964],0x7020
	mov word[es:di+1284],0x7020
	mov word[es:di+328],0x7020
	mov word[es:di+648],0x7020
	mov word[es:di+968],0x7020
	mov word[es:di+1288],0x7020
	mov word[es:di+1604],0x7020
	mov word[es:di+1608],0x7020

	push word 10
	push word 12
	push word 0x0720
	mov di,2088
	push di
	call rectan

	mov word[es:di+324],0x7020
	mov word[es:di+644],0x7020
	mov word[es:di+964],0x7020
	mov word[es:di+1284],0x7020
	mov word[es:di+330],0x7020
	mov word[es:di+650],0x7020
	mov word[es:di+970],0x7020
	mov word[es:di+1290],0x7020
	mov word[es:di+336],0x7020
	mov word[es:di+656],0x7020
	mov word[es:di+976],0x7020
	mov word[es:di+1296],0x7020

	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

trees:	 	push es
		push ax
		push di
		push cx
		push dx

		mov ax, 0xb800
		mov es, ax	

		push word 0x2F5E	;color attribute
		push word 5		;height of triangle
		push word 16		;top margin
		push word 25	;left margin
		call printTriangle
		
		mov di, 3410
		mov word [es:di], 0x6020
		add di,160
		mov word [es:di], 0x6020

		push word 0x2F5E	;color attribute
		push word 4		;height of triangle
		push word 18		;top margin
		push word 35	;left margin
		call printTriangle

		mov di, 3590
		mov word [es:di], 0x6020

		push word 0x2F5E	;color attribute
		push word 5		;height of triangle
		push word 16		;top margin
		push word 45	;left margin
		call printTriangle

		mov di, 3450
		mov word [es:di], 0x6020
		add di,160
		mov word [es:di], 0x6020
		
		pop dx
		pop cx
		pop di
  		pop ax
  		pop es
  		ret 	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
boww:	 	push es
		push ax
		push di
		push cx
		push si

		mov ax, 0xb800
		mov es, ax
		mov di, 3692

		mov cx,3
	lll3:	mov word [es:di], 0x0720
		sub di,160
		loop lll3
		
		mov si,di
		sub di,2
		mov word [es:di], 0x0720
		sub di,162
		mov word [es:di], 0x0720

		mov di,si
		add di,2
		mov word [es:di], 0x0720
		sub di,158
		mov word [es:di], 0x0720

  		pop si
		pop cx
		pop di
  		pop ax
  		pop es
  		ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;THIS SECTION DISPLAYS THE ENDING PAGE USING INT 10 VIDEO MODE;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;FIRST CLEAR THE SCREEN WITH ANIMATION,THEN DISPLAYED;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;LETTERS ARE PRINTED BY USING RECTANGLE SUBRUTINE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



sleep0:		pusha

		mov cx, 0x009F
	del:	loop del

		popa

		ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

clrscr: PUSHA

	mov ax,0xA000
	mov es,ax	

	mov cx,200
	mov si,0
	mov bx,0
	mov di,si
	
ll1:	call sleep0
	mov word [es:di], 0
	add di,1
	add bx,1
	cmp bx,320
	jne ll1

	add si,320
	mov di,si
	mov bx,0
	loop ll1

	popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphT:	push bp
	mov bp,sp

	PUSHA

	push word 25 ;height
	push word 2 ;width
	push word 40
	mov ax,[bp+4]
	add ax,10
	push ax
	call rect 

	push word 3
	push word 22
	push word 40
	mov ax,[bp+4]
	push ax
	call rect 

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphH:	push bp
	mov bp,sp

	PUSHA

	push word 25 ;height
	push word 2 ;width
	push word 40
	mov ax,[bp+4]
	push ax
	call rect 

	push word 25 ;height
	push word 2 ;width
	push word 40
	mov ax,[bp+4]
	add ax,16
	push ax
	call rect 

	push word 2
	push word 16
	push word 40
	mov ax,[bp+4]
	add ax,3520
	push ax
	call rect 

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphE:	push bp
	mov bp,sp

	PUSHA

	push word 25 ;height
	push word 2 ;width
	push word 40
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 3
	push word 15
	push word 40
	mov ax,[bp+4]
	push ax
	call rect

	push word 2
	push word 12
	push word 40
	mov ax,[bp+4]
	add ax,3520
	push ax
	call rect

	push word 3
	push word 15
	push word 40
	mov ax,[bp+4]
	add ax,7040
	push ax
	call rect 


	POPA
	pop bp
	ret 2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphN2:	push bp
	mov bp,sp

	PUSHA

	push word 21 ;height
	push word 2 ;width
	push word 40
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 21
	push word 2
	push word 40
	mov ax,[bp+4]
	add ax,2
	push ax
	call x1

	push word 21
	push word 2
	push word 40
	mov ax,[bp+4]
	add ax,22
	push ax
	call rect 

	POPA
	pop bp
	ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

aphD2:	push bp
	mov bp,sp

	PUSHA

	push word 25 ;height
	push word 2 ;width
	push word 40
	mov ax,[bp+4]
	push ax
	call rect 
	
	push word 2
	push word 10
	push word 40
	mov ax,[bp+4]
	push ax
	call rect

	push word 2
	push word 10
	push word 40
	mov ax,[bp+4]
	add ax,7360
	push ax
	call rect 

	push word 12
	push word 2
	push word 40
	mov ax,[bp+4]
	add ax,10
	push ax
	call x1

	push word 13
	push word 2
	push word 40
	mov ax,[bp+4]
	add ax,3860
	push ax
	call x2


	POPA
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bgp: PUSHA

	mov ax,0xA000
	mov es,ax	

	mov cx,200
	mov si,0
	mov bx,0
	mov di,si
	
ll19:	call sleep0
	mov word [es:di], 68
	add di,1
	add bx,1
	cmp bx,320
	jne ll19

	add si,320
	mov di,si
	mov bx,0
	loop ll19

	popa
	ret

endp:	pusha
	
		
		call bgp
		call printbird

		push word 22405
		call aphT

		push word 22435
		call aphH

		push word 22465
		call aphE

		push word 22629
		call aphE

		push word 23294
		call aphN2

		push word 22688
		call aphD2

		popa
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;THIS SECTION DISPLAYS THE GAME PAGE USING DEFAULT VIDEO MODE;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;DIFFERENT BACKGROUNDS ARE USED FOR DIFFERENT LEVELS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;4 STAGES IN TOTAL, PRESS ECS FOR NEXT STAGE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

backgnd1: 	push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax
		mov di, 0

	nextlo:  mov word [es:di], 0x7FDB
  		add di, 2
		cmp di, 4000
  		jne nextlo


		push word 0xF3B0
		mov di,242
		push di
		call clouds

		push word 0xF3B0
		mov di,196
		push di
		call clouds

		push word 0x01DB
		call build2

  		pop di
  		pop ax
  		pop es
  		ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

backgnd2: 	push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax
		mov di, 0

	nextlo1:  mov word [es:di], 0x3BDB
  		add di, 2
		cmp di, 4000
  		jne nextlo1


		push word 0x77DB
		mov di,242
		push di
		call clouds

		push word 0x77DB
		mov di,196
		push di
		call clouds

		push word 0x01DB
		call build2


  		pop di
  		pop ax
  		pop es
  		ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

backgnd3: 	push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax
		mov di, 0

	nextlo3:  mov word [es:di], 0x19DB
  		add di, 2
		cmp di, 4000
  		jne nextlo3


		push word 0xB1B0
		mov di,242
		push di
		call clouds

		push word 0xB1B0
		mov di,196
		push di
		call clouds

		push word 0x0720
		call build2


  		pop di
  		pop ax
  		pop es
  		ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

backgnd4: 	push es
		push ax
		push di
		mov ax, 0xb800
		mov es, ax
		mov di, 0

	nextlo4:  mov word [es:di], 0x1020
  		add di, 2
		cmp di, 4000
  		jne nextlo4


		push word 0xF3DB
		mov di,236
		push di
		call clouds

		push word 0xF3DB
		mov di,196
		push di
		call clouds

		push word 0x0720
		call build2


  		pop di
  		pop ax
  		pop es
  		ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

endpage:
	pusha

	mov ax, 0xb800
	mov es, ax
	mov cx, 2000
	mov ax, 0x1020
	mov di, 0
	cld
	rep stosw

	push word 34
	push word 5
	push word 0x1E
	push opn2
	call printstr

	push word 30
	push word 9
	push word 0x71
	push opn3 
	call printstr

	mov di,1528 ; push di position
	push di
	mov ax,[scorevalue]
	push ax
	call printnum

	push word 22
	push word 11
	push word 0x9E
	push opn4
	call printstr

	call grass
	call trees
	call build

	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printnum: 	push bp	
		mov bp, sp
		push es
		push ax
		push bx
		push cx
		push dx
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov ax, [bp+4] ; load number in ax
		mov bx, 10 ; use base 10 for division
		mov cx, 0 ; initialize count of digits

   nextdigit: 	mov dx, 0 ; zero upper half of dividend
		div bx ; divide by 10
		add dl, 0x30 ; convert digit into ascii value
		push dx ; save ascii value on stack
		inc cx ; increment count of values
		cmp ax, 0 ; is the quotient zero
		jnz nextdigit ; if no divide it again
		mov di, [bp+6]

  nextpos: 	pop dx ; remove a digit from the stack
		mov dh, 0x71 ; use normal attribute
		mov [es:di], dx ; print char on screen
		add di, 2 ; move to next screen location
		loop nextpos ; repeat for all digits on stack
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;THE FOLLOWING CODE DISPLAYS SCORE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

prt0:		pusha
		mov ax, 0xb800
		mov es, ax
		mov word [es:di], 0x1020   	 ;print 0
		mov word [es:di +2], 0x1020
		mov word [es:di +4], 0x1020
		mov word [es:di + 160], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 480], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 640], 0x1020
		mov word [es:di + 642], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret


prt1:		pusha
		mov ax, 0xb800
		mov es, ax 		;print 1
		mov word [es:di + 4], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret

prt2:		pusha
		mov ax, 0xb800
		mov es, ax			;print 2
		mov word [es:di + 0], 0x1020
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 480], 0x1020
		mov word [es:di + 640], 0x1020
		mov word [es:di + 642], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret

prt3:		pusha
		mov ax, 0xb800
		mov es, ax
		mov word [es:di + 0], 0x1020    ;print 3
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 640], 0x1020
		mov word [es:di + 642], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret
	
prt4:		pusha
		mov ax, 0xb800
		mov es, ax			;print 4
		mov word [es:di + 0], 0x1020
		mov word [es:di + 160], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret
	
prt5: 		pusha
		mov ax, 0xb800
		mov es, ax		;print 5
		mov word [es:di + 0], 0x1020
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 160], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 640], 0x1020
		mov word [es:di + 642], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret

prt6:		pusha
		mov ax, 0xb800
		mov es, ax			;print 6
		mov word [es:di + 0], 0x1020
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 160], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 640], 0x1020
		mov word [es:di + 642], 0x1020
		mov word [es:di + 644], 0x1020
		mov word [es:di + 480], 0x1020
		popa
		ret

prt7:		pusha
		mov ax, 0xb800
		mov es, ax			;print 7
		mov word [es:di + 0], 0x1020
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret

prt8:		pusha
		mov ax, 0xb800
		mov es, ax		;print 8
		mov word [es:di + 0], 0x1020
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 160], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 640], 0x1020
		mov word [es:di + 642], 0x1020
		mov word [es:di + 644], 0x1020
		mov word [es:di + 480], 0x1020
		popa
		ret

prt9:		
		pusha
		mov ax, 0xb800
		mov es, ax		;print 9
		mov word [es:di + 0], 0x1020
		mov word [es:di + 2], 0x1020
		mov word [es:di + 4], 0x1020
		mov word [es:di + 160], 0x1020
		mov word [es:di + 164], 0x1020
		mov word [es:di + 324], 0x1020
		mov word [es:di + 322], 0x1020
		mov word [es:di + 320], 0x1020
		mov word [es:di + 484], 0x1020
		mov word [es:di + 644], 0x1020
		popa
		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

printscore: 	
			push es
			push ax
			push di
			push bx
			push cx

			mov ax, 0xb800
			mov es, ax
			mov ax,0x7FDB
			mov di,0
			mov cx,8

					
       		lq1:	mov word [es:di], ax
			mov word [es:di +2], ax
			mov word [es:di +4], ax
			mov word [es:di +6], ax
			mov word [es:di +8], ax
			mov word [es:di +10], ax
			mov word [es:di +12], ax
			mov word [es:di +14], ax
			mov word [es:di +16], ax
			add di,160
			loop lq1

	printdigit1:	mov di,170

			mov al,[units]   ;ah will have first digit
					;al will have second digit

			cmp al,0
			je prin0

			cmp al,1
			je prin1

			cmp al,2
			je prin2

			cmp al,3
			je prin3

			cmp al,4
			je prin4

			cmp al,5
			je prin5

			cmp al,6
			je prin6

			cmp al,7
			je prin7

			cmp al,8
			je prin8

			cmp al,9
			je prin9

	prin0:	call prt0
		jmp printdigit2

	prin1:	call prt1
		jmp printdigit2

	prin2: call prt2
		jmp printdigit2

	prin3: call prt3
		jmp printdigit2

	prin4: call prt4
		jmp printdigit2

	prin5:	call prt5
		jmp printdigit2

	prin6: call prt6
		jmp printdigit2

	prin7: call prt7
		jmp printdigit2

	prin8: call prt8
		jmp printdigit2

	prin9: call prt9
		jmp printdigit2


	printdigit2:	mov di,162

			mov al,[tens]
			
			cmp al,0
			je print0

			cmp al,1
			je print1

			cmp al,2
			je print2

			cmp al,3
			je print3

			cmp al,4
			je print4

			cmp al,5
			je print5

			cmp al,6
			je print6

			cmp al,7
			je print7

			cmp al,8
			je print8

			cmp al,9
			je print9

	print0:	call prt0
		jmp end
	
	print1:	call prt1
		jmp end

	print2: call prt2
		jmp end

	print3: call prt3
		jmp end

	print4: call prt4
		jmp end

	print5:	call prt5
		jmp end

	print6: call prt6
		jmp end

	print7: call prt7
		jmp end

	print8: call prt8
		jmp end

	print9: call prt9
		jmp end
	
		
		end:	mov di,1120  ; push di position
			push di

			mov ax,score
			push ax ; push address of str

			push word 9 ;push size of str
			call printstr2

			pop cx
			pop bx
  			pop di
  			pop ax
  			pop es
  			ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		


updatescore:		pusha

			inc word [scorevalue]

			inc word [units]

  			cmp word [units],10
			je tenp
			jne outa

	tenp:	inc word [tens]
		mov word [units],0

	outa:	popa
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printstr2: 	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov di, [bp+8] ; point di to top left column
		mov si, [bp+6] ; point si to string
		mov cx, [bp+4] ; load length of string in cx
		mov ah, 0x71 ; normal attribute fixed in al

    nextchar2: 	mov al, [si] ; load next char of string
		mov [es:di], ax ; show this char on screen
		add di, 2 ; move to next screen location
		add si, 1 ; move to next char in string
		loop nextchar2 ; repeat the operation cx times
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;THIS SECTION PLAYS THE GAME , USING KEYBOARD INTERUPTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;4 STAGES IN TOTAL, PRESS ECS FOR NEXT STAGE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

mysleep:
	push ax
	push cx
	mov cx, [left]
	mov ax, 4
	sub ax, cx
	add ax, 1
	mov cx, ax
lop1:
	call sleepp
	loop lop1
	pop cx
	pop ax
	ret

;;;;;;;;;;;;;;;;;;;;

mysleep2:
	push ax
	push cx
	mov cx, [down]
	mov ax, 4
	sub ax, cx
	add ax, 1
	mov cx, ax
lop2:
	call sleepp
	loop lop2
	pop cx
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
removingx:
	pusha
	mov bp, sp
	mov cx, 10
	mov di, [bp + 18]

remh:
	mov ax, [bgc]
	mov word [es:di], ax
	add di, 2
	loop remh
oyt2:
	popa
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
removingy:
	pusha
	mov bp, sp
	mov di, [bp+18]
	mov cx, 4
	vrem
	mov ax, [bgc]
	mov word [es:di], ax
	add di, 160
	cmp di, 3680
	jge oyt
	loop vrem
oyt:
	popa
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
iftouch:
	pusha
	mov bp, sp
	mov ax, [bp + 18]
	cmp al, 0x30
	je removex
	cmp al, 0x31
	je removey
	popa
	ret 4
removey:
	mov word [btouch], 1
	mov word [touchb], 1
	push word [bp + 20]
	call removingy
	popa
	ret 4
removex:
	mov word [btouch], 1
	mov word [touchb], 1
	push word [bp + 20]
	call removingx
	popa
	ret 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ifenetouch:
	pusha
	mov bx, 0
	mov cx, 0
	mov ax, [es:di]
	cmp ah, 0x20
	jge cg2
	jmp outofa
cg2:
	cmp ah, 0x30
	jl cg3
	jmp outofa
cg3:
	cmp al, 0x5E
	jne cmpring
	jmp outofa
	cmpring:
	cmp cx, 15
	jg outof
	cmp di, [enes+bx]
	jge enck2
	jmp outofa
enck2:
	cmp di, [enes+bx+2]
	jle enck3
loiu:
	add bx, 4
	add cx, 1
	jmp cmpring
enck3:
	push bx
	mov ax, [enes + bx]
	mov dx, 0
	mov bx, 160
	div bx
	mov si, dx
	mov ax, di
	mov dx, 0
	mov bx, 160
	div bx
	pop bx
	cmp dx, si
	jl loiu
	add si, 12
	cmp dx, si
	jg loiu
	mov di, [enes+bx]
	mov word [touchb], 1
	call enemeyclear
	mov word [btouch], 1
outof:
	popa
	ret
outofa:
	popa
	ret
;;;;;;;;;;;;;;;;;;;;
incrscore:
	call updatescore
	mov word [btouch], 0
	call printscore
	jmp backfunc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
redbirdclear:
		pusha
		sub di, 2
		mov ax, 0xb800
		mov es, ax
		mov cx, 3
		mov bx, 0
recti:
		push di
		push cx
		mov cx, 7
rect2i:
		mov ax, [path + bx]
		mov word [es:di], ax
		push di
		push word [es:di]
		call iftouch
		cmp word [btouch], 1
		je incrscore
backfunc:
		call ifenetouch
		cmp word [btouch], 1
		je incrscore
backfunc2:
		add di, 2
		add bx, 2
		loop rect2i
		pop cx
		pop di
		add di, 160
		loop recti
		popa
		ret
;;;;;;;;;;;;;;;;;;;;;;;;;;
redbird:
	pusha
	mov ax, 0xb800
	mov es, ax

	mov cx, 3
	push di
	mov bx, 0
	sub di, 2
	srecti:        
	push di
	push cx
	mov cx, 7
	srect2i:
	mov ax, [es:di]
	mov [path + bx], ax
	add di, 2
	add bx, 2
	loop srect2i
	pop cx
	pop di
	add di, 160
	loop srecti

	mov bx, 0
	mov cx, 3
	pop di
rectt:         ;printing body
	push di
	push cx
	mov cx, 4
rect2:
	mov word [es:di], 0x44DB
	add di, 2
	loop rect2
	pop cx
	pop di
	add di, 160
	loop rectt

	;printing the beak 
	mov word [es:di - 320 + 8], 0xEE00
	mov word [es:di - 320 + 10], 0xE02d


	mov word [es:di - 480 + 4], 0x432d; printing eyebrow
	mov word [es:di - 320 + 4], 0x8E6f; printing the eye

	mov word [es:di - 160], 0xEE00;printing tail
	mov word [es:di - 320], 0xEE00;
	mov word [es:di - 160 - 2], 0xEE00;

	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;
enemeyclear:

	push ax
	push bx
	push cx
	push di

	mov ax,[bgc]
	mov cx, 3
	en11:         ;printing body
	push di
	push cx
	mov cx, 7
en22:
	mov word [es:di], ax
	add di, 2
	loop en22
	pop cx
	pop di
	add di, 160
	loop en11

	mov [es:di - 480 + 4], ax ;
	mov [es:di - 480 + 8], ax

	mov [es:di - 320 + 4],  ax
	mov [es:di - 320 + 8],  ax

	mov  [es:di - 160 + 4],  ax
	mov  [es:di - 160 + 6],  ax
	mov  [es:di - 160 + 8],  ax

	pop di
	pop cx
	pop bx
	pop ax
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;
enemey:
	push ax
	push bx
	push cx
	push di

	mov cx, 3
	en1:         ;printing body
	push di
	push cx
	mov cx, 7
en2:
	mov word [es:di], 0x22DB
	add di, 2
	loop en2
	pop cx
	pop di
	add di, 160
	loop en1

	mov word [es:di - 480 + 4], 0x2b40; printing eyes
	mov word [es:di - 480 + 8], 0x2b40

	mov word [es:di - 320 + 4], 0x8020; printing nose
	mov word [es:di - 320 + 8], 0x8020

	mov word [es:di - 160 + 4], 0x276f; printing teeth
	mov word [es:di - 160 + 6], 0x276f
	mov word [es:di - 160 + 8], 0x276f

	pop di
	pop cx
	pop bx
	pop ax
	ret
;;;;;;;;;;;;;;;;;
hline:
	push bp
	mov bp, sp
	push ax
	push cx
	push di
	mov cx, [bp + 6];length
	mov ax, [bp + 4];color
cloop:
	mov [es:di], ax
	add di, 2
	loop cloop
	pop di
	pop cx
	pop ax
	pop bp
	ret 4
;;;;;;;;;;;;;;;;;
msleep:
	push bp
	mov bp, sp
	push cx
	mov dx, [bp+4]
l9:
	call sleepp
	loop l9
	pop cx
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;
clouds:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push di
	mov di, [bp + 4]
	push 10
	mov ax,[bp+6]
	push ax
	call hline
	;
	add di, 160
	sub di, 2
	push 14
	push ax
	call hline
	;
	add di, 160
	sub di, 4
	push 16
	push ax
	call hline
	pop di
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tower:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push es
	push di
	push si
	mov di, 584
	mov ax, 0xb800
	mov es, ax
	mov si, [bp + 4]
	mov bx, -2
	mov cx, 5
tloop:
	push di
	push cx
	mov cx, 7
	nloop:
		add bx, 2
		cmp word [si + bx], 1
		je draw
		cmp cx, 7
		je veradd
		cmp cx, 5
		je veradd
		cmp cx, 3
		je veradd
		cmp cx, 1
		je veradd
		add di, 18
		loop nloop
pop cx
pop di
add di, 640
loop tloop
jmp tend
		veradd:
		add di, 0
		conti:
		loop nloop
pop cx
pop di
add di, 640
loop tloop
jmp tend
		draw:
			cmp cx, 7
			je ver
			cmp cx, 5
			je ver
			cmp cx, 3
			je ver
			cmp cx, 1
			je ver
			push cx
			mov cx, 9
		hor:
			mov word [es:di], 0x4430
			add di, 2
			loop hor
			pop cx
			jmp conti
			ver:
			push cx
			mov cx, 4
		verl:
			mov word [es:di], 0x4431
			add di, 160
			loop verl
			pop cx
			sub di, 640
			jmp conti
tend:
	pop si
	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
minuslife:
	sub word [live2], 1
	push word [live2]
	call livesleft
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stra:
	mov cx, [hori]
line1:
	call redbirdclear
	cmp word [touchb], 1
	je endo2
	add di, 2
	call redbird
	call mysleep
	loop line1
endo2:
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
endu:
	call redbirdclear
	mov word [touchb], 0
	ret
;;;;;;;;;;
release:
	mov cx, [height]
	mov di, [currpos]

curve:
	call redbirdclear
	cmp word [touchb], 1
	je endo
	sub di, 160
	add di, 2
	call redbird
	call mysleep
	loop curve

	call stra
	cmp word [touchb], 1
	je endo

curve2:
	call redbirdclear
	cmp word [touchb], 1
	je endo
	add di, 160
	add di, 2
	mov ax, di
	mov dx, 0
	mov bx, 160
	div bx
	cmp dx, 10
	jl endo
	cmp di, 3200
	jg endo
	call redbird
	call mysleep
	jmp curve2

endo:
	mov word [touchb], 0
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
setpathpara:
	mov word [hori], 0
	mov word [height], 0
	mov ax, [down]
	mov bx, 10
	mov dx, 0
	mul bx
	mov cx, ax
	mov ax, [left]
	mov dx, 0
	mov bx, 2
	mul bx
	add cx, ax
	mov bx, cx
	mov ax, [proj + bx]
	mov dx, 0
	mov bx, 10
	div bx
	mov [hori], dx
	mov dx, 0
	div bx
	push ax
	mov ax, dx
	mov dx, 0
	mul bx
	add [hori], ax
	pop ax
	mov dx, 0
	div bx
	mov [height], dx
	mov dx, 0
	div bx
	mov ax, dx
	mov dx, 0
	mul bx
	add [height], ax
endol:
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
moveup:
	mov ax, [currpos]
	sub ax, 160
	cmp ax, 2234
	jl lok
	cmp word [down], 0
	jg decres
	contu:
	mov di, [currpos]
	call redbirdclear
	sub word [currpos], 160
	mov di, [currpos]
	call redbird
	cmp word [currpos], 2720
	jge input
	add word [up], 1
	jmp input
	decres:
	sub word [down], 1
	jmp contu
	lok:
	jmp input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
movedown:
	mov ax, [currpos]
	add ax, 160
	cmp ax, 3210
	jg input
	cmp word [up], 0
	jg decres2
	contu2:
	mov di, [currpos]
	call redbirdclear
	add word [currpos], 160
	mov di, [currpos]
	call redbird
	cmp word [currpos], 2730
	jle input
	add word [down], 1
	jmp input
	decres2:
	sub word [up], 1
	jmp contu2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
setpath:
	cmp word [left], 0
	je mov2
	cmp word [down], 0
	je mov3
	call setpathpara
	call release
	jmp k
mov2:
	call movs2
	jmp k
mov3:
	call movs3
	jmp k
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
startgame:
	pusha
	mov si, 10
input:
	cmp word [live2], 0
	je endinoe
	mov ah, 0
	int 16h
	cmp ah, 4dh
	je lifeh
	cmp ah, 4bh
	je moveback
	cmp ah, 48h
	je moveup
	cmp ah, 50h
	je movedown
	cmp ah, 01h
	je endinoe
	jmp input
k:
	sub si, 1
	cmp si, 0
	jle endino
	push 0
	call eyes1
	pop ax
	cmp ax, 0
	je endino
	mov word [currpos], 2730
	mov di, [currpos]
	call redbird
	mov word [left], 0
	mov word [down], 0
	jmp input
endino:
	popa
	ret
endinoe:
	mov word [forcej], 1
	popa
	ret
;;;;;;;;;;;;;;
lifeh:
	mov word [htime], 0
	call minuslife
	jmp setpath
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
moveback:
	mov ax, [currpos]
	sub ax, 2
	mov dx, 0
	mov bx, 160
	div bx
	cmp dx, 0
	je input
	add word [left], 1
	mov di, [currpos]
	call redbirdclear
	sub word [currpos], 2
	mov di, [currpos]
	call redbird
	jmp input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
movebirdup:
	push bp
	mov bp, sp
	mov ax, [bp + 4]
	mov bx, 6
	mov dx, 0
	mul bx
	mov cx, ax
goingup:
	cmp word [touchb], 1
	je doend2
	call redbirdclear
	sub di, 160
	call redbird
	call mysleep2
	loop goingup

goingdown2:
	call redbirdclear
	cmp word [touchb], 1
	je doend2
	add di, 160
	cmp di, 3680
	jg doend2
	call redbird
	call mysleep2
	jmp goingdown2
doend2:
	mov word [touchb], 0
	pop bp
	ret 2

rc1:
	mov di, [currpos]
	call redbirdclear
	jmp koi
movs2:
	cmp word [down], 0
	je rc1
	push word [down]
	call movebirdup
koi:
	call redbirdclear
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
movebirdright:
	push bp
	mov bp, sp
	mov ax, [bp + 4]
	mov bx, 17
	mov dx, 0
	mul bx
	mov cx, ax
	goingright:
	cmp word [touchb], 1
	je doend
	call redbirdclear
	cmp word [touchb], 1
	je doend
	add di, 2
	call redbird
	call mysleep
	loop goingright

goingdown:
	call redbirdclear
	cmp word [touchb], 1
	je doend
	add di, 162
	cmp di, 3200
	jg doend
	call redbird
	call mysleep
	jmp goingdown
doend:
	mov word [touchb], 0
	pop bp
	ret 2

rc3:
	mov di, [currpos]
	call redbirdclear
	jmp koi2
movs3:
	cmp word [left], 0
	je rc3
	push word [left]
	call movebirdright
koi2:
	ret
;;;;;;;;;;;;;;;
eyes1:
	pusha
	mov bp, sp
	mov cx, 2000
	mov ax, 0xb800
	mov es, ax
	mov di, 0
loopy:
	mov ax, [es:di]
	cmp al, 0x40
	je incret
iconti:
	add di,2
	loop loopy
	mov ax, [bp + 18]
	mov dx, 0
	mov bx, 2
	div bx
	mov word [bp + 18], ax
	popa
	ret
incret:
	add word [bp + 18], 1
	jmp iconti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
intro:
	pusha 
	mov ah, 0h
	mov al, 13h
	int 10h
	call open1
	mov ax, 0003h
	int 10h
	call open2
	mov ah, 0
	int 16h
	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
level1:
	pusha
	mov ax, 0xb800
	mov es, ax
	mov word [bgc], 0x7FDB
	call backgnd1
	call trees
	call printscore
	call boww
	call grass
	push struc1
	call tower
	mov di, [enes+20]
	call enemey
	mov di, [enes+28]
	call enemey
	mov di, [enes+36]
	call enemey
	mov word [totaenes], 3

	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
level2:
	pusha
	mov ax, 0xb800
	mov es, ax
	mov word [bgc], 0x3BDB
	call backgnd2
	call trees
	call boww
	call printscore
	call grass
	push struc2
	call tower
	mov di, [enes+16]
	call enemey
	mov di, [enes+20]
	call enemey
	mov di, [enes+24]
	call enemey
	mov word [totaenes], 3
	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
level3:
	pusha
	mov ax, 0xb800
	mov es, ax
	mov word [bgc], 0x19DB
	call backgnd3
	call trees
	call printscore
	call boww
	call grass
	push struc3
	call tower
	mov di, [enes+4]
	call enemey
	mov di, [enes+16]
	call enemey
	mov di, [enes+48]
	call enemey
	mov di, [enes+56]
	call enemey
	mov word [totaenes], 4
	
	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
level4:
	pusha
	mov ax, 0xb800
	mov es, ax
	mov word [bgc], 0x1020
	call backgnd4
	call printscore
	call trees
	call boww
	call grass
	push struc4
	call tower

	mov di, [enes+0]
	call enemey
	mov di, [enes+8]
	call enemey
	mov di, [enes+36]
	call enemey
	mov di, [enes+44]
	call enemey
	mov di, [enes+52]
	call enemey
	mov word [totaenes], 5

	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;
reset2:
	pusha
	mov ax, 0xb800
	mov es, ax
	mov word [left], 0
	mov word [down], 0
	mov word [currpos], 2730
	mov di, [currpos]
	call redbird
	push 10
	call livesleft
	mov word [live2], 10
	mov word [forcej], 0
	mov word [btouch], 0
	mov word [touchb], 0
	popa
	ret
;;;;;;;;;;;;;;;;;;;;
stringprinter:
	pusha
	mov bp, sp
	mov ax, 0xb800
	mov es, ax
	mov ah, 0x13
	mov al, 1
	mov bx, [bp + 24]
	mov dx, [bp + 22] ; position
	mov cx, [bp + 20] ;len
	push cs
	pop es
	mov bp, [bp + 18] ;string
	int 0x10
	popa
	ret 8
;;;;;;;;;;;;;;;;;;;;;
livesleft:
	pusha
	mov bp, sp

	push word 0
	push word 8
	push word 0x74
	push livess
	call printstr

	mov ax, 0xb800
	mov es, ax
	mov ax, [bp + 18]
	mov dx, 0
	mov bx, 10
	div bx
	mov dh, 0xF4
	add dl, 0x30
	mov word [es:1294], dx
	mov dx, 0
	div bx
	mov dh, 0xF4
	add dl, 0x30
	mov word [es:1292], dx

	popa
	ret 2
;;;;;;;;;;;;;;;;;;;;;
endingpage:
	pusha
	mov ah, 0h
	mov al, 13h
	int 10h
	call endp
	mov ax, 0003h
	int 10h
	popa
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
	call intro
	call runtime
	call level1
	call reset2
	call startgame

	push 0
	call eyes1
	pop ax
	cmp ax, 0
	je lvl2
	cmp word [forcej], 1
	je lvl2
	jmp termin
lvl2:
	call level2
	call reset2
	call startgame

	push 0
	call eyes1
	pop ax
	cmp ax, 0
	je lvl3
	cmp word [forcej], 1
	je lvl3
	jmp termin

lvl3:
	call level3
	call reset2
	call startgame

	push 0
	call eyes1
	pop ax
	cmp ax, 0
	je lvl4
	cmp word [forcej], 1
	je lvl4
	jmp termin
lvl4:
	call level4
	call reset2
	call startgame
termin:
	call endtime
	call endingpage

	mov ax, 0003h
	int 10h
	call opens
	call endpage
	mov ah, 0	
	int 16h
	cmp al, 'r'
	je start
mov ax, 0x4c00
int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

opens: pusha	;CX=FREQUENCY IN HERTZ. DESTROYS AX & DX

MOV DX, 00001H	;HIGH WORD OF 1193180
MOV AX, 00001H	;LOW WORD OF 1193180
DIV CX
MOV DX, AX
MOV AL, 0B6H
PUSHF
CLI	;!!!
OUT 043H, AL
MOV AL, DL
OUT 042H, AL
MOV AL, DH
OUT 042H, AL
POPF
IN AL, 061H
OR AL, 003H
OUT 061H, AL

popa 
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


btouch: dw 0
forcej: dw 0
bgc: dw 0
currpos: dw 2730
up: dw 0
down: dw 0
left: dw 0
height: dw 0
hori: dw 0
totaenes: dw 0

struc1: dw 0,0,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0,0,0,1,1,1, 0,0,1,1,1,1,1, 1,1,1,1,1,1,1
struc2: dw 0,0,0,0,0,0,0, 0,0,1,1,1,0,0, 0,0,1,1,1,1,0, 0,1,1,1,1,0,0, 1,1,1,1,1,1,1
struc3: dw 0,0,1,0,1,0,0, 0,1,1,1,1,1,0, 0,0,1,1,1,0,0, 0,0,1,1,1,0,0, 1,1,1,0,1,1,1
struc4: dw 0,0,0,0,0,0,0, 1,1,1,0,1,1,1, 1,1,1,1,1,1,1, 1,1,1,0,1,1,1, 1,1,1,0,1,1,1
touchb: dw 0

livess: dw "LIVES:   ",0
live2: dw 10

enes: dw 746, 1078, 766, 1098, 784, 1116, 1386, 1718, 1406, 1738, 1424, 1756, 2026, 2358, 2046, 2378, 2064, 2396, 2666, 2998, 2686, 3018 ,2704, 3036, 3306, 3638, 3326, 3658, 3344, 3676
proj: dw 0, 0, 0, 0, 0, 0, 1225, 1134, 1042, 0951, 0, 1617, 1523, 1433, 1345, 0, 2020, 1923, 1835, 1748
path: times 10 dw 0