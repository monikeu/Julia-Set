dane segment

	argumenty  		db 128 dup (?) ; miejsce na argumenty wpisane do programu
	offsetarg   	db 128 dup (?) ; offsety początku kolejnych argumentów
	ilearg 			db  0			
	dlargumentow	db 128 dup (?) ; dugości poszczególnych argumentów
	dlarg 			db 0 			; długość pojedynczego argumentu
	
	
	error0 db "Brak argumentow! $"
	error1 db "Niepoprawna liczba argumentow $"
	error2 db "Bledne dane wejsciowe, argumenty powinny być liczbami zmiennoprzecinkowymi $"
	
	x dw 0
	y dw 0
	
	xmin		dq	?;-1.5
	xmax		dq	?;1.5
	ymin		dq	 ?;-1.5
	ymax		dq	?;1.5
	cr			dq	?;-0.123
	ci			dq	?;0.745
	
	kolor db 0
	
	pisiont dq 50
	zero dq 0
	wynik dq 0
	
	rozdzx dq 320.0
	rozdzy dq 200.0
	
	cztery dq 4.0
	
	bylskok db "byl skok $"
	niebyloskoku db "nie bylo skoku $"
	
	ujemna db 0
	dziesiec dq 10.0
	jedna_dziesiata dq 0.1
	minus_jeden dq -1.0
	cyfra dw ? ; tymczasowe miejsce na cyfry 
	
	
dane ends

code segment
	.386
	.387
	start:
	mov ax, seg stos1
	mov ss, ax
	mov sp, offset wstosu
	
	finit
	
	call czy_sa_argumenty
	call czy6arg
	
	call na_rzeczywiste
	
	;jmp ominn
	
    mov	al,13h  ;tryb graficzny 320x200 pikseli 256 kolorow
	mov	ah,0
	int	10h
		
	push cx
	push dx
	mov cx, 200
	
	petlay:
	push cx
		mov ax, seg y
		mov ds, ax
		mov ds:[x],0
		mov cx, 320
		
			petlax:
				push cx
					call obliczenia
					
					;rysowanie
					mov ax, seg x
					mov ds, ax
					mov cx, ds:[x] 				; do cx współrzędna x
					
					mov ax, seg y
					mov ds, ax
					mov dx, ds:[y]   			; do dx współrzędna y
					
					mov ax, seg kolor
					mov ds, ax
					mov al, ds:[kolor]
					mov ah, 0ch
					int 10h
					
					mov ax, seg x
					mov ds, ax
					inc ds:[x]
				pop cx
			loop petlax
		mov ax, seg y
		mov ds, ax	
		inc ds:[y]
	pop cx
	loop petlay
	
	pop dx
	pop cx
	
	
	
	mov ax, 0
	int	16h  ;czekaj na dow. klawisz

	mov	al,3h  ;tryb tekstowy
	xor ah,ah
	int	10h

	;ominn:
	
	call koniec

;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	czy_sa_argumenty: ;sprawdza czy są jakiekolwiek argumenty
		push bx	
			mov bx,0     				
			mov bl, byte ptr es:[080h] 	;ile znakow w buforze wejsciowym
		
			cmp bx, 1h 					
			ja wykryto_arg 				; A>B
		pop bx	
			call nie_ma_argumentow		; jeśli nie ma arg
			koniec_wykryto_arg:
		pop bx
	ret
	
	nie_ma_argumentow: ;wypisuje stosowny komunikat
		push ds
		push dx
			mov ax, seg error0  		 
			mov ds,ax
			mov dx, offset error0
		
			call print
		pop dx
		pop ds
			call koniec
	ret
	
	koniec:  ; koniec programu
		mov ax,04c00h 				
		int 21h
	
	print:  ;wypisywanie
		mov ah, 9h 					
		int 21h 					
	ret
	
	wykryto_arg: ; uruchomiona tylko wtedy, gdy zostaną wykryte argumenty    
		push ds
		push si
		push di
		push cx
			mov	ax, seg argumenty 			
			mov	ds, ax       
			mov	si, 0 		;   si będzie iterować po kolejnych miejscach ciągu bajtów "argumenty"

			mov cx, 0 				
			
			mov cx, bx 		;w BX - ilość argumentów w buforze , w CX (licznik) - ilosc powtorzen    
			
			dec bx
			mov di, bx
			mov byte ptr es:[082h+di], 20h ; wstawiam spację zamiast entera na koniec bufora
			
			mov di, 0 
			mov bx, 0 		
			mov ax, 0		
			
			call wczytaj_z_przerwami
		pop cx
		pop di
		pop si
		pop ds
	jmp koniec_wykryto_arg
	
	
	wczytaj_z_przerwami: ; wczytuje argumenty do 'argumenty' w  postaci "arg$arg$arg$" itd. 					
		wczyt_znak:	
		
			mov al, byte ptr es:[082h+di] 					; di iteruje po buforze
		
			jmp czy_bialy 									;sprawdzam czy znak jest biały, jeśli jest powrót nastąpi do etykiety "idz_na_kon"
			koniec_czy_bialy:
			
			cmp bl, 0d										; w bl  - informacja o poprzednim znaku, (znak 'zerowy' ustawiony na biały)
			
			je offset_poczatku_i_zliczanie       		 ; skok jeśli poprzedni znak był biały, a obecny jest nie-biały (czyli początek argumentu)
			koniec_offset_poczatku_i_zliczanie:	
			
			mov byte ptr ds:[argumenty+si], al 				; si iteruje po 'argumenty'	
			inc di 						
			inc si						
			inc ds:[dlarg] 				
			
			mov bl, 1d
			idz_na_kon:					; koniec procedury 'zjedz_bialy'
		
		
		loop  wczyt_znak                      
	ret
	
	czy_bialy: ; sprawdza czy znak jest biały 
		cmp al, 20h		
		je zjedz_bialy  
			
		cmp al , 9d
		je zjedz_bialy
	jmp koniec_czy_bialy
	
	zjedz_bialy: ; zjada białe znaki
			inc di  							
					cmp bl,1d
					je wstaw_przerwe
					koniec_wstaw_przerwe:
					
					cmp bl, 1d
					je dl_arg    ; zapisuje długośc arguemntu
					koniec_dl_arg:
			mov bl, 0d
	jmp idz_na_kon
	
	wstaw_przerwe: ; wstawia między argumenty $
		mov al , '$'
		mov byte ptr ds:[argumenty+si], al
		inc si
	jmp koniec_wstaw_przerwe
	
	dl_arg: ;zapisuje długości kolejnych argumentów w "dlargumentow", zeruje "dlarg" 
		push bx
		push ax
		push di
			mov al, byte ptr ds:[ilearg]	;  do al ilość argumentów
			mov ah, 0 						
			mov di, ax						;  do di ilość argumentów
				
			mov al, byte ptr ds:[dlarg]					;do al długość argumentu
			mov byte ptr ds:[dlargumentow+di],al
		
			mov ds:[dlarg], 0
		
		pop di
		pop ax
		pop bx
	jmp koniec_dl_arg
	
	offset_poczatku_i_zliczanie: ; zapisuje offset początku argumentu w "offsetarg" i zlicza argumenty w "ilearg"
	push bx
	push ax
	push di
		inc ds:[ilearg]
		mov al, byte ptr ds:[ilearg]	;  do al ilość argumentów
		mov ah, 0 						
		mov bx, si						;  do bx offset początku argumentu
		mov di, ax						;  do di ilość argumentów
		mov byte ptr ds:[offsetarg+di], bl	
		
	pop di
	pop ax
	pop bx
	jmp koniec_offset_poczatku_i_zliczanie
	
	funkcja_ile_arg: ;zwraca ilość argumentów w al
	push ds
	    mov ax, seg ilearg
	    mov ds, ax
		mov al, ds:[ilearg]
	pop ds
	ret
	
	funkcja_offset_arg_o_nr: ;przyjmuje w ax numer argumentu, zwraca w al jego offset
		push di
		push ds
			mov di, ax
		    mov ax, seg offsetarg
		    mov ds, ax
			mov al, byte ptr ds:[offsetarg+di]
		pop ds
		pop di
	ret
	
	funkcja_dl_arg_o_nr: ;przyjmuje w ax numer argumentu, zwraca w al jego długość
		push di
		push ds
			mov di, ax
			mov ax, seg dlargumentow
			mov ds, ax
			mov al, byte ptr ds:[dlargumentow+di]
		pop ds
		pop di
	ret
	

;////////////////////////////////////////POPORAWNOŚĆ I PARSOWANIE ARGUMENTÓW/////////////////////////////////////////////////////////

czy6arg: ;czy jest 6 argumentów
	call funkcja_ile_arg
	cmp al, 6
	jne printerror1
ret

na_rzeczywiste:
	push ds
	push si
		mov ax, 1
			call zamien_na_rzeczyista
			mov ax,seg xmin
			mov ds,ax
			mov si, offset xmin
			fstp qword ptr ds:[si] ; ściąga ze stosi i wrzuca pod podany adres
		mov ax,2
			call zamien_na_rzeczyista
			mov ax,seg xmax
			mov ds,ax
			mov si, offset xmax
			fstp qword ptr ds:[si]
		mov ax, 3
			call zamien_na_rzeczyista
			mov ax,seg ymin
			mov ds,ax
			mov si, offset ymin
			fstp qword ptr ds:[si]
		mov ax, 4
			call zamien_na_rzeczyista
			mov ax,seg ymax
			mov ds,ax
			mov si, offset ymax
			fstp qword ptr ds:[si]
		mov ax, 5
			call zamien_na_rzeczyista
			mov ax,seg cr
			mov ds,ax
			mov si, offset cr
			fstp qword ptr ds:[si]
		mov ax,6
			call zamien_na_rzeczyista
			mov ax,seg ci
			mov ds,ax
			mov si, offset ci
			fstp qword ptr ds:[si]
		
	pop si
	pop ds
ret

zamien_na_rzeczyista: ;przyjmuje w ax numer argumentu  
	push si
	push bx
	push ax
		call funkcja_offset_arg_o_nr; zwroci offset w al
		mov ah, 0 
		mov si, ax ; offset argumentu do si
	pop ax
		call funkcja_dl_arg_o_nr
		mov ah,0
		mov bx, ax ;długość argumentu do bx
		
		call parsuj_argument
	pop bx
	pop si
ret

parsuj_argument: ; przyjmuje w bx długość argumentu, w si offset argumentu w obrębie "argumenty"
	call czy_minus
	
	push si
		call przed_kropka
		fstp st(1)	; zdejmuje 10.0 ze stosu
	pop si
	
	call po_kropce

	fstp st(1)				;zdejmuje 0.1 ze stosu
	faddp st(1), st(0)			;dodajemy czesc calkowitą do ulamkowej, usuwamy to co jest na szczycie stosu
	
	call czy_ujemna			;jesli  był "-" mnoży przez -1.0
	
ret

czy_minus: ;sprawdza czy pierwszy znak liczby to "-", jeśli nie jest "-" to czy jest to cyfra 0-9
	push ds
		mov ax, seg argumenty
		mov ds, ax
		
		cmp ds:[argumenty+si], "-"
		jne nieujemna
			;liczba jest ujemna
			mov ax,seg ujemna
			mov ds, ax
			mov ds:[ujemna], 1
			inc si ;na następny znak
			dec bx	;zmniejszam dl argumentu o 1 (bo minus)
			jmp koniec_czy_minus
			
		;licznba jest nieujemna
		nieujemna:
		cmp ds:[argumenty+si], '0' ; jesli nie "- ", to czy jest to 0-9?  
		jb bledne_dane_wejsciowe
		
		cmp ds:[argumenty+si], '9'
		jg bledne_dane_wejsciowe
		
		mov ax,seg ujemna
		mov ds, ax
		mov ds:[ujemna], 0 
		
		koniec_czy_minus:
	pop ds
ret

czy_ujemna: ;mnozy przez -1
	push ds
	push di
		mov ax, seg ujemna
		mov ds, ax
		cmp byte ptr ds:[ujemna], 1
			
		jne nie_jest_ujemna
			mov ax, seg minus_jeden
			mov ds, ax
			mov di, offset minus_jeden
			fld qword ptr ds:[di]
			fmulp st(1),st(0) ; st(1)=liczba, st=-1.0, mnoży przez siebie, wynik zapisuje w st(1), usuwa st  (-1.0)
		nie_jest_ujemna:
	pop di
	pop ds
ret

przed_kropka: ; przyjmuje w si offset argumentu, zczytuje cyfry sprzed kropki
	push ds
	push cx
	push di
		mov ax,seg dziesiec
		mov ds,ax
		mov di, offset dziesiec
		fld qword ptr ds:[di] ;10.0 na stos
		fldz ; zero na stos

		mov ax, seg argumenty
		mov ds, ax
		
	zamien_cyfre: ;zamienia kolejne cyfry

		mov ch, 0
		mov cl,byte ptr ds:[argumenty+si]
		
		cmp cl,"." ; jeśli kropka to zakończ czytanie
		jz zakoncz_parsowanie
		
		cmp cl, '0'
		jb bledne_dane_wejsciowe
		
		cmp cl, '9'
		jg bledne_dane_wejsciowe
	
		fmul st, st(1) ; liczba poprzednia * 10.0, 
	
		sub cx, "0" ; zamiana znaku na wartość
		mov ax, seg cyfra 
		mov ds, ax
		mov word ptr ds:[cyfra],cx

		
		mov di, offset cyfra
		fild word ptr ds:[di] ; wrzuca na stos liczbę spod adresu przekonwertowaną na format zmiennoprzecinkowy
		faddp st(1), st ; dodajemy poprzednią część całkowitą i usuwamy wartosc z wierzcholka stosu
	
		inc si							
	
	jmp zamien_cyfre
	zakoncz_parsowanie:
	
	pop di
	pop cx
	pop ds	
ret

po_kropce:  ; przyjmuje w si offset argumentu, w bx jego długość (bez minusa jeśli takowy wystapił)
	push ds
	push cx
	push di
		
		mov ax,seg jedna_dziesiata
		mov ds,ax
		mov di, offset jedna_dziesiata
		fld qword ptr ds:[di] ; 0,1 na stos
		fldz 	;0 na stos
		
		dec bx ;żeby zacząć czytanie od końca liczby
		add si,bx
		
		mov ax, seg argumenty
		mov ds, ax
		
		zamien_cyfre2:

			mov ch, 0
			mov cl,byte ptr ds:[argumenty+si]
			
			cmp cl,"."
			jz zakoncz_parsowanie2
			
			cmp cl, '0'
			jb bledne_dane_wejsciowe
		
			cmp cl, '9'
			jg bledne_dane_wejsciowe
		
			sub cx, "0"
			mov word ptr ds:[cyfra],cx
			mov di, offset cyfra
			
			
			fild word ptr ds:[di]		
			faddp st(1), st ; suma poprzedniej części ułamkowej i obecnej, usuwany wierzchołek stosu
			
			fmul st, st(1) ;liczba poprzednia * 0.1
		
			dec si
		
		jmp zamien_cyfre2
		zakoncz_parsowanie2:
	pop di
	pop cx
	pop ds
ret
	
	
obliczenia:
	call oblicz_zi ; I zi I
	
	call oblicz_zr ; I zr I zi I

	call reszta_obliczen	
ret	
	
oblicz_zi:
push es
push si
	mov ax, seg ymax
	mov es, ax 
	mov si, offset ymax 
	fld qword ptr es:[si] ; I ymay I
	
	mov ax, seg ymin
	mov es, ax 
	mov si, offset ymin 
	fld qword ptr es:[si] ; I ymin I ymay I
	
	fsubp st(1), st(0)	; I ymay - ymin I
	
	mov ax, seg y
	mov es, ax 
	mov si, offset y
	fild word ptr es:[si]		; I y I  ymay - ymin I
	
	fmulp st(1), st(0)	; I y * ( ymay - ymin) I
	
	mov ax, seg rozdzy
	mov es, ax 
	mov si, offset rozdzy
	fld qword ptr es:[si] ; I rozdzy I y * ( ymay - ymin) I
	
	fdivp st(1), st(0) ; I y * ( ymay - ymin)/rozdzy  I
	
	mov ax, seg ymin
	mov es, ax 
	mov si, offset ymin 
	fld qword ptr es:[si] ;I ymin I y * ( ymay - ymin)/rozdzy  I
	
	faddp st(1), st(0) ; I ymin I y * ( ymay - ymin)/rozdzy  I
pop si
pop es
ret

oblicz_zr:
push es
push si
	mov ax, seg xmax
	mov es, ax 
	mov si, offset xmax 
	fld qword ptr es:[si] ; I xmax I
	
	mov ax, seg xmin
	mov es, ax 
	mov si, offset xmin 
	fld qword ptr es:[si] ; I xmin I xmax I
	
	fsubp st(1), st(0)	; I xmax - xmin I
	
	mov ax, seg x
	mov es, ax 
	mov si, offset x
	fild word ptr es:[si]		; I x I  xmax - xmin I
	
	fmulp st(1), st(0)	; I x * ( xmax - xmin) I
	
	mov ax, seg rozdzx
	mov es, ax 
	mov si, offset rozdzx
	fld qword ptr es:[si] ; I rozdzx I x * ( xmax - xmin) I
	
	fdivp st(1), st(0) ; I x * ( xmax - xmin)/rozdzx  I
	
	mov ax, seg xmin
	mov es, ax 
	mov si, offset xmin 
	fld qword ptr es:[si] ;I xmin I x * ( xmax - xmin)/rozdzx  I
	
	faddp st(1), st(0) ; I xmin I y * ( xmax - xmin)/rozdzy  I
pop si
pop es
ret	

reszta_obliczen: ; I zr I zi I  =  I x I y I
push cx
push es
	mov cx, 1000
	
	petla1000: ;I x I y I
	
	fld st(0) ; I x I x I y I
	fld st(0)  ; I x I x I x I y I
	fmulp st(1), st(0) ; I x*x I x I y I
	
	
	fld st(2) ; I y I x*x I x I y I
	fld st(0) 	; I y I y I x*x I x I y I
	fmulp st(1), st(0) ; I y*y I x*x I x I y I
	
	
	fsubp st(1), st(0) ;  I x*x - y*y I x I y I
		
	mov ax, seg cr
	mov es, ax
	mov si, offset cr
	fld qword ptr es:[si] ; I cr I x*x - y*y I x I y I
	
	faddp st(1), st(0) ; I cr + x*x - y*y I x I y I
	
	fxch st(2) ; I x I y I cr + x*x - y*y I
	
	fld st(0) ; I x I x I y I cr + x*x - y*y I
	
	faddp st(1), st(0) ; I 2x I y I cr + x*x - y*y I
	fmulp st(1), st(0) ; I 2xy I cr + x*x - y*y I
	
	
	mov ax, seg ci 
	mov es, ax
	mov si, offset ci
	fld qword ptr es:[si] ; I ci I 2xy I cr + x*x - y*y I
	
	faddp st(1), st(0) ;I ci + 2xy I cr + x*x - y*y I

						; I y I x I
	fld st(0)			; I y I y I x I
	fld st(0)			; I y I y I y I x I
	
	fmulp st(1), st(0)  ; I y*y I y I x I
	
	fld st(2) ; I x I y*y I y I x I
	fld st(0) ;  I x I x I y*y I y I x I
	
	fmulp st(1), st(0) ; I x*x I y*y I y I x I
	
	faddp st(1), st(0) ; I x*x + y*y I y I x I
	
	mov ax, seg cztery
	mov es, ax
	mov si, offset cztery
	fld qword ptr es:[si] ;  I 4.0 I x*x + y*y I y I x I
	
	fcomp st(1) ; 4.0 ? x*x + y*y ; I x*x + y*y I y I x I
	
	fstsw ax ; skopiuj słowo stanu koprocesora do ax / interesujący bit - C0. if C0 == 1 break
	
	sahf	; skopiuj ah do młodszej części rejestru flag, interesujący bit teraz w CF
	jb break ; if CF == 1
	
	fstp st	 ;I y I x I
	fxch st(1) ; I x I y I

	loop petla1000	

	;wykonało sie 1000 pętli
	
	mov ax, seg kolor
	mov es, ax
	mov si, offset kolor 
	mov byte ptr es:[si] , 15
	
	
	fstp st ;I y I x I
	fstp st ;I x I
	fstp st ;stos pusty
	
	jmp koniec_obliczen
	
	;wykonało się mniej niż 1000 pętli
	break:
	
	mov ax, seg kolor
	mov es, ax
	mov si, offset kolor 
	;sub cl, 50h
	mov byte ptr es:[si] , 1
	
	fstp st ; I x I
	fstp st ; stos pusty
	
	koniec_obliczen:
	
	
pop es	
pop cx
ret


printerror1:
		mov ax, seg error1
		mov ds, ax
		mov dx, offset error1
		call print
	call koniec
	
bledne_dane_wejsciowe:
	mov ax, seg error2
		mov ds, ax
		mov dx, offset error2
		call print
	call koniec


	
code ends

stos1 segment stack
	dw 1000 dup (?)
	wstosu dw ?
stos1 ends

end start