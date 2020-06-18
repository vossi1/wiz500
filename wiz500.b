; Disassembled by Vossi 05/2020
; Prepared for ACME reassembling
; Comments by Vossi 05/2020
; Converted for P500 by Vossi 05/2020
!cpu 6502
; switches
;P500 = 1       ; P500 bank 0 file
;CRT = 1        ; CRT header for VICE
!ifdef  P500{!to "wiz500.prg", cbm
} else  { !ifdef CRT {!to "wiz500.crt", plain
	} else{ !to "wiz500.rom", plain }}
!cpu 6502
; ########################################### TODO ################################################
;
;
; ########################################### BUGS ################################################
;
; ######################################### P500 MODS #############################################
; Indirect reg standard = $15, switch only to $0 for game indirect pointer instructions 
; ******************************************* INFO ************************************************
; ******************************************* FONT ************************************************
; ***************************************** CONSTANTS *********************************************
FILL		= $aa	; fills free memory areas with $aa
NOPCODE		= $ea	; nop instruction for fill
GAMEBANK	= 0	; Game code bank
SYSTEMBANK	= $f	; systembank

LIVES		= 3	; start lives
; table codes
ADR	= $fe	; new target address follows
LIN	= $fd	; new line (target +40)
END	= $ff	; end of data
; color codes
BLACK		= $00
WHITE		= $01
RED		= $02
CYAN		= $03
MAGENTA		= $04
GREEN		= $05
BLUE		= $06
YELLOW		= $07
ORANGE		= $08
BROWN		= $09
LIGHTRED	= $0a
GRAY1		= $0b
GRAY2		= $0c
LIGHTGREEN	= $0d
LIGHTBLUE	= $0e
GRAY3		= $0f
MCM		= $08	; bit#3 for multicolor character
; game
; ************************************** P500 REGISTER ********************************************
; vic
MOBX		= $00
MOBY		= $01
MOBMSB		= $10
MODEY		= $11
RASTER		= $12
MOBENA		= $15
MCMCSX		= $16
MOBYEX		= $17
MEMPT		= $18
IRQ		= $19
EIRQ		= $1a
MOBPRI		= $1b
MOBMC		= $1c
MOBXEX		= $1d
MOBMOB		= $1e
MOBBGR		= $1f
EXTCOL		= $20
BGRCOL		= $21
BGRCO1		= $22
BGRCO2		= $23
BGRCO3		= $24
MOBMC0		= $25
MOBMC1		= $26
MOBCOL		= $27
; sid
V1LO		= $00
V1HI		= $01
V1CTRL		= $04
V1AD		= $05
V1SR		= $06
V2LO		= $07
V2HI		= $08
V2CTRL		= $0b
V2AD		= $0c
V2SR		= $0d
V3LO		= $0e
V3HI		= $0f
V3CTRL		= $12
V3AD		= $13
V3SR		= $14
FCLO		= $15
FCHI		= $16
RESFIL		= $17
MODVOL		= $18
RANDOM		= $1b
ENV3		= $1c
; cia
PRA		= $0	; Data reg A
PRB		= $1	; Data reg B
DDRA		= $2	; Direction reg a
DDRB		= $3	; Direction reg b
TALO		= $4	; Timer A low  byte
TAHI		= $5	; Timer A high byte
TBLO		= $6	; Timer B low  byte
TBHI		= $7	; Timer B high byte
TOD10		= $8	; 10ths of seconds
TODSEC		= $9	; Seconds
TODMIN		= $A	; Minutes
TODHR		= $B	; Hours
SDR		= $C	; Serial data register
ICR		= $D	; Interrupt control register
CRA		= $E	; Control register A
CRB		= $F	; Control register B
; ************************************** P500 ADDRESSES *******************************************
!addr CodeBank          = $00		; code bank register
!addr IndirectBank      = $01		; indirect bank register
!addr ScreenRAMbase	= $0400		; screen matrix
!addr SpritePointer	= $07f8		; sprite data pointer
!addr CharROMbase       = $c000		; Character ROM
!addr ColorRAMbase      = $d400		; Color RAM
!addr VICbase           = $d800		; VIC
!addr SIDbase           = $da00		; SID
!addr CIAbase           = $dc00		; CIA
!addr TPI1base          = $de00		; TPI1
!addr TPI2base          = $df00		; TPI2
!addr HW_IRQ            = $fffe		; System IRQ Vector
!addr HW_NMI            = $fffa		; System NMI Vector
SH = >ScreenRAMbase			; Highbyte Screen RAM base
; *************************************** C64 ADDRESSES *******************************************
!addr CPUPort64         = $01		; 6510 CPU port
!addr VIC64		= $d000		; VIC
!addr SID64		= $d400		; SID
!addr ColorRAM64        = $d800		; Color RAM
!addr CIA64		= $dc00		; CIA
; ************************************** USER ADDRESSES *******************************************

; ***************************************** ZERO PAGE *********************************************
!addr score		= $02		; 3bytes score
!addr highscore		= $05		; 3bytes highscore
!addr key		= $08		; pressed key/joystick
!addr ptr1		= $09		; 16bit pointer
!addr ptr2		= $0b		; 16bit pointer
!addr temp1		= $0d
!addr temp2		= $0e
!addr temp3		= $0f
!addr data_ctr		= $12		; data counter
!addr timer		= $13		; game timer - inc with every irq / CIA timer b
!addr color		= $14
!addr monster_dir	= $1f		; 5 bytes start screen monster x pos
!addr players		= $28		; lives
!addr delay		= $2b		; delay for monster movment on satrt screen
!addr fire		= $2c		; fire pressed bit#7=1
!addr worrior_dir	= $32		; worrior direction 1-4
!addr draw_ptr		= $39		; pointer to print maze on screen
!addr draw_column	= $36		; draw column
!addr draw_line		= $37		; draw line
!addr draw_char		= $38		; draw char/tile
!addr level		= $3b		; level 1-4
!addr maze_column	= $3c		; actual maze column
!addr maze_line		= $3d		; actual maze line
!addr mazedata_ptr	= $44		; pointer to mazedata
!addr move		= $4c		; movement 0=none, 1=up, 2=down, 3=left, 4=right
!addr sprite_data	= $4d		; 8 bytes sprite data
!addr sound_ptr		= $70		; pointer to sound data
; ***************************************** VARIABLES *********************************************
; ************************************** P500 ZERO PAGE *******************************************
!addr ColorRAM0         = $e6
!addr ColorRAM1         = $e8
!addr ColorRAM2         = $ea
!addr ColorRAM3         = $ec
!addr VIC               = $ee
!addr VIC01             = $f0
!addr VIC27             = $f2
!addr SID               = $f4
!addr CIA               = $f6
!addr TPI1              = $f8
!addr TPI2              = $fa
!addr CharROM0          = $fc
!addr CharROM1          = $fe
; ****************************************** MACROS ***********************************************
; **************************************** CRT HEADER *********************************************
!zone crt
!ifdef  CRT{
*= $dfb0
!byte $43, $36, $34, $20, $43, $41, $52, $54
!byte $52, $49, $44, $47, $45, $20, $20, $20
!byte $00, $00, $00, $40, $01, $00, $00, $00
!byte $01, $00, $00, $00, $00, $00, $00, $00
!byte $00, $00, $00, $00, $00, $00, $00, $00
!byte $00, $00, $00, $00, $00, $00, $00, $00
!byte $00, $00, $00, $00, $00, $00, $00, $00
!byte $00, $00, $00, $00, $00, $00, $00, $00
!byte $43, $48, $49, $50, $00, $00, $20, $10
!byte $00, $00, $00, $00, $e0, $00, $20, $00
}
; ***************************************** ZONE CODE *********************************************
!zone code
!initmem FILL
*= $e000
start:	sei				; disable interrupts
	cld
	ldx #$ff			; init stack
	txs
	ldx #$2e			; init vic regs
viclp:lda VICRegs,x
	sta VIC64+MOBX,x
	dex
	bpl viclp
	ldx #$18			; init sid regs
sidlp:lda SIDRegs,x
	sta SID64,x
	dex
	bpl sidlp
	ldx #$00			; clear RAM
	txa
clramlp:sta $02,x
	sta $0200,x
	sta $0300,x
	inx
	bne clramlp
	lda #$1f
	sta CIA64+ICR			; clear all irq
	lda #$82
	sta CIA64+ICR			; set irq timer b
	lda #$01
	sta CIA64+CRB			; timer b phi2, cont, start
	lda #$38
	sta CIA64+TBLO			; timer b = 56
	lda #$00
	sta CIA64+TBHI
	cli				; enable irq
StartNew:
	jsr StartScreen			; shows start screen and waits for F1
	jsr InitGame			; reset score, init lives and sprite colors
NextLevel:
	jsr SetupGame			; increase + init new level
TryAgain:
	jsr SetupGameScreen
	jsr SetupWorrior
	lda #$1f
	sta SID64+MODVOL		; full volume, filter low pass
	ldx #1				; start with sound 1
	lda $29
	bpl +
	ldx #2				; if $29 bit#7 set, start with sound 2
+	txa
	jsr PlaySound
GameLoop:
	lda timer
	bne GameLoop
	jsr CopySpritePointer
	lda VIC64+MOBMOB
	sta $55
	lda VIC64+MOBBGR
	sta $56
	inc $2a
	lda $2a
	bne +
	inc delay
	lda #$05
	cmp delay
	bne +
	lda #2
	jsr PlaySound			; Play sound 2
	dec $29
+	lda delay
	cmp #$10
	bcc +
	jsr le855
+	jsr le754
	jsr MoveWorrior
	jsr le855
	jsr le944
	jsr lebfe
	jsr lecab
	jsr leaef
	jsr lea17
	jsr GameCycle
	lda $1e
	cmp #$ff
	beq decwor
	lda $26
	bne GameLoop

	lda $1e
	bmi decwor
	jmp LevelFinished

decwor:	dec players			; decrease lives
	bne TryAgain

	jmp GameOver			; game over
; -------------------------------------------------------------------------------------------------
; $e0c5 Checks F1 key for game start
;   returns .y = 1 if F1 pressed
CheckF1Key:
	ldx #$ff
	stx CIA64+DDRA			; port a output
	inx
	stx CIA64+DDRB			; port b input
	ldy #$00			; clear .y
	ldx #$fe
	jsr chkkey
	cpx #$ef
	bne chkf1x
	iny				; returns 1 if F1 pressed
chkf1x:	rts
; -------------------------------------------------------------------------------------------------
; $e0db check joystick and keyboard movement/fire
CheckJoyKey:
	ldx #$ff
	stx CIA64+DDRA			; port a output
	inx
	stx CIA64+DDRB			; port b input
	lda #$1f			; init key value
	ldx #$df
	jsr chkkey
	cpx #$fb			; check 'L' = left
	bne chkup
	and #$fb			; clear bit #2
	bne chkx
chkup:  cpx #$fd			; check 'P'
	bne chkdown
	and #$fe			; clear bit #0
	bne chkx
chkdown:cpx #$ef			; check '.'
	bne chkfire
	and #$fd			; clear bit #1
	bne chkx
chkfire:ldx #$fd
	jsr chkkey
	cpx #$fb			; check 'A'
	bne chkrght
	and #$ef			; clear bit #4
	bne chkx
chkrght:ldx #$bf
	jsr chkkey
	cpx #$fb			; check ';'
	bne chkx
	and #$f7			; clear bit #3
chkx:	sta key				; store key
	jmp chkjoy
; $e120 check cia
chkkey:	stx CIA64+PRA
debounc:ldx CIA64+PRB
	cpx CIA64+PRB
	bne debounc
	rts
; $e12c check joystick
chkjoy:	ldx #$00
	stx fire
	stx CIA64+DDRA			; all CIA ports input
	stx CIA64+DDRB
debjoy:	lda CIA64+PRB			; load joystick port 2 (bit #0-4)
	cmp CIA64+PRB
	bne debjoy			; debounce joystick
	and key				; and pressed key (bit# = 0)
; store joystick/keyboard movement/fire
	tay
	and #$10			; check fire
	bne jkdown
	lda #$80
	sta fire			; store fire
	bne jkx
jkdown:	tya
	and #$02			; check down
	bne jkup
	ldx #2
	bne jkx
jkup:	tya
	and #$01			; check up
	bne jkleft
	ldx #1
	bne jkx
jkleft:	tya
	and #$04			; check left
	bne jkright
	ldx #3
	bne jkx
jkright:tya
	and #$08			; check right
	bne jkx
	ldx #4
jkx:	stx $46				; store move direction
	rts
; -------------------------------------------------------------------------------------------------
; $e170 Game Cycle
GameUpdate:
	lda timer
	bne GameUpdate
	tya
	pha
	txa
	pha
	jsr GameCycle
	pla
	tax
	pla
	tay
waitcyc:lda timer
	bne waitcyc
	dex
	bne GameUpdate
	rts
; -------------------------------------------------------------------------------------------------
; $e187 Clears screen with color
ClearScreen:  
	ldx #$00
clscrlp:lda color			; color
	sta ColorRAM64,x
	sta ColorRAM64+$100,x
	sta ColorRAM64+$200,x
	sta ColorRAM64+$2e8,x
	lda #$00			; space
	sta ScreenRAMbase,x
	sta ScreenRAMbase+$100,x
	sta ScreenRAMbase+$200,x
	sta ScreenRAMbase+$2e8,x
	inx
	bne clscrlp
	rts
; -------------------------------------------------------------------------------------------------
; $e1a9 copies all sprites data pointers to the vic pointers
CopySpritePointer:
	ldx #8
cpsprlp:lda sprite_data-1,x
	sta SpritePointer-1,x
	dex
	bne cpsprlp
	ldx #8
chksplp:lda sprite_data-1,x
	cmp SpritePointer-1,x
	bne CopySpritePointer
	dex
	bne chksplp
	rts
; -------------------------------------------------------------------------------------------------
; $e1c0 Copies data from xy to address in first two bytes till $ff
;   $fe = new target address
ScreenCopy:				; copies from .x, .y
	stx ptr1
	sty ptr1+1

scrnewt: ldy #0				; set pointer1 to new target
	sty temp2
	lda (ptr1),y
	sta ptr2
	iny
	lda (ptr1),y
	sta ptr2+1

scrcplp:iny
	lda (ptr1),y			; load data byte
	cmp #$ff
	beq scrcpyx			; $ff = end
	cmp #$fe
	beq scrtarg			; $fe = new target address
	cmp #$fd
	beq scrline
	sty temp1
	ldy temp2
	sta (ptr2),y
	inc temp2
	ldy temp1
	lda move
	beq scrcplp			; next if no movement
	ldx #10
	jsr GameUpdate			; 10 game updates
	jmp scrcplp
; $e1f6	byte $fe = new target address
scrtarg:iny
	clc
	tya
	adc ptr1			; add counter .y to pointer 1
	sta ptr1
	lda ptr1+1
	adc #$00
	sta ptr1+1
	jmp scrnewt			; copy to new target address
; $e206	byte $fd = new line (target +40)
scrline:clc
	lda ptr2
	adc #40				; pointer2 +40 = new line
	sta ptr2
	lda ptr2+1
	adc #$00
	sta ptr2+1
	lda #$00
	sta temp2
	beq scrcplp			; always next
scrcpyx:lda #0
	sta move			; store no movement
	rts
; -------------------------------------------------------------------------------------------------
; $e21e Draw game screen
SetupGameScreen:
	lda #CYAN
	sta color
	jsr ClearScreen			; clear game screen
	ldx #<GameScreenData
	ldy #>GameScreenData
	jsr ScreenCopy			; copy game screen
	lda level
	and #$03			; maximum maze 3
	asl				; x2 for mazedata address
	tax
	lda MazePointer,x		; init pointer to mazedata level 1-4
	sta mazedata_ptr
	lda MazePointer+1,x
	sta mazedata_ptr+1
; decode and print maze
	lda #0		
	sta data_ctr			; reset data counter
	sta maze_line			; reset screen line
	sta maze_column			; reset screem column
mazelp:	ldy data_ctr
	lda (mazedata_ptr),y		; load data byte
	cmp #$19
	bne mzchkv
	jsr mzhtile			; draw horizontal line
mzchkv:	cmp #$18
	bne mzchkhv
	jsr mzvtile			; draw vertical line
mzchkhv:cmp #$1a
	bne mzblank
	jsr mzhvtil			; draw h+v line
mzblank:inc data_ctr			; increase data pointer
	lda #3
	clc
	adc maze_column			; add actual column + 3
	sta maze_column			; ...and store to draw column
	cmp #39
	bne mazelp			; next field if not end of line

	lda #0
	sta maze_column			; reset column
	lda #3
	clc
	adc maze_line			; add 3 lines
	sta maze_line
	cmp #21
	bne mazelp			; repeat if < line 21

	lda #$00
	jsr AddScore			; print zero score
	ldx players
	stx draw_char			; store player char
	lda #30
	sta draw_column			; store column 30, line 24
	lda #24
	sta draw_line
	jsr DrawMazeTile		; draw player count
	ldx #$d0			; set screen pointer to highscore
	ldy #SH+3
	stx ptr1
	sty ptr1+1
	ldx #$03
	jsr PrintScore			; print highscore (score+3)
	rts
; -------------------------------------------------------------------------------------------------
; $e29b Table with maze addresses
MazePointer:
	!word Maze1
	!word Maze2
	!word Maze3
	!word Maze4
; -------------------------------------------------------------------------------------------------
; $e2a3 Draw 3x horizontal line
mzhtile:ldx #0
mzhlp:	lda maze_line
	clc
	adc #2				; add 2 lines
	sta draw_line			; ...and store to draw line
	txa
	clc
	adc maze_column				; load actual column
	sta draw_column			; ...and add to draw column
	lda #$2b
	sta draw_char
	jsr DrawMazeTile		; draw tile $2b
	inx
	cpx #3
	bne mzhlp			; print 3 column
	lda #$05
	rts
; -------------------------------------------------------------------------------------------------
; $e2c1 Draw 3x vertical line
mzvtile:ldx #0
mzvlp:	lda maze_column			; load actual column
	sta draw_column			; ...and store to draw column
	txa
	clc
	adc maze_line			; load actual line+carry
	sta draw_line			; ...and store to draw line
	lda #$2a
	sta draw_char
	jsr DrawMazeTile		; draw tile $2a
	inx
	cpx #3
	bne mzvlp			; print 3 lines
	lda #$05
	rts
; -------------------------------------------------------------------------------------------------
; $e2dc Draw left-low corner
mzhvtil:jsr mzhtile			; draw 3x horizontal line
	jsr mzvtile			; draw 3x vertical line
	lda #2
	clc
	adc maze_line			; add actual line + 2
	sta draw_line			; ...and store to draw line
	lda maze_column
	sta draw_column			; store actual column
	lda #$2c
	sta draw_char
	jsr DrawMazeTile		; draw low-left corner 'L'-tile
	rts
; -------------------------------------------------------------------------------------------------
; $e2f5 inits some vars and sprite colors
InitGame:
	ldy #0
	sty score
	sty score+1
	sty score+2
	sty level			; reset level
	sty $76
	lda #LIVES			; 3 lives
	sta players

	lda #BLUE			; all sprites blue
	ldx #7
incollp:sta VIC64+MOBCOL,x
	dex
	bpl incollp

	rts
; -------------------------------------------------------------------------------------------------
; $e310
SetupGame:
	ldx #$00
	stx VIC64+MOBENA		; disable sprites

sg1elp:	lda #$ff
	sta $1e,x			; set $1e- 8 vars to $ff 
	inx
	cpx #$08
	bne sg1elp

	ldy #$00			; clear some vars
	sty delay
	sty $2a
	sty $29

	inc level			; raise level (1-4)
	ldx level
	cpx #4				; maximum 4
	bcc levmax4
	ldx #4
levmax4:lda MonsterSpeedTable-1,x	; setup monster speeds?
	sta $27
	lda MonsterSpeedTable-1+4,x
	sta $47
	lda MonsterSpeedTable-1+8,x
	sta $48
	lda MonsterSpeedTable-1+12,x
	sta $49
	lda MonsterSpeedTable-1+16,x
	sta $4a
	lda MonsterSpeedTable-1+20,x
	sta $26
	lda #$00
	sta $4b
	rts
; -------------------------------------------------------------------------------------------------
; speeds 1-4
MonsterSpeedTable:
	!byte $04, $05, $06, $06
	!byte $03, $04, $05, $06
	!byte $02, $04, $06, $06
	!byte $01, $02, $03, $04
	!byte $00, $01, $00, $02
	!byte $0a, $10, $14, $18
; -------------------------------------------------------------------------------------------------
; $e36b setup worrior sprite
SetupWorrior:
	lda #$e6			; init worrior sprite
	sta sprite_data
	sta SpritePointer
	lda #$03
	sta $2e
	lda #$01			; set worrior start position
	sta VIC64+MOBMSB
	lda #$37
	sta VIC64+MOBX
	lda #$ab
	sta VIC64+MOBY
	lda #$fd
	sta $54
	ldx #$ff
	stx $25
	stx $24
	inx
	stx $1e
	stx $2d
	lda VIC64+MOBENA
	ora #$01			; enable worrior sprite
	and #$7f			; disable sprite #7
	sta VIC64+MOBENA
	rts
; -------------------------------------------------------------------------------------------------
; $e39f
TableX:  
	!byte $35, $4d, $65, $7d, $95, $ad, $c5, $f5
; $e3a7
TableY:
	!byte $7b, $63, $7b, $93, $ab, $c3, $7b, $ab
; -------------------------------------------------------------------------------------------------
; $e3af Level finished
LevelFinished:
	lda #$00
	sta VIC64+MOBENA		; disable sprites
	ldx #<TextBonus3000
	ldy #>TextBonus3000
	jsr ScreenCopy			; print 'BONUS 3000'
	lda #7
	jsr PlaySound			; play bonus sound
	lda #$30
	jsr AddScore
	ldx #$00
	jsr GameUpdate
	jmp NextLevel

TextBonus3000:
!byte $24, $05, $0c, $19, $18, $1e, $1c, $00	; 'BONUS 3000'
!byte $04, $01, $01, $01, $ff

GameOver:
	ldx #2
chkhisc:lda highscore,x
	cmp score,x			; check if new high score
	bcc newhisc
	bne nohisc
	dex
	bpl chkhisc

newhisc:lda score			; store new highscore
	sta highscore
	lda score+1
	sta highscore+1
	lda score+2
	sta highscore+2
	ldx #$d0			; set screen ptr to highscore
	ldy #SH+3
	stx ptr1
	sty ptr1+1
	ldx #3
	jsr PrintScore

nohisc:	lda #$00
	sta SID64+MODVOL		; sound off
	inc move
	ldx #<TextGameOver
	ldy #>TextGameOver
	jsr ScreenCopy			; print 'G A M E  O V E R'
	ldx #$50
	jsr GameUpdate
	jmp StartNew			; start new

TextGameOver:
	!byte $24,SH+1, $11, $00, $0b, $00, $17, $00
	!byte $0f, $00, $00, $19, $00, $1f, $00, $0f
	!byte $00, $1b, END
; add and print score
AddScore:
	clc
	sed
	adc score+1			; add score
	sta score+1
	lda score+2
	adc #0
	sta score+2
	cld
	ldx #$c4			; set screen pointer to score
	ldy #SH+3
	stx ptr1
	sty ptr1+1
	ldx #0				; print score

PrintScore:
	ldy #5
pslp:	lda score,x
	and #$0f			; clear hinibble
	clc
	adc #1
	sta (ptr1),y
	dey
	lda score,x
	lsr
	lsr
	lsr
	lsr
	clc
	adc #1
	sta (ptr1),y
	inx
	dey
	bpl pslp
; check bonus player
	lda score+2
	cmp #2				; check score
	bcc psx				; not enough
	lda $76
	bne psx				; already bonus player
	inc $76
	inc players
	ldx players
	stx ScreenRAMbase+$03de		; print bonus player
	lda #7
	jsr PlaySound			; play bonus sound
psx:	rts
; -------------------------------------------------------------------------------------------------
; $e474
StartScreen:
	lda #$00
	sta VIC64+MOBENA		; disable all sprites
	lda #BLUE
	sta color
	jsr ClearScreen			; clear screen with blue chars
	lda #WHITE
	sta VIC64+BGRCOL		; set bgr+ext white
	sta VIC64+EXTCOL
	ldy #>StartScreenData
	ldx #<StartScreenData
	jsr ScreenCopy			; Copies start screen

	ldx #$04
ssinspr:txa
	asl
	tay
	lda #$d7			; sprite start h pos
	sta monster_dir,x		; set right direction = $d7
	sta VIC64+MOBX+2,y		; setup monsters sprites 1-5
	lda StartScreenMonsterVpos,x
	sta VIC64+MOBY+2,y
	lda StartScreenMonsterData,x
	sta SpritePointer+1,x
	lda StartScreenMonsterColors,x
	sta VIC64+MOBCOL+1,x
	dex
	bpl ssinspr			; setup next sprite

	lda #$00
	sta VIC64+MOBMSB		; clear sprite x-msb
	lda #$3e
	sta VIC64+MOBENA		; enable monsters

sssprlp:lda timer			; wait
	bne sssprlp
sswait	lda timer
	bne sswait
	inc delay
	lda delay
	and #$1f			; delay next movement
	bne sschkf1

	ldy #$04			; move 5 monsters
ssright:tya			
	asl
	tax
	lda monster_dir,y
	beq ssleft
	inc VIC64+MOBX+2,x		; move monsters right
	lda VIC64+MOBX+2,x
	cmp StartScreenMonsterRLimit,y	; check if right limit
	bcc ssnxspr
	lda #$00
	sta monster_dir,y		; set left direction
	cpy #$03
	beq ssnxspr			; skip if monster #3 (unidir monster)
	tya
	tax
	inc SpritePointer+1,x		; turn monster sprite left
	bne ssnxspr
ssleft:	dec VIC64+MOBX+2,x		; move monster left
	lda VIC64+MOBX+2,x
	cmp #$d7
	bcs ssnxspr			; skip if left limit not reached
	sta monster_dir,y		; set right dir
	cpy #$03
	beq ssnxspr			; skip if monster #3 (unidir monster)
	tya
	tax
	dec SpritePointer+1,x		; turn monster sprite right
ssnxspr:dey
	bpl ssright
sschkf1:jsr CheckF1Key			; check f1 key pressed
	cpy #$00
	beq sssprlp			; continue movement, if not F1 pressed

	lda #BLACK			; set game bgr+ext colors
	sta VIC64+BGRCOL
	lda #BLUE
	sta VIC64+EXTCOL
	rts
; -------------------------------------------------------------------------------------------------
; $e51a
StartScreenMonsterVpos:
	!byte $52, $6a, $82, $9a, $b2
StartScreenMonsterData:
	!byte $e9, $ed, $f1, $f5, $f9
StartScreenMonsterRLimit:
	!byte $db, $dc, $db, $dc, $db
; -------------------------------------------------------------------------------------------------
; $e529 interrupt
Interrupt:
	pha
	lda CIA64+ICR			; load irq-reg
	and #$02
	beq irqx			; skip if not timer b
	inc timer			; inc timer
irqx:	pla
	rti
; -------------------------------------------------------------------------------------------------
; $e535
MoveWorrior:  lda $2a
	and #$01
	beq +
	rts
; $e53c
+	lda $1e
	bpl +
	rts
; $e541
+	jsr CheckJoyKey
	lda #$00
	sta $30
	lda #$01
	sta $31
	lda $46
	sta worrior_dir
	lda $2e
	sta $35
	jsr le56f
	lda $35
	sta $2e
	ldx worrior_dir
	lda WorriorSpriteTable,x
	bne +
	ldx $2e
	lda WorriorSpriteTable,x
+	sta sprite_data
	rts
; -------------------------------------------------------------------------------------------------
; $e56a
WorriorSpriteTable:  
	!byte $00, $e8, $e7, $e6, $e5
; -------------------------------------------------------------------------------------------------
; $e56f
le56f:  ldx $30
	lda #$00
	sta ptr1
	jsr le65d
	lda $31
	sta $41
	lda VIC64+MOBX,x
	sta $3f
	lda VIC64+MOBY,x
	sta $40
	jsr le709
	lda worrior_dir
	bne le590
	jmp le6b3
le590:  lda $35
	cmp #$03
	bcs le5ca
	lda worrior_dir
	cmp #$03
	bcc le601
	dec $40
	lda $40
le5a0:  beq le5b6
	cmp #$01
	beq le5b6
	cmp #$ff
	beq le5b6
	cmp #$00
	bcc le5c3
	inc ptr1
	sec
	sbc #$03
	jmp le5a0
le5b6:  ldx ptr1
	lda Table14+13,x
	ldx $30
	sta VIC64+MOBY,x
	jmp le601
le5c3:  lda #$00
	sta worrior_dir
	jmp le6b3
le5ca:  lda worrior_dir
	cmp #$03
	bcs le601
	lda $3f
	sec
	sbc #$01
le5d5:  beq le5eb
	cmp #$01
	beq le5eb
	cmp #$ff
	beq le5eb
	cmp #$00
	bcc le5c3
	inc ptr1
	sec
	sbc #$03
	jmp le5d5
le5eb:  ldx ptr1
	cpx #$0a
	bne le5f9
	lda $31
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
le5f9:  lda Table14,x
	ldx $30
	sta VIC64+MOBX,x
le601:  lda worrior_dir
	sta $35
	ldx $30
	lda worrior_dir
	cmp #$01
	bne le616
	dec VIC64+MOBY,x
	dec VIC64+MOBY,x
	jmp le6b3
le616:  cmp #$02
	bne le623
	inc VIC64+MOBY,x
	inc VIC64+MOBY,x
	jmp le6b3
le623:  cmp #$03
	bne le641
	dec VIC64+MOBX,x
	dec VIC64+MOBX,x
	lda VIC64+MOBX,x
	cmp #$fe
	bcc le63e
	lda $31
	eor #$ff
	and VIC64+MOBMSB
	sta VIC64+MOBMSB
le63e:  jmp le6b3
le641:  cmp #$04
	bne le6b3
	inc VIC64+MOBX,x
	inc VIC64+MOBX,x
	lda VIC64+MOBX,x
	cmp #$02
	bcs le6b3
	lda $31
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
	jmp le6b3
le65d:  lda VIC64+MOBX,x
	sta $3f
	lda VIC64+MOBY,x
	clc
	sbc #$06
	sta $40
	lda $31
	sta $41
	jsr le709
	lda worrior_dir
	beq le6b2
	cmp #$03
	bcc le685
	beq le680
	inc $3f
	jmp le690
le680:  dec $3f
	jmp le690
le685:  cmp #$01
	bne le68e
	dec $40
	jmp le690
le68e:  inc $40
le690:  lda #$04
	sta draw_ptr+1
	lda $3f
le696:  ldy $40
	beq le6a6
	dec $40
	clc
	adc #$28
	bcc le696
	inc draw_ptr+1
	jmp le696
le6a6:  sta draw_ptr
	ldy #$00
	lda (draw_ptr),y
	beq le6b2
	lda #$00
	sta worrior_dir
le6b2:  rts
; -------------------------------------------------------------------------------------------------
; $
le6b3:  lda $31
	and VIC64+MOBMSB
	bne le6cf
	lda VIC64+MOBX,x
	cmp #$16
	bcs le6e7
	lda $31
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
	lda #$40
	sta VIC64+MOBX,x
	rts
; $
le6cf:  beq le6e7
	lda VIC64+MOBX,x
	cmp #$42
	bcc le6e7
	lda $31
	eor #$ff
	and VIC64+MOBMSB
	sta VIC64+MOBMSB
	lda #$18
	sta VIC64+MOBX,x
le6e7:  lda VIC64+MOBY,x
	cmp #$31
	bcs +
	inc VIC64+MOBY,x
	inc VIC64+MOBY,x
+	rts
; -------------------------------------------------------------------------------------------------
; $e6f5
Table14:!byte $19, $31, $49, $61, $79, $91, $a9, $c1
	!byte $d9, $f1, $0a, $22, $39
; $e702
Table15:
	!byte $33, $4b, $63, $7b, $93, $ab, $c3
; -------------------------------------------------------------------------------------------------
; 
le709:  lda $40
	sec
	sbc #$26
	lsr
	lsr
	lsr
	sta $40
	lda $3f
	sec
	sbc #$0b
	bcs +
	clc
	bcc le728
+	pha
	lda $41
	and VIC64+MOBMSB
	clc
	beq +
	sec
+	pla
le728:  ror
	lsr
	lsr
	sta $3f
	rts
; -------------------------------------------------------------------------------------------------
; $e72e Calc screen position and draw maze tile
DrawMazeTile:
	lda #$00			; reset draw ptr hi
	sta draw_ptr+1

	lda draw_column			; load column+carry
; add 40 cols for each line
dmlinlp:ldy draw_line			; load line
	beq drawmz			; line 0 ?
	dec draw_line
	clc
	adc #40				; decrease line and add 40 to lo
	bcc dmlinlp
	inc draw_ptr+1			; inc draw ptr hi if carry
	jmp dmlinlp

drawmz:	sta draw_ptr			; store draw ptr lo
	lda draw_ptr+1
	clc
	adc #SH				; add screen base address hi
	sta draw_ptr+1
	ldy #0
	lda draw_char
	sta (draw_ptr),y		; store tile to screen
	rts
; -------------------------------------------------------------------------------------------------
; $e754
le754:  lda $25
	bpl +
	jmp le7fc
+	lda $56
	and #$80
	beq le776
le761:  lda #$ff
	sta $25
	lda #$7f
	and VIC64+MOBENA
	sta VIC64+MOBENA
	lda #$7f
	and VIC64+MOBMSB
	sta VIC64+MOBMSB
	rts
;$ e776
le776:  lda VIC64+MOBMSB
	and #$80
	bne le786
	lda VIC64+MOBX+14
	cmp #$14
	bcc le761

	bcs le78d
le786:  lda VIC64+MOBX+14
	cmp #$42
	bcs le761
; $e78d
le78d:  lda $2d
	cmp #$03
	bcc le7cd
	bne le7b1
	dec VIC64+MOBX+14
	dec VIC64+MOBX+14
	dec VIC64+MOBX+14
	dec VIC64+MOBX+14
	lda VIC64+MOBX+14
	cmp #$fc
	bcc le7b0
	lda #$7f
	and VIC64+MOBMSB
	sta VIC64+MOBMSB
le7b0:  rts
; $e7b1
le7b1:  inc VIC64+MOBX+14
	inc VIC64+MOBX+14
	inc VIC64+MOBX+14
	inc VIC64+MOBX+14
	lda VIC64+MOBX+14
	cmp #$04
	bcs le7b0
	lda #$80
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
	rts
; $e7cd
le7cd:  cmp #$01
	bne le7e5
	dec VIC64+MOBY+14
	dec VIC64+MOBY+14
	dec VIC64+MOBY+14
	dec VIC64+MOBY+14
	lda VIC64+MOBY+14
	cmp #$2a
	bcc le7f8
	rts
; $e7e5
le7e5:  inc VIC64+MOBY+14
	inc VIC64+MOBY+14
	inc VIC64+MOBY+14
	inc VIC64+MOBY+14
	lda VIC64+MOBY+14
	cmp #$ca
	bcc le7fb
le7f8:  jmp le761
le7fb:  rts
; -------------------------------------------------------------------------------------------------
; $e7fc
le7fc:  cmp #$ff
	bne +
	lda $1e
	bpl chkshot
+	rts
; -------------------------------------------------------------------------------------------------
; $e805 check fire pressed
chkshot:lda fire
	bmi shoot
	rts
; $e80a shoot
shoot:	lda #4
	jsr PlaySound			; play shoot sound
	lda VIC64
	sta VIC64+MOBX+14
	lda VIC64+MOBY
	sta VIC64+MOBY+14
	lda VIC64+MOBMSB
	and #$01
	beq le82d
	lda #$80
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
	jmp le835
le82d:  lda #$7f
	and VIC64+MOBMSB
	sta VIC64+MOBMSB
le835:  lda $2e
	sta $2d
	cmp #$03
	bcc le844
	lda #$fd
	sta $54
	jmp le848
le844:  lda #$fe
	sta $54
le848:  lda #$80
	ora VIC64+MOBENA
	sta VIC64+MOBENA
	lda #$00
	sta $25
	rts
; -------------------------------------------------------------------------------------------------
; $
le855:  lda $29
	bne le860
	lda $2a
	and #$01
	bne le860
	rts
; -------------------------------------------------------------------------------------------------
; $
le860:  lda $1e
	bpl le865
	rts
; -------------------------------------------------------------------------------------------------
; $
le865:  ldx #$01
	stx $10
le869:  lda $1e,x
	bpl le876
le86d:  inc $10
	ldx $10
	cpx #$06
	bne le869
	rts
; -------------------------------------------------------------------------------------------------
; $
le876:  lda Table16,x
	sta $30
	lda bits,x
	sta $31
	dec $5f,x
	dec $5f,x
	bmi le88b
	lda $67,x
	jmp le8e8
le88b:  lda #$17
	sta $5f,x
	ldx $30
	lda SID64+RANDOM
	and #$01
	beq le8cb
	lda bits,x
	ora #$01
	and VIC64+MOBMSB
	beq le8ab
	cmp #$01
	beq le8b3
	cmp bits,x
	beq le8bf
le8ab:  lda VIC64
	cmp VIC64+MOBX,x
	bcc le8bf
le8b3:  lda SID64+RANDOM
	and #$03
	beq le8c6
le8ba:  lda #$04
	jmp le8e8
le8bf:  lda SID64+RANDOM
	and #$03
	beq le8ba
le8c6:  lda #$03
	jmp le8e8
le8cb:  lda VIC64+MOBY
	cmp VIC64+MOBY,x
	bcc le8df
	lda SID64+RANDOM
	and #$03
	beq le8e6
le8da:  lda #$02
	jmp le8e8
le8df:  lda SID64+RANDOM
	and #$03
	beq le8da
le8e6:  lda #$01
le8e8:  ldx $10
	sta worrior_dir
	lda $67,x
	sta $35
	jsr le56f
	lda worrior_dir
	bne le8fa
	jsr le912
le8fa:  ldx $10
	lda $35
	sta $67,x
	clc
	adc $56,x
	tax
	lda Table17,x
	bne le90b
	beq le90f
le90b:  ldx $10
	sta sprite_data,x
le90f:  jmp le86d
le912:  lda $35
	clc
	adc #$01
	cmp #$04
	bcc le920
	lda SID64+RANDOM
	and #$03
le920:  sta $35
le922:  rts
; -------------------------------------------------------------------------------------------------
; $e923
Table16:
	!byte $00, $02, $04, $06, $08, $0a, $0c, $0e
; $e92b
Table17:
	!byte $ec, $ec, $eb, $ea, $e9, $f0, $f0, $ef
	!byte $ee, $ed, $f4, $f4, $f3, $f2, $f1, $f5
	!byte $f5, $f5, $f5, $f5, $fc, $fc, $fb, $fa
	!byte $f9
; -------------------------------------------------------------------------------------------------
; 
le944:  lda $1e
	bmi le95b
	lda $2a
	and #$1f
	bne le95b
	ldx #$01
le950:  lda $1e,x
	cmp #$ff
	beq le95c
	inx
	cpx #$06
	bne le950
le95b:  rts
; -------------------------------------------------------------------------------------------------
; $
le95c:  ldy #$00
	lda $27
	beq le967
	dec $27
	jmp le9ab
le967:  iny
	lda $47
	beq le971
	dec $47
	jmp le9ab
le971:  iny
	lda $48
	beq le97b
	dec $48
	jmp le9ab
le97b:  iny
	ldx #$01
	lda #$ff
le980:  and $1e,x
	and monster_dir,x
	inx
	inx
	cpx #$07
	bne le980
	cmp #$ff
	bne le95b
	ldx #$01
	lda $49
	beq le999
	dec $49
	jmp le9a7
le999:  iny
	lda $4a
	beq le95b
	lda #3
	jsr PlaySound
	dec $4a
	dec $4b
le9a7:  lda #$01
	sta $29
le9ab:  tya
	pha
	txa
	pha
	asl
	tay
	lda SID64+RANDOM
	and #$07
	tax
	lda TableX,x
	sta VIC64,y
	lda $2a
	lsr
	lsr
	lsr
	lsr
	lsr
	and #$07
	tax
	lda TableY,x
	sta VIC64+MOBY,y
	clc
	adc #$18
	cmp VIC64+MOBY
	bcs le9e5
	sec
	sbc #$30
	cmp VIC64+MOBY
	bcc le9e5
	lda VIC64+MOBY
	sbc #$24
	sta VIC64+MOBY,y
le9e5:  pla
	tax
	pla
	tay
	inc $1e,x
	lda Table07,y
	sta $56,x
	lda Table09,y
	sta sprite_data,x
	sta SpritePointer,x
	lda StartScreenMonsterColors,y
	sta VIC64+MOBCOL,x
	lda bits,x
	ora VIC64+MOBENA
	sta VIC64+MOBENA
	rts
; -------------------------------------------------------------------------------------------------
; $ea08
Table07:  
	!byte $00, $05, $0a, $0f, $14
StartScreenMonsterColors:
	!byte BLUE, YELLOW, CYAN, LIGHTGREEN, MAGENTA
Table09:
	!byte $ec, $f0, $f4, $f5, $fc
; -------------------------------------------------------------------------------------------------
; 
lea17:  ldx #$07
lea19:  lda $1e,x
	bmi lea21
lea1d:  dex
	bpl lea19
	rts
; -------------------------------------------------------------------------------------------------
; $
lea21:  cmp #$ff
	beq lea1d
	inc $1e,x
	txa
	bne lea77
	lda $1e,x
	cmp #$81
	bne lea3c
	lda #$f6
	sta sprite_data
	lda #6
	jsr PlaySound
	jmp lea1d
lea3c:  cmp #$90
	bne lea47
	lda #$f7
	sta sprite_data
	jmp lea1d
lea47:  cmp #$a0
	bne lea52
	lda #$f6
	sta sprite_data
	jmp lea1d
lea52:  cmp #$b0
	bne lea5d
	lda #$f7
	sta sprite_data
	jmp lea1d
lea5d:  cmp #$c0
	bne lea68
	lda #$f8
	sta sprite_data
	jmp lea1d
lea68:  cmp #$d0
	bne lea74
	lda VIC64+MOBENA
	and #$fe
	sta VIC64+MOBENA
lea74:  jmp lea1d
lea77:  lda $1e,x
	cmp #$81
	bne leaa6
	lda #$f6
	sta sprite_data,x
	cpx #$06
	beq lea9e
	lda $56,x
	lsr
	lsr
	and #$07
	tay
	lda Table18,y
	cmp #$05
	bne lea9b
	lda SID64+RANDOM
	and #$30
	clc
	adc #$10
lea9b:  jsr AddScore
lea9e:  lda #5
	jsr PlaySound
	jmp lea1d
leaa6:  cmp #$90
	bne leab1
	lda #$f7
	sta sprite_data,x
	jmp lea1d
leab1:  cmp #$a0
	bne leabc
	lda #$f8
	sta sprite_data,x
	jmp lea1d
leabc:  cmp #$b0
	bne leace
	lda bits,x
	eor #$ff
	and VIC64+MOBENA
	sta VIC64+MOBENA
	jmp lea1d
leace:  cpx #$06
	beq leae4
	cmp #$fe
	bne leae4
	lda $27
	ora $47
	ora $48
	ora $49
	ora $4a
	bne leae4
	sta $26
leae4:  jmp lea1d
; -------------------------------------------------------------------------------------------------
; $eae7
Table18:  
	!byte $01, $02, $03, $04, $05, $05, $05, $05
; -------------------------------------------------------------------------------------------------
; $eaef
leaef:  lda $1e
	bmi leb2b
	lda $55
	and #$01
	beq leb2b
	lda $55
	and #$80
	bne leb6f
	ldx #$00
leb01:  inx
	cpx #$07
	beq leb2b
	lda $1e,x
	bmi leb01
	lda VIC64
	sta ptr1
	lda #$00
	sta ptr1+1
	lda #$01
	and VIC64+MOBMSB
	beq leb1c
	inc ptr1+1
leb1c:  lda VIC64+MOBY
	sta ptr2
	jsr leb78
	bcc leb01
	lda #$80
	sta $1e
	rts
; -------------------------------------------------------------------------------------------------
; $
leb2b:  lda $25
	bmi leb6f
	lda #$00
	sta ptr1+1
	lda #$80
	and VIC64+MOBMSB
	beq leb3c
	inc ptr1+1
leb3c:  lda VIC64+MOBX+14
	sta ptr1
	lda VIC64+MOBY+14
	sta ptr2
	lda $55
	and #$80
	beq leb6f
	ldx #$01
leb4e:  lda $1e,x
	bmi leb6a
	jsr leb78
	bcc leb6a
	lda #$ff
	sta $25
	lda #$7f
	and VIC64+MOBENA
	sta VIC64+MOBENA
	lda #$80
	sta $1e,x
	jmp leb6f
leb6a:  inx
	cpx #$07
	bne leb4e
leb6f:  rts
; -------------------------------------------------------------------------------------------------
; $
bits:  
	!byte $01, $02, $04, $08, $10, $20, $40, $80
; -------------------------------------------------------------------------------------------------
; $eb78
leb78:  lda $55
	and bits,x
	bne leb82
	jmp lebb8
leb82:  txa
	asl
	tay
	lda VIC64,y
	sta ptr2+1
	lda VIC64+MOBY,y
	sta temp2
	lda #$00
	sta temp1
	lda VIC64+MOBMSB
leb96:  and bits,x
	beq leb9d
	inc temp1
leb9d:  lda #$0e
	sta temp3
	lda #$0e
	sta $10
	lda #$0b
	sta $11
	lda #$17
	sta data_ctr
	jsr lebba
	bcc lebb9
	lda #$80
	sta $1e,x
	bne lebb9
lebb8:  clc
lebb9:  rts
; -------------------------------------------------------------------------------------------------
; $
lebba:  sec
	lda temp2
	sbc temp3
	cmp ptr2
	bcs lebfc
	clc
	lda temp2
	adc $10
	cmp ptr2
	bcc lebfc
	sec
	lda ptr2+1
	sbc $11
	sta ptr2+1
	lda temp1
	sbc #$00
	sta temp1
	sec
	lda ptr2+1
	sbc ptr1
	lda temp1
	sbc ptr1+1
	bcs lebfc
	lda ptr2+1
	adc data_ctr
	sta ptr2+1
	lda temp1
	adc #$00
	sta temp1
	sec
	lda ptr2+1
	sbc ptr1
	lda temp1
	sbc ptr1+1
	bcc lebfc
	rts
; -------------------------------------------------------------------------------------------------
; $
lebfc:  clc
	rts
; -------------------------------------------------------------------------------------------------
; $
lebfe:  lda $1e
	bpl lec03
	rts
; -------------------------------------------------------------------------------------------------
; $
lec03:  lda $24
	bne lec08
	rts
; -------------------------------------------------------------------------------------------------
; $
lec08:  lda $4b
	bmi lec13
	lda $2a
	and #$0f
	beq lec13
	rts
; -------------------------------------------------------------------------------------------------
; $
lec13:  ldx #$01
lec15:  lda $1e,x
	bpl lec1f
lec19:  inx
	cpx #$06
	bne lec15
	rts
; -------------------------------------------------------------------------------------------------
; $
lec1f:  txa
	asl
	tay
	lda VIC64
	cmp VIC64,y
	beq lec65
	lda VIC64+MOBY
	cmp VIC64+MOBY,y
	bne lec19
	lda VIC64
	cmp VIC64,y
	lda VIC64+MOBMSB
	and #$01
	bne lec49
	lda VIC64+MOBMSB
	and bits,x
	bne lec51
	beq lec57
lec49:  lda VIC64+MOBMSB
	and bits,x
	bne lec57
lec51:  bcc lec56
	clc
	bcc lec57
lec56:  sec
lec57:  lda #$04
	bcs lec5d
	lda #$03
lec5d:  sta $77
	lda #$fd
	sta $53
	bne lec77
lec65:  lda VIC64+MOBY
	cmp VIC64+MOBY,y
	lda #$02
	bcs lec71
	lda #$01
lec71:  sta $77
	lda #$fe
	sta $53
lec77:  lda #$00
	sta $24
	lda VIC64,y
	sta VIC64+MOBX+12
	lda VIC64+MOBY,y
	sta VIC64+MOBY+12
	lda VIC64+MOBMSB
	and bits,x
	beq lec96
	lda VIC64+MOBMSB
	ora #$40
	bne lec9b
lec96:  lda VIC64+MOBMSB
	and #$bf
lec9b:  sta VIC64+MOBMSB
	lda VIC64+MOBENA
	ora #$40
	sta VIC64+MOBENA
	lda $77
	sta $67,x
	rts
; -------------------------------------------------------------------------------------------------
; $
lecab:  lda $24
	bpl lecb0
	rts
; -------------------------------------------------------------------------------------------------
; $
lecb0:  lda $56
	and #$40
	beq lecc3
lecb6:  lda #$ff
	sta $24
	lda VIC64+MOBENA
	and #$bf
	sta VIC64+MOBENA
	rts
; -------------------------------------------------------------------------------------------------
; $
lecc3:  lda VIC64+MOBMSB
	and #$40
	bne lecd3
	lda VIC64+MOBX+12
	cmp #$14
	bcc lecb6
	bcs lecda
lecd3:  lda VIC64+MOBX+12
	cmp #$42
	bcs lecb6
lecda:  lda $77
	cmp #$03
	bcc led0e
	bne lecf8
	dec VIC64+MOBX+12
	dec VIC64+MOBX+12
	lda VIC64+MOBX+12
	cmp #$fe
	bcc led2d
	lda VIC64+MOBMSB
	and #$bf
	sta VIC64+MOBMSB
	rts
; -------------------------------------------------------------------------------------------------
; $
lecf8:  inc VIC64+MOBX+12
	inc VIC64+MOBX+12
	lda VIC64+MOBX+12
	cmp #$02
	bcs led2d
	lda VIC64+MOBMSB
	ora #$40
	sta VIC64+MOBMSB
	rts
; -------------------------------------------------------------------------------------------------
; $
led0e:  cmp #$01
	bne led20
	dec VIC64+MOBY+12
	dec VIC64+MOBY+12
	lda VIC64+MOBY+12
	cmp #$2a
	bcc led2e
	rts
; -------------------------------------------------------------------------------------------------
; $
led20:  inc VIC64+MOBY+12
	inc VIC64+MOBY+12
	lda VIC64+MOBY+12
	cmp #$ca
	bcs led2e
led2d:  rts
; -------------------------------------------------------------------------------------------------
; $ed2e
led2e:  jmp lecb6
; -------------------------------------------------------------------------------------------------
; $ed31 play sound no. x
PlaySound:
	cmp #1
	bne pls02
; start sound
	sta $6f
	lda #<Sound1
	sta sound_ptr
	lda #>Sound1
	sta sound_ptr+1
	lda #$21
	sta SID64+V1CTRL
	lda #$01
	sta $72
	rts
; $ed49 alternative start sound
pls02:  cmp #2
	bne pls03
	sta $6f
	lda #<Sound2
	sta sound_ptr
	lda #>Sound2
	sta sound_ptr+1
	lda #$01
	sta $72
	rts
; $ed5c
pls03:  cmp #3
	bne pls04
	sta $6f
	lda #<Sound3
	sta sound_ptr
	lda #>Sound3
	sta sound_ptr+1
	lda #$01
	sta $72
	rts
; $ed6f Shoot sound
pls04:  cmp #4
	bne pls05
	lda #$0f
	sta $74
	lda #$81
	sta SID64+V2CTRL
	rts
; $ed7d
pls05:  cmp #5
	bne pls06
	lda #$0f
	sta $75
	lda #$81
	sta SID64+V3CTRL
	rts
; $ed8b
pls06:  cmp #6
	bne pls07
	lda #$3f
	sta $75
	sta $74
	lda #$81
	sta SID64+V2CTRL
	sta SID64+V3CTRL
	rts
; $ed9e Bonus sound
pls07:  cmp #7
	bne pls08
	lda #$3f
	sta $73
	asl
	sta SID64+V2HI
	lda #$21
	sta SID64+V2CTRL
	lda #$00
	sta $74
	rts
; $edb4
pls08:  cmp #8
	bne plsx
	lda #$07
	sta $73
	lda #$21
	sta SID64+V2CTRL
	rts
plsx:	rts
; -------------------------------------------------------------------------------------------------
; $
GameCycle:
	lda $74
	beq ledd6
	asl
	asl
	asl
	sta SID64+V2HI
	dec $74
	bne ledd6
	lda #$80
	sta SID64+V2CTRL
ledd6:  lda $75
	beq ledea
	asl
	asl
	adc #$07
	sta SID64+V3HI
	dec $75
	bne ledea
	lda #$80
	sta SID64+V3CTRL
ledea:  lda $73
	beq lee04
	lda $73
	lsr
	lsr
	and #$01
	tax
	lda Sound1,x
	sta SID64+V2HI
	dec $73
	bne lee04
	lda #$20
	sta SID64+V2CTRL
lee04:  dec $72
	bne lee42
	clc
	lda sound_ptr
	adc #$02
	sta sound_ptr
	lda sound_ptr+1
	adc #$00
	sta sound_ptr+1
	ldy #$00
	lda (sound_ptr),y
	bne lee2e
	lda $6f
	cmp #$01
	bne lee24
	jmp PlaySound

lee24:  cmp #2
	bne lee2b
	jmp pls02
lee2b:  jmp pls03
lee2e:  sta $72
	ldy #$01
	lda (sound_ptr),y
	asl
	tax
	lda Notes,x
	sta SID64+V1LO
	lda Notes+1,x
	sta SID64+V1HI
lee42:  rts
; ***************************************** ZONE NOTES ********************************************
!zone notes
; $ee43
Sound1: !byte $30, $40, $18, $04, $18, $03, $18, $02, $18
; $ee4c
Sound2:	!byte $01, $00, $08, $04, $08, $19, $08, $03, $08
; $ee55
Sound3:	!byte $01, $00, $04, $04, $04, $19, $04, $03, $04

	!byte $01, $00
; $ee60
Notes:
	!byte $00, $00, $0d, $0a, $72, $0b, $20, $0c
	!byte $d6, $0c, $9c, $0d, $65, $0e, $46, $0f
	!byte $2f, $10, $25, $11, $2a, $12, $3f, $13
	!byte $64, $14, $9a, $15, $e3, $16, $3f, $18
	!byte $b1, $19, $38, $1b, $d6, $1c, $8d, $1e
	!byte $5e, $20, $4b, $22, $55, $24, $7e, $26
	!byte $c8, $28, $34, $2b, $c6, $2d, $7f, $30
	!byte $61, $33, $6f, $36, $ac, $39, $1a, $3d
	!byte $bc, $40, $95, $44, $a9, $48, $fc, $4c
	!byte $a1, $51, $69, $56, $8c, $5b, $fe, $60
	!byte $c2, $66, $df, $6c, $58, $73, $34, $7a
	!byte $78, $81, $2b, $89, $53, $91, $f7, $99
	!byte $1f, $a3
; $eec2
; ***************************************** ZONE FONT *********************************************
!zone font
*= $f000
	!byte $00, $00, $00, $00, $00, $00, $00, $00	; $00 space
	!byte $3c, $66, $6e, $76, $66, $66, $3c, $00	; $01 '0'
	!byte $18, $18, $38, $18, $18, $18, $7e, $00
	!byte $3c, $66, $06, $0c, $30, $60, $7e, $00
	!byte $3c, $66, $06, $1c, $06, $66, $3c, $00
	!byte $06, $0e, $1e, $66, $7f, $06, $06, $00
	!byte $7e, $60, $7c, $06, $06, $66, $3c, $00
	!byte $3c, $66, $60, $7c, $66, $66, $3c, $00
	!byte $7e, $66, $0c, $18, $18, $18, $18, $00
	!byte $3c, $66, $66, $3c, $66, $66, $3c, $00
	!byte $3c, $66, $66, $3c, $06, $66, $3c, $00	; $0a '9'
	!byte $18, $3c, $66, $7e, $66, $66, $66, $00	; $0b 'A'
	!byte $7c, $66, $66, $7c, $66, $66, $7c, $00
	!byte $3c, $66, $60, $60, $60, $66, $3c, $00
	!byte $78, $6c, $66, $66, $66, $6c, $78, $00
	!byte $7e, $60, $60, $78, $60, $60, $7e, $00
	!byte $7e, $60, $60, $78, $60, $60, $60, $00
	!byte $3c, $66, $60, $6e, $66, $66, $3c, $00
	!byte $66, $66, $66, $7e, $66, $66, $66, $00
	!byte $3c, $18, $18, $18, $18, $18, $3c, $00
	!byte $1e, $0c, $0c, $0c, $0c, $6c, $38, $00
	!byte $66, $6c, $78, $70, $78, $6c, $66, $00
	!byte $60, $60, $60, $60, $60, $60, $7e, $00
	!byte $63, $77, $7f, $6b, $63, $63, $63, $00
	!byte $66, $76, $7e, $7e, $6e, $66, $66, $00
	!byte $3c, $66, $66, $66, $66, $66, $3c, $00
	!byte $7c, $66, $66, $78, $60, $60, $60, $00	; $1a 'P'
	!byte $7c, $66, $66, $7c, $78, $6c, $66, $00	; $1b 'R'
	!byte $3c, $66, $60, $3c, $06, $66, $3c, $00
	!byte $7e, $18, $18, $18, $18, $18, $18, $00
	!byte $66, $66, $66, $66, $66, $66, $3c, $00
	!byte $66, $66, $66, $66, $66, $3c, $18, $00
	!byte $63, $63, $63, $6b, $7f, $77, $63, $00
	!byte $66, $66, $3c, $18, $3c, $66, $66, $00
	!byte $66, $66, $66, $3c, $18, $18, $18, $00	; $22 'Y'
	!byte $03, $06, $0c, $18, $30, $60, $c0, $00	; $23 '/'
	!byte $0c, $18, $30, $30, $30, $18, $0c, $00	; $24 '('
	!byte $30, $18, $0c, $0c, $0c, $18, $30, $00	; $25 ')'
	!byte $00, $00, $00, $7e, $00, $00, $00, $00	; $26 '-'
	!byte $00, $00, $18, $00, $00, $18, $18, $30	; $27 ';'
	!byte $00, $00, $00, $00, $00, $18, $18, $00	; $28 '.'
	!byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa	; $29 ||||
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0	; $2a vertical line
	!byte $00, $00, $00, $00, $00, $00, $00, $ff	; $2b low line
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $ff	; $2c left-low corner
	!byte $ff, $00, $00, $00, $00, $00, $00, $00	; $2d high line
; ***************************************** ZONE DATA *********************************************
!zone data
; $f170 initial vic reg values
VICRegs:
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $1b, $00, $00, $00, $00, $0b, $00	; display on, 25 rows, 40 columns
	!byte $1d, $ff, $00, $00, $ff, $00, $00, $00	; VM = $0400, CB = $f000, multicolor sprites
	!byte $06, $00, $00, $00, $00, $02, $07, $06	; sprite_mcm_color0 = red, _color1 = yellow
	!byte $06, $06, $06, $06, $06, $06, $06
; $f19f initial sid reg values
SIDRegs:
	!byte $00, $20, $00, $00, $20, $00, $f9		; osc1: f = $2000, triangle, s = $f, r = $9
	!byte $00, $60, $00, $00, $80, $00, $fa		; osc2: f = $6000, noise, s = $f, r = $a
	!byte $ff, $ff, $00, $00, $80, $00, $fa		; osc3: f = $ffff, noise, s = $f, r = $a
	!byte $ff, $ff, $07, $1f			; filter = $ffff osc 1-3, lowpass, max vol
; $f1b8 Mazedata Level 1
Maze1:
	!byte $1a, $1c, $19, $1c, $19, $19, $1c, $19
	!byte $19, $1c, $19, $1c, $18, $1c, $1c, $1c
	!byte $18, $1c, $19, $19, $19, $1c, $1c, $19
	!byte $1c, $1c, $19, $1c, $18, $1c, $1a, $1c
	!byte $1c, $1c, $19, $18, $19, $1c, $1a, $1c
	!byte $1c, $1c, $18, $19, $19, $18, $1a, $19
	!byte $1c, $19, $1c, $1c, $1a, $1c, $1a, $1c
	!byte $1c, $18, $19, $1c, $18, $1c, $1c, $18
	!byte $1a, $1c, $19, $19, $1c, $18, $1c, $19
	!byte $19, $1c, $18, $1a, $1c, $1c, $1a, $1c
	!byte $1c, $1c, $1a, $1c, $1c, $1c, $1c, $1a
	!byte $1c, $1c, $1a
; $f213 Mazedata Level 2
Maze2:
	!byte $18, $1c, $18, $19, $1c, $19, $1c, $19
	!byte $1c, $19, $19, $1c, $1c, $1a, $1c, $1c
	!byte $1c, $18, $1a, $1c, $19, $18, $18, $19
	!byte $1c, $1a, $1c, $19, $1c, $18, $1c, $1c
	!byte $19, $1c, $1c, $1c, $18, $19, $1c, $1a
	!byte $1c, $19, $19, $18, $18, $19, $1c, $18
	!byte $1a, $19, $1c, $1a, $1c, $1c, $19, $1c
	!byte $1c, $1c, $19, $1c, $1c, $1c, $19, $1c
	!byte $1c, $1a, $1c, $1a, $1c, $1c, $18, $1c
	!byte $1c, $18, $1c, $1a, $1c, $18, $1c, $1c
	!byte $1c, $1c, $1c, $1a, $1c, $1c, $1c, $1a
	!byte $1c, $1c, $1c
; $f26e Mazedata Level 3
Maze3:
	!byte $18, $1c, $18, $19, $1c, $19, $1c, $19
	!byte $1c, $19, $19, $19, $1c, $18, $1c, $19
	!byte $18, $1c, $1c, $1c, $1c, $1c, $1c, $1a
	!byte $1c, $19, $19, $1c, $19, $1c, $1c, $1c
	!byte $1c, $1c, $1c, $1c, $19, $1c, $19, $18
	!byte $1c, $1c, $1a, $19, $19, $1c, $19, $19
	!byte $19, $1a, $1c, $1c, $1a, $1c, $1c, $1c
	!byte $1c, $19, $1c, $19, $1c, $1c, $19, $1c
	!byte $19, $18, $19, $1c, $1c, $18, $1c, $19
	!byte $1c, $1c, $18, $19, $1c, $19, $1a, $1c
	!byte $1c, $1a, $1c, $1c, $1c, $1c, $1c, $1a
	!byte $1c, $1c, $1c
; $f2c9 Mazedata Level 4
Maze4:
	!byte $1a, $1c, $19, $1c, $18, $19, $1c, $19
	!byte $1c, $18, $19, $1c, $18, $1c, $1c, $19
	!byte $19, $1c, $18, $19, $1c, $18, $19, $19
	!byte $1c, $1c, $1c, $19, $1c, $1c, $1c, $1c
	!byte $1c, $1c, $1c, $1c, $19, $19, $1c, $1a
	!byte $1c, $19, $18, $18, $18, $18, $18, $18
	!byte $18, $19, $1c, $18, $18, $1c, $1c, $1c
	!byte $1c, $1a, $1c, $19, $18, $1c, $19, $1c
	!byte $18, $1c, $1a, $1c, $1c, $18, $1c, $1c
	!byte $1c, $1c, $18, $1c, $1c, $1c, $1c, $1c
	!byte $1c, $1c, $1c, $1c, $1c, $1c, $1c, $1a
	!byte $1c, $1c, $1c, $01, $01, $03, $02, $02
	!byte $01, $01, $01
; $f32c
	!byte $01
; $f32d
GameScreenData:
	!byte $00, SH , $2a, LIN, $2a, LIN, $2a, LIN
	!byte $2a, LIN, $2a, LIN, $2a, LIN, LIN, LIN
	!byte LIN, $2a, LIN, $2a, LIN, $2a, LIN, $2a
	!byte LIN, $2a, LIN, $2a, LIN, $2a, LIN, $2a
	!byte LIN, $2a, LIN, $2a, LIN, $2a, LIN, $2c
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, ADR, $27
	!byte SH , $2a, LIN, $2a, LIN, $2a, LIN, $2a
	!byte LIN, $2a, LIN, $2a, LIN, LIN, LIN, LIN
	!byte $2a, LIN, $2a, LIN, $2a, LIN, $2a, LIN
	!byte $2a, LIN, $2a, LIN, $2a, LIN, $2a, LIN
	!byte $2a, LIN, $2a, LIN, $2a, LIN, $2a, ADR
	!byte $72, $07, $1c, $0d, $19, $1b, $0f, $00
	!byte $00, $00, $00, $12, $13, $26, $1c, $0d
	!byte $19, $1b, $0f, ADR, $8a,SH+3, $1a, $16
	!byte $0b, $22, $0f, $1b, END
; $f3c2
StartScreenData:
	!byte $52, $04, $1a, $1e, $1c, $12, $00, $24
	!byte $10, $02, $25, $00, $19, $1b, ADR, $a2
	!byte SH , $0c, $1e, $1d, $1d, $19, $18, $00
	!byte $19, $18, $00, $14, $19, $22, $1c, $1d
	!byte $13, $0d, $15, ADR, $f2, SH , $1d, $19
	!byte $00, $1c, $1d, $0b, $1b, $1d, $28, ADR
	!byte $6a,SH+1, $24, $0b, $25, $26, $10, $13
	!byte $1b, $0f, ADR, $ba,SH+1, $24, $16, $25
	!byte $26, $16, $0f, $10, $1d, ADR, $0a,SH+2
	!byte $24, $27, $25, $26, $1b, $13, $11, $12
	!byte $1d, ADR, $5a,SH+2, $24, $1a, $25, $26
	!byte $1e, $1a, ADR, $aa,SH+2, $24, $28, $25
	!byte $26, $0e, $19, $20, $18, ADR, $69, SH
	!byte $26, $00, $1c, $0d, $19, $1b, $0f, $00
	!byte $26, ADR, $e7, SH , $02, $01, $01, $00
	!byte $1a, $1d, $1c, $28, ADR, $5f,SH+1, $03
	!byte $01, $01, $00, $1a, $1d, $1c, $28, ADR
	!byte $d7, $05, SH , $01, $01, $00, $1a, $1d
	!byte $1c, $28, ADR, $4f,SH+2, $05, $01, $01
	!byte $00, $1a, $1d, $1c, $28, ADR, $c3,SH+2
	!byte $17, $22, $1c, $1d, $0f, $1b, $22, $00
	!byte $1a, $1d, $1c, $28, ADR, $37,SH+3, $0f
	!byte $21, $1d, $1b, $0b, $00, $0c, $19, $18
	!byte $1e, $1c, ADR, $61,SH+3, $10, $19, $1b
	!byte $00, $03, $01, $01, $01, $01, $00, $1a
	!byte $1d, $1c, $28, ADR, $78,SH+3, $02, $0a
	!byte $09, $03, $00, $0c, $22, $00, $0d, $19
	!byte $17, $17, $19, $0e, $19, $1b, $0f, $00
	!byte $16, $1d, $0e, ADR, $a0,SH+3, $02, $0a
	!byte $09, $02, $00, $0c, $22, $00, $0c, $0b
	!byte $16, $16, $22, $23, $17, $13, $0e, $20
	!byte $0b, $22, END
; $f4bd
; **************************************** ZONE SPRITES *******************************************
!zone sprites ; sprite data $e5 - $fe
*= $f940
; $e5 worrior right
	!byte $00, $2a, $00, $00, $29, $00, $00, $2a
	!byte $00, $00, $2a, $00, $00, $2a, $00, $00
	!byte $08, $00, $02, $2a, $00, $02, $a9, $00
	!byte $02, $69, $08, $02, $69, $59, $02, $69
	!byte $48, $02, $a8, $00, $02, $28, $00, $00
	!byte $2a, $00, $00, $a2, $00, $00, $a2, $80
	!byte $02, $80, $80, $02, $80, $a0, $0a, $00
	!byte $30, $0a, $00, $30, $00, $00, $00, $00
; $e6 worrior left
	!byte $00, $0a, $80, $00, $06, $80, $00, $0a
	!byte $80, $00, $0a, $80, $00, $0a, $80, $00
	!byte $02, $00, $00, $0a, $88, $00, $06, $a8
	!byte $02, $06, $98, $06, $56, $98, $02, $16
	!byte $98, $00, $02, $a8, $00, $02, $88, $00
	!byte $0a, $80, $00, $08, $a0, $00, $28, $a0
	!byte $00, $20, $28, $00, $a0, $28, $00, $80
	!byte $0a, $02, $00, $0a, $00, $00, $00, $00
; $e7 worrior down
	!byte $00, $00, $01, $00, $00, $0a, $00, $00
	!byte $0a, $00, $0a, $8a, $00, $09, $a8, $00
	!byte $0a, $a8, $0a, $8a, $a8, $0a, $8a, $a0
	!byte $0a, $aa, $a0, $09, $aa, $80, $09, $85
	!byte $a8, $00, $85, $aa, $00, $01, $0a, $00
	!byte $01, $00, $00, $01, $00, $00, $0a, $00
	!byte $00, $0a, $00, $00, $01, $00, $00, $01
	!byte $00, $00, $01, $00, $00, $00, $00, $00
; $e8 worrior up
	!byte $00, $01, $00, $00, $01, $00, $00, $01
	!byte $00, $00, $0a, $00, $00, $0a, $00, $00
	!byte $01, $00, $00, $01, $02, $00, $01, $0a
	!byte $09, $85, $aa, $09, $85, $a8, $09, $aa
	!byte $80, $0a, $aa, $80, $0a, $8a, $a0, $0a
	!byte $8a, $a0, $00, $09, $a8, $00, $08, $a8
	!byte $00, $0a, $8a, $00, $00, $0a, $00, $00
	!byte $0a, $00, $00, $02, $00, $00, $00, $00
; $e9 monster 100 blue right
	!byte $00, $a0, $00, $00, $28, $00, $a0, $2a
	!byte $00, $a0, $ad, $80, $a0, $2d, $80, $a0
	!byte $20, $00, $20, $aa, $00, $28, $a8, $00
	!byte $08, $a8, $00, $08, $a8, $00, $08, $a8
	!byte $00, $08, $a8, $00, $0a, $a8, $00, $02
	!byte $aa, $80, $0a, $aa, $80, $0a, $02, $80
	!byte $08, $00, $a0, $28, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $ea monster 100 blue left
	!byte $00, $a0, $00, $02, $80, $00, $0a, $80
	!byte $a0, $2b, $a0, $a0, $2b, $80, $a0, $00
	!byte $80, $a0, $0a, $a0, $80, $02, $a2, $80
	!byte $02, $a2, $00, $02, $a2, $00, $02, $a2
	!byte $00, $02, $a2, $00, $02, $aa, $00, $02
	!byte $a8, $00, $02, $aa, $00, $28, $0a, $00
	!byte $20, $02, $00, $a0, $02, $80, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $eb monster 100 blue down
	!byte $0a, $00, $00, $0a, $00, $00, $0a, $a0
	!byte $08, $0a, $a0, $08, $00, $2a, $28, $00
	!byte $2a, $a2, $00, $02, $a0, $00, $02, $a0
	!byte $22, $2a, $a0, $22, $2a, $a0, $22, $2a
	!byte $80, $22, $2a, $80, $2a, $aa, $a0, $0c
	!byte $aa, $a0, $0c, $aa, $a8, $22, $80, $28
	!byte $08, $80, $08, $08, $80, $08, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $ec monster 100 blue up
	!byte $08, $80, $08, $08, $80, $08, $28, $80
	!byte $28, $38, $2a, $a8, $38, $2a, $a8, $2a
	!byte $aa, $a0, $2a, $aa, $80, $22, $2a, $80
	!byte $22, $2a, $a0, $22, $2a, $a0, $00, $02
	!byte $20, $00, $02, $20, $00, $2a, $28, $00
	!byte $2a, $28, $0a, $a0, $08, $0a, $a0, $08
	!byte $0a, $00, $00, $0a, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $ed monster 200 yellow right
	!byte $00, $00, $00, $00, $00, $00, $30, $0f
	!byte $c0, $30, $0d, $c0, $30, $3d, $f0, $30
	!byte $fd, $fc, $30, $ff, $fc, $30, $ff, $00
	!byte $30, $ff, $fc, $30, $ff, $f0, $33, $ff
	!byte $c0, $3f, $ff, $00, $3f, $ff, $00, $3f
	!byte $ff, $fc, $0f, $ff, $fc, $03, $ff, $cc
	!byte $03, $ff, $00, $03, $0f, $00, $0f, $03
	!byte $00, $3f, $03, $c0, $3f, $03, $c0, $00
; $ee monster 200 yellow left
	!byte $00, $00, $00, $00, $00, $00, $03, $e0
	!byte $0c, $03, $e0, $0c, $0d, $ec, $0c, $3d
	!byte $ff, $0c, $3f, $ff, $0c, $00, $ff, $0c
	!byte $3f, $ff, $0c, $0f, $ff, $0c, $03, $ff
	!byte $cc, $00, $ff, $fc, $00, $ff, $fc, $3f
	!byte $ff, $fc, $3f, $ff, $f0, $33, $ff, $c0
	!byte $00, $ff, $c0, $00, $f0, $c0, $00, $c0
	!byte $f0, $03, $c0, $fc, $03, $c0, $fc, $00
; $ef monster 200 yellow down
	!byte $00, $00, $00, $30, $00, $00, $3c, $3f
	!byte $f0, $3c, $30, $00, $0f, $30, $00, $0f
	!byte $fc, $00, $0f, $ff, $00, $03, $ff, $c0
	!byte $03, $ff, $c0, $03, $ff, $c0, $03, $ff
	!byte $f0, $0f, $ff, $f0, $0f, $ff, $70, $3f
	!byte $ff, $70, $3f, $3f, $f0, $33, $33, $f0
	!byte $30, $33, $f0, $00, $03, $c0, $00, $03
	!byte $c0, $00, $03, $c0, $00, $00, $00, $00
; $f0 monster 200 yellow up
	!byte $00, $00, $00, $0f, $00, $00, $0f, $00
	!byte $00, $0f, $00, $00, $3f, $30, $30, $3f
	!byte $33, $30, $3f, $f3, $f0, $37, $ff, $f0
	!byte $37, $ff, $c0, $3f, $ff, $c0, $3f, $ff
	!byte $00, $3f, $ff, $00, $0f, $ff, $00, $0f
	!byte $ff, $00, $03, $ff, $c0, $00, $ff, $c0
	!byte $00, $ff, $c0, $00, $33, $f0, $ff, $30
	!byte $f0, $00, $f0, $20, $00, $00, $00, $00
; $f1 monster 300 cyan right
	!byte $00, $00, $00, $0a, $80, $a8, $0a, $80
	!byte $a8, $08, $02, $80, $08, $02, $80, $08
	!byte $2a, $a0, $08, $2b, $e0, $2a, $ab, $e8
	!byte $2a, $aa, $a8, $2a, $a2, $a8, $2a, $a0
	!byte $00, $2a, $a0, $00, $2a, $aa, $a8, $2a
	!byte $aa, $a8, $2a, $aa, $a0, $2a, $aa, $a0
	!byte $08, $20, $80, $08, $20, $80, $08, $20
	!byte $80, $0a, $28, $a0, $0a, $28, $a0, $00
; $f2 monster 300 cyan left
	!byte $00, $00, $00, $2a, $02, $a0, $2a, $02
	!byte $a0, $02, $80, $20, $02, $80, $20, $0a
	!byte $a8, $20, $0b, $e8, $20, $2b, $ea, $a8
	!byte $2a, $aa, $a8, $2a, $8a, $a8, $00, $0a
	!byte $a8, $00, $0a, $a8, $2a, $aa, $a8, $2a
	!byte $aa, $a8, $0a, $aa, $a8, $0a, $aa, $a8
	!byte $02, $08, $20, $02, $08, $20, $02, $08
	!byte $20, $0a, $28, $a0, $0a, $28, $a0, $00
; $f3 monster 300 cyan down
	!byte $00, $28, $00, $00, $aa, $00, $00, $aa
	!byte $a8, $2a, $aa, $a8, $2a, $aa, $08, $20
	!byte $aa, $08, $00, $aa, $08, $00, $aa, $80
	!byte $2a, $aa, $80, $2a, $aa, $a0, $20, $a2
	!byte $a0, $20, $a2, $a0, $20, $a2, $e0, $00
	!byte $a2, $e0, $00, $a2, $e8, $2a, $a2, $e8
	!byte $2a, $a2, $a8, $20, $a2, $88, $20, $a2
	!byte $88, $00, $22, $08, $00, $22, $08, $00
; $f4 monster 300 cyan up
	!byte $20, $88, $00, $20, $88, $00, $22, $8a
	!byte $08, $22, $8a, $08, $2a, $8a, $a8, $2b
	!byte $8a, $a8, $2b, $8a, $00, $0b, $8a, $00
	!byte $0b, $8a, $08, $0a, $8a, $08, $0a, $aa
	!byte $a8, $0a, $aa, $a8, $0a, $aa, $00, $20
	!byte $aa, $00, $20, $aa, $08, $20, $aa, $08
	!byte $20, $aa, $a8, $2a, $aa, $a8, $2a, $aa
	!byte $00, $00, $aa, $00, $00, $28, $00, $00
; $f5 monster 400 yellow uni
	!byte $00, $00, $00, $0c, $00, $30, $03, $00
	!byte $c0, $00, $c3, $00, $00, $41, $00, $01
	!byte $55, $40, $33, $69, $cc, $3f, $69, $fc
	!byte $3f, $aa, $fc, $3f, $95, $fc, $3f, $95
	!byte $fc, $3f, $95, $fc, $0f, $95, $f0, $03
	!byte $aa, $c0, $03, $aa, $c0, $00, $aa, $00
	!byte $00, $aa, $00, $02, $aa, $80, $02, $aa
	!byte $80, $2a, $00, $a8, $2a, $00, $a8, $00
; $f6 explosion 1
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $30, $00, $00, $3c, $00
	!byte $03, $3c, $00, $03, $ff, $c0, $00, $ff
	!byte $00, $00, $ff, $00, $00, $ff, $c0, $03
	!byte $ff, $c0, $0f, $ff, $c0, $0f, $ff, $f0
	!byte $03, $0f, $c0, $00, $00, $c0, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $f7 explosion 2
	!byte $00, $00, $00, $00, $28, $00, $20, $28
	!byte $20, $00, $2a, $00, $0c, $aa, $80, $00
	!byte $aa, $a8, $02, $be, $a8, $2a, $bf, $a0
	!byte $2a, $be, $80, $2f, $fe, $80, $2b, $ff
	!byte $8c, $0b, $ff, $80, $0a, $ff, $a0, $02
	!byte $ff, $e0, $02, $fa, $e8, $32, $ea, $b8
	!byte $02, $a0, $a8, $0a, $80, $28, $0a, $00
	!byte $08, $28, $08, $00, $20, $00, $00, $00
; $f8 explosion 3
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $0c, $20, $00, $03, $00, $c0, $00
	!byte $03, $00, $00, $0c, $0c, $02, $00, $30
	!byte $00, $80, $c0, $00, $00, $00, $00, $08
	!byte $00, $28, $00, $30, $00, $20, $00, $00
	!byte $c2, $00, $00, $00, $a0, $08, $00, $0c
	!byte $00, $30, $00, $00, $c2, $00, $03, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $f9 mystery right
	!byte $00, $08, $00, $00, $28, $00, $00, $2c
	!byte $00, $00, $2c, $00, $00, $2c, $00, $00
	!byte $2c, $00, $00, $28, $00, $00, $aa, $50
	!byte $00, $aa, $40, $00, $a8, $00, $00, $a8
	!byte $00, $00, $a0, $00, $00, $a0, $00, $00
	!byte $a0, $00, $00, $a0, $00, $00, $a8, $00
	!byte $00, $a8, $00, $00, $a8, $00, $00, $aa
	!byte $00, $02, $aa, $80, $02, $aa, $80, $00
; $fa mystery left
	!byte $00, $20, $00, $00, $28, $00, $00, $38
	!byte $00, $00, $38, $00, $00, $38, $00, $00
	!byte $38, $00, $00, $28, $00, $05, $aa, $00
	!byte $01, $aa, $00, $00, $2a, $00, $00, $2a
	!byte $00, $00, $0a, $00, $00, $0a, $00, $00
	!byte $0a, $00, $00, $0a, $00, $00, $2a, $00
	!byte $00, $2a, $00, $00, $2a, $00, $00, $aa
	!byte $00, $02, $aa, $80, $02, $aa, $80, $00
; $fb mystery down
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $20, $00, $00, $20
	!byte $00, $00, $2a, $a8, $00, $2a, $aa, $80
	!byte $2a, $aa, $a0, $2a, $aa, $a8, $2a, $2a
	!byte $a8, $28, $0a, $f8, $28, $02, $30, $20
	!byte $02, $00, $20, $02, $00, $20, $01, $00
	!byte $00, $01, $00, $00, $01, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $fc mystery up
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $40, $00, $00, $40, $00, $00
	!byte $40, $08, $00, $80, $08, $00, $80, $08
	!byte $0c, $80, $28, $2f, $a0, $28, $2a, $a8
	!byte $a8, $2a, $aa, $a8, $0a, $aa, $a8, $02
	!byte $aa, $a8, $00, $2a, $a8, $00, $00, $08
	!byte $00, $00, $08, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $fd shot horizontal
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $ff, $00, $00, $ff
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $fe shot vertical
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $03, $00, $00, $03, $00, $00, $03
	!byte $00, $00, $03, $00, $00, $03, $00, $00
	!byte $03, $00, $00, $03, $00, $00, $03, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $ffc0
; **************************************** ZONE VECTORS *******************************************
; $fffa Hardware vectors
!zone vectors
*= $fffa
	!word start
	!word start
	!word Interrupt