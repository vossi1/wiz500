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
!addr coll1_x		= $09		; 16bit pointer
!addr ptr2		= $0b		; 16bit pointer
!addr temp1		= $0d
!addr temp2		= $0e
; collision usage
!addr coll1_x		= $09
!addr coll1_y		= $0b
!addr coll2_x		= $0c
!addr coll2_y		= $0e

!addr temp3		= $0f
!addr temp4		= $10
!addr temp5		= $11
!addr data_ctr		= $12		; data counter
!addr timer		= $13		; game timer - inc with every irq / CIA timer b
!addr color		= $14
!addr sprite_state	= $1e		; 8 bytes state: $ff=off, $00=on, $80-$d0 player explosion
					; start-screen: monster direction
!addr finished		= $26		; sum of all targets to go: 0 = level finished
!addr ttarget		= $27		; targets
!addr players		= $28		; lives
!addr state		= $29		; state 0=new level, neg.=player died
!addr timer2		= $2a
!addr delay		= $2b		; delay for monster movment on satrt screen
!addr fire		= $2c		; fire pressed bit#7=1
!addr sprite_xreg	= $30		; actual sprite x register
!addr sprite_xmsb	= $31		; actual sprite x-msb
!addr sprite_dir	= $32		; actual sprite direction 1-4
!addr draw_ptr		= $39		; pointer to print maze on screen
!addr draw_column	= $36		; draw column
!addr draw_line		= $37		; draw line
!addr draw_char		= $38		; draw char/tile
!addr level		= $3b		; level 1-4
!addr maze_column	= $3c		; actual maze column
!addr maze_line		= $3d		; actual maze line
!addr move_x		= $3f		; sprite mov sub x
!addr move_y		= $40		; sprite mov sub y
!addr move_xmsb		= $41		; sprite mov sub xmsb
!addr mazedata_ptr	= $44		; pointer to mazedata
!addr worrior_dir	= $46		; worrior direction after joy/key check
!addr target		= $47		; 4 bytes targets to destroy
!addr move		= $4c		; movement 0=none, 1=up, 2=down, 3=left, 4=right
!addr sprite_data	= $4d		; 8 bytes sprite data
!addr collision_mob	= $55		; mob-mob collision
!addr collision_bgr	= $56		; mob-bgr collision
!addr monster_value	= $57		; 5 bytes monster values
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
viclp:	lda VICRegs,x
	sta VIC64+MOBX,x
	dex
	bpl viclp
	ldx #$18			; init sid regs
sidlp:	lda SIDRegs,x
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
	jsr SetupGame			; increase + init new level, state=0
TryAgain:
	jsr SetupGameScreen		; draw game screen
	jsr SetupWorrior		; setup player sprite
	lda #$1f
	sta SID64+MODVOL		; full volume, filter low pass
	ldx #1
	lda state
	bpl newlev			; start with sound 1, if new level (state=0)
	ldx #2				; if not, start with sound 2
newlev:	txa
	jsr PlaySound
GameLoop:
	lda timer
	bne GameLoop			; wait 1 inc of timer
	jsr CopySpritePointer		; copies all sprites data pointers to the vic pointers
	lda VIC64+MOBMOB
	sta collision_mob		; save mob-mob collision
	lda VIC64+MOBBGR
	sta collision_bgr		; save mob-bgr collision
	inc timer2			; inc timer2
	lda timer2
	bne nosound			; skip if not time for sound
	inc delay			; inc delay
	lda #$05
	cmp delay
	bne nosound			; skip if not time for sound
	lda #2
	jsr PlaySound			; Play sound 2
	dec state			; state=neg for sound 2 
nosound:lda delay
	cmp #$10
	bcc skipmm
	jsr MoveMonsters		; move monsters
skipmm:	jsr WorriorShot			; check and move worrior shot
	jsr MoveWorrior			; move worrior
	jsr MoveMonsters		; move monsters
	jsr StartMonsters		; check if monsters are off and start new monsters
	jsr MonsterShot			; start monster shot ?
	jsr MoveMonsterShot		; move monster shot and disable if touching wall
	jsr CheckCollision		; check for hits
	jsr CheckExplosions		; Explosions: switch patterns, sound, score, finished ? 
	jsr UpdateSound			; update game sound
	lda sprite_state
	cmp #$ff
	beq decwor			; branch if player sprite off (dead)
	lda finished			; level finished ? (0=finished)
	bne GameLoop			; branch if not

	lda sprite_state
	bmi decwor			; branch if player sprite off (dead)
	jmp LevelFinished		; level finished

decwor:	dec players			; decrease lives
	bne TryAgain			; next try if live > 0

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
jkx:	stx worrior_dir				; store move direction
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
	jsr UpdateSound
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
	stx coll1_x
	sty coll1_x+1

scrnewt: ldy #0				; set pointer1 to new target
	sty temp2
	lda (coll1_x),y
	sta ptr2
	iny
	lda (coll1_x),y
	sta ptr2+1

scrcplp:iny
	lda (coll1_x),y			; load data byte
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
	adc coll1_x			; add counter .y to pointer 1
	sta coll1_x
	lda coll1_x+1
	adc #$00
	sta coll1_x+1
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
	stx coll1_x
	sty coll1_x+1
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
; $e2f5 Inits some vars and sprite colors
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
; $e310 Setup game states, targets
SetupGame:
	ldx #$00
	stx VIC64+MOBENA		; disable sprites

sg1elp:	lda #$ff
	sta sprite_state,x		; clear state for all sprites
	inx
	cpx #8
	bne sg1elp

	ldy #$00			; clear some vars
	sty delay
	sty timer2
	sty state

	inc level			; raise level (1-4)
	ldx level
	cpx #4				; maximum 4
	bcc levmax4
	ldx #4
levmax4:lda TargetTable-1,x		; setup targets
	sta ttarget
	lda TargetTable-1+4,x
	sta target
	lda TargetTable-1+8,x
	sta target+1
	lda TargetTable-1+12,x
	sta target+2
	lda TargetTable-1+16,x
	sta target+3
	lda TargetTable-1+20,x
	sta finished
	lda #$00
	sta $4b
	rts
; -------------------------------------------------------------------------------------------------
; targets 1-4
TargetTable:
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
	stx sprite_state+7
	stx sprite_state+6
	inx
	stx sprite_state
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
	stx coll1_x
	sty coll1_x+1
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
	stx coll1_x
	sty coll1_x+1
	ldx #0				; print score

PrintScore:
	ldy #5
pslp:	lda score,x
	and #$0f			; clear hinibble
	clc
	adc #1
	sta (coll1_x),y
	dey
	lda score,x
	lsr
	lsr
	lsr
	lsr
	clc
	adc #1
	sta (coll1_x),y
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
; $e474 start screen sub
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
	sta sprite_state+1,x		; set right direction = $d7
	sta VIC64+MOBX+2,y		; setup monsters sprites 1-5
	lda StartScreenMonsterVpos,x
	sta VIC64+MOBY+2,y
	lda StartScreenMonsterData,x
	sta SpritePointer+1,x
	lda MonsterColorTable,x
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
	lda sprite_state+1,y
	beq ssleft
	inc VIC64+MOBX+2,x		; move monsters right
	lda VIC64+MOBX+2,x
	cmp StartScreenMonsterRLimit,y	; check if right limit
	bcc ssnxspr
	lda #$00
	sta sprite_state+1,y		; set left direction
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
	sta sprite_state+1,y		; set right dir
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
; $e51a start screen tables
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
; $e535 move worrior sprite
MoveWorrior:
	lda timer2
	and #$01
	beq +
	rts
; $e53c
+	lda sprite_state
	bpl +
	rts
; $e541
+	jsr CheckJoyKey			; check movement
	lda #$00
	sta sprite_xreg			; store xregs for worrior sprite
	lda #$01
	sta sprite_xmsb
	lda worrior_dir
	sta sprite_dir
	lda $2e
	sta $35
	jsr MoveSprite
	lda $35
	sta $2e
	ldx sprite_dir
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
; $e56f move sprite (sprite_xreg, _xmsb, _dir)
MoveSprite:
	ldx sprite_xreg
	lda #$00
	sta coll1_x
	jsr le65d
	lda sprite_xmsb
	sta move_xmsb
	lda VIC64+MOBX,x
	sta move_x
	lda VIC64+MOBY,x
	sta move_y
	jsr CalcScreenPosition		; calc sprite screen position (char based)
	lda sprite_dir
	bne msdirok
	jmp le6b3
msdirok:lda $35
	cmp #$03
	bcs le5ca
	lda sprite_dir
	cmp #$03
	bcc le601
	dec move_y
	lda move_y
le5a0:  beq le5b6
	cmp #$01
	beq le5b6
	cmp #$ff
	beq le5b6
	cmp #$00
	bcc le5c3
	inc coll1_x
	sec
	sbc #$03
	jmp le5a0
le5b6:  ldx coll1_x
	lda Table14+13,x
	ldx sprite_xreg
	sta VIC64+MOBY,x
	jmp le601
le5c3:  lda #$00
	sta sprite_dir
	jmp le6b3
le5ca:  lda sprite_dir
	cmp #$03
	bcs le601
	lda move_x
	sec
	sbc #$01
le5d5:  beq le5eb
	cmp #$01
	beq le5eb
	cmp #$ff
	beq le5eb
	cmp #$00
	bcc le5c3
	inc coll1_x
	sec
	sbc #$03
	jmp le5d5
le5eb:  ldx coll1_x
	cpx #$0a
	bne le5f9
	lda sprite_xmsb
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
le5f9:  lda Table14,x
	ldx sprite_xreg
	sta VIC64+MOBX,x
le601:  lda sprite_dir
	sta $35
	ldx sprite_xreg
	lda sprite_dir
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
	lda sprite_xmsb
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
	lda sprite_xmsb
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
	jmp le6b3
le65d:  lda VIC64+MOBX,x
	sta move_x
	lda VIC64+MOBY,x
	clc
	sbc #$06
	sta move_y
	lda sprite_xmsb
	sta move_xmsb
	jsr CalcScreenPosition
	lda sprite_dir
	beq le6b2
	cmp #$03
	bcc le685
	beq le680
	inc move_x
	jmp le690
le680:  dec move_x
	jmp le690
le685:  cmp #$01
	bne le68e
	dec move_y
	jmp le690
le68e:  inc move_y
le690:  lda #$04
	sta draw_ptr+1
	lda move_x
le696:  ldy move_y
	beq le6a6
	dec move_y
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
	sta sprite_dir
le6b2:  rts
; $e6b3
le6b3:  lda sprite_xmsb
	and VIC64+MOBMSB
	bne le6cf
	lda VIC64+MOBX,x
	cmp #$16
	bcs le6e7
	lda sprite_xmsb
	ora VIC64+MOBMSB
	sta VIC64+MOBMSB
	lda #$40
	sta VIC64+MOBX,x
	rts
; $e6cf
le6cf:  beq le6e7
	lda VIC64+MOBX,x
	cmp #$42
	bcc le6e7
	lda sprite_xmsb
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
; e709 Calc sprite screen position
CalcScreenPosition:
	lda move_y
	sec
	sbc #38				; sub upper offset
	lsr				; ...and divide by 8
	lsr
	lsr
	sta move_y			; store screen y
	lda move_x
	sec
	sbc #11				; sub left offset
	bcs cspmsb
	clc
	bcc cspdivx
cspmsb:	pha				; remember x
	lda move_xmsb
	and VIC64+MOBMSB		; load and isolate x-msb
	clc
	beq cspnmsb			; skip if msb not set
	sec				; set c
cspnmsb:pla
cspdivx:ror				; divide by 8 (with carry if msb set)
	lsr
	lsr
	sta move_x			; store screen x
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
; $e754 Move Worrior Shot
WorriorShot:
	lda sprite_state+7
	bpl +
	jmp le7fc
+	lda collision_bgr
	and #$80
	beq +
colllp:	lda #$ff
	sta sprite_state+7
	lda #$7f
	and VIC64+MOBENA
	sta VIC64+MOBENA
	lda #$7f
	and VIC64+MOBMSB
	sta VIC64+MOBMSB
	rts
;$ e776
+	lda VIC64+MOBMSB
	and #$80
	bne le786
	lda VIC64+MOBX+14
	cmp #$14
	bcc colllp

	bcs le78d
le786:  lda VIC64+MOBX+14
	cmp #$42
	bcs colllp
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
le7f8:  jmp colllp
le7fb:  rts
; $e7fc
le7fc:  cmp #$ff
	bne +
	lda sprite_state
	bpl chkshot
+	rts
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
	sta sprite_state+7
	rts
; -------------------------------------------------------------------------------------------------
; $e855 MoveMonsters
MoveMonsters:
	lda state
	bne +
	lda timer2
	and #$01
	bne +
	rts
; $e860
+	lda sprite_state
	bpl +
	rts
; $e865
+	ldx #1
	stx temp4
-	lda sprite_state,x
	bpl +
mmloop:	inc temp4
	ldx temp4
	cpx #6
	bne -
	rts
; $e876
+	lda SpritePosTable,x
	sta sprite_xreg
	lda BitTable,x
	sta sprite_xmsb
	dec $5f,x
	dec $5f,x
	bmi le88b
	lda $67,x
	jmp le8e8
le88b:  lda #$17
	sta $5f,x
	ldx sprite_xreg
	lda SID64+RANDOM
	and #$01
	beq le8cb
	lda BitTable,x
	ora #$01
	and VIC64+MOBMSB
	beq le8ab
	cmp #$01
	beq le8b3
	cmp BitTable,x
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
le8e8:  ldx temp4
	sta sprite_dir
	lda $67,x
	sta $35
	jsr MoveSprite
	lda sprite_dir
	bne le8fa
	jsr le912
le8fa:  ldx temp4
	lda $35
	sta $67,x
	clc
	adc monster_value-1,x
	tax
	lda Table17,x
	bne le90b
	beq le90f
le90b:  ldx temp4
	sta sprite_data,x
le90f:  jmp mmloop
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
SpritePosTable:
	!byte $00, $02, $04, $06, $08, $0a, $0c, $0e
; $e92b
Table17:
	!byte $ec, $ec, $eb, $ea, $e9, $f0, $f0, $ef
	!byte $ee, $ed, $f4, $f4, $f3, $f2, $f1, $f5
	!byte $f5, $f5, $f5, $f5, $fc, $fc, $fb, $fa
	!byte $f9
; -------------------------------------------------------------------------------------------------
; $e944 check monster state and start new monster if needed
StartMonsters:
	lda sprite_state
	bmi smx
	lda timer2
	and #$1f
	bne smx
	ldx #1
smchklp:lda sprite_state,x
	cmp #$ff
	beq le95c
	inx
	cpx #6
	bne smchklp
smx:	rts
; $e95c
le95c:  ldy #$00
	lda ttarget
	beq le967
	dec ttarget
	jmp le9ab
le967:  iny
	lda target
	beq le971
	dec target
	jmp le9ab
le971:  iny
	lda target+1
	beq le97b
	dec target+1
	jmp le9ab
le97b:  iny
	ldx #1
	lda #$ff
le980:  and sprite_state,x
	and sprite_state+1,x
	inx
	inx
	cpx #7
	bne le980
	cmp #$ff
	bne smx
	ldx #1
	lda target+2
	beq le999
	dec target+2
	jmp le9a7
le999:  iny
	lda target+3
	beq smx
	lda #3
	jsr PlaySound
	dec target+3
	dec $4b
le9a7:  lda #1
	sta state
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
	sta VIC64+MOBX,y
	lda timer2
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
	inc sprite_state,x
	lda MonsterValueTable,y
	sta monster_value-1,x
	lda MonsterTypeTable,y
	sta sprite_data,x
	sta SpritePointer,x
	lda MonsterColorTable,y
	sta VIC64+MOBCOL,x
	lda BitTable,x
	ora VIC64+MOBENA
	sta VIC64+MOBENA
	rts
; -------------------------------------------------------------------------------------------------
; $ea08
MonsterValueTable:  
	!byte 0, 5, 10, 15, 20
MonsterColorTable:
	!byte BLUE, YELLOW, CYAN, LIGHTGREEN, MAGENTA
MonsterTypeTable:
	!byte $ec, $f0, $f4, $f5, $fc
; -------------------------------------------------------------------------------------------------
; $ea17 Explosions: switch patterns, sound, score, finished ? 
CheckExplosions:
	ldx #7				; all sprites
exloop:	lda sprite_state,x
	bmi exspdis			; branch if state neg
exnext:	dex
	bpl exloop			; next sprite
	rts
; $ea21
exspdis:cmp #$ff
	beq exnext			; next sprite, if off

	inc sprite_state,x
	txa
	bne exnotpl			; branch if not worrior
; player sprite, state neg.
	lda sprite_state,x
	cmp #$81
	bne expl1			; branch if explosion in progress
; start player explosion
	lda #$f6
	sta sprite_data			; explosion pattern 1
	lda #6
	jsr PlaySound			; start explosion sound 6
	jmp exnext			; switch next sprite pattern

expl1:	cmp #$90			; explosion movie ;)
	bne expl2
	lda #$f7
	sta sprite_data
	jmp exnext

expl2:	cmp #$a0
	bne expl3
	lda #$f6
	sta sprite_data
	jmp exnext

expl3:	cmp #$b0
	bne expl4
	lda #$f7
	sta sprite_data
	jmp exnext

expl4:	cmp #$c0
	bne expl5
	lda #$f8
	sta sprite_data
	jmp exnext

expl5:	cmp #$d0
	bne explx
	lda VIC64+MOBENA
	and #$ff-1
	sta VIC64+MOBENA		; disable player sprite
explx:	jmp exnext
; state neg, but not player sprite
exnotpl:lda sprite_state,x
	cmp #$81
	bne exmon1			; branch if explosion in progress
	lda #$f6
	sta sprite_data,x		; explosion pattern 1
	cpx #6
	beq spshot			; no score for shot-sprites
	lda monster_value-1,x		; load monster value
	lsr
	lsr
	and #$07			; calc final value
	tay
	lda ScoreTablex100,y		; load monster score
	cmp #5
	bne spadsco			; skip if not max score 5
	lda SID64+RANDOM
	and #$30			; calc special score
	clc
	adc #$10
spadsco:jsr AddScore			; add and print score
spshot:	lda #5
	jsr PlaySound			; start explosion sound 5
	jmp exnext			; switch next sprite pattern

exmon1:	cmp #$90
	bne exmon2
	lda #$f7
	sta sprite_data,x
	jmp exnext

exmon2:	cmp #$a0
	bne exmon3
	lda #$f8
	sta sprite_data,x
	jmp exnext

exmon3:	cmp #$b0
	bne exmon4
	lda BitTable,x			; load monster bit
	eor #$ff
	and VIC64+MOBENA
	sta VIC64+MOBENA		; disable monster sprite
	jmp exnext
; check if level finished - all targets destroyed
exmon4:	cpx #6
	beq notfin			; skip finished check, if shot sprite
	cmp #$fe
	bne notfin
	lda ttarget
	ora target
	ora target+1
	ora target+2
	ora target+3
	bne notfin
	sta finished			; store finished var (0=finished)
notfin:	jmp exnext
; -------------------------------------------------------------------------------------------------
; $eae7 Monster scores (x100)
ScoreTablex100:  
	!byte 1, 2, 3, 4, 5, 5, 5, 5
; -------------------------------------------------------------------------------------------------
; $eaef check collision
CheckCollision:
	lda sprite_state
	bmi ccmonst			; skip if player off
	lda collision_mob
	and #$01			; isolate collison player bit
	beq ccmonst			; skip if not player
	lda collision_mob
	and #$80
	bne ccx				; exit if collision with player shot
	ldx #0
; check collision partner
ccchklp:inx				; start with monster sprite 1
	cpx #7
	beq ccmonst			; branch if player shot to check monster hit
	lda sprite_state,x
	bmi ccchklp			; next sprite, if off
	lda VIC64+MOBX
	sta coll1_x			; store player x in coll1_x
	lda #$00
	sta coll1_x+1
	lda #$01			; player sprite
	and VIC64+MOBMSB		; isolate x msb bit
	beq +
	inc coll1_x+1			; inc coll1_x hi if msb set
+	lda VIC64+MOBY
	sta coll1_y			; store player y to coll1_y
	jsr Collision
	bcc ccchklp			; next sprite if c=0
	lda #$80
	sta sprite_state		; store state=$80 player shot down
	rts
; $eb2b check monster/monster-shot hit
ccmonst:lda sprite_state+7
	bmi ccx				; player shot active ?
	lda #$00
	sta coll1_x+1
	lda #$80			; player shot sprite
	and VIC64+MOBMSB		; isolate x msb
	beq +
	inc coll1_x+1			; inc coll1_x hi if msb set
+	lda VIC64+MOBX+14
	sta coll1_x			; store player shot x in coll1_x
	lda VIC64+MOBY+14
	sta coll1_y			; store player shot y in coll1_y
	lda collision_mob
	and #$80
	beq ccx				; exit if not collision with player shot
	ldx #$01			; check all monsters
ccmonlp:lda sprite_state,x
	bmi ccnxmon			; skip if monster off
	jsr Collision
	bcc ccnxmon			; next monster if c=0
	lda #$ff
	sta sprite_state+7		; reset player shot state
	lda #$7f
	and VIC64+MOBENA
	sta VIC64+MOBENA		; disable player shot sprite 7
	lda #$80
	sta sprite_state,x		; set player shot state to $80=dead
	jmp ccx				; exit
ccnxmon:inx
	cpx #7
	bne ccmonlp			; next monster/monster-shot
ccx:	rts
; -------------------------------------------------------------------------------------------------
; $
BitTable:  
	!byte $01, $02, $04, $08, $10, $20, $40, $80
; -------------------------------------------------------------------------------------------------
; $eb78 x=sprite, coll1_x, coll1_y = partners
Collision:
	lda collision_mob
	and BitTable,x			; isolate collision sprite bit
	bne ccoll
	jmp collx0			; exit if no collision
ccoll:  txa
	asl				; x2 calc x-reg
	tay
	lda VIC64+MOBX,y		; store x to coll2
	sta coll2_x
	lda VIC64+MOBY,y
	sta coll2_y			; store y to coll2
	lda #$00
	sta coll2_x+1
	lda VIC64+MOBMSB
leb96:  and BitTable,x			; isolate x-msb
	beq +
	inc coll2_x+1			; inc coll2 x-hi
+	lda #14
	sta temp3
	lda #14
	sta temp4
	lda #11
	sta temp5
	lda #23
	sta data_ctr
	jsr cchkhit
	bcc collx
	lda #$80
	sta sprite_state,x		; set status dead
	bne collx
collx0:	clc				; exit no hit c=0
collx:  rts
; $ebba check hit
cchkhit:sec
	lda coll2_y
	sbc temp3
	cmp coll1_y
	bcs collx00
	clc
	lda coll2_y
	adc temp4
	cmp coll1_y
	bcc collx00
	sec
	lda coll2_x
	sbc temp5
	sta coll2_x
	lda coll2_x+1
	sbc #$00
	sta coll2_x+1
	sec
	lda coll2_x
	sbc coll1_x
	lda coll2_x+1
	sbc coll1_x+1
	bcs collx00
	lda coll2_x
	adc data_ctr
	sta coll2_x
	lda coll2_x+1
	adc #$00
	sta coll2_x+1
	sec
	lda coll2_x
	sbc coll1_x
	lda coll2_x+1
	sbc coll1_x+1
	bcc collx00
	rts				; return hit with c=1

collx00:clc
	rts				; return no hit c=0
; -------------------------------------------------------------------------------------------------
; $ebfe start monster shot
MonsterShot:
	lda sprite_state
	bpl +				; branch if player sprite on
	rts
; $ec03
+	lda sprite_state+6		; branch if monster shot on
	bne +
	rts
; $ec08
+	lda $4b
	bmi +
	lda timer2
	and #$0f
	beq +
	rts
; $
+	ldx #1				; check all monsters
-	lda sprite_state,x
	bpl +				; branch if monster on
lec19:  inx
	cpx #6
	bne -				; neext monster
	rts
; $ec1f
+	txa
	asl
	tay
	lda VIC64+MOBX
	cmp VIC64+MOBX,y
	beq lec65
	lda VIC64+MOBY
	cmp VIC64+MOBY,y
	bne lec19
	lda VIC64+MOBX
	cmp VIC64+MOBX,y
	lda VIC64+MOBMSB
	and #$01
	bne lec49
	lda VIC64+MOBMSB
	and BitTable,x
	bne lec51
	beq lec57
lec49:  lda VIC64+MOBMSB
	and BitTable,x
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
	sta sprite_state+6
	lda VIC64+MOBX,y
	sta VIC64+MOBX+12
	lda VIC64+MOBY,y
	sta VIC64+MOBY+12
	lda VIC64+MOBMSB
	and BitTable,x
	beq lec96
	lda VIC64+MOBMSB
	ora #$40
	bne lec9b
lec96:  lda VIC64+MOBMSB
	and #$bf
lec9b:  sta VIC64+MOBMSB
	lda VIC64+MOBENA
	ora #$40
	sta VIC64+MOBENA		; enable monster shot
	lda $77
	sta $67,x
	rts
; -------------------------------------------------------------------------------------------------
; $ecab move monster shot and disable if reaching wall
MoveMonsterShot:
	lda sprite_state+6
	bpl msactiv			; branch if monster shot on
	rts
; $ecb0 disable monster shot if reaching background
msactiv:lda collision_bgr
	and #$40			; isloate monster shot collision bit
	beq +				; branch if no bgr-collision
; disable monster shot
msdisab:lda #$ff
	sta sprite_state+6		; disable monster shot
	lda VIC64+MOBENA
	and #$bf
	sta VIC64+MOBENA		; disable sprite 6
	rts
; -------------------------------------------------------------------------------------------------
; $ecc3 move monster shot
+	lda VIC64+MOBMSB
	and #$40
	bne +
	lda VIC64+MOBX+12
	cmp #$14
	bcc msdisab
	bcs lecda
+	lda VIC64+MOBX+12
	cmp #$42
	bcs msdisab
lecda:  lda $77
	cmp #$03
	bcc led0e
	bne +
	dec VIC64+MOBX+12
	dec VIC64+MOBX+12
	lda VIC64+MOBX+12
	cmp #$fe
	bcc led2d
	lda VIC64+MOBMSB
	and #$bf
	sta VIC64+MOBMSB
	rts
; $ecf8
+	inc VIC64+MOBX+12
	inc VIC64+MOBX+12
	lda VIC64+MOBX+12
	cmp #$02
	bcs led2d
	lda VIC64+MOBMSB
	ora #$40
	sta VIC64+MOBMSB
	rts
; $
led0e:  cmp #$01
	bne +
	dec VIC64+MOBY+12
	dec VIC64+MOBY+12
	lda VIC64+MOBY+12
	cmp #$2a
	bcc ++
	rts
; $ed20
+	inc VIC64+MOBY+12
	inc VIC64+MOBY+12
	lda VIC64+MOBY+12
	cmp #$ca
	bcs ++
led2d:  rts

++	jmp msdisab
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
; $ed8b Player explosion
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
; $edc3 update game sound
UpdateSound:
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