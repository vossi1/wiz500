; Disassembled by Vossi 05/2020
; Prepared for ACME reassembling
; Comments, Labels by Vossi 06/2020
; Converted for P500 by Vossi 06/2020
!cpu 6502
; switches
!ct pet
!to "wiz500.prg", cbm
!cpu 6502
; ########################################### TODO ################################################
;
;
; ########################################### BUGS ################################################
; sometimes the 'worlok' doesn't appears and you stuck without monster ;)
; ######################################### P500 MODS #############################################
; Indirect reg is always = 15, all other indirect operations with ora(),y lda(,x) sta(,x)
; ******************************************* INFO ************************************************
; ESC in start screen enables 'never die' cheat - only allowed for testing the levels !!!
; ***************************************** CONSTANTS *********************************************
FILL		= $aa	; fills free memory areas with $aa
NOPCODE		= $ea	; nop instruction for fill
GAMEBANK	= 0	; Game code bank
SYSTEMBANK	= $f	; systembank

LIVES		= 3	; start lives
; directions
WALL		= 0
UP		= 1
DOWN		= 2
LEFT		= 3
RIGHT		= 4
; table codes
ADR		= $fe	; new target address follows
LIN		= $fd	; new line (target +40)
END		= $ff	; end of data
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
; ***************************************** REGISTER **********************************************
; vic
MOBX		= $00	; mob pos
MOBY		= $01
MOBMSB		= $10
MODEY		= $11	; mode
RASTER		= $12
MOBENA		= $15
MCMCSX		= $16
MOBYEX		= $17
MEMPT		= $18
IRQ		= $19	; irq
EIRQ		= $1a
MOBPRI		= $1b	; mob
MOBMC		= $1c
MOBXEX		= $1d
MOBMOB		= $1e
MOBBGR		= $1f
EXTCOL		= $20	; colors
BGRCOL		= $21
BGRCO1		= $22
BGRCO2		= $23
BGRCO3		= $24
MOBMC0		= $25
MOBMC1		= $26
MOBCOL		= $27
; sid
V1LO		= $00	; osc1
V1HI		= $01
V1CTRL		= $04
V1AD		= $05
V1SR		= $06
V2LO		= $07	; osc2
V2HI		= $08
V2CTRL		= $0b
V2AD		= $0c
V2SR		= $0d
V3LO		= $0e	; osc3
V3HI		= $0f
V3CTRL		= $12
V3AD		= $13
V3SR		= $14
FCLO		= $15	; filter
FCHI		= $16
RESFIL		= $17
MODVOL		= $18	; mode / volume
RANDOM		= $1b	; osc3 out
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
; tpi
PA		= $0	; Port register A
PB		= $1	; Port register B
PC		= $2	; Port register C
LIR		= $2	; Interrupt latch register mc=1
DDPA		= $3	; Data direction register A
DDPB		= $4	; Data direction register B
DDPC		= $5	; Data direction register C
MIR		= $5	; Interrupt mask register mc=1
CREG		= $6	; Control reg: #0 mc=IRQ mode / #1 ip= IRQ parity / #2-3 edge i3,i4	
AIR		= $7	; Active interrupt register
; ************************************** P500 ADDRESSES *******************************************
!addr CodeBank          = $00		; code bank register
!addr IndirectBank      = $01		; indirect bank register
!addr ScreenRAMbase	= $c400		; screen matrix
!addr SpritePointer	= $c7f8		; sprite data pointer
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
; ***************************************** ZERO PAGE *********************************************
!addr score		= $02		; 3bytes score
!addr highscore		= $05		; 3bytes highscore
!addr key		= $08		; pressed key/joystick
!addr ptr1		= $09		; 16bit pointer
!addr ptr2		= $0b		; 16bit pointer
!addr temp1		= $0d		; temporary screen copy, debounce, indirect adaptation
!addr temp2		= $0e		; temporary screen copy, indirect adaptation
; double usage: collision
!addr coll1_x		= $09
!addr coll1_y		= $0b
!addr coll2_x		= $0c
!addr coll2_y		= $0e

!addr temp3		= $0f		; temporary collision
!addr temp4		= $10		; temporary collision, move monster
!addr temp5		= $11		; temporary collision
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
!addr worrior_shot_dir	= $2d
!addr worrior_dir	= $2e
!addr sprite_xreg	= $30		; actual sprite x register
!addr sprite_xmsb	= $31		; actual sprite x-msb
!addr sprite_dir	= $32		; actual sprite direction 1-4
!addr move_dir		= $35
!addr draw_column	= $36		; draw column
!addr draw_line		= $37		; draw line
!addr draw_char		= $38		; draw char/tile
!addr draw_ptr		= $39		; pointer to print maze on screen
!addr level		= $3b		; level 1-4
!addr maze_column	= $3c		; actual maze column
!addr maze_line		= $3d		; actual maze line
!addr move_x		= $3f		; sprite mov sub x
!addr move_y		= $40		; sprite mov sub y
!addr move_xmsb		= $41		; sprite mov sub xmsb
!addr mazedata_ptr	= $44		; pointer to mazedata
!addr joykey_dir	= $46		; worrior direction after joy/key check
!addr target		= $47		; 4 bytes targets to destroy
!addr hit_target3	= $4b		; neg after hit target 3
!addr move		= $4c		; movement 0=none, 1=up, 2=down, 3=left, 4=right
!addr sprite_data	= $4d		; 8 bytes sprite data
!addr collision_mob	= $55		; mob-mob collision
!addr collision_bgr	= $56		; mob-bgr collision
!addr monster_value	= $57		; 5 bytes monster values
!addr monster_cnt	= $60		; 5 bytes monster counter
!addr monster_dir	= $68		; 5 bytes monster directions
!addr sound_no		= $6f
!addr sound_ptr		= $70		; pointer to sound data
!addr sound1		= $72
!addr sound2		= $73
!addr sound3		= $74
!addr sound4		= $75
!addr bonus_player	= $76
!addr monster_shot_dir	= $77
!addr cheat		= $78		; >0 = never die
; ************************************** P500 ZERO PAGE *******************************************
!addr ColorRAM0		= $e6
!addr ColorRAM1		= $e8
!addr ColorRAM2		= $ea
!addr ColorRAM3		= $ec
!addr VIC		= $ee
!addr VIC_MOBY		= $f0
!addr VIC_MOBCOL	= $f2
!addr SID		= $f4
!addr CIA		= $f6
!addr TPI1		= $f8
!addr TPI2		= $fa
; ****************************************** MACROS ***********************************************
; ***************************************** ZONE CODE *********************************************
!zone code
!initmem FILL
*= $e000
; init
start:	sei				; disable interrupts
	cld
	ldx #$ff			; init stack
	txs
; clear ram
	ldx #$00
	txa
clramlp:sta $02,x			; zp
	sta $0200,x			; page 2
	sta $0300,x			; page 3
	inx
	bne clramlp

	jsr Init			; init IO Pointer, IO-regs
; init vic regs
	ldy #0
viclp:	lda VICRegs,y
	sta (VIC),y
	iny
	cpy #SIDRegs-VICRegs
	bne viclp
; init sid regs
	ldy #0
sidlp:	lda SIDRegs,y
	sta (SID),y
	iny
	cpy #Maze1-SIDRegs
	bne sidlp
	cli				; enable irq

StartNew:
	jsr StartScreen			; shows start screen and waits for F1
	jsr InitGame			; reset score, level, init lives and sprite colors

NextLevel:
	jsr SetupGame			; inc level, setup targets, init sprite states, state = 0

TryAgain:
	jsr SetupGameScreen		; draw game screen
	jsr SetupWorrior		; setup player sprite
	lda #$1f
	ldy #MODVOL
	sta (SID),y			; full volume, filter low pass
	ldx #1
	lda state
	bpl newlev			; start sound 1, if new level (state=0)
	ldx #2				; if not, start with sound 2
newlev:	txa
	jsr PlaySound			; play start sound

GameLoop:
	lda timer
	and #$fc
	bne GameLoop			; wait 1 loop of timer
	jsr CopySpritePointer		; copies all sprites data pointers to the vic pointers
	ldy #MOBMOB
	lda (VIC),y
	sta collision_mob		; save mob-mob collision
	iny				; MOBBGR
	lda (VIC),y
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
	dec state			; state = neg for sound 2 
nosound:lda delay
	cmp #$10
	bcc slowmon			; slow monsters
	jsr MoveMonsters		; move monsters extra for fast-mode
slowmon:jsr WorriorShot			; check and move worrior shot
	jsr MoveWorrior			; move worrior
	jsr MoveMonsters		; move monsters
	jsr StartMonsters		; check if monsters are off and start new monsters
	jsr MonsterShot			; start monster shot ?
	jsr MoveMonsterShot		; move monster shot and disable if reaching wall
	jsr CheckCollision		; check for hits
	jsr CheckExplosions		; Explosions: switch patterns, sound, score, finished ? 
	jsr UpdateSound			; update game sound
	lda sprite_state
	cmp #$ff
	beq decwor			; branch if player sprite off (dead)
	lda finished			; level finished ? (0=finished)
	bne GameLoop			; next cycle

	lda sprite_state
	bmi decwor			; branch if player sprite off (dead)
	jmp LevelFinished		; level finished

decwor:	lda cheat			; check cheat
	bne TryAgain			; never die
	dec players			; decrease lives
	bne TryAgain			; next try if live > 0

	jmp GameOver			; game over
; -------------------------------------------------------------------------------------------------
; Checks F1 key for game start
;   returns .a = 0 if F1 pressed
CheckF1Key:
	ldx #$fe
	jsr chkkey			; output .x to keyboard 0-7 and returns input in .x
	txa
	and #$01			; isolate bit#1 F1
	beq chkf1x			; F1 pressed
; check cheat
	txa
	and #$02			; check ESC
	bne chkbut
	stx cheat			; never die
; check button
chkbut:	ldy #PRA
	lda (CIA),y			; load cia port a - bit#6 button joystick 1
	and #$40			; isolate button joy1 bit #6
chkf1x:	rts
; -------------------------------------------------------------------------------------------------
; $e0db Check joystick and keyboard movement/fire
CheckJoyKey:
	lda #$ff			; init key value
	ldx #$7f
	jsr chkkey
	cpx #$2f			; check ',' = left
	bne chkrght
	and #$fb			; clear bit #2
	bne chkx			; always
chkrght:cpx #$1f			; check '.' = right
	bne chkup
	and #$f7			; clear bit #3
	bne chkx			; always
chkup:	ldx #$fd
	jsr chkkey
	cpx #$3b			; check 'Q' = up
	bne chkdown
	and #$fe			; clear bit #0
	bne chkx			; always
chkdown:cpx #$37			; check 'A' = down
	bne chkfire
	and #$fd			; clear bit #1
	bne chkx			; always
chkfire:ldx #$bf
	jsr chkkey
	cpx #$1f			; check spc = fire
	bne chkx
	and #$bf			; clear bit #6

chkx:	sta key				; store key
	jmp chkjoy
; check tpi2
chkkey:	pha				; remember key var
	txa				; move output key value to .a
	ldy #PB
	sta (TPI2),y			; set TPI2 port B keyboard out 0-7
	iny				; pc
debounc:lda (TPI2),y			; load TPI2 port C
	sta temp1
	lda (TPI2),y
	cmp temp1
	bne debounc			; debounce
	and #$3f			; isolate keyboard bits 0-5
	tax				; key input to .x
	pla				; restore key var
	rts
; check joystick
chkjoy:	ldy #PRA
	lda (CIA),y			; load cia port a - bit#6 button joystick 1
	ora #$bf			; set all other bits
	sta temp1
	iny				; prb
	lda (CIA),y			; load cia port b - bit#0-3 joystick 1 movement
	ora #$f0			; set other bit
	and temp1			; and button

	and key				; and pressed key (bit# = 0)
; store joystick/keyboard movement/fire
	ldx #0				; direction var
	tay
	and #$40			; check fire
	bne jkdown
	lda #$80
	sta fire			; store fire
	bne jkx
jkdown:	tya
	and #$02			; check down
	bne jkup
	ldx #DOWN
	bne jkx
jkup:	tya
	and #$01			; check up
	bne jkleft
	ldx #UP
	bne jkx
jkleft:	tya
	and #$04			; check left
	bne jkright
	ldx #LEFT
	bne jkx
jkright:tya
	and #$08			; check right
	bne jkx
	ldx #RIGHT
jkx:	stx joykey_dir			; store move direction
	rts
; -------------------------------------------------------------------------------------------------
; $e170 Game Cycle: do .x cycles
GameUpdate:
	lda timer
	and #$fc
	bne GameUpdate			; wait timer

	tya				; save regs
	pha
	txa
	pha
	jsr UpdateSound			; update sound
	pla				; restore regs
	tax
	pla
	tay

guwait:	lda timer
	and #$fc
	bne guwait			; wait timer

	dex
	bne GameUpdate			; next cyle if .x > 0

	rts
; -------------------------------------------------------------------------------------------------
; $e187 Clears screen with color
ClearScreen:  
	ldy #$00
clscrlp:lda color			; color
	sta (ColorRAM0),y
	sta (ColorRAM1),y
	sta (ColorRAM2),y
	sta (ColorRAM3),y
	lda #$00			; space
	sta ScreenRAMbase,y
	sta ScreenRAMbase+$100,y
	sta ScreenRAMbase+$200,y
	sta ScreenRAMbase+$2e8,y
	iny
	bne clscrlp
	rts
; -------------------------------------------------------------------------------------------------
; $e1a9 Copies all sprites data pointers to the vic pointers
CopySpritePointer:
	ldx #8				; 8 sprites
cpsprlp:lda sprite_data-1,x
	sta SpritePointer-1,x		; store pointer
	dex
	bne cpsprlp
; check pointers
	ldx #8				; 8 sprites
chksplp:lda sprite_data-1,x
	cmp SpritePointer-1,x
	bne CopySpritePointer		; copy again if not copied
	dex
	bne chksplp			; check next

	rts
; -------------------------------------------------------------------------------------------------
; $e1c0 Copies data from xy to address in first two bytes till $ff
;   $fd = new line, $fe = new target address, $ff = end
ScreenCopy:				; copies from .x, .y
	stx ptr1
	sty ptr1+1

scrnewt:ldy #0				; set pointer1 to new source
	sty temp2
	tya				; clear .a
	ora (ptr1),y
	sta ptr2
	tya				; clear .a
	iny
	ora (ptr1),y
	sta ptr2+1

scrcplp:iny
	lda #$00			; clear .a
	ora (ptr1),y			; load data byte
	cmp #$ff
	beq scrcpyx			; $ff = end
	cmp #$fe
	beq scrtarg			; $fe = new target address
	cmp #$fd
	beq scrline
	sty temp1

	pha				; save data
	clc
	lda ptr2			; calc ptr2+temp2
	adc temp2
	sta draw_ptr
	lda ptr2+1
	adc #0				; add carry
	sta draw_ptr+1
	pla
	ldx #0
	sta (draw_ptr,x)
	
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
	lda #$00			; clear .a
	ora (mazedata_ptr),y		; load data byte
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

; print score
	lda #$00
	jsr AddScore			; print zero score
; print players
	ldx players
	stx draw_char			; store player char
	lda #30
	sta draw_column			; store column 30, line 24
	lda #24
	sta draw_line
	jsr DrawMazeTile		; draw player count
; print highscore
	ldx #$d0+5			; set screen pointer to highscore
	ldy #SH+3
	stx ptr1
	sty ptr1+1
	ldy #$03
	jsr PrintScore			; print highscore (score+3)
	rts
; -------------------------------------------------------------------------------------------------
; $e29b Maze addresses level 1 - 4
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
; $e2f5 Reset score, level, init lives and sprite colors
InitGame:
	ldy #0
	sty score
	sty score+1
	sty score+2
	sty level			; reset level
	sty bonus_player		; clear bonus player
	lda #LIVES			; 3 lives
	sta players

	lda #BLUE			; all sprites blue
	ldy #MOBCOL
incollp:sta (VIC),y
	iny
	cpy #MOBCOL+8
	bne incollp

	rts
; -------------------------------------------------------------------------------------------------
; $e310 Inc level, setup targets, init sprite states
SetupGame:
	lda #$00
	tax
	ldy #MOBENA
	sta (VIC),y			; disable sprites

sg1elp:	lda #$ff
	sta sprite_state,x		; clear state for all sprites
	inx
	cpx #8
	bne sg1elp

	ldy #$00			; clear timer
	sty delay
	sty timer2
	sty state			; state = 0

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
	sta hit_target3
	rts
; -------------------------------------------------------------------------------------------------
; Targets level 1-4
TargetTable:
; Level		1    2    3    4
	!byte $04, $05, $06, $06	; Burwors
	!byte $03, $04, $05, $06	; Gorwors
	!byte $02, $04, $06, $06	; Thorwors
	!byte $01, $02, $03, $04	; Worloks
	!byte $00, $01, $00, $02	; Wizard Of Wars
	!byte $0a, $10, $14, $18	; Total monsters to kill in level
; -------------------------------------------------------------------------------------------------
; $e36b Setup worrior sprite
SetupWorrior:
	lda #$e6			; init worrior sprite
	sta sprite_data
	sta SpritePointer
	lda #$03
	sta worrior_dir
	lda #$01			; set worrior start position
	ldy #MOBMSB
	sta (VIC),y
	lda #$37
	ldy #MOBX
	sta (VIC),y
	lda #$ab
	sta (VIC_MOBY),y
	lda #$fd
	sta sprite_data+7		; set shot horizontal pattern
	ldx #$ff
	stx sprite_state+7		; shot sprites off
	stx sprite_state+6
	inx
	stx sprite_state		; player sprite on
	stx worrior_shot_dir		; clear worrior shot dir
	ldy #MOBENA
	lda (VIC),y
	ora #$01			; enable worrior sprite
	and #$7f			; disable player shot
	sta (VIC),y
	rts
; -------------------------------------------------------------------------------------------------
; $e39f Monster start positions
MonsterStartX:  
	!byte $35, $4d, $65, $7d, $95, $ad, $c5, $f5
; $e3a7
MonsterStartY:
	!byte $7b, $63, $7b, $93, $ab, $c3, $7b, $ab
; -------------------------------------------------------------------------------------------------
; $e3af Level finished
LevelFinished:
	lda #$00
	ldy #MOBENA
	sta (VIC),y			; disable sprites
	ldx #<TextBonus3000
	ldy #>TextBonus3000
	jsr ScreenCopy			; print 'BONUS 3000'
	lda #7
	jsr PlaySound			; play bonus sound
	lda #$30
	jsr AddScore
	ldx #0
	jsr GameUpdate			; 1 game update
	jmp NextLevel

TextBonus3000:
!byte $24, $05, $0c, $19, $18, $1e, $1c, $00	; 'BONUS 3000'
!byte $04, $01, $01, $01, $ff

; Game over
GameOver:
	ldx #2
chkhisc:lda highscore,x
	cmp score,x			; check if new high score
	bcc newhisc
	bne nohisc
	dex
	bpl chkhisc
; new highscore
newhisc:lda score			; store new highscore
	sta highscore
	lda score+1
	sta highscore+1
	lda score+2
	sta highscore+2
	ldx #$d0+5			; set screen ptr to highscore
	ldy #SH+3
	stx ptr1
	sty ptr1+1
	ldy #3
	jsr PrintScore
; game over - no highscore
nohisc:	lda #$00
	ldy #MODVOL
	sta (SID),y
	inc move
	ldx #<TextGameOver
	ldy #>TextGameOver
	jsr ScreenCopy			; print 'G A M E  O V E R'
	ldx #80
	jsr GameUpdate			; do 80 Game updates
	jmp StartNew			; start new

TextGameOver:
	!byte $24,SH+1, $11, $00, $0b, $00, $17, $00
	!byte $0f, $00, $00, $19, $00, $1f, $00, $0f
	!byte $00, $1b, END
; Add and print score
AddScore:
	clc
	sed
	adc score+1			; add score
	sta score+1
	lda score+2
	adc #0
	sta score+2
	cld
	ldx #$c4+5			; set screen pointer to score
	ldy #SH+3
	stx ptr1
	sty ptr1+1
	ldy #0				; print score
; Print score
PrintScore:
	ldx #0

pslp:	lda score,y
	and #$0f			; clear hinibble
	clc
	adc #1
	sta (ptr1,x)
	dec ptr1
	lda score,y
	lsr
	lsr
	lsr
	lsr
	clc
	adc #1
	sta (ptr1,x)
	dec ptr1
	iny
	cpy #6				; end highscore
	beq chkbon
	cpy #3				; end score
	bne pslp
; check bonus player
chkbon:	lda score+2
	cmp #2				; check score
	bcc psx				; not enough
	lda bonus_player
	bne psx				; already bonus player
	inc bonus_player
	inc players
	ldx players
	stx ScreenRAMbase+$03de		; print bonus player
	lda #7
	jsr PlaySound			; play bonus sound
psx:	rts
; -------------------------------------------------------------------------------------------------
; $e474 Start screen - exit with F1 only
StartScreen:
	lda #$00
	ldy #MOBENA
	sta (VIC),y			; disable all sprites
	lda #BLUE
	sta color
	jsr ClearScreen			; clear screen with blue chars
	lda #CYAN
	ldy #EXTCOL
	sta (VIC),y			; set ext cyan
	lda #WHITE
	iny
	sta (VIC),y			; set bgr white
	ldy #>StartScreenData
	ldx #<StartScreenData
	jsr ScreenCopy			; Copies start screen

	ldx #$04
ssinspr:txa
	asl
	clc
	adc #2				; start with sprite 1
	tay
	lda #$d7			; sprite start h pos
	sta sprite_state+1,x		; set right direction = $d7
	sta (VIC),y			; setup monsters sprites 1-5
	lda StartScreenMonsterVpos,x
	sta (VIC_MOBY),y
	lda StartScreenMonsterData,x
	sta SpritePointer+1,x
	txa
	clc
	adc #MOBCOL+1			; calc monster sprite color reg
	tay
	lda MonsterColorTable,x
	sta (VIC),y
	dex
	bpl ssinspr			; setup next sprite

	lda #$00
	ldy #MOBMSB
	sta (VIC),y			; clear sprite x-msb
	lda #$3e
	ldy #MOBENA
	sta (VIC),y			; enable monsters

sssprlp:lda timer
	and #$fc
	bne sssprlp			; wait
sswait:	lda timer
	and #$fc
	bne sswait			; wait
	inc delay
	lda delay
	and #$3f			; delay next movement
	bne sschkf1
; move monsters
	ldx #4				; move 5 monsters
ssright:txa			
	asl
	clc
	adc #2				; start with sprite 1
	tay
	lda sprite_state+1,x
	beq ssleft
	lda (VIC),y
	clc
	adc #1				; move monsters right
	sta (VIC),y
	cmp StartScreenMonsterRLimit,x	; check if right limit
	bcc ssnxspr
	lda #$00
	sta sprite_state+1,x		; set left direction
	cpx #3
	beq ssnxspr			; skip if monster #3 (unidir monster)
	inc SpritePointer+1,x		; turn monster sprite left
	bne ssnxspr
ssleft:	lda (VIC),y
	sec
	sbc #1				; move monster left
	sta (VIC),y
	cmp #$d7
	bcs ssnxspr			; skip if left limit not reached
	sta sprite_state+1,x		; set right dir
	cpx #3
	beq ssnxspr			; skip if monster #3 (unidir monster)
	dec SpritePointer+1,x		; turn monster sprite right
ssnxspr:dex
	bpl ssright
sschkf1:jsr CheckF1Key			; check f1 key pressed
	bne sssprlp			; continue movement, if not F1 pressed
; game screen colors
	lda #BLUE			; set game bgr+ext colors
	ldy #EXTCOL
	sta (VIC),y
	lda #BLACK
	iny
	sta (VIC),y
	rts
; -------------------------------------------------------------------------------------------------
; Start screen tables
StartScreenMonsterVpos:
	!byte $52, $6a, $82, $9a, $b2
StartScreenMonsterData:
	!byte $e9, $ed, $f1, $f5, $f9
StartScreenMonsterRLimit:
	!byte $db, $dc, $db, $dc, $db
; -------------------------------------------------------------------------------------------------
; Interrupt handler (always in system indirect bank)
Interrupt:
	pha
	tya
	pha
	ldy #AIR
	lda (TPI1),y
;	beq irqx
	lda #$00
	sta (TPI1),y			; pop interrupt
	ldy #ICR
	lda (CIA),y			; load irq-reg
;	and #$02
;	beq irqx			; skip if not timer b
	lda timer
	clc
	adc #4				; add 4
	sta timer
irqx:	pla
	tay
	pla
	rti
; -------------------------------------------------------------------------------------------------
; Move worrior sprite
MoveWorrior:
	lda timer2
	and #$01
	beq mwtime			; time to move worrior
	rts
; time for worrior
mwtime:	lda sprite_state		; worrior state
	bpl mwon			; branch if worrior on (not dead)
	rts
; move worrior
mwon:	jsr CheckJoyKey			; check movement
	lda #$00
	sta sprite_xreg			; store xregs for worrior sprite
	lda #$01
	sta sprite_xmsb
	lda joykey_dir
	sta sprite_dir			; store joystick direction
	lda worrior_dir
	sta move_dir			; save old worrior dir
	jsr MoveSprite			; move sprite

	lda move_dir
	sta worrior_dir			; restore old worrior dir
	ldx sprite_dir
	lda WorriorSpriteTable,x	; load worrior pattern
	bne mwstpat			; skip if dir > 0 (no wall)
	ldx worrior_dir			; load old pattern if wall
	lda WorriorSpriteTable,x
mwstpat:sta sprite_data			; store sprite pattern
	rts
; -------------------------------------------------------------------------------------------------
; Worrior patterns for direction 0 - 4
WorriorSpriteTable:  
	!byte $00, $e8, $e7, $e6, $e5
; -------------------------------------------------------------------------------------------------
; Move sprite (sprite_xreg, _xmsb, _dir)
MoveSprite:
	ldy sprite_xreg
	lda #$00
	sta ptr1
	jsr msmove			; move sprite (sprite dir = 0 -> wall reached!)
	lda sprite_xmsb
	sta move_xmsb
	ldy sprite_xreg
	lda (VIC),y			; set new position to vic
	sta move_x
	lda (VIC_MOBY),y
	sta move_y
	jsr CalcScreenPosition		; calc sprite screen position (char based)
	lda sprite_dir
	bne msdirok			; move sprite
	jmp mstunn			; check tunnel

msdirok:lda move_dir
	cmp #LEFT
	bcs ms040
	lda sprite_dir
	cmp #LEFT
	bcc ms080
	dec move_y
	lda move_y
ms010:	beq ms020
	cmp #UP
	beq ms020
	cmp #$ff
	beq ms020
	cmp #$00
	bcc ms030
	inc ptr1
	sec
	sbc #$03
	jmp ms010

ms020:	ldx ptr1
	lda SpriteMazePosY,x
	ldy sprite_xreg
	sta (VIC_MOBY),y
	jmp ms080

ms030:	lda #$00
	sta sprite_dir
	jmp mstunn
ms040:	lda sprite_dir
	cmp #LEFT
	bcs ms080
	lda move_x
	sec
	sbc #$01
ms050:	beq ms060
	cmp #UP
	beq ms060
	cmp #$ff
	beq ms060
	cmp #$00
	bcc ms030
	inc ptr1
	sec
	sbc #$03
	jmp ms050
ms060:	ldx ptr1
	cpx #$0a
	bne ms070
	ldy #MOBMSB
	lda (VIC),y
	ora sprite_xmsb
	sta (VIC),y
ms070:	lda SpriteMazePosX,x
	ldy sprite_xreg
	sta (VIC),y
ms080:	lda sprite_dir
	sta move_dir
	ldy sprite_xreg
	lda sprite_dir
	cmp #UP
	bne ms090
	lda (VIC_MOBY),y
	sec
	sbc #2
	sta (VIC_MOBY),y
	jmp mstunn
ms090:	cmp #$02
	bne ms100
	lda (VIC_MOBY),y
	clc
	adc #2
	sta (VIC_MOBY),y
	jmp mstunn
ms100:  cmp #LEFT
	bne ms120
	lda (VIC),y
	sec
	sbc #2
	sta (VIC),y
	cmp #$fe
	bcc ms110
	lda sprite_xmsb
	eor #$ff
	sta temp1
	ldy #MOBMSB
	lda (VIC),y
	and temp1
	sta (VIC),y
ms110:  jmp mstunn
ms120:  cmp #RIGHT
	bne mstunn
	lda (VIC),y
	clc
	adc #2
	sta (VIC),y
	cmp #$02
	bcs mstunn
	ldy #MOBMSB
	lda (VIC),y
	ora sprite_xmsb
	sta (VIC),y
	jmp mstunn
; move sprite
msmove:	lda (VIC),y
	sta move_x			; store x
	lda (VIC_MOBY),y
	clc
	sbc #6
	sta move_y			; store y-6
	lda sprite_xmsb
	sta move_xmsb			; store msb
	jsr CalcScreenPosition		; calc char position
	lda sprite_dir
	beq msx
	cmp #LEFT			; check dirs and move x, y
	bcc msverti
	beq msleft
; move right
	inc move_x
	jmp msmvscr
; move left
msleft:	dec move_x
	jmp msmvscr

msverti:cmp #UP
	bne msdown
; move up
	dec move_y
	jmp msmvscr
; move down
msdown:	inc move_y
; calc screen pointer to new position
msmvscr:lda #SH
	sta draw_ptr+1			; set screen address hi
	lda move_x
mslinlp:ldy move_y
	beq mschkwa			; exit f all lines added
	dec move_y			; dec line
	clc
	adc #40				; add 40 for each line to screen ptr lo
	bcc mslinlp			; next line
	inc draw_ptr+1			; inc ptr hi
	jmp mslinlp			; next line
; check if wall = dir not allowed
mschkwa:sta draw_ptr
	ldx #0
	lda (draw_ptr,x)		; load char
	beq msx				; return if no wall
	lda #WALL
	sta sprite_dir			; clear sprite dir if dir not allowed
msx:	rts
; $e6b3 tunnel
mstunn:	ldy #MOBMSB
	lda (VIC),y
	and sprite_xmsb
	bne msckrig			; branch if x msb set
	ldy sprite_xreg
	lda (VIC),y
	cmp #$16			; left tunnel reached?
	bcs mstuckv			; not...skip
; tunnel left
	ldy #MOBMSB
	lda (VIC),y
	ora sprite_xmsb
	sta (VIC),y			; set x msb
	lda #$40
	ldy sprite_xreg
	sta (VIC),y			; set x to tunnel right
	rts
; $e6cf tunnel right
msckrig:beq mstuckv			; skip if .a = 0
	ldy sprite_xreg
	lda (VIC),y
	cmp #$42			; check tunnel reached
	bcc mstuckv			; not...skip
	
	lda sprite_xmsb
	eor #$ff			; xor x msb for and operation
	sta temp1
	ldy #MOBMSB
	lda (VIC),y
	and temp1
	sta (VIC),y			; clear x msb
	
	ldy sprite_xreg
	lda #$18
	sta (VIC),y			; set x to tunnel left
; check tunnel vertical position
mstuckv:ldy sprite_xreg
	lda (VIC_MOBY),y
	cmp #$31
	bcs msx2			; skip if not exactly tunnel v pos
	lda (VIC_MOBY),y
	clc
	adc #2
	sta (VIC_MOBY),y
msx2:	rts
; -------------------------------------------------------------------------------------------------
; $e6f5
SpriteMazePosX:
	!byte $19, $31, $49, $61, $79, $91, $a9, $c1, $d9, $f1, $0a, $22, $39
; $e702
SpriteMazePosY:
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
	ldy #MOBMSB
	lda (VIC),y
	and move_xmsb			; load and isolate x-msb
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
	stx temp5
	ldx #0
	lda draw_char
	sta (draw_ptr,x)		; store tile to screen
	ldx temp5
	rts
; -------------------------------------------------------------------------------------------------
; $e754 Move Worrior Shot
WorriorShot:
	lda sprite_state+7
	bpl wsshot			; shot in progress
	jmp wscksht			; check for new shot
; check collision
wsshot:	lda collision_bgr
	and #$80
	beq wsbgr			; no collision, check maze limits
; collison with bgr - disbale worrior shot
wsdisab:lda #$ff
	sta sprite_state+7		; state off
	ldy #MOBENA
	lda (VIC),y
	and #$7f
	sta (VIC),y			; disable sprite
	lda #$7f
	ldy #MOBMSB
	lda (VIC),y
	and #$7f
	sta (VIC),y			; clear x msb
	rts
; $e776 check maze limits
wsbgr:	ldy #MOBMSB
	lda (VIC),y
	and #$80
	bne wswallr			; branch if x msb set
; check left maze limit
	ldy #MOBX+14
	lda (VIC),y
	cmp #$14			; check left limit
	bcc wsdisab			; ...reached - shot off
	bcs wsmove			; always
; check maze right limit
wswallr:ldy #MOBX+14
	lda (VIC),y
	cmp #$42
	bcs wsdisab			; ...reached - shot off
; $e78d move shot
wsmove:	lda worrior_shot_dir
	cmp #LEFT
	bcc wsverti			; branch to vertical shot move
	bne wsright			; branch to shot right
; move shot left
	ldy #MOBX+14
	lda (VIC),y
	sec
	sbc #4				; left 4 steps
	sta (VIC),y
	cmp #$fc			; check x byte max limit
	bcc wsx
	ldy #MOBMSB
	lda (VIC),y
	and #$7f
	sta (VIC),y			; clear x msb bit #7
wsx:	rts
; $e7b1 move shot right
wsright:ldy #MOBX+14
	lda (VIC),y
	clc
	adc #4				; right 4 steps
	sta (VIC),y
	cmp #$04			; check x byte min limit
	bcs wsx
	ldy #MOBMSB
	lda (VIC),y
	ora #$80
	sta (VIC),y			; set x msb bit #7
	rts
; $e7cd move shot up
wsverti:cmp #UP
	bne wsdown
	ldy #MOBY+14
	lda (VIC),y
	sec
	sbc #4				; up 4 steps
	sta (VIC),y
	cmp #$2a
	bcc wswallv			; branch if upper maze limit reached
	rts
; $e7e5 move shot down
wsdown:	ldy #MOBY+14
	lda (VIC),y
	clc
	adc #4				; down 4 steps
	sta (VIC),y
	cmp #$ca
	bcc wsx2			; branch if lower maze limit reached
wswallv:jmp wsdisab			; shot off
wsx2:	rts
; $e7fc check worrior ready for shot
wscksht:cmp #$ff
	bne wsx3			; exit if shot in pgrogress
	lda sprite_state
	bpl chkshot			; branch if worrior not dead
wsx3:	rts
; $e805 check fire pressed
chkshot:lda fire
	bmi shoot			; branch if fire pressed
	rts
; $e80a new worrior shot
shoot:	lda #4
	jsr PlaySound			; play shoot sound
	ldy #MOBX
	sty fire			; reset fire var
	lda (VIC),y
	ldy #MOBX+14
	sta (VIC),y			; set shot position
	ldy #MOBY
	lda (VIC),y
	ldy #MOBY+14
	sta (VIC),y
	ldy #MOBMSB
	lda (VIC),y
	and #$01
	beq nsnomsb			; skip if not worrior x msb
	lda (VIC),y
	ora #$80
	sta (VIC),y			; set x msb
	jmp nsdir
nsnomsb:lda (VIC),y
	and #$7f
	sta (VIC),y			; clear x msb
nsdir:  lda worrior_dir
	sta worrior_shot_dir
	cmp #LEFT
	bcc nsverti			; branch if vertiacl worrior direction
	lda #$fd
	sta sprite_data+7		; sprite pattern horizontal
	jmp nsenabl
nsverti:lda #$fe
	sta sprite_data+7		; sprite pattern vertical
; enable worrior shot
nsenabl:ldy #MOBENA
	lda (VIC),y
	ora #$80
	sta (VIC),y			; enable sprite
	lda #$00
	sta sprite_state+7		; set state
	rts
; -------------------------------------------------------------------------------------------------
; $e855 Move monsters
MoveMonsters:
	lda state
	bne mm010
	lda timer2
	and #$01
	bne mm010
	rts
; $e860
mm010:	lda sprite_state
	bpl mm020
	rts
; $e865
mm020:	ldx #1
	stx temp4
mmlp1:	lda sprite_state,x
	bpl mm030
mmloop:	inc temp4
	ldx temp4
	cpx #6
	bne mmlp1
	rts
; $e876
mm030:	lda SpritePosTable,x
	sta sprite_xreg
	lda BitTable,x
	sta sprite_xmsb
	dec monster_cnt-1,x
	dec monster_cnt-1,x
	bmi mm040
	lda monster_dir-1,x
	jmp mm140


mm040:  lda #$17
	sta monster_cnt-1,x
	ldx sprite_xreg
	ldy #RANDOM
	lda (SID),y			; get random
	and #$01
	beq mm100

	ldy #MOBMSB
	lda (VIC),y
	sta temp1			; save mob x msb

	lda BitTable,x
	ora #$01
	and temp1
	beq mm050
	cmp #$01
	beq mm060
	cmp BitTable,x
	beq mm080
mm050:  txa
	tay
	lda (VIC),y
	sta temp1			; save monster x pos
	ldy #MOBX
	lda (VIC),y			; load worrior x pos
	cmp temp1
	bcc mm080
mm060:  ldy #RANDOM
	lda (SID),y			; get random
	and #$03
	beq mm090
mm070:  lda #$04
	jmp mm140
mm080:  ldy #RANDOM
	lda (SID),y			; get random
	and #$03
	beq mm070
mm090:  lda #$03
	jmp mm140
mm100:  txa
	tay
	lda (VIC_MOBY),y
	sta temp1			; save monster y pos
	ldy #MOBY
	lda (VIC),y			; load worrior y pos
	cmp temp1
	bcc mm120
	ldy #RANDOM
	lda (SID),y			; get random
	and #$03
	beq mm130
mm110:  lda #$02
	jmp mm140
mm120:  ldy #RANDOM
	lda (SID),y			; get random
	and #$03
	beq mm110
mm130:  lda #$01
mm140:  ldx temp4
	sta sprite_dir
	lda monster_dir-1,x
	sta move_dir
	jsr MoveSprite
	lda sprite_dir
	bne mm150
	jsr mm180
mm150:  ldx temp4
	lda move_dir
	sta monster_dir-1,x
	clc
	adc monster_value-1,x
	tax
	lda MonsterPatternTable,x
	bne mm160
	beq mm170
mm160:  ldx temp4
	sta sprite_data,x
mm170:  jmp mmloop
mm180:  lda move_dir
	clc
	adc #$01
	cmp #RIGHT
	bcc mm190
	ldy #RANDOM
	lda (SID),y			; get random
	and #$03
mm190:  sta move_dir
mmx:	rts
; -------------------------------------------------------------------------------------------------
; $e923 Postions of sprite x,y regs
SpritePosTable:
	!byte $00, $02, $04, $06, $08, $0a, $0c, $0e
; $e92b Sprite patterns for dir 0 - 4
MonsterPatternTable:
	!byte $ec, $ec, $eb, $ea, $e9	; Burwor
	!byte $f0, $f0, $ef, $ee, $ed	; Gorwor
	!byte $f4, $f4, $f3, $f2, $f1	; Thorwor
	!byte $f5, $f5, $f5, $f5, $f5	; Worlok
	!byte $fc, $fc, $fb, $fa, $f9	; Wizard of Wor
; -------------------------------------------------------------------------------------------------
; $e944 Check monster state and decide to start new monster
StartMonsters:
	lda sprite_state
	bmi smx
	lda timer2
	and #$1f
	bne smx
	ldx #1
smchklp:lda sprite_state,x
	cmp #$ff
	beq sm10
	inx
	cpx #6
	bne smchklp
smx:	rts
; $e95c
sm10:	ldy #$00
	lda ttarget
	beq sm20
	dec ttarget
	jmp sm80
sm20:	iny
	lda target
	beq sm30
	dec target
	jmp sm80
sm30:	iny
	lda target+1
	beq sm40
	dec target+1
	jmp sm80
sm40:	iny
	ldx #1
	lda #$ff
sm50:	and sprite_state,x
	and sprite_state+1,x
	inx
	inx
	cpx #7
	bne sm50
	cmp #$ff
	bne smx
	ldx #1
	lda target+2
	beq sm60
	dec target+2
	jmp sm70
sm60:	iny
	lda target+3
	beq smx
	lda #3
	jsr PlaySound
	dec target+3
	dec hit_target3
sm70:	lda #1
	sta state
sm80:	tya
	pha
	
	ldy #0
	lda (VIC_MOBY),y
	sta temp1			; save worrior y pos
	ldy #RANDOM
	lda (SID),y
	sta temp2			; save random

	txa
	pha
	asl
	tay
	lda temp2
	and #$07
	tax
	lda MonsterStartX,x
	sta (VIC),y
	lda timer2
	lsr
	lsr
	lsr
	lsr
	lsr
	and #$07
	tax
	lda MonsterStartY,x
	sta (VIC_MOBY),y
	clc
	adc #$18
	cmp temp1			; worrior y pos
	bcs sm90
	sec
	sbc #$30
	cmp temp1
	bcc sm90
	lda temp1
	sbc #$24
	sta (VIC_MOBY),y
sm90:	pla
	tay
	pla
	tax
	lda MonsterValueTable,x
	sta monster_value-1,y
	lda MonsterTypeTable,x
	sta sprite_data,y
	sta SpritePointer,y
	lda MonsterColorTable,x
	sta (VIC_MOBCOL),y
	tya
	tax
	inc sprite_state,x
	ldy #MOBENA
	lda (VIC),y
	ora BitTable,x
	sta (VIC),y
	rts
; -------------------------------------------------------------------------------------------------
; $ea08 Monster tables
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
; $ea21 check in progress
exspdis:cmp #$ff
	beq exnext			; next sprite, if off
; ex in progress
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
	ldy #MOBENA
	lda (VIC),y
	and #$ff-1
	sta (VIC),y			; disable player sprite
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
	lda ScoreMonsterStartX100,y	; load monster score
	cmp #5
	bne spadsco			; skip if not max score 5
	ldy #RANDOM
	lda (SID),y			; get random
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
	sta temp1
	ldy #MOBENA
	lda (VIC),y
	and temp1
	sta (VIC),y			; disable monster sprite
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
ScoreMonsterStartX100:  
	!byte 1, 2, 3, 4, 5, 5, 5, 5
; -------------------------------------------------------------------------------------------------
; $eaef Check sprite collision
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
	ldy #MOBX
	lda (VIC),y
	sta coll1_x			; store player x in coll1_x
	lda #$00
	sta coll1_x+1
	ldy #MOBMSB
	lda (VIC),y
	and #$01			; isolate x msb bit player
	beq cc10
	inc coll1_x+1			; inc coll1_x hi if msb set
cc10:	ldy #MOBY
	lda (VIC),y
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
	ldy #MOBMSB
	lda (VIC),y
	and #$80			; isolate x msb player shot sprite
	beq cc20
	inc coll1_x+1			; inc coll1_x hi if msb set
cc20:	ldy #MOBX+14
	lda (VIC),y
	sta coll1_x			; store player shot x in coll1_x
	lda (VIC_MOBY),y
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
	ldy #MOBENA
	lda (VIC),y
	and #$7f
	sta (VIC),y			; disable player shot sprite 7
	lda #$80
	sta sprite_state,x		; set player shot state to $80=dead
	jmp ccx				; exit
ccnxmon:inx
	cpx #7
	bne ccmonlp			; next monster/monster-shot
ccx:	rts
; -------------------------------------------------------------------------------------------------
; Table for MSB
BitTable:  
	!byte $01, $02, $04, $08, $10, $20, $40, $80
; -------------------------------------------------------------------------------------------------
; $eb78 Collision; x=sprite, coll1_x, coll1_y = partners
Collision:
	lda collision_mob
	and BitTable,x			; isolate collision sprite bit
	bne ccoll
	jmp collx0			; exit if no collision
ccoll:  txa
	asl				; x2 calc x-reg
	tay
	lda (VIC),y			; store x to coll2
	sta coll2_x
	lda (VIC_MOBY),y
	sta coll2_y			; store y to coll2
	lda #$00
	sta coll2_x+1
	ldy #MOBMSB
	lda (VIC),y
	and BitTable,x			; isolate x-msb
	beq co10
	inc coll2_x+1			; inc coll2 x-hi
co10:	lda #14
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
; $ebfe Start monster shot
MonsterShot:
	lda sprite_state
	bpl shplok			; branch if player sprite on
	rts
; $ec03
shplok:	lda sprite_state+6
	bne shshot			; branch if no shot in progress
	rts
; $ec08
shshot:	lda hit_target3
	bmi shcheck			; check monsters
	lda timer2
	and #$0f
	beq shcheck			; check monsters
	rts
; check monster on?
shcheck:ldx #1				; check all monsters
shlp1:	lda sprite_state,x
	bpl shmonon			; branch if monster on
shnextm:inx
	cpx #6				; last monster ?
	bne shlp1			; next monster
	rts
; $ec1f check if same x or y pos with worrior
shmonon:ldy #MOBX
	lda (VIC),y
	sta temp1			; save worrior x
	lda (VIC_MOBY),y
	sta temp2			; save worrior y
	txa
	asl				; cal x pos for monster
	tay
	lda (VIC_MOBY),y
	sta temp4			; save monster y
	lda (VIC),y
	sta temp3			; save monster x
	cmp temp1			; compare with worrior
	beq shv				; if x same -> shoot vertical
; horizontal
	lda temp4
	cmp temp2			; compare y
	bne shnextm			; if not same -> check next monster
; find h direction
	lda temp1
	cmp temp3			; compare x
	ldy #MOBMSB
	lda (VIC),y
	and #$01			; check worrior x msb
	bne shwmsb			; branch if set
; worrior x msb = 0
	ldy #MOBMSB
	lda (VIC),y
	and BitTable,x			; check monster x msb 
	bne shmmsb
	beq shh				; both msb clear
; worrior x msb = 1
shwmsb:	ldy #MOBMSB
	lda (VIC),y
	and BitTable,x
	bne shh				; both msb set
; x msb's are different
shmmsb:	bcc shright
	clc				; C = 0 left
	bcc shh
shright:sec
shh:	lda #4				; right if C = 1
	bcs shhoriz
	lda #3				; left
shhoriz:sta monster_shot_dir
	lda #$fd
	sta sprite_data+6		; set pattern horizontal
	bne shset			; always
; vertical
shv:	lda temp2
	cmp temp4
	lda #2				; down
	bcs shverti
	lda #1				; up

shverti:sta monster_shot_dir
	lda #$fe
	sta sprite_data+6		; set pattern vertical
shset:  lda #$00
	sta sprite_state+6		; set state on
	ldy #12
	lda temp3			; set position
	sta (VIC),y
	lda temp4
	sta (VIC_MOBY),y
	ldy #MOBMSB
	lda (VIC),y
	and BitTable,x			; check x msb
	beq shclmsb
	lda (VIC),y			; set x msb
	ora #$40
	bne shenabl
shclmsb:lda (VIC),y
	and #$bf
shenabl:sta (VIC),y
	ldy #MOBENA
	lda (VIC),y
	ora #$40
	sta (VIC),y			; enable monster shot
	lda monster_shot_dir
	sta monster_dir-1,x
	rts
; -------------------------------------------------------------------------------------------------
; $ecab move monster shot and disable if reaching wall
MoveMonsterShot:
	lda sprite_state+6
	bpl mssshot			; shot in progress
	rts
; $ecb0 check collision
mssshot:lda collision_bgr
	and #$40
	beq mmsbgr			; no collision, check maze limits
; collison with bgr - disable monster shot
msdisab:lda #$ff
	sta sprite_state+6		; state off
	ldy #MOBENA
	lda (VIC),y
	and #$bf
	sta (VIC),y			; disable sprite
	rts
; $ecc3 check maze limits
mmsbgr:	ldy #MOBMSB
	lda (VIC),y
	and #$40
	bne mmswalr			; branch if x msb set
; check left maze limit
	ldy #MOBX+12
	lda (VIC),y
	cmp #$14			; check left limit
	bcc msdisab			; ...reached - shot off
	bcs mmsmove			; always
; check maze right limit
mmswalr:ldy #MOBX+12
	lda (VIC),y
	cmp #$42
	bcs msdisab			; ...reached - shot off
; move shot
mmsmove:lda monster_shot_dir
	cmp #LEFT
	bcc mmsvert			; branch to vertical shot move
	bne mmsrigt			; branch to shot right
; move shot left
	ldy #MOBX+12
	lda (VIC),y
	sec
	sbc #2				; left 2 steps
	sta (VIC),y
	cmp #$fe			; check x byte max limit
	bcc mmsx
	ldy #MOBMSB
	lda (VIC),y
	and #$bf
	sta (VIC),y			; clear x msb bit#6
	rts
; $ecf8 move shot right
mmsrigt:ldy #MOBX+12
	lda (VIC),y
	clc
	adc #2				; right 2 steps
	sta (VIC),y
	cmp #$02			; check x byte min limit
	bcs mmsx
	ldy #MOBMSB
	lda (VIC),y
	ora #$40
	sta (VIC),y			; set x msb bit#6
	rts
; $ move shot up
mmsvert:cmp #UP
	bne mmsdown
	ldy #MOBY+12
	lda (VIC),y
	sec
	sbc #2				; up 2 steps
	sta (VIC),y
	cmp #$2a
	bcc mmswall			; branch if upper maze limit reached
	rts
; $ed20 move shot down
mmsdown:ldy #MOBY+12
	clc
	adc #2				; down 2 steps
	sta (VIC),y 	
	cmp #$ca
	bcs mmswall			; branch if lower maze limit reached
mmsx:	rts

mmswall:jmp msdisab			; shot off
; -------------------------------------------------------------------------------------------------
; $ed31 play sound no. .x
PlaySound:
	cmp #1
	bne pls02
; start sound
	sta sound_no
	lda #<Sound1
	sta sound_ptr
	lda #>Sound1
	sta sound_ptr+1
	lda #$21
	ldy #V1CTRL
	sta (SID),y
	lda #$01
	sta sound1
	rts
; $ed49 alternative start sound
pls02:  cmp #2
	bne pls03
	sta sound_no
	lda #<Sound2
	sta sound_ptr
	lda #>Sound2
	sta sound_ptr+1
	lda #$01
	sta sound1
	rts
; $ed5c
pls03:  cmp #3
	bne pls04
	sta sound_no
	lda #<Sound3
	sta sound_ptr
	lda #>Sound3
	sta sound_ptr+1
	lda #$01
	sta sound1
	rts
; $ed6f Shoot sound
pls04:  cmp #4
	bne pls05
	lda #$0f
	sta sound3
	lda #$81
	ldy #V2CTRL
	sta (SID),y
	rts
; $ed7d
pls05:  cmp #5
	bne pls06
	lda #$0f
	sta sound4
	lda #$81
	ldy #V3CTRL
	sta (SID),y
	rts
; $ed8b Player explosion
pls06:  cmp #6
	bne pls07
	lda #$3f
	sta sound4
	sta sound3
	lda #$81
	ldy #V2CTRL
	sta (SID),y
	ldy #V3CTRL
	sta (SID),y
	rts
; $ed9e Bonus sound
pls07:  cmp #7
	bne pls08
	lda #$3f
	sta sound2
	asl
	ldy #V2HI
	sta (SID),y
	lda #$21
	ldy #V2CTRL
	sta (SID),y
	lda #$00
	sta sound3
	rts
; $edb4
pls08:  cmp #8
	bne plsx
	lda #$07
	sta sound2
	lda #$21
	ldy #V2CTRL
	sta (SID),y
plsx:	rts
; -------------------------------------------------------------------------------------------------
; $edc3 update game sound
UpdateSound:
	lda sound3
	beq us10
	asl
	asl
	asl
	ldy #V2HI
	sta (SID),y
	dec sound3
	bne us10
	lda #$80
	ldy #V2CTRL
	sta (SID),y
us10:	lda sound4
	beq us20
	asl
	asl
	adc #$07
	ldy #V3HI
	sta (SID),y
	dec sound4
	bne us20
	lda #$80
	ldy #V3CTRL
	sta (SID),y
us20:	lda sound2
	beq us30
	lda sound2
	lsr
	lsr
	and #$01
	tax
	lda Sound1,x
	ldy #V2HI
	sta (SID),y
	dec sound2
	bne us30
	lda #$20
	ldy #V2CTRL
	sta (SID),y
us30:	dec sound1
	bne usx
	clc
	lda sound_ptr
	adc #2
	sta sound_ptr
	lda sound_ptr+1
	adc #$00
	sta sound_ptr+1
	ldy #$00
	tya
	ora (sound_ptr),y
	bne us60
	lda sound_no
	cmp #1
	bne us40
	jmp PlaySound

us40:	cmp #2
	bne us50
	jmp pls02
us50:	jmp pls03
us60:	sta sound1
	ldy #1
	lda #$00
	ora (sound_ptr),y
	asl
	tax
	lda Notes,x
	ldy #V1LO
	sta (SID),y
	lda Notes+1,x
	ldy #V1HI
	sta (SID),y
usx:	rts
; $ee43

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
	!byte $00, $00, $00, $00, $00, $18, $18, $30	; $27 ','
	!byte $00, $00, $00, $00, $00, $18, $18, $00	; $28 '.'
	!byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa	; $29 ||||
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0	; $2a vertical line
	!byte $00, $00, $00, $00, $00, $00, $00, $ff	; $2b low line
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $ff	; $2c left-low corner
	!byte $ff, $00, $00, $00, $00, $00, $00, $00	; $2d high line
	!byte $3c, $66, $66, $66, $66, $3c, $0e, $00	; $2e 'Q'
; ***************************************** ZONE DATA *********************************************
!zone data
; $f170 initial vic reg values
VICRegs:
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $1b, $00, $00, $00, $00, $0b, $00	; display on, 25 rows, 40 columns
	!byte $1d, $ff, $00, $00, $ff, $00, $00, $00	; VM = $c400, CB = $f000, multicolor sprites
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
	!byte $52, SH , $1a, $1e, $1c, $12, $00, $24
	!byte $10, $02, $25, $00, $19, $1b, ADR, $a2
	!byte SH , $0c, $1e, $1d, $1d, $19, $18, $00
	!byte $19, $18, $00, $14, $19, $22, $1c, $1d
	!byte $13, $0d, $15, ADR, $f2, SH , $1d, $19
	!byte $00, $1c, $1d, $0b, $1b, $1d, $28, ADR
	!byte $6a,SH+1, $1c, $1a, $0d, $26, $10, $13	; $1c, $1a, $0d = "SPC"
	!byte $1b, $0f, ADR, $ba,SH+1, $24, $27, $25	; $27 = ','
	!byte $26, $16, $0f, $10, $1d, ADR, $0a,SH+2
	!byte $24, $28, $25, $26, $1b, $13, $11, $12	; $28 = '.'
	!byte $1d, ADR, $5a,SH+2, $24, $2e, $25, $26	; $2e = 'Q'
	!byte $1e, $1a, ADR, $aa,SH+2, $24, $0b, $25	; $0b = 'A'
	!byte $26, $0e, $19, $20, $18, ADR, $69, SH
	!byte $26, $00, $1c, $0d, $19, $1b, $0f, $00
	!byte $26, ADR, $e7, SH , $02, $01, $01, $00
	!byte $1a, $1d, $1c, $28, ADR, $5f,SH+1, $03
	!byte $01, $01, $00, $1a, $1d, $1c, $28, ADR
	!byte $d7, SH+1,$04, $01, $01, $00, $1a, $1d
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
; ***************************************** ZONE NOTES ********************************************
!zone notes
; $f4bd
Sound1: !byte $30, $40, $18, $04, $18, $03, $18, $02, $18
Sound2:	!byte $01, $00, $08, $04, $08, $19, $08, $03, $08
Sound3:	!byte $01, $00, $04, $04, $04, $19, $04, $03, $04

	!byte $01, $00

Notes:	!byte $00, $00, $0d, $0a, $72, $0b, $20, $0c
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
; ***************************************** ZONE P500 *********************************************
!zone code500
; $f53c	P500 I/O pointer init
Init:
	lda #SYSTEMBANK
	sta IndirectBank                ; select bank 15
	ldx #0
iniiolp:lda IOPointerTable,x            ; copy 8 IO pointer to ZP
	sta ColorRAM0,x
	inx
	cpx #IOPointerEnd-IOPointerTable; number of IO pointers
	bne iniiolp
	
	ldy #CREG
	lda (TPI1),y                    ; load TRI1 control register
	and #$0f                        ; clear CA, CB control bits#4-7 vic bank 0/15 select 
	ora #$a0                        ; set bit#5,4=10 CA=low -> Video matrix in bank 0
	sta (TPI1),y                    ; set bit#7,6=10 CB=high -> Characterset in bank 0 
	ldy #PC
	lda (TPI2),y                    ; load TPI2 port c
	ora #$c0                        ; set bit#6,7 vic 16k select bank $c000-$ffff
	sta (TPI2),y                    ; store to TPI2 port c
;	lda #$1c
;	ldy #$18                        ; VIC reg $18 memory pointers
;	sta (VIC),y                     ; set VM13-10=$1 screen at $c400, CB13,12,11,x=1100 char at $f000
;	lda #$7f                        ; bit#7=0 clears/mask out all 5 irq sources with bit#0-4 = 1
;	ldy #$0d                        ; CIA interrupt control register
;	sta (CIA),y                     ; disable all hardware interrupts
	lda #$04
	ldy #MIR
	sta (TPI1),y                    ; set TPI1 reg $5 interrupt mask reg = $04 - enable CIA irq
	lda #$ff
	ldy #PA
	sta (TPI2),y                    ; reset TPI2 port a to no column

	lda #$1f
	ldy #ICR
	sta (CIA),y			; clear all irq
	lda #$82
	sta (CIA),y			; set irq timer b
	lda #$01
	ldy #CRB
	sta (CIA),y			; timer b phi2, cont, start
	lda #$e0	; ***** x4 slower because longer irq handler > compensated with timer add 4
	ldy #TBLO
	sta (CIA),y			; timer b prescaler = 56
	lda #$00
	ldy #TBHI
	sta (CIA),y
	rts	
; -------------------------------------------------------------------------------------------------
; I/O pointer table
IOPointerTable:
	!word ColorRAMbase
	!word ColorRAMbase+$100
	!word ColorRAMbase+$200
	!word ColorRAMbase+$300
	!word VICbase
	!word VICbase+MOBY
	!word VICbase+MOBCOL
	!word SIDbase
	!word CIAbase
	!word TPI1base
	!word TPI2base
IOPointerEnd:
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
; Burwor
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
; Gorwor
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
; Thorwor
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
; Worlok
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
; $f9 Wizard of Wor right
	!byte $00, $08, $00, $00, $28, $00, $00, $2c
	!byte $00, $00, $2c, $00, $00, $2c, $00, $00
	!byte $2c, $00, $00, $28, $00, $00, $aa, $50
	!byte $00, $aa, $40, $00, $a8, $00, $00, $a8
	!byte $00, $00, $a0, $00, $00, $a0, $00, $00
	!byte $a0, $00, $00, $a0, $00, $00, $a8, $00
	!byte $00, $a8, $00, $00, $a8, $00, $00, $aa
	!byte $00, $02, $aa, $80, $02, $aa, $80, $00
; $fa Wizard of Wor left
	!byte $00, $20, $00, $00, $28, $00, $00, $38
	!byte $00, $00, $38, $00, $00, $38, $00, $00
	!byte $38, $00, $00, $28, $00, $05, $aa, $00
	!byte $01, $aa, $00, $00, $2a, $00, $00, $2a
	!byte $00, $00, $0a, $00, $00, $0a, $00, $00
	!byte $0a, $00, $00, $0a, $00, $00, $2a, $00
	!byte $00, $2a, $00, $00, $2a, $00, $00, $aa
	!byte $00, $02, $aa, $80, $02, $aa, $80, $00
; $fb Wizard of Wor down
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $20, $00, $00, $20
	!byte $00, $00, $2a, $a8, $00, $2a, $aa, $80
	!byte $2a, $aa, $a0, $2a, $aa, $a8, $2a, $2a
	!byte $a8, $28, $0a, $f8, $28, $02, $30, $20
	!byte $02, $00, $20, $02, $00, $20, $01, $00
	!byte $00, $01, $00, $00, $01, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
; $fc Wizard of Wor up
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
