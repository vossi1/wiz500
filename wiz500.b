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
FILL                    = $aa       ; fills free memory areas with $aa
NOPCODE                 = $ea       ; nop instruction for fill
GAMEBANK                = $00       ; Game code bank
SYSTEMBANK              = $0f       ; systembank
; color codes
BLACK                   = $00
WHITE                   = $01
RED                     = $02
GREEN                   = $05
BLUE                    = $06
YELLOW                  = $07
ORANGE                  = $08
BROWN                   = $09
LIGHTRED                = $0a
LIGHTBLUE               = $0e
GRAY3                   = $0f
MCM                     = $08       ; bit#3 for multicolor character
; game

; ************************************** P500 REGISTER ********************************************
VR_MODEY                = $11
VR_RASTER               = $12
VR_MOBENA               = $15
VR_MCMCSX               = $16
VR_MEMPT                = $18
VR_IRQ                  = $19
VR_EIRQ                 = $1a
VR_MOBXPA               = $1d
VR_MOBMOB               = $1e
VR_EXTCOL               = $20
VR_BGRCOL               = $21
VR_MOBCOL               = $27
SR_V1FREQ               = $00
SR_V1CTRL               = $04
SR_V1SR                 = $06
SR_V2FREQ               = $07
SR_V2CTRL               = $0b
SR_V2SR                 = $0d
SR_V3FREQ               = $0e
SR_V3CTRL               = $12
SR_MODVOL               = $18
SR_RANDOM               = $1b
; ************************************** P500 ADDRESSES *******************************************
!addr CodeBank          = $00       ; code bank register
!addr IndirectBank      = $01       ; indirect bank register
!addr CharROMbase       = $c000     ; Character ROM
!addr ColorRAMbase      = $d400     ; Color RAM
!addr VICbase           = $d800     ; VIC
!addr SIDbase           = $da00     ; SID
!addr CIAbase           = $dc00     ; CIA
!addr TPI1base          = $de00     ; TPI1
!addr TPI2base          = $df00     ; TPI2
!addr HW_IRQ            = $fffe     ; System IRQ Vector
!addr HW_NMI            = $fffa     ; System NMI Vector
; *************************************** C64 ADDRESSES *******************************************
!addr CPUPort64         = $01       ; 6510 CPU port
!addr CharROM64         = $d000     ; Character RAM
!addr ColorRAM64        = $d800     ; Color RAM
; ************************************** USER ADDRESSES *******************************************

; ***************************************** ZERO PAGE *********************************************
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
start:	sei
	cld
	ldx #$ff
	txs
	ldx #$2e
le007:  lda vicregs,x
	sta $d000,x
	dex
	bpl le007
	ldx #$18
le012:  lda sidregs,x
	sta $d400,x
	dex
	bpl le012
	ldx #$00
	txa
le01e:  sta $02,x
	sta $0200,x
	sta $0300,x
	inx
	bne le01e
	lda #$1f
	sta $dc0d
	lda #$82
	sta $dc0d
	lda #$01
	sta $dc0f
	lda #$38
	sta $dc06
	lda #$00
	sta $dc07
	cli
le043:  jsr le474
	jsr le2f5
le049:  jsr le310
le04c:  jsr le21e
	jsr le36b
	lda #$1f
	sta $d418
	ldx #$01
	lda $29
	bpl le05f
	ldx #$02
le05f:  txa
	jsr led31
le063:  lda $13
	bne le063
	jsr le1a9
	lda $d01e
	sta $55
	lda $d01f
	sta $56
	inc $2a
	lda $2a
	bne le089
	inc $2b
	lda #$05
	cmp $2b
	bne le089
	lda #$02
	jsr led31
	dec $29
le089:  lda $2b
	cmp #$10
	bcc le092
	jsr le855
le092:  jsr le754
	jsr le535
	jsr le855
	jsr le944
	jsr lebfe
	jsr lecab
	jsr leaef
	jsr lea17
	jsr ledc3
	lda $1e
	cmp #$ff
	beq le0be
	lda $26
	bne le063
	lda $1e
	bmi le0be
	jmp le3af
le0be:  dec $28
	bne le04c
	jmp le3da
le0c5:  ldx #$ff
	stx $dc02
	inx
	stx $dc03
	ldy #$00
	ldx #$fe
	jsr le120
	cpx #$ef
	bne le0da
	iny
le0da:  rts
; -------------------------------------------------------------------------------------------------
; $
le0db:  ldx #$ff
	stx $dc02
	inx
	stx $dc03
	lda #$1f
	ldx #$df
	jsr le120
	cpx #$fb
	bne le0f3
	and #$fb
	bne le11b
le0f3:  cpx #$fd
	bne le0fb
	and #$fe
	bne le11b
le0fb:  cpx #$ef
	bne le103
	and #$fd
	bne le11b
le103:  ldx #$fd
	jsr le120
	cpx #$fb
	bne le110
	and #$ef
	bne le11b
le110:  ldx #$bf
	jsr le120
	cpx #$fb
	bne le11b
	and #$f7
le11b:  sta $08
	jmp le12c
le120:  stx $dc00
le123:  ldx $dc01
	cpx $dc01
	bne le123
	rts
; -------------------------------------------------------------------------------------------------
; $
le12c:  ldx #$00
	stx $2c
	stx $dc02
	stx $dc03
le136:  lda $dc01
	cmp $dc01
	bne le136
	and $08
	tay
	and #$10
	bne le14b
	lda #$80
	sta $2c
	bne le16d
le14b:  tya
	and #$02
	bne le154
	ldx #$02
	bne le16d
le154:  tya
	and #$01
	bne le15d
	ldx #$01
	bne le16d
le15d:  tya
	and #$04
	bne le166
	ldx #$03
	bne le16d
le166:  tya
	and #$08
	bne le16d
	ldx #$04
le16d:  stx $46
	rts
; -------------------------------------------------------------------------------------------------
; $
le170:  lda $13
	bne le170
	tya
	pha
	txa
	pha
	jsr ledc3
	pla
	tax
	pla
	tay
le17f:  lda $13
	bne le17f
	dex
	bne le170
	rts
; -------------------------------------------------------------------------------------------------
; $
le187:  ldx #$00
le189:  lda $14
	sta $d800,x
	sta $d900,x
	sta $da00,x
	sta $dae8,x
	lda #$00
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $06e8,x
	inx
	bne le189
	rts
; -------------------------------------------------------------------------------------------------
; $
le1a9:  ldx #$08
le1ab:  lda $4c,x
	sta $07f7,x
	dex
	bne le1ab
	ldx #$08
le1b5:  lda $4c,x
	cmp $07f7,x
	bne le1a9
	dex
	bne le1b5
	rts
; -------------------------------------------------------------------------------------------------
; $
le1c0:  stx $09
	sty $0a
le1c4:  ldy #$00
	sty $0e
	lda ($09),y
	sta $0b
	iny
	lda ($09),y
	sta $0c
le1d1:  iny
	lda ($09),y
	cmp #$ff
	beq le219
	cmp #$fe
	beq le1f6
	cmp #$fd
	beq le206
	sty $0d
	ldy $0e
	sta ($0b),y
	inc $0e
	ldy $0d
	lda $4c
	beq le1d1
	ldx #$0a
	jsr le170
	jmp le1d1
; -------------------------------------------------------------------------------------------------
; $
le1f6:  iny
	clc
	tya
	adc $09
	sta $09
	lda $0a
	adc #$00
	sta $0a
	jmp le1c4
; -------------------------------------------------------------------------------------------------
; $
le206:  clc
	lda $0b
	adc #$28
	sta $0b
	lda $0c
	adc #$00
	sta $0c
	lda #$00
	sta $0e
	beq le1d1
le219:  lda #$00
	sta $4c
	rts
; -------------------------------------------------------------------------------------------------
; $
le21e:  lda #$03
	sta $14
	jsr le187
	ldx #<Table03
	ldy #>Table03
	jsr le1c0
	lda $3b
	and #$03
	asl
	tax
	lda Table05,x
	sta $44
	lda Table05+1,x
	sta $45
	lda #$00
	sta $12
	sta $3d
	sta $3c
le244:  ldy $12
	lda ($44),y
	cmp #$19
	bne le24f
	jsr le2a3
le24f:  cmp #$18
	bne le256
	jsr le2c1
le256:  cmp #$1a
	bne le25d
	jsr le2dc
le25d:  inc $12
	lda #$03
	clc
	adc $3c
	sta $3c
	cmp #$27
	bne le244
	lda #$00
	sta $3c
	lda #$03
	clc
	adc $3d
	sta $3d
	cmp #$15
	bne le244
	lda #$00
	jsr le429
	ldx $28
	stx $38
	lda #$1e
	sta $36
	lda #$18
	sta $37
	jsr le72e
	ldx #$d0
	ldy #$07
	stx $09
	sty $0a
	ldx #$03
	jsr le440
	rts
; -------------------------------------------------------------------------------------------------
; $e29b
Table05:
	!word Tablei1
	!word Tablei2
	!word Tablei3
	!word Tablei4
; -------------------------------------------------------------------------------------------------
; $
le2a3:  ldx #$00
le2a5:  lda $3d
	clc
	adc #$02
	sta $37
	txa
	clc
	adc $3c
	sta $36
	lda #$2b
	sta $38
	jsr le72e
	inx
	cpx #$03
	bne le2a5
	lda #$05
	rts
; -------------------------------------------------------------------------------------------------
; $
le2c1:  ldx #$00
le2c3:  lda $3c
	sta $36
	txa
	clc
	adc $3d
	sta $37
	lda #$2a
	sta $38
	jsr le72e
	inx
	cpx #$03
	bne le2c3
	lda #$05
	rts
; -------------------------------------------------------------------------------------------------
; $
le2dc:  jsr le2a3
	jsr le2c1
	lda #$02
	clc
	adc $3d
	sta $37
	lda $3c
	sta $36
	lda #$2c
	sta $38
	jsr le72e
	rts
; -------------------------------------------------------------------------------------------------
; $
le2f5:  ldy #$00
	sty $02
	sty $03
	sty $04
	sty $3b
	sty $76
	lda #$03
	sta $28
	lda #$06
	ldx #$07
le309:  sta $d027,x
	dex
	bpl le309
	rts
; -------------------------------------------------------------------------------------------------
; $
le310:  ldx #$00
	stx $d015
le315:  lda #$ff
	sta $1e,x
	inx
	cpx #$08
	bne le315
	ldy #$00
	sty $2b
	sty $2a
	sty $29
	inc $3b
	ldx $3b
	cpx #$04
	bcc le330
	ldx #$04
le330:  lda Table06,x
	sta $27
	lda Table06+4,x
	sta $47
	lda Table06+8,x
	sta $48
	lda Table06+12,x
	sta $49
	lda Table06+16,x
	sta $4a
	lda Table06+20,x
	sta $26
	lda #$00
	sta $4b
	rts
; -------------------------------------------------------------------------------------------------
; 
Table06	= *-1
	!byte $04, $05, $06, $06
	!byte $03, $04, $05, $06
	!byte $02, $04, $06, $06
	!byte $01, $02, $03, $04
	!byte $00, $01, $00, $02
	!byte $0a, $10, $14, $18
; -------------------------------------------------------------------------------------------------
; 
le36b:  lda #$e6
	sta $4d
	sta $07f8
	lda #$03
	sta $2e
	lda #$01
	sta $d010
	lda #$37
	sta $d000
	lda #$ab
	sta $d001
	lda #$fd
	sta $54
	ldx #$ff
	stx $25
	stx $24
	inx
	stx $1e
	stx $2d
	lda $d015
	ora #$01
	and #$7f
	sta $d015
	rts
; -------------------------------------------------------------------------------------------------
; $e39f
TableX:  
	!byte $35, $4d, $65, $7d, $95, $ad, $c5, $f5
; $e3a7
TableY:
	!byte $7b, $63, $7b, $93, $ab, $c3, $7b, $ab

le3af:  lda #$00
	sta $d015
	ldx #<Table02
	ldy #>Table02
	jsr le1c0
	lda #$07
	jsr led31
	lda #$30
	jsr le429
	ldx #$00
	jsr le170
	jmp le049

Table02:
!byte $24, $05, $0c, $19, $18, $1e, $1c, $00
!byte $04, $01, $01, $01, $ff

le3da:  ldx #$02
le3dc:  lda $05,x
	cmp $02,x
	bcc le3e7
	bne le400
	dex
	bpl le3dc
le3e7:  lda $02
	sta $05
	lda $03
	sta $06
	lda $04
	sta $07
	ldx #$d0
	ldy #$07
	stx $09
	sty $0a
	ldx #$03
	jsr le440
le400:  lda #$00
	sta $d418
	inc $4c
	ldx #<Table01
	ldy #>Table01
	jsr le1c0
	ldx #$50
	jsr le170
	jmp le043

Table01:!byte $24, $05, $11, $00, $0b, $00, $17, $00
	!byte $0f, $00, $00, $19, $00, $1f, $00, $0f
	!byte $00, $1b, $ff

le429:  clc
	sed
	adc $03
	sta $03
	lda $04
	adc #$00
	sta $04
	cld
	ldx #$c4
	ldy #$07
	stx $09
	sty $0a
	ldx #$00
le440:  ldy #$05
le442:  lda $02,x
	and #$0f
	clc
	adc #$01
	sta ($09),y
	dey
	lda $02,x
	lsr
	lsr
	lsr
	lsr
	clc
	adc #$01
	sta ($09),y
	inx
	dey
	bpl le442
	lda $04
	cmp #$02
	bcc le473
	lda $76
	bne le473
	inc $76
	inc $28
	ldx $28
	stx $07de
	lda #$07
	jsr led31
le473:  rts
; -------------------------------------------------------------------------------------------------
; $
le474:  lda #$00
	sta $d015
	lda #$06
	sta $14
	jsr le187
	lda #$01
	sta $d021
	sta $d020
	ldy #>Table04
	ldx #<Table04
	jsr le1c0
	ldx #$04
le491:  txa
	asl
	tay
	lda #$d7
	sta $1f,x
	sta $d002,y
	lda Table10,x
	sta $d003,y
	lda Table11,x
	sta $07f9,x
	lda Table08,x
	sta $d028,x
	dex
	bpl le491
	lda #$00
	sta $d010
	lda #$3e
	sta $d015
le4ba:  lda $13
	bne le4ba
le4be:  lda $13
	bne le4be
	inc $2b
	lda $2b
	and #$1f
	bne le508
	ldy #$04
le4cc:  tya
	asl
	tax
	lda $001f,y
	beq le4ef
	inc $d002,x
	lda $d002,x
	cmp Table12,y
	bcc le505
	lda #$00
	sta $001f,y
	cpy #$03
	beq le505
	tya
	tax
	inc $07f9,x
	bne le505
le4ef:  dec $d002,x
	lda $d002,x
	cmp #$d7
	bcs le505
	sta $001f,y
	cpy #$03
	beq le505
	tya
	tax
	dec $07f9,x
le505:  dey
	bpl le4cc
le508:  jsr le0c5
	cpy #$00
	beq le4ba
	lda #$00
	sta $d021
	lda #$06
	sta $d020
	rts
; -------------------------------------------------------------------------------------------------
; $e51a
Table10:
	!byte $52, $6a, $82, $9a, $b2
Table11:
	!byte $e9, $ed, $f1, $f5, $f9
Table12:
	!byte $db, $dc, $db, $dc, $db
; -------------------------------------------------------------------------------------------------
; $e529 interrupt
Interrupt:
	pha
	lda $dc0d
	and #$02
	beq le533
	inc $13
le533:  pla
	rti

le535:  lda $2a
	and #$01
	beq le53c
	rts
; -------------------------------------------------------------------------------------------------
; $
le53c:  lda $1e
	bpl le541
	rts
; -------------------------------------------------------------------------------------------------
; $
le541:  jsr le0db
	lda #$00
	sta $30
	lda #$01
	sta $31
	lda $46
	sta $32
	lda $2e
	sta $35
	jsr le56f
	lda $35
	sta $2e
	ldx $32
	lda Table13,x
	bne le567
	ldx $2e
	lda Table13,x
le567:  sta $4d
	rts
; -------------------------------------------------------------------------------------------------
; $e56a
Table13:  
	!byte $00, $e8, $e7, $e6, $e5
; -------------------------------------------------------------------------------------------------
; $e56f
le56f:  ldx $30
	lda #$00
	sta $09
	jsr le65d
	lda $31
	sta $41
	lda $d000,x
	sta $3f
	lda $d001,x
	sta $40
	jsr le709
	lda $32
	bne le590
	jmp le6b3
le590:  lda $35
	cmp #$03
	bcs le5ca
	lda $32
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
	inc $09
	sec
	sbc #$03
	jmp le5a0
le5b6:  ldx $09
	lda Table14+13,x
	ldx $30
	sta $d001,x
	jmp le601
le5c3:  lda #$00
	sta $32
	jmp le6b3
le5ca:  lda $32
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
	inc $09
	sec
	sbc #$03
	jmp le5d5
le5eb:  ldx $09
	cpx #$0a
	bne le5f9
	lda $31
	ora $d010
	sta $d010
le5f9:  lda Table14,x
	ldx $30
	sta $d000,x
le601:  lda $32
	sta $35
	ldx $30
	lda $32
	cmp #$01
	bne le616
	dec $d001,x
	dec $d001,x
	jmp le6b3
le616:  cmp #$02
	bne le623
	inc $d001,x
	inc $d001,x
	jmp le6b3
le623:  cmp #$03
	bne le641
	dec $d000,x
	dec $d000,x
	lda $d000,x
	cmp #$fe
	bcc le63e
	lda $31
	eor #$ff
	and $d010
	sta $d010
le63e:  jmp le6b3
le641:  cmp #$04
	bne le6b3
	inc $d000,x
	inc $d000,x
	lda $d000,x
	cmp #$02
	bcs le6b3
	lda $31
	ora $d010
	sta $d010
	jmp le6b3
le65d:  lda $d000,x
	sta $3f
	lda $d001,x
	clc
	sbc #$06
	sta $40
	lda $31
	sta $41
	jsr le709
	lda $32
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
	sta $3a
	lda $3f
le696:  ldy $40
	beq le6a6
	dec $40
	clc
	adc #$28
	bcc le696
	inc $3a
	jmp le696
le6a6:  sta $39
	ldy #$00
	lda ($39),y
	beq le6b2
	lda #$00
	sta $32
le6b2:  rts
; -------------------------------------------------------------------------------------------------
; $
le6b3:  lda $31
	and $d010
	bne le6cf
	lda $d000,x
	cmp #$16
	bcs le6e7
	lda $31
	ora $d010
	sta $d010
	lda #$40
	sta $d000,x
	rts
; -------------------------------------------------------------------------------------------------
; $
le6cf:  beq le6e7
	lda $d000,x
	cmp #$42
	bcc le6e7
	lda $31
	eor #$ff
	and $d010
	sta $d010
	lda #$18
	sta $d000,x
le6e7:  lda $d001,x
	cmp #$31
	bcs le6f4
	inc $d001,x
	inc $d001,x
le6f4:  rts
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
	bcs le71d
	clc
	bcc le728
le71d:  pha
	lda $41
	and $d010
	clc
	beq le727
	sec
le727:  pla
le728:  ror
	lsr
	lsr
	sta $3f
	rts
; -------------------------------------------------------------------------------------------------
; $
le72e:  lda #$00
	sta $3a
	lda $36
le734:  ldy $37
	beq le744
	dec $37
	clc
	adc #$28
	bcc le734
	inc $3a
	jmp le734
le744:  sta $39
	lda $3a
	clc
	adc #$04
	sta $3a
	ldy #$00
	lda $38
	sta ($39),y
	rts
; -------------------------------------------------------------------------------------------------
; $
le754:  lda $25
	bpl le75b
	jmp le7fc
le75b:  lda $56
	and #$80
	beq le776
le761:  lda #$ff
	sta $25
	lda #$7f
	and $d015
	sta $d015
	lda #$7f
	and $d010
	sta $d010
	rts
; -------------------------------------------------------------------------------------------------
; $
le776:  lda $d010
	and #$80
	bne le786
	lda $d00e
	cmp #$14
	bcc le761
	bcs le78d
le786:  lda $d00e
	cmp #$42
	bcs le761
le78d:  lda $2d
	cmp #$03
	bcc le7cd
	bne le7b1
	dec $d00e
	dec $d00e
	dec $d00e
	dec $d00e
	lda $d00e
	cmp #$fc
	bcc le7b0
	lda #$7f
	and $d010
	sta $d010
le7b0:  rts
; -------------------------------------------------------------------------------------------------
; $
le7b1:  inc $d00e
	inc $d00e
	inc $d00e
	inc $d00e
	lda $d00e
	cmp #$04
	bcs le7b0
	lda #$80
	ora $d010
	sta $d010
	rts
; -------------------------------------------------------------------------------------------------
; $
le7cd:  cmp #$01
	bne le7e5
	dec $d00f
	dec $d00f
	dec $d00f
	dec $d00f
	lda $d00f
	cmp #$2a
	bcc le7f8
	rts
; -------------------------------------------------------------------------------------------------
; $
le7e5:  inc $d00f
	inc $d00f
	inc $d00f
	inc $d00f
	lda $d00f
	cmp #$ca
	bcc le7fb
le7f8:  jmp le761
le7fb:  rts
; -------------------------------------------------------------------------------------------------
; $
le7fc:  cmp #$ff
	bne le804
	lda $1e
	bpl le805
le804:  rts
; -------------------------------------------------------------------------------------------------
; $
le805:  lda $2c
	bmi le80a
	rts
; -------------------------------------------------------------------------------------------------
; $
le80a:  lda #$04
	jsr led31
	lda $d000
	sta $d00e
	lda $d001
	sta $d00f
	lda $d010
	and #$01
	beq le82d
	lda #$80
	ora $d010
	sta $d010
	jmp le835
le82d:  lda #$7f
	and $d010
	sta $d010
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
	ora $d015
	sta $d015
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
	lda $d41b
	and #$01
	beq le8cb
	lda bits,x
	ora #$01
	and $d010
	beq le8ab
	cmp #$01
	beq le8b3
	cmp bits,x
	beq le8bf
le8ab:  lda $d000
	cmp $d000,x
	bcc le8bf
le8b3:  lda $d41b
	and #$03
	beq le8c6
le8ba:  lda #$04
	jmp le8e8
le8bf:  lda $d41b
	and #$03
	beq le8ba
le8c6:  lda #$03
	jmp le8e8
le8cb:  lda $d001
	cmp $d001,x
	bcc le8df
	lda $d41b
	and #$03
	beq le8e6
le8da:  lda #$02
	jmp le8e8
le8df:  lda $d41b
	and #$03
	beq le8da
le8e6:  lda #$01
le8e8:  ldx $10
	sta $32
	lda $67,x
	sta $35
	jsr le56f
	lda $32
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
	sta $4d,x
le90f:  jmp le86d
le912:  lda $35
	clc
	adc #$01
	cmp #$04
	bcc le920
	lda $d41b
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
	and $1f,x
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
	lda #$03
	jsr led31
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
	lda $d41b
	and #$07
	tax
	lda TableX,x
	sta $d000,y
	lda $2a
	lsr
	lsr
	lsr
	lsr
	lsr
	and #$07
	tax
	lda TableY,x
	sta $d001,y
	clc
	adc #$18
	cmp $d001
	bcs le9e5
	sec
	sbc #$30
	cmp $d001
	bcc le9e5
	lda $d001
	sbc #$24
	sta $d001,y
le9e5:  pla
	tax
	pla
	tay
	inc $1e,x
	lda Table07,y
	sta $56,x
	lda Table09,y
	sta $4d,x
	sta $07f8,x
	lda Table08,y
	sta $d027,x
	lda bits,x
	ora $d015
	sta $d015
	rts
; -------------------------------------------------------------------------------------------------
; $ea08
Table07:  
	!byte $00, $05, $0a, $0f, $14
Table08:
	!byte $06, $07, $03, $0d, $04
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
	sta $4d
	lda #$06
	jsr led31
	jmp lea1d
lea3c:  cmp #$90
	bne lea47
	lda #$f7
	sta $4d
	jmp lea1d
lea47:  cmp #$a0
	bne lea52
	lda #$f6
	sta $4d
	jmp lea1d
lea52:  cmp #$b0
	bne lea5d
	lda #$f7
	sta $4d
	jmp lea1d
lea5d:  cmp #$c0
	bne lea68
	lda #$f8
	sta $4d
	jmp lea1d
lea68:  cmp #$d0
	bne lea74
	lda $d015
	and #$fe
	sta $d015
lea74:  jmp lea1d
lea77:  lda $1e,x
	cmp #$81
	bne leaa6
	lda #$f6
	sta $4d,x
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
	lda $d41b
	and #$30
	clc
	adc #$10
lea9b:  jsr le429
lea9e:  lda #$05
	jsr led31
	jmp lea1d
leaa6:  cmp #$90
	bne leab1
	lda #$f7
	sta $4d,x
	jmp lea1d
leab1:  cmp #$a0
	bne leabc
	lda #$f8
	sta $4d,x
	jmp lea1d
leabc:  cmp #$b0
	bne leace
	lda bits,x
	eor #$ff
	and $d015
	sta $d015
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
	lda $d000
	sta $09
	lda #$00
	sta $0a
	lda #$01
	and $d010
	beq leb1c
	inc $0a
leb1c:  lda $d001
	sta $0b
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
	sta $0a
	lda #$80
	and $d010
	beq leb3c
	inc $0a
leb3c:  lda $d00e
	sta $09
	lda $d00f
	sta $0b
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
	and $d015
	sta $d015
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
	lda $d000,y
	sta $0c
	lda $d001,y
	sta $0e
	lda #$00
	sta $0d
	lda $d010
leb96:  and bits,x
	beq leb9d
	inc $0d
leb9d:  lda #$0e
	sta $0f
	lda #$0e
	sta $10
	lda #$0b
	sta $11
	lda #$17
	sta $12
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
	lda $0e
	sbc $0f
	cmp $0b
	bcs lebfc
	clc
	lda $0e
	adc $10
	cmp $0b
	bcc lebfc
	sec
	lda $0c
	sbc $11
	sta $0c
	lda $0d
	sbc #$00
	sta $0d
	sec
	lda $0c
	sbc $09
	lda $0d
	sbc $0a
	bcs lebfc
	lda $0c
	adc $12
	sta $0c
	lda $0d
	adc #$00
	sta $0d
	sec
	lda $0c
	sbc $09
	lda $0d
	sbc $0a
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
	lda $d000
	cmp $d000,y
	beq lec65
	lda $d001
	cmp $d001,y
	bne lec19
	lda $d000
	cmp $d000,y
	lda $d010
	and #$01
	bne lec49
	lda $d010
	and bits,x
	bne lec51
	beq lec57
lec49:  lda $d010
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
lec65:  lda $d001
	cmp $d001,y
	lda #$02
	bcs lec71
	lda #$01
lec71:  sta $77
	lda #$fe
	sta $53
lec77:  lda #$00
	sta $24
	lda $d000,y
	sta $d00c
	lda $d001,y
	sta $d00d
	lda $d010
	and bits,x
	beq lec96
	lda $d010
	ora #$40
	bne lec9b
lec96:  lda $d010
	and #$bf
lec9b:  sta $d010
	lda $d015
	ora #$40
	sta $d015
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
	lda $d015
	and #$bf
	sta $d015
	rts
; -------------------------------------------------------------------------------------------------
; $
lecc3:  lda $d010
	and #$40
	bne lecd3
	lda $d00c
	cmp #$14
	bcc lecb6
	bcs lecda
lecd3:  lda $d00c
	cmp #$42
	bcs lecb6
lecda:  lda $77
	cmp #$03
	bcc led0e
	bne lecf8
	dec $d00c
	dec $d00c
	lda $d00c
	cmp #$fe
	bcc led2d
	lda $d010
	and #$bf
	sta $d010
	rts
; -------------------------------------------------------------------------------------------------
; $
lecf8:  inc $d00c
	inc $d00c
	lda $d00c
	cmp #$02
	bcs led2d
	lda $d010
	ora #$40
	sta $d010
	rts
; -------------------------------------------------------------------------------------------------
; $
led0e:  cmp #$01
	bne led20
	dec $d00d
	dec $d00d
	lda $d00d
	cmp #$2a
	bcc led2e
	rts
; -------------------------------------------------------------------------------------------------
; $
led20:  inc $d00d
	inc $d00d
	lda $d00d
	cmp #$ca
	bcs led2e
led2d:  rts
; -------------------------------------------------------------------------------------------------
; $
led2e:  jmp lecb6
led31:  cmp #$01
	bne led49
	sta $6f
	lda #$43
	sta $70
	lda #$ee
	sta $71
	lda #$21
	sta $d404
	lda #$01
	sta $72
	rts
; -------------------------------------------------------------------------------------------------
; $
led49:  cmp #$02
	bne led5c
	sta $6f
	lda #$4c
	sta $70
	lda #$ee
	sta $71
	lda #$01
	sta $72
	rts
; -------------------------------------------------------------------------------------------------
; $
led5c:  cmp #$03
	bne led6f
	sta $6f
	lda #$55
	sta $70
	lda #$ee
	sta $71
	lda #$01
	sta $72
	rts
; -------------------------------------------------------------------------------------------------
; $
led6f:  cmp #$04
	bne led7d
	lda #$0f
	sta $74
	lda #$81
	sta $d40b
	rts
; -------------------------------------------------------------------------------------------------
; $
led7d:  cmp #$05
	bne led8b
	lda #$0f
	sta $75
	lda #$81
	sta $d412
	rts
; -------------------------------------------------------------------------------------------------
; $
led8b:  cmp #$06
	bne led9e
	lda #$3f
	sta $75
	sta $74
	lda #$81
	sta $d40b
	sta $d412
	rts
; -------------------------------------------------------------------------------------------------
; $
led9e:  cmp #$07
	bne ledb4
	lda #$3f
	sta $73
	asl
	sta $d408
	lda #$21
	sta $d40b
	lda #$00
	sta $74
	rts
; -------------------------------------------------------------------------------------------------
; $
ledb4:  cmp #$08
	bne ledc2
	lda #$07
	sta $73
	lda #$21
	sta $d40b
	rts
; -------------------------------------------------------------------------------------------------
; $
ledc2:  rts
; -------------------------------------------------------------------------------------------------
; $
ledc3:  lda $74
	beq ledd6
	asl
	asl
	asl
	sta $d408
	dec $74
	bne ledd6
	lda #$80
	sta $d40b
ledd6:  lda $75
	beq ledea
	asl
	asl
	adc #$07
	sta $d40f
	dec $75
	bne ledea
	lda #$80
	sta $d412
ledea:  lda $73
	beq lee04
	lda $73
	lsr
	lsr
	and #$01
	tax
	lda Notes1,x
	sta $d408
	dec $73
	bne lee04
	lda #$20
	sta $d40b
lee04:  dec $72
	bne lee42
	clc
	lda $70
	adc #$02
	sta $70
	lda $71
	adc #$00
	sta $71
	ldy #$00
	lda ($70),y
	bne lee2e
	lda $6f
	cmp #$01
	bne lee24
	jmp led31
lee24:  cmp #$02
	bne lee2b
	jmp led49
lee2b:  jmp led5c
; -------------------------------------------------------------------------------------------------
; $
lee2e:  sta $72
	ldy #$01
	lda ($70),y
	asl
	tax
	lda Notes2,x
	sta $d400
	lda Notes2+1,x
	sta $d401
lee42:  rts
; ***************************************** ZONE NOTES ********************************************
!zone notes
; $ee43
Notes1: !byte $30, $40, $18, $04, $18, $03, $18, $02
	!byte $18, $01, $00, $08, $04, $08, $19, $08
	!byte $03, $08, $01, $00, $04, $04, $04, $19
	!byte $04, $03, $04, $01, $00
; $ee60
Notes2:
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
; ***************************************** ZONE DATA2 ********************************************
; $f000 font
!zone data2
*= $f000
	!byte $00, $00, $00, $00, $00, $00, $00, $00	; space
	!byte $3c, $66, $6e, $76, $66, $66, $3c, $00	; 0
	!byte $18, $18, $38, $18, $18, $18, $7e, $00
	!byte $3c, $66, $06, $0c, $30, $60, $7e, $00
	!byte $3c, $66, $06, $1c, $06, $66, $3c, $00
	!byte $06, $0e, $1e, $66, $7f, $06, $06, $00
	!byte $7e, $60, $7c, $06, $06, $66, $3c, $00
	!byte $3c, $66, $60, $7c, $66, $66, $3c, $00
	!byte $7e, $66, $0c, $18, $18, $18, $18, $00
	!byte $3c, $66, $66, $3c, $66, $66, $3c, $00
	!byte $3c, $66, $66, $3c, $06, $66, $3c, $00	; 9
	!byte $18, $3c, $66, $7e, $66, $66, $66, $00	; A
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
	!byte $7c, $66, $66, $78, $60, $60, $60, $00	; P
	!byte $7c, $66, $66, $7c, $78, $6c, $66, $00	; R
	!byte $3c, $66, $60, $3c, $06, $66, $3c, $00
	!byte $7e, $18, $18, $18, $18, $18, $18, $00
	!byte $66, $66, $66, $66, $66, $66, $3c, $00
	!byte $66, $66, $66, $66, $66, $3c, $18, $00
	!byte $63, $63, $63, $6b, $7f, $77, $63, $00
	!byte $66, $66, $3c, $18, $3c, $66, $66, $00
	!byte $66, $66, $66, $3c, $18, $18, $18, $00	; Y
	!byte $03, $06, $0c, $18, $30, $60, $c0, $00	; /
	!byte $0c, $18, $30, $30, $30, $18, $0c, $00	; (
	!byte $30, $18, $0c, $0c, $0c, $18, $30, $00	; )
	!byte $00, $00, $00, $7e, $00, $00, $00, $00	; -
	!byte $00, $00, $18, $00, $00, $18, $18, $30	; ;
	!byte $00, $00, $00, $00, $00, $18, $18, $00	; .
	!byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa	; ||||
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0	; |
	!byte $00, $00, $00, $00, $00, $00, $00, $ff	; _
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $ff	; L
	!byte $ff, $00, $00, $00, $00, $00, $00, $00	; -
; $f170
vicregs:
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $1b, $00, $00, $00, $00, $0b, $00
	!byte $1d, $ff, $00, $00, $ff, $00, $00, $00
	!byte $06, $00, $00, $00, $00, $02, $07, $06
	!byte $06, $06, $06, $06, $06, $06, $06
; $f19f
sidregs:
	!byte $00, $20, $00, $00, $20, $00, $f9, $00
	!byte $60, $00, $00, $80, $00, $fa, $ff, $ff
	!byte $00, $00, $80, $00, $fa, $ff, $ff, $07

; $f1b7
	!byte $1f
; $f1b8
Tablei1:
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
; $f213
Tablei2:
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
; $f26e
Tablei3:
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
; $f2c9
Tablei4:
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
Table03:
	!byte $00, $04, $2a, $fd, $2a, $fd, $2a, $fd
	!byte $2a, $fd, $2a, $fd, $2a, $fd, $fd, $fd
	!byte $fd, $2a, $fd, $2a, $fd, $2a, $fd, $2a
	!byte $fd, $2a, $fd, $2a, $fd, $2a, $fd, $2a
	!byte $fd, $2a, $fd, $2a, $fd, $2a, $fd, $2c
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $2b, $2b
	!byte $2b, $2b, $2b, $2b, $2b, $2b, $fe, $27
	!byte $04, $2a, $fd, $2a, $fd, $2a, $fd, $2a
	!byte $fd, $2a, $fd, $2a, $fd, $fd, $fd, $fd
	!byte $2a, $fd, $2a, $fd, $2a, $fd, $2a, $fd
	!byte $2a, $fd, $2a, $fd, $2a, $fd, $2a, $fd
	!byte $2a, $fd, $2a, $fd, $2a, $fd, $2a, $fe
	!byte $72, $07, $1c, $0d, $19, $1b, $0f, $00
	!byte $00, $00, $00, $12, $13, $26, $1c, $0d
	!byte $19, $1b, $0f, $fe, $8a, $07, $1a, $16
	!byte $0b, $22, $0f, $1b, $ff
; $f3c2
Table04:
	!byte $52, $04, $1a, $1e, $1c, $12, $00, $24
	!byte $10, $02, $25, $00, $19, $1b, $fe, $a2
	!byte $04, $0c, $1e, $1d, $1d, $19, $18, $00
	!byte $19, $18, $00, $14, $19, $22, $1c, $1d
	!byte $13, $0d, $15, $fe, $f2, $04, $1d, $19
	!byte $00, $1c, $1d, $0b, $1b, $1d, $28, $fe
	!byte $6a, $05, $24, $0b, $25, $26, $10, $13
	!byte $1b, $0f, $fe, $ba, $05, $24, $16, $25
	!byte $26, $16, $0f, $10, $1d, $fe, $0a, $06
	!byte $24, $27, $25, $26, $1b, $13, $11, $12
	!byte $1d, $fe, $5a, $06, $24, $1a, $25, $26
	!byte $1e, $1a, $fe, $aa, $06, $24, $28, $25
	!byte $26, $0e, $19, $20, $18, $fe, $69, $04
	!byte $26, $00, $1c, $0d, $19, $1b, $0f, $00
	!byte $26, $fe, $e7, $04, $02, $01, $01, $00
	!byte $1a, $1d, $1c, $28, $fe, $5f, $05, $03
	!byte $01, $01, $00, $1a, $1d, $1c, $28, $fe
	!byte $d7, $05, $04, $01, $01, $00, $1a, $1d
	!byte $1c, $28, $fe, $4f, $06, $05, $01, $01
	!byte $00, $1a, $1d, $1c, $28, $fe, $c3, $06
	!byte $17, $22, $1c, $1d, $0f, $1b, $22, $00
	!byte $1a, $1d, $1c, $28, $fe, $37, $07, $0f
	!byte $21, $1d, $1b, $0b, $00, $0c, $19, $18
	!byte $1e, $1c, $fe, $61, $07, $10, $19, $1b
	!byte $00, $03, $01, $01, $01, $01, $00, $1a
	!byte $1d, $1c, $28, $fe, $78, $07, $02, $0a
	!byte $09, $03, $00, $0c, $22, $00, $0d, $19
	!byte $17, $17, $19, $0e, $19, $1b, $0f, $00
	!byte $16, $1d, $0e, $fe, $a0, $07, $02, $0a
	!byte $09, $02, $00, $0c, $22, $00, $0c, $0b
	!byte $16, $16, $22, $23, $17, $13, $0e, $20
	!byte $0b, $22, $ff
; $f4bd
; ***************************************** ZONE DATA3 ********************************************
; $f940 sprites?
!zone data3
*= $f940
	!byte $00, $2a, $00, $00, $29, $00, $00, $2a
	!byte $00, $00, $2a, $00, $00, $2a, $00, $00
	!byte $08, $00, $02, $2a, $00, $02, $a9, $00
	!byte $02, $69, $08, $02, $69, $59, $02, $69
	!byte $48, $02, $a8, $00, $02, $28, $00, $00
	!byte $2a, $00, $00, $a2, $00, $00, $a2, $80
	!byte $02, $80, $80, $02, $80, $a0, $0a, $00
	!byte $30, $0a, $00, $30, $00, $00, $00, $00
	!byte $00, $0a, $80, $00, $06, $80, $00, $0a
	!byte $80, $00, $0a, $80, $00, $0a, $80, $00
	!byte $02, $00, $00, $0a, $88, $00, $06, $a8
	!byte $02, $06, $98, $06, $56, $98, $02, $16
	!byte $98, $00, $02, $a8, $00, $02, $88, $00
	!byte $0a, $80, $00, $08, $a0, $00, $28, $a0
	!byte $00, $20, $28, $00, $a0, $28, $00, $80
	!byte $0a, $02, $00, $0a, $00, $00, $00, $00
	!byte $00, $00, $01, $00, $00, $0a, $00, $00
	!byte $0a, $00, $0a, $8a, $00, $09, $a8, $00
	!byte $0a, $a8, $0a, $8a, $a8, $0a, $8a, $a0
	!byte $0a, $aa, $a0, $09, $aa, $80, $09, $85
	!byte $a8, $00, $85, $aa, $00, $01, $0a, $00
	!byte $01, $00, $00, $01, $00, $00, $0a, $00
	!byte $00, $0a, $00, $00, $01, $00, $00, $01
	!byte $00, $00, $01, $00, $00, $00, $00, $00
	!byte $00, $01, $00, $00, $01, $00, $00, $01
	!byte $00, $00, $0a, $00, $00, $0a, $00, $00
	!byte $01, $00, $00, $01, $02, $00, $01, $0a
	!byte $09, $85, $aa, $09, $85, $a8, $09, $aa
	!byte $80, $0a, $aa, $80, $0a, $8a, $a0, $0a
	!byte $8a, $a0, $00, $09, $a8, $00, $08, $a8
	!byte $00, $0a, $8a, $00, $00, $0a, $00, $00
	!byte $0a, $00, $00, $02, $00, $00, $00, $00
	!byte $00, $a0, $00, $00, $28, $00, $a0, $2a
	!byte $00, $a0, $ad, $80, $a0, $2d, $80, $a0
	!byte $20, $00, $20, $aa, $00, $28, $a8, $00
	!byte $08, $a8, $00, $08, $a8, $00, $08, $a8
	!byte $00, $08, $a8, $00, $0a, $a8, $00, $02
	!byte $aa, $80, $0a, $aa, $80, $0a, $02, $80
	!byte $08, $00, $a0, $28, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $a0, $00, $02, $80, $00, $0a, $80
	!byte $a0, $2b, $a0, $a0, $2b, $80, $a0, $00
	!byte $80, $a0, $0a, $a0, $80, $02, $a2, $80
	!byte $02, $a2, $00, $02, $a2, $00, $02, $a2
	!byte $00, $02, $a2, $00, $02, $aa, $00, $02
	!byte $a8, $00, $02, $aa, $00, $28, $0a, $00
	!byte $20, $02, $00, $a0, $02, $80, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $0a, $00, $00, $0a, $00, $00, $0a, $a0
	!byte $08, $0a, $a0, $08, $00, $2a, $28, $00
	!byte $2a, $a2, $00, $02, $a0, $00, $02, $a0
	!byte $22, $2a, $a0, $22, $2a, $a0, $22, $2a
	!byte $80, $22, $2a, $80, $2a, $aa, $a0, $0c
	!byte $aa, $a0, $0c, $aa, $a8, $22, $80, $28
	!byte $08, $80, $08, $08, $80, $08, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $08, $80, $08, $08, $80, $08, $28, $80
	!byte $28, $38, $2a, $a8, $38, $2a, $a8, $2a
	!byte $aa, $a0, $2a, $aa, $80, $22, $2a, $80
	!byte $22, $2a, $a0, $22, $2a, $a0, $00, $02
	!byte $20, $00, $02, $20, $00, $2a, $28, $00
	!byte $2a, $28, $0a, $a0, $08, $0a, $a0, $08
	!byte $0a, $00, $00, $0a, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $30, $0f
	!byte $c0, $30, $0d, $c0, $30, $3d, $f0, $30
	!byte $fd, $fc, $30, $ff, $fc, $30, $ff, $00
	!byte $30, $ff, $fc, $30, $ff, $f0, $33, $ff
	!byte $c0, $3f, $ff, $00, $3f, $ff, $00, $3f
	!byte $ff, $fc, $0f, $ff, $fc, $03, $ff, $cc
	!byte $03, $ff, $00, $03, $0f, $00, $0f, $03
	!byte $00, $3f, $03, $c0, $3f, $03, $c0, $00
	!byte $00, $00, $00, $00, $00, $00, $03, $e0
	!byte $0c, $03, $e0, $0c, $0d, $ec, $0c, $3d
	!byte $ff, $0c, $3f, $ff, $0c, $00, $ff, $0c
	!byte $3f, $ff, $0c, $0f, $ff, $0c, $03, $ff
	!byte $cc, $00, $ff, $fc, $00, $ff, $fc, $3f
	!byte $ff, $fc, $3f, $ff, $f0, $33, $ff, $c0
	!byte $00, $ff, $c0, $00, $f0, $c0, $00, $c0
	!byte $f0, $03, $c0, $fc, $03, $c0, $fc, $00
	!byte $00, $00, $00, $30, $00, $00, $3c, $3f
	!byte $f0, $3c, $30, $00, $0f, $30, $00, $0f
	!byte $fc, $00, $0f, $ff, $00, $03, $ff, $c0
	!byte $03, $ff, $c0, $03, $ff, $c0, $03, $ff
	!byte $f0, $0f, $ff, $f0, $0f, $ff, $70, $3f
	!byte $ff, $70, $3f, $3f, $f0, $33, $33, $f0
	!byte $30, $33, $f0, $00, $03, $c0, $00, $03
	!byte $c0, $00, $03, $c0, $00, $00, $00, $00
	!byte $00, $00, $00, $0f, $00, $00, $0f, $00
	!byte $00, $0f, $00, $00, $3f, $30, $30, $3f
	!byte $33, $30, $3f, $f3, $f0, $37, $ff, $f0
	!byte $37, $ff, $c0, $3f, $ff, $c0, $3f, $ff
	!byte $00, $3f, $ff, $00, $0f, $ff, $00, $0f
	!byte $ff, $00, $03, $ff, $c0, $00, $ff, $c0
	!byte $00, $ff, $c0, $00, $33, $f0, $ff, $30
	!byte $f0, $00, $f0, $20, $00, $00, $00, $00
	!byte $00, $00, $00, $0a, $80, $a8, $0a, $80
	!byte $a8, $08, $02, $80, $08, $02, $80, $08
	!byte $2a, $a0, $08, $2b, $e0, $2a, $ab, $e8
	!byte $2a, $aa, $a8, $2a, $a2, $a8, $2a, $a0
	!byte $00, $2a, $a0, $00, $2a, $aa, $a8, $2a
	!byte $aa, $a8, $2a, $aa, $a0, $2a, $aa, $a0
	!byte $08, $20, $80, $08, $20, $80, $08, $20
	!byte $80, $0a, $28, $a0, $0a, $28, $a0, $00
	!byte $00, $00, $00, $2a, $02, $a0, $2a, $02
	!byte $a0, $02, $80, $20, $02, $80, $20, $0a
	!byte $a8, $20, $0b, $e8, $20, $2b, $ea, $a8
	!byte $2a, $aa, $a8, $2a, $8a, $a8, $00, $0a
	!byte $a8, $00, $0a, $a8, $2a, $aa, $a8, $2a
	!byte $aa, $a8, $0a, $aa, $a8, $0a, $aa, $a8
	!byte $02, $08, $20, $02, $08, $20, $02, $08
	!byte $20, $0a, $28, $a0, $0a, $28, $a0, $00
	!byte $00, $28, $00, $00, $aa, $00, $00, $aa
	!byte $a8, $2a, $aa, $a8, $2a, $aa, $08, $20
	!byte $aa, $08, $00, $aa, $08, $00, $aa, $80
	!byte $2a, $aa, $80, $2a, $aa, $a0, $20, $a2
	!byte $a0, $20, $a2, $a0, $20, $a2, $e0, $00
	!byte $a2, $e0, $00, $a2, $e8, $2a, $a2, $e8
	!byte $2a, $a2, $a8, $20, $a2, $88, $20, $a2
	!byte $88, $00, $22, $08, $00, $22, $08, $00
	!byte $20, $88, $00, $20, $88, $00, $22, $8a
	!byte $08, $22, $8a, $08, $2a, $8a, $a8, $2b
	!byte $8a, $a8, $2b, $8a, $00, $0b, $8a, $00
	!byte $0b, $8a, $08, $0a, $8a, $08, $0a, $aa
	!byte $a8, $0a, $aa, $a8, $0a, $aa, $00, $20
	!byte $aa, $00, $20, $aa, $08, $20, $aa, $08
	!byte $20, $aa, $a8, $2a, $aa, $a8, $2a, $aa
	!byte $00, $00, $aa, $00, $00, $28, $00, $00
	!byte $00, $00, $00, $0c, $00, $30, $03, $00
	!byte $c0, $00, $c3, $00, $00, $41, $00, $01
	!byte $55, $40, $33, $69, $cc, $3f, $69, $fc
	!byte $3f, $aa, $fc, $3f, $95, $fc, $3f, $95
	!byte $fc, $3f, $95, $fc, $0f, $95, $f0, $03
	!byte $aa, $c0, $03, $aa, $c0, $00, $aa, $00
	!byte $00, $aa, $00, $02, $aa, $80, $02, $aa
	!byte $80, $2a, $00, $a8, $2a, $00, $a8, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $30, $00, $00, $3c, $00
	!byte $03, $3c, $00, $03, $ff, $c0, $00, $ff
	!byte $00, $00, $ff, $00, $00, $ff, $c0, $03
	!byte $ff, $c0, $0f, $ff, $c0, $0f, $ff, $f0
	!byte $03, $0f, $c0, $00, $00, $c0, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $28, $00, $20, $28
	!byte $20, $00, $2a, $00, $0c, $aa, $80, $00
	!byte $aa, $a8, $02, $be, $a8, $2a, $bf, $a0
	!byte $2a, $be, $80, $2f, $fe, $80, $2b, $ff
	!byte $8c, $0b, $ff, $80, $0a, $ff, $a0, $02
	!byte $ff, $e0, $02, $fa, $e8, $32, $ea, $b8
	!byte $02, $a0, $a8, $0a, $80, $28, $0a, $00
	!byte $08, $28, $08, $00, $20, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $0c, $20, $00, $03, $00, $c0, $00
	!byte $03, $00, $00, $0c, $0c, $02, $00, $30
	!byte $00, $80, $c0, $00, $00, $00, $00, $08
	!byte $00, $28, $00, $30, $00, $20, $00, $00
	!byte $c2, $00, $00, $00, $a0, $08, $00, $0c
	!byte $00, $30, $00, $00, $c2, $00, $03, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $08, $00, $00, $28, $00, $00, $2c
	!byte $00, $00, $2c, $00, $00, $2c, $00, $00
	!byte $2c, $00, $00, $28, $00, $00, $aa, $50
	!byte $00, $aa, $40, $00, $a8, $00, $00, $a8
	!byte $00, $00, $a0, $00, $00, $a0, $00, $00
	!byte $a0, $00, $00, $a0, $00, $00, $a8, $00
	!byte $00, $a8, $00, $00, $a8, $00, $00, $aa
	!byte $00, $02, $aa, $80, $02, $aa, $80, $00
	!byte $00, $20, $00, $00, $28, $00, $00, $38
	!byte $00, $00, $38, $00, $00, $38, $00, $00
	!byte $38, $00, $00, $28, $00, $05, $aa, $00
	!byte $01, $aa, $00, $00, $2a, $00, $00, $2a
	!byte $00, $00, $0a, $00, $00, $0a, $00, $00
	!byte $0a, $00, $00, $0a, $00, $00, $2a, $00
	!byte $00, $2a, $00, $00, $2a, $00, $00, $aa
	!byte $00, $02, $aa, $80, $02, $aa, $80, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $20, $00, $00, $20
	!byte $00, $00, $2a, $a8, $00, $2a, $aa, $80
	!byte $2a, $aa, $a0, $2a, $aa, $a8, $2a, $2a
	!byte $a8, $28, $0a, $f8, $28, $02, $30, $20
	!byte $02, $00, $20, $02, $00, $20, $01, $00
	!byte $00, $01, $00, $00, $01, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $40, $00, $00, $40, $00, $00
	!byte $40, $08, $00, $80, $08, $00, $80, $08
	!byte $0c, $80, $28, $2f, $a0, $28, $2a, $a8
	!byte $a8, $2a, $aa, $a8, $0a, $aa, $a8, $02
	!byte $aa, $a8, $00, $2a, $a8, $00, $00, $08
	!byte $00, $00, $08, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $ff, $00, $00, $ff
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
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
!zone VECTORS
*= $fffa
	!word start
	!word start
	!word Interrupt