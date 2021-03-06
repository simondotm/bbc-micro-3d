
;----------------------------------------------------------------------------------------------------------
; De-serialise triplet byte vertex data (for vertices & surfaces)
;----------------------------------------------------------------------------------------------------------
; inputs - A=npts
;          X/Y=points array address lo/hi
;
; Function is necessary since BeebAsm doesn't allow arbitrary memory
; layouts/data writes.
; We assemble the data interleaved X/Y/Z
; then this function copies the buffer to temp memory
; before copying it back, de-interleaved
;----------------------------------------------------------------------------------------------------------
TEMP_ADDR = &0E00
.fix_verts
{
    sta npts
    stx adr+0
    sty adr+1
    lda #LO(TEMP_ADDR)
    sta odr+0
    lda #HI(TEMP_ADDR)
    sta odr+1
    
    ; *3
    lda npts
    clc
    adc npts
    adc npts
    tay
    dey


    ; copy points to spare memory
    .copyloop
    lda (adr),y
    sta (odr),y
    dey
    bpl copyloop

    ; copy back de-interleaved
    lda #0
    sta rx
    lda npts
    sta ry    
    clc
    adc npts
    sta rz
    ldx npts
    .fixloop
    ldy #0:lda (odr),y:ldy rx:sta (adr),y
    ldy #1:lda (odr),y:ldy ry:sta (adr),y
    ldy #2:lda (odr),y:ldy rz:sta (adr),y
    inc rx:inc ry:inc rz
    lda odr+0
    clc
    adc #3
    sta odr+0
    lda odr+1
    adc #0
    sta odr+1
    dex
    bne fixloop
    rts
}

MACRO FIX_MODEL    model_data, vert_data, surfs_data
    lda model_data+0    ; npts
    clc
    adc #1
    ldx #LO(vert_data)
    ldy #HI(vert_data)
    jsr fix_verts

    lda model_data+2    ; nsurfs
    clc
    adc #1
    ldx #LO(surfs_data)
    ldy #HI(surfs_data)
    jsr fix_verts    
ENDMACRO

;----------------------------------------------------------------------------------------------------------
; Initialisation function to fix-up the model data
; Converts model data from an assembled data format into a runtime data format
;----------------------------------------------------------------------------------------------------------

.initialise_models
{
IF WIREFRAME
    FIX_MODEL   model_data_tetra, verts_data_tetra, surfs_data_tetra
ENDIF
    FIX_MODEL   model_data_cube, verts_data_cube, surfs_data_cube

IF WIREFRAME
    FIX_MODEL   model_data_octa, verts_data_octa, surfs_data_octa
    FIX_MODEL   model_data_dodeca, verts_data_dodeca, surfs_data_dodeca
; icosa not compatible with code, as it has >15 surfaces
;    FIX_MODEL   model_data_icosa, verts_data_icosa, surfs_data_icosa
ENDIF


    FIX_MODEL   model_data_viper, verts_data_viper, surfs_data_viper
    FIX_MODEL   model_data_cobra, verts_data_cobra, surfs_data_cobra
    rts
}


;----------------------------------------------------------------------------------------------------------
; Reset to the first model, then fall into the load model data routine
;----------------------------------------------------------------------------------------------------------

.reset_model 
{
    LDA#coordinates_start AND &FF:STA odr
    LDA#coordinates_start DIV 256:STA odr+1

}

;----------------------------------------------------------------------------------------------------------
; Load model data
; Sets up variables and address pointers for the next model in the model data array
;----------------------------------------------------------------------------------------------------------

.load_next_model
{
    ; initialise all surfaces to hidden
    LDA#&FF:STA oldsurfs:STA oldsurfs+1

    LDY#0
    LDA(odr),Y
    ; if first byte of model data is 255 we have reached
    ; end of model list, so reset to the beginnning
    BMI reset_model
    
    ; otherwise capture model data
    STA npts:INY
    LDA(odr),Y:STA nlines:INY
    LDA(odr),Y:STA nsurfs:INY
    LDA(odr),Y:STA maxvis

    ; setup transform routine to load vertices from this model
    LDA odr:SEC:ADC#3:STA odr:STA x+1
    LDA odr+1:ADC#0:STA odr+1:STA x+2
    LDA odr:SEC:ADC npts:STA odr:STA y+1
    LDA odr+1:ADC#0:STA odr+1:STA y+2
    LDA odr:SEC:ADC npts:STA odr:STA z+1
    LDA odr+1:ADC#0:STA odr+1:STA z+2

    ; setup clockwisetest routine 
    ; - load ptrs to surfaces for this model 

    ; store ptr to surfaces p0
    LDA odr:SEC:ADC npts:STA odr:STA clock0+1
    LDA odr+1:ADC#0:STA odr+1:STA clock0+2
    ; store ptr to surfaces p1
    LDA odr:SEC:ADC nsurfs:STA odr:STA clock1+1
    LDA odr+1:ADC#0:STA odr+1:STA clock1+2
    ; store ptr to surfaces p2
    LDA odr:SEC:ADC nsurfs:STA odr:STA clock2+1
    LDA odr+1:ADC#0:STA odr+1:STA clock2+2

    ; setup hiddensurfaceremoval routine
    ; - load ptrs to opposites data array for this model
    LDA odr:SEC:ADC nsurfs:STA odr:STA opposite0+1:STA opposite1+1:STA opposite2+1:STA opposite3+1
    LDA odr+1:ADC#0:STA odr+1:STA opposite0+2:STA opposite1+2:STA opposite2+2:STA opposite3+2

    ; setup lines address (ZP) to point to the lines array for this model
    LDA odr:SEC:ADC nsurfs:STA odr
    LDA odr+1:ADC#0:STA odr+1
    LDA odr:SEC:ADC nsurfs:STA odr:STA lines
    LDA odr+1:ADC#0:STA odr+1:STA lines+1
    
    ; setup the drawlines routine 
    ; - load ptr to the vertex indices for this model
    LDY#7
    .loopE
    LDA odr:SEC:ADC nsurfs:STA odr
    LDA odr+1:ADC#0:STA odr+1
    DEY:BPL loopE
    LDA odr:STA linestarts+1:SEC:ADC nlines:STA odr:STA lineends+1
    LDA odr+1:STA linestarts+2:ADC#0:STA odr+1:STA lineends+2
    LDA odr:SEC:ADC nlines:STA odr
    LDA odr+1:ADC#0:STA odr+1
    RTS
}


;----------------------------------------------------------------------------------------------------------
; start of model data block
;----------------------------------------------------------------------------------------------------------
.coordinates_start 

; models comprise:
;   header
;   vertex data     - 3 bytes per vertex (x,y,z)
;   surface data    - 3 bytes per surface (v0,v1,v2), describe a CW triangle/plane of the surface
;   opposites data  - 1 byte per surface
;   lines data      - 8 bytes per surface, a 64-bit array of the lines rendered by each surface
;   linelist data   - 2 bytes per surface, the start and end vertex index for each line in the model 

MACRO MD_HEADER npts, nlines, nsurfs, maxvis
    EQUB npts-1
    EQUB nlines-1
    EQUB nsurfs-1
    EQUB maxvis
ENDMACRO    

; store a signed X,Y,Z vertex, scaled by 'scale' 
; stored in 8 bits unsigned format
MACRO MD_POINT x, y, z, scale
    EQUB INT(x * scale + 128)
    EQUB INT(y * scale + 128)
    EQUB INT(z * scale + 128)
ENDMACRO



; store the first three indices of vertices that describe
; a surface.
; presented in clockwise orientation
; if rendered CCW they are considered hidden 
MACRO MD_SURF  p0, p1, p2
    EQUB p0
    EQUB p1
    EQUB p2
ENDMACRO


; opposites array describes any surfaces that are opposite to the current surface
; in this way they can be eliminated without extra clockwisetest's
; stored as a 16-bit bitfield, where #bit is set for the opposite surface id
MACRO MD_OPP    opps
    IF opps > &7f
        EQUW 0
    ELSE
; sdm: some shapes use opps ids > 15 which is larger than 16bit
        A = 2^ opps
        EQUB LO(A)
        EQUB HI(A)
    ENDIF
ENDMACRO

; declare lines that are rendered for a given surface
; stored as 64-bit array (8 bytes)
; each bit in the array corresponds to an MD_LINE object
; There must be one MD_LINE declared for each MD_SURF
MACRO MD_LINE   p0, p1
    EQUD    p1
    EQUD    p0
ENDMACRO


; add a line to the linelist for the model
; where p0 and p1 are the two vertex indices for the line
MACRO MD_INDEX  p0, p1
    EQUB p0
    EQUB p1
ENDMACRO


IF WIREFRAME
; Tetrahedron data
.model_data_tetra
TETRA_NPTS = 4
TETRA_NLINES = 6
TETRA_NSURFS = 4
TETRA_MAXVIS = 3
TETRA_SCALE = 112

MD_HEADER TETRA_NPTS,TETRA_NLINES,TETRA_NSURFS,TETRA_MAXVIS

.verts_data_tetra


MD_POINT 1,-0.3536,0, TETRA_SCALE
MD_POINT -0.5,-0.3536,-0.866, TETRA_SCALE
MD_POINT -0.5,-0.3536,0.866, TETRA_SCALE
MD_POINT 0,1.0607,0, TETRA_SCALE

.surfs_data_tetra
MD_SURF 1,2,3
MD_SURF 0,3,2
MD_SURF 0,1,3
MD_SURF 0,2,1

MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80

MD_LINE 0,7
MD_LINE 0,&1A
MD_LINE 0,&2C
MD_LINE 0,&31

MD_INDEX 1,2
MD_INDEX 1,0
MD_INDEX 0,0
MD_INDEX 2,3
MD_INDEX 3,3
MD_INDEX 2,1
ENDIF




; Cube data
.model_data_cube
CUBE_NPTS = 8
CUBE_NLINES = 12
CUBE_NSURFS = 6
CUBE_MAXVIS = 3
CUBE_SCALE = 1.369

MD_HEADER CUBE_NPTS,CUBE_NLINES,CUBE_NSURFS,CUBE_MAXVIS

.verts_data_cube
MD_POINT +50,+50,+50, CUBE_SCALE
MD_POINT +50,+50,-50, CUBE_SCALE
MD_POINT +50,-50,+50, CUBE_SCALE
MD_POINT +50,-50,-50, CUBE_SCALE
MD_POINT -50,+50,+50, CUBE_SCALE
MD_POINT -50,+50,-50, CUBE_SCALE
MD_POINT -50,-50,+50, CUBE_SCALE
MD_POINT -50,-50,-50, CUBE_SCALE


.surfs_data_cube
MD_SURF 4,6,0
MD_SURF 5,7,4
MD_SURF 1,4,0
MD_SURF 3,1,0
MD_SURF 3,2,6
MD_SURF 7,1,3

MD_OPP 5
MD_OPP 3
MD_OPP 4
MD_OPP 1
MD_OPP 2
MD_OPP 0

IF WIREFRAME
    MD_LINE 0,&701
    MD_LINE 0,&164
    MD_LINE 0,&00F
    MD_LINE 0,&A12
    MD_LINE 0,&CC0
    MD_LINE 0,&0B8
ELSE
    MD_LINE 0,&150001
    MD_LINE 0,&022820
    MD_LINE 0,&0000FF
    MD_LINE 0,&880208
    MD_LINE 0,&F0F000
    MD_LINE 0,&004540
ENDIF

MD_INDEX 0,1
MD_INDEX 4,1
MD_INDEX 1,5
MD_INDEX 6,3
MD_INDEX 4,0
MD_INDEX 2,2
MD_INDEX 4,0
MD_INDEX 5,5
MD_INDEX 3,7
MD_INDEX 7,7
MD_INDEX 6,2
MD_INDEX 6,3

; wireframe only models
IF WIREFRAME
; Octahedron data
.model_data_octa

OCTA_NPTS = 6
OCTA_NLINES = 12
OCTA_NSURFS = 8
OCTA_MAXVIS = 4
OCTA_SCALE = 119

MD_HEADER OCTA_NPTS,OCTA_NLINES,OCTA_NSURFS,OCTA_MAXVIS

.verts_data_octa
MD_POINT 0,-1,0, OCTA_SCALE
MD_POINT -1,0,0, OCTA_SCALE
MD_POINT 0,0,-1, OCTA_SCALE
MD_POINT 0,1,0, OCTA_SCALE
MD_POINT 1,0,0, OCTA_SCALE
MD_POINT 0,0,1, OCTA_SCALE

.surfs_data_octa
MD_SURF 0,1,2
MD_SURF 0,5,1
MD_SURF 1,5,3
MD_SURF 1,3,2
MD_SURF 3,5,4
MD_SURF 2,3,4
MD_SURF 0,2,4
MD_SURF 0,4,5

MD_OPP 4
MD_OPP 5
MD_OPP 6
MD_OPP 7
MD_OPP 0
MD_OPP 1
MD_OPP 2
MD_OPP 3

MD_LINE 0,7
MD_LINE 0,&19
MD_LINE 0,&70
MD_LINE 0,&C4
MD_LINE 0,&320
MD_LINE 0,&680
MD_LINE 0,&C02
MD_LINE 0,&908

MD_INDEX 0,0
MD_INDEX 1,0
MD_INDEX 1,3
MD_INDEX 1,2
MD_INDEX 4,3
MD_INDEX 2,0
MD_INDEX 1,2
MD_INDEX 2,5
MD_INDEX 5,5
MD_INDEX 3,3
MD_INDEX 5,4
MD_INDEX 4,4


; Dodecahedron data
.model_data_dodeca
DODECA_NPTS = 20
DODECA_NLINES = 30
DODECA_NSURFS = 12
DODECA_MAXVIS = 6
DODECA_SCALE = -72

MD_HEADER DODECA_NPTS,DODECA_NLINES,DODECA_NSURFS,DODECA_MAXVIS

.verts_data_dodeca
MD_POINT 1,-1.3090,0, DODECA_SCALE
MD_POINT 0.3090,-1.3090,-0.9511, DODECA_SCALE
MD_POINT -0.8090,-1.3090,-0.5878, DODECA_SCALE
MD_POINT -0.8090,-1.3090,0.5878, DODECA_SCALE
MD_POINT 0.3090,-1.3090,0.9511, DODECA_SCALE
MD_POINT 1.6180,-0.3090,0, DODECA_SCALE
MD_POINT 0.5,-0.3090,-1.5388, DODECA_SCALE
MD_POINT -1.3090,-0.3090,-0.9511, DODECA_SCALE
MD_POINT -1.3090,-0.3090,0.9511, DODECA_SCALE
MD_POINT 0.5,-0.3090,1.5388, DODECA_SCALE
MD_POINT -1,1.3090,0, DODECA_SCALE
MD_POINT -0.3090,1.3090,0.9511, DODECA_SCALE
MD_POINT 0.8090,1.3090,0.5878, DODECA_SCALE
MD_POINT 0.8090,1.3090,-0.5878, DODECA_SCALE
MD_POINT -0.3090,1.3090,-0.9511, DODECA_SCALE
MD_POINT -1.6180,0.3090,0, DODECA_SCALE
MD_POINT -0.5,0.3090,1.5388, DODECA_SCALE
MD_POINT 1.3090,0.3090,0.9511, DODECA_SCALE
MD_POINT 1.3090,0.3090,-0.9511, DODECA_SCALE
MD_POINT -0.5,0.3090,-1.5388, DODECA_SCALE

.surfs_data_dodeca
MD_SURF 0,2,4
MD_SURF 10,7,14
MD_SURF 3,16,4
MD_SURF 18,6,0
MD_SURF 9,16,12
MD_SURF 2,7,8
MD_SURF 10,14,12
MD_SURF 0,4,17
MD_SURF 13,14,6
MD_SURF 8,10,16
MD_SURF 19,2,6
MD_SURF 12,18,17

MD_OPP 6
MD_OPP 7
MD_OPP 8
MD_OPP 9
MD_OPP 10
MD_OPP 11
MD_OPP 0
MD_OPP 1
MD_OPP 2
MD_OPP 3
MD_OPP 4
MD_OPP 5

MD_LINE 0,&307
MD_LINE 0,&12040440
MD_LINE 0,&4084804
MD_LINE 0,&21101001
MD_LINE 0,&28A010
MD_LINE 0,&2414200
MD_LINE 0,&F8
MD_LINE 0,&28200802
MD_LINE 0,&920480
MD_LINE 0,&14408008
MD_LINE 0,&851100
MD_LINE 0,&9022020

MD_INDEX 0,0
MD_INDEX 3,10
MD_INDEX 11,12
MD_INDEX 10,13
MD_INDEX 1,2
MD_INDEX 14,4
MD_INDEX 1,12
MD_INDEX 3,11
MD_INDEX 2,13
MD_INDEX 7,9
MD_INDEX 6,9
MD_INDEX 8,6
MD_INDEX 5,7
MD_INDEX 8,5
MD_INDEX 10,0
MD_INDEX 1,4
MD_INDEX 4,11
MD_INDEX 12,13
MD_INDEX 14,14
MD_INDEX 2,3
MD_INDEX 19,9
MD_INDEX 6,17
MD_INDEX 8,16
MD_INDEX 7,18
MD_INDEX 19,16
MD_INDEX 18,17
MD_INDEX 15,19
MD_INDEX 18,15
MD_INDEX 16,17
MD_INDEX 15,5

; Icosahedron model contains more than 15 surfaces
; so cannot be rendered using the optimized surface removal, unless that optimization is disabled
IF FALSE

; Icosahedron data
.model_data_icosa
ICOSA_NPTS = 12
ICOSA_NLINES = 30
ICOSA_NSURFS = 20
ICOSA_MAXVIS = 10
ICOSA_SCALE = -91

MD_HEADER ICOSA_NPTS,ICOSA_NLINES,ICOSA_NSURFS,ICOSA_MAXVIS

.verts_data_icosa
MD_POINT 2.32830644E-11,-1.309,0, ICOSA_SCALE
MD_POINT -0.9472,0.5854,-0.6882, ICOSA_SCALE
MD_POINT -0.3618,-0.5854,1.11352, ICOSA_SCALE
MD_POINT 0.9472,-0.5854,-0.6882, ICOSA_SCALE
MD_POINT 0.3618,0.5854,1.11352, ICOSA_SCALE
MD_POINT -1.1708,-0.5854,0, ICOSA_SCALE
MD_POINT -2.32830644E-11,1.309,0, ICOSA_SCALE
MD_POINT 0.9472,-0.5854,0.6882, ICOSA_SCALE
MD_POINT 0.3618,0.5854,-1.11352, ICOSA_SCALE
MD_POINT -0.9472,0.5854,0.6882, ICOSA_SCALE
MD_POINT -0.3618,-0.5854,-1.11352, ICOSA_SCALE
MD_POINT 1.1708,0.5854,0, ICOSA_SCALE

.surfs_data_icosa
MD_SURF 0,7,3
MD_SURF 0,3,10
MD_SURF 0,10,5
MD_SURF 0,5,2
MD_SURF 0,2,7
MD_SURF 3,7,11
MD_SURF 3,8,10
MD_SURF 1,5,10
MD_SURF 2,5,9
MD_SURF 2,4,7
MD_SURF 9,1,6
MD_SURF 4,9,6
MD_SURF 11,4,6
MD_SURF 8,11,6
MD_SURF 1,8,6
MD_SURF 5,1,9
MD_SURF 4,2,9
MD_SURF 4,11,7
MD_SURF 3,11,8
MD_SURF 1,10,8

MD_OPP 10
MD_OPP 11
MD_OPP 12
MD_OPP 13
MD_OPP 14
MD_OPP 15
MD_OPP 16
MD_OPP 17
MD_OPP 18
MD_OPP 19
MD_OPP 0
MD_OPP 1
MD_OPP 2
MD_OPP 3
MD_OPP 4
MD_OPP 5
MD_OPP 6
MD_OPP 7
MD_OPP 8
MD_OPP 9

MD_LINE 0,&E0000
MD_LINE 0,&38000
MD_LINE 0,&E000
MD_LINE 0,&3800
MD_LINE 0,&80C00
MD_LINE 0,&40300
MD_LINE 0,&100C0
MD_LINE 0,&4030
MD_LINE 0,&100C
MD_LINE 0,&403
MD_LINE 0,&700000
MD_LINE 0,&1C00000
MD_LINE 0,&7000000
MD_LINE 0,&1C000000
MD_LINE 0,&30200000
MD_LINE 0,&100028
MD_LINE 0,&800006
MD_LINE 0,&2000201
MD_LINE 0,&8000180
MD_LINE 0,&20000050

MD_INDEX 4,2
MD_INDEX 2,5
MD_INDEX 1,1
MD_INDEX 8,3
MD_INDEX 3,7
MD_INDEX 2,0
MD_INDEX 2,0
MD_INDEX 5,0
MD_INDEX 3,0
MD_INDEX 3,0
MD_INDEX 1,1
MD_INDEX 6,4
MD_INDEX 4,4
MD_INDEX 6,8
MD_INDEX 6,1
MD_INDEX 7,4
MD_INDEX 9,9
MD_INDEX 10,5
MD_INDEX 10,8
MD_INDEX 11,11
MD_INDEX 7,2
MD_INDEX 5,5
MD_INDEX 10,10
MD_INDEX 10,3
MD_INDEX 7,7
MD_INDEX 9,6
MD_INDEX 9,9
MD_INDEX 6,11
MD_INDEX 11,11
MD_INDEX 8,8

ENDIF ; Icosahedron only

ENDIF ; wireframe only



; Viper data
.model_data_viper
VIPER_NPTS = 15
VIPER_NLINES = 20
VIPER_NSURFS = 7
VIPER_MAXVIS = 5
VIPER_SCALE = 4

MD_HEADER VIPER_NPTS, VIPER_NLINES, VIPER_NSURFS, VIPER_MAXVIS

.verts_data_viper
MD_POINT 0,19,0, VIPER_SCALE
MD_POINT 0,1,-6, VIPER_SCALE
MD_POINT 0,1,6, VIPER_SCALE
MD_POINT -10,-19,-6, VIPER_SCALE
MD_POINT -19,-19,0, VIPER_SCALE
MD_POINT -10,-19,6, VIPER_SCALE
MD_POINT 10,-19,6, VIPER_SCALE
MD_POINT 19,-19,0, VIPER_SCALE
MD_POINT 10,-19,-6, VIPER_SCALE
MD_POINT -3,-19,-4, VIPER_SCALE
MD_POINT -12,-19,0, VIPER_SCALE
MD_POINT -3,-19,4, VIPER_SCALE
MD_POINT 3,-19,-4, VIPER_SCALE
MD_POINT 3,-19,4, VIPER_SCALE
MD_POINT 12,-19,0, VIPER_SCALE

.surfs_data_viper
MD_SURF 1,8,3
MD_SURF 0,1,3
MD_SURF 0,8,1
MD_SURF 0,2,6
MD_SURF 0,5,2
MD_SURF 2,5,6
MD_SURF 6,3,8

MD_OPP 5
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP 0
MD_OPP &80

IF WIREFRAME
    MD_LINE 0,&18100
    MD_LINE 0,&30009
    MD_LINE 0,&08085
    MD_LINE 0,&80046
    MD_LINE 0,&60012
    MD_LINE 0,&C0020
    MD_LINE 0,&07FF8
ELSE
    MD_LINE &01,&40010000
    MD_LINE &0A,&00000082
    MD_LINE &00,&C000C033
    MD_LINE &80,&00002028
    MD_LINE &3C,&0000030C
    MD_LINE &50,&00000400
    MD_LINE &00,&3FFFFFC0
ENDIF

MD_INDEX 0,2
MD_INDEX 0,3
MD_INDEX 4,5
MD_INDEX 6,7
MD_INDEX 8,9
MD_INDEX 10,11
MD_INDEX 12,13
MD_INDEX 14,1
MD_INDEX 3,0
MD_INDEX 5,2
MD_INDEX 1,0
MD_INDEX 7,4
MD_INDEX 5,6
MD_INDEX 7,8
MD_INDEX 3,10
MD_INDEX 11,9
MD_INDEX 13,14
MD_INDEX 12,8
MD_INDEX 1,4
MD_INDEX 2,6

; Cobra MkIII data
.model_data_cobra
COBRA_NPTS = 22
COBRA_NLINES = 32
COBRA_NSURFS = 13
COBRA_MAXVIS = 9
COBRA_SCALE = 6

MD_HEADER COBRA_NPTS, COBRA_NLINES, COBRA_NSURFS, COBRA_MAXVIS

.verts_data_cobra
MD_POINT -19,-8,0, COBRA_SCALE
MD_POINT -19,-4.5,0, COBRA_SCALE
MD_POINT -16,-8,-1, COBRA_SCALE
MD_POINT -5,9,0, COBRA_SCALE
MD_POINT -5,-8,2, COBRA_SCALE
MD_POINT 5,-8,2, COBRA_SCALE
MD_POINT 0,3,-3.5, COBRA_SCALE
MD_POINT 0,9,0, COBRA_SCALE
MD_POINT 0,12,0, COBRA_SCALE
MD_POINT 5,9,0, COBRA_SCALE
MD_POINT 0,-8,-2, COBRA_SCALE
MD_POINT 16,-8,-1, COBRA_SCALE
MD_POINT 19,-4.5,0, COBRA_SCALE
MD_POINT 19,-8,0, COBRA_SCALE
MD_POINT -5,-8,-1, COBRA_SCALE
MD_POINT -2,-8,-1, COBRA_SCALE
MD_POINT -2,-8,1.5, COBRA_SCALE
MD_POINT -5,-8,1, COBRA_SCALE
MD_POINT 2,-8,-1, COBRA_SCALE
MD_POINT 5,-8,-1, COBRA_SCALE
MD_POINT 5,-8,1, COBRA_SCALE
MD_POINT 2,-8,1.5, COBRA_SCALE

.surfs_data_cobra
MD_SURF 0,1,2
MD_SURF 1,3,2
MD_SURF 2,3,6
MD_SURF 2,6,10
MD_SURF 3,9,6
MD_SURF 10,6,11
MD_SURF 6,9,11
MD_SURF 11,9,12
MD_SURF 11,12,13
MD_SURF 13,9,5
MD_SURF 5,3,4
MD_SURF 4,3,0
MD_SURF 4,10,5

MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80
MD_OPP &80

IF WIREFRAME
    MD_LINE 0,&00000007
    MD_LINE 0,&0000001C
    MD_LINE 0,&000000B0
    MD_LINE 0,&00020420
    MD_LINE 0,&000003C0
    MD_LINE 0,&00040C00
    MD_LINE 0,&00001900
    MD_LINE 0,&00007000
    MD_LINE 0,&0001C000
    MD_LINE 0,&80092000
    MD_LINE 0,&C0100240
    MD_LINE 0,&40200009
    MD_LINE 0,&3FFE8002
ELSE
    MD_LINE 0,&15
    MD_LINE 0,&02A0
    MD_LINE 0,&4500
    MD_LINE &08,&00200800
    MD_LINE 0,&03F000
    MD_LINE &20,&00A00000
    MD_LINE 0,&01410000
    MD_LINE 0,&2A000000
    MD_LINE &01,&50000000
    MD_LINE &C00000C3,&0C000000
    MD_LINE &A0000200,&00002000
    MD_LINE &30000C00,&000000C3
    MD_LINE &0FFFFFFC,&C000000C
ENDIF

MD_INDEX 0,0
MD_INDEX 1,1
MD_INDEX 2,2
MD_INDEX 3,3
MD_INDEX 6,7
MD_INDEX 10,6
MD_INDEX 9,9
MD_INDEX 11,11
MD_INDEX 12,2
MD_INDEX 11,5
MD_INDEX 4,0
MD_INDEX 14,15
MD_INDEX 16,14
MD_INDEX 18,19
MD_INDEX 20,18
MD_INDEX 3,5
MD_INDEX 1,2
MD_INDEX 2,3
MD_INDEX 3,6
MD_INDEX 9,6
MD_INDEX 9,8
MD_INDEX 6,11
MD_INDEX 11,12
MD_INDEX 12,13
MD_INDEX 13,10
MD_INDEX 10,13
MD_INDEX 5,4
MD_INDEX 15,16
MD_INDEX 17,17
MD_INDEX 19,20
MD_INDEX 21,21
MD_INDEX 4,9

.model_data_finish
EQUB &FF

.coordinates_end
