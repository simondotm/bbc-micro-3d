;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------
; 
; Ported to BeebAsm & Annotated - by https://github.com/simondotm
;----------------------------------------------------------------------------------------------------------

WIREFRAME = TRUE

; quarter square lookup tables
;  table1 = n*n/4, where n=0..510
;  table2 = (n-255)*(n-255)/4, where n=0..510
;
; table2 + table1 origins must be page aligned

CONTIGUOUS_TABLES = FALSE

IF CONTIGUOUS_TABLES
; specify contiguous tables
    SQUARETABLE2_LSB = &0E00
    SQUARETABLE1_LSB = SQUARETABLE2_LSB+256
    SQUARETABLE2_MSB = SQUARETABLE1_LSB+512
    SQUARETABLE1_MSB = SQUARETABLE2_MSB+256
ELSE
; msb & lsb tables can be in different memory locations
; enables us to move the program org down a bit
; as we can spread the two 768 bytes tables around the memory map 
    SQUARETABLE2_LSB = &0E00
    SQUARETABLE1_LSB = SQUARETABLE2_LSB+256

    SQUARETABLE2_MSB = &0900 ;&1100
    SQUARETABLE1_MSB = SQUARETABLE2_MSB+256
ENDIF


;----------------------------------------------------------------------------------------------------------
; Zero Page Vars
;----------------------------------------------------------------------------------------------------------


; 3x3 rotation matrix
; 16-bit unit vectors
m00lsb=0:m00msb=9
m01lsb=1:m01msb=&A
m02lsb=2:m02msb=&B
m10lsb=3:m10msb=&C
m11lsb=4:m11msb=&D
m12lsb=5:m12msb=&E
m20lsb=6:m20msb=&F
m21lsb=7:m21msb=&10
m22lsb=8:m22msb=&11

adr=&12

xr=&1A
yr=&1C
zr=&1E

product=&20

; 8-bit rotation angles
rx=&22
ry=&23
rz=&24

; model data
npts=&30
nlines=&31
nsurfs=&32
maxvis=&33

lhs=&40
rhs=&42

lmul0=&44
lmul1=&46
rmul0=&48
rmul1=&4A

surfs=&50
oldsurfs=&52
surfsdone=&54
visible=&56

; ptr to the lines array for the current model
lines=&57

; 64-bit line flag array (8 bytes)
; set by 'hiddenlineremoval'
line=&59

; temp address
odr=&61

; logic vars
space=&63
p=&64
f=&65
flicker=&66
pause=&67
culling=&68     ; culling = 0=disabled, 255=enabled
cullingdb=&69   ; culling key debounce

; line rendering coordinates, start and end
x0=&70:y0=&71
x1=&72:y1=&73

; logic vars
scr=&74
err=&76
errs=&77
cnt=&78
ls=&79
dx=&FF
dy=&7A
scrstrt=&7B
c=&7C


;----------------------------------------------------------------------------------------------------------
; Code
;----------------------------------------------------------------------------------------------------------

IF CONTIGUOUS_TABLES

ORG &1400

ELSE

ORG &1100

ENDIF

.start




;----------------------------------------------------------------------------------------------------------
; screen space 3D perspective projection table
;----------------------------------------------------------------------------------------------------------
; 256 x 8-bit entries. Uses unsigned Z as a table index.
; [page aligned]
.perspective 
    d=&100
    oz=&80
    FOR Z%, -128, 127
        EQUB &FF*d/(d+oz+Z%)+.5
    NEXT 

;----------------------------------------------------------------------------------------------------------
; transformed vertex buffers (max 64 verts per model)
;----------------------------------------------------------------------------------------------------------

; array of bytes to indicate if vertex N has already been transformed in the current render frame
;  0=untransformed, 255=transformed 
.ptsdone SKIP &40

;----------------------------------------------------------------------------------------------------------
; screen space vertex coordinates, max 64 verts per model
;----------------------------------------------------------------------------------------------------------
; calculated by 'transform' routine
.sx SKIP &40
.sy SKIP &40

;----------------------------------------------------------------------------------------------------------
; sin/cos lookup table
;----------------------------------------------------------------------------------------------------------
; provides 256 degrees of range for angles

; data format:
;  16-bit (approximately 1 bit sign, 3 bits integer, 12 bits fraction [1:3:12]) entries
;
; cos table is offset from sin table by 64 bytes (90 degrees)
;
; stored as:
;  256+64 bytes lsb
; followed by:
;  256+64 bytes msb
; 
; could potentially be optimized using page alignment, 
;  but since it is really only used to create rotation matrix once per frame, and would need more memory,
;  probably not worth it.

.trigtable_start
.slsb 
smsb=slsb+&140
; cos table offsets
clsb=slsb+&40
cmsb=clsb+&140


; sin table values are stored as 16-bit values
; they are multiplied by &1fa0 = 8096 or (&fd << 5) or (253 << 5) or (% 0001 1111 1010 0000)
;  which gives fixed point precision as well as overall scale
; 
; Original author (Nick) notes:
;  The &1FA0 is a bit arbitrary.
;  Note that the way the program uses the sine table to build the rotation matrix 
;  (no multiplications there, it's all done with compound angle formulae) 
;  it doesn't matter what this number is - it just results in a scaling of the whole object. 
;  I knew it had to be a bit less than &2000 but obviously couldn't be bothered to work out
;  exactly how big it could be. Changing it to &1FE0 seems to be fine but &1FF0 is too big.

SINCOS_SCALE = 253 << 5 ; = &1fa0, but can be 255 << 5 (&1fe0) as a maximum, given current range of input coordinates
; SM: could be calculated as the largest number that can support full range of 8-bit vertex coordinates without overflow 

    FOR A%, 0, &13F
        S% = SINCOS_SCALE * SIN( A%*2*PI /256 )+.5
        EQUB LO(S%)
    NEXT
    FOR A%, 0, &13F
        S% = SINCOS_SCALE * SIN( A%*2*PI /256 )+.5
        EQUB HI(S%)
    NEXT
.trigtable_end

;----------------------------------------------------------------------------------------------------------
; surface index to 16-bit bitmask
; lookup table
;----------------------------------------------------------------------------------------------------------
; used by hiddensurfaceremoval routine to convert a surface id to a bitfield for use with surfs array
; supports maximum 16 surfaces
.bits   
        ; lsb table for surface ID[0-7]
        EQUB 1,2,4,8,16,32,64,128
        EQUD 0:EQUD 0
        ; msb table for surface ID[8-15]
        EQUD 0:EQUD 0
        EQUB 1,2,4,8,16,32,64,128

;----------------------------------------------------------------------------------------------------------
; Display initialisation table
;----------------------------------------------------------------------------------------------------------
.vdus
IF WIREFRAME
    EQUB 22,4
ELSE
    EQUB 22,5
ENDIF

    EQUB 23,0,10,32,0,0,0,0,0,0
    EQUB 255





;----------------------------------------------------------------------------------------------------------
; main loop
;----------------------------------------------------------------------------------------------------------

.entry
{
    ; initialise variables
    LDX#0
    STX adr
    STX space:STX p:STX f:STX flicker:STX cullingdb
    LDA#1:STA pause
    LDA#255:STA culling

    ; initialise display
    .loop
    LDA vdus,X:BMI nomess:JSR &FFEE
    INX:BNE loop
    .nomess

    ; prepare model data for runtime
    JSR initialise_models

    ; disable interrupts & NMI
    SEI:LDA#&40:STA &D00:LDX#&FF:TXS

    ; initialise rotation angles of model
    LDA#0:STA rx
    LDA#&7B:STA ry
    LDA#&C3:STA rz


    ; setup multiplication tables
    jsr initialise_multiply


    ; load first model
    JSR reset_model

IF WIREFRAME
    LDA#&58:STA scrstrt
ELSE
    LDA#&35:STA scrstrt
ENDIF

    ; main loop
    .frame

    ; check for space bar pressed
    LDA#&81:LDX#&9D:LDY#&FF:JSR &FFF4
    TYA:BEQ nopress:LDA space:BNE nopress
    JSR load_next_model:LDA#1
    .nopress STA space


IF WIREFRAME
    ; wireframe mode runs in 10K mode 4, 1 bit per pixel
    ; and uses a double buffer
    LDA scrstrt:LSR A:LSR A:LSR A
    LDX#&C:STX &FE00:STA &FE01

    ; check for "F" keypress to toggle vsync
    LDA#&81:LDX#&BC:LDY#&FF:JSR &FFF4
    TYA:BEQ nof:LDA f:BNE nof
    LDA flicker:EOR #1:STA flicker:LDA#1
    .nof STA f
    LDA flicker:AND pause:BNE fastandflicker
    CLI:LDA#19:JSR &FFF4:SEI
    .fastandflicker
    LDA scrstrt:EOR #&68:STA scrstrt
ENDIF

    ; clear the draw buffer
    JSR wipe

    ; check for "C" pressed to toggle backface culling
    LDA#&81:LDX#&AD:LDY#&FF:JSR &FFF4
    TYA:BEQ noc:LDA cullingdb:BNE noc
    LDA culling:EOR #255:STA culling:LDA#1
    .noc STA cullingdb


    ; check for "P" pressed to pause rotation
    LDA#&81:LDX#&C8:LDY#&FF:JSR &FFF4
    TYA:BEQ nop:LDA p:BNE nop
    LDA pause:EOR #1:STA pause:LDA#1
    .nop STA p
    LDA pause:BEQ nrot

    ; rotate the model
    JSR rotate

    .nrot

    ; compute rotation matrix
    JSR matrix

    ; clear transformed vertex buffer
    JSR newpoints

    ; initialise visibility of lines array for this frame
    JSR resetvisibility

    ; check if back face culling is enabled, skip if not
    LDA culling
    BEQ noculling

    ; eliminate hidden surfaces
    JSR hiddensurfaceremoval

    ; determine visible lines to be rendered
    JSR hiddenlineremoval

.noculling

    ; render the model
    JSR drawlines

IF WIREFRAME == FALSE
    ; apply shading
    JSR fill
ENDIF

    JMP frame

}

;----------------------------------------------------------------------------------------------------------
; clear the draw buffer
;----------------------------------------------------------------------------------------------------------


IF WIREFRAME
.wipe
{
    LDX#&2F:CMP#&30:BNE wipe0:JMP wipe1
    .wipe0 LDA#0
    .loop
    FOR Y%, &5D40, &7A00, &140
        FOR X%, Y%, Y%+144, 48
            STA X%,X
        NEXT
    NEXT
    DEX:BMI wiped:JMP loop
    .wiped RTS
    .wipe1 LDA#0
    .loop2
    FOR Y%, &3540, &5200, &140
        FOR X%, Y%, Y%+144, 48
            STA X%,X
        NEXT
    NEXT
    DEX:BMI wiped1:JMP loop2
    .wiped1 
    RTS
}

ELSE
.wipe 
{
    LDA#0:LDX#&2F
    .loop5
    FOR Y%, &3A40, &5700, &140
        FOR X%, Y%, Y%+144, 48
           STA X%,X
        NEXT
    NEXT
    DEX:BMI wiped:JMP loop5
    .wiped 
    RTS
}

.fill
{ 
    SEC:LDX#&B8
    .loop6
    LDA &3A40,X
    EOR &3A41,X:STA&5D41,X
    EOR &3A42,X:STA&5D42,X
    EOR &3A43,X:STA&5D43,X
    EOR &3A44,X:STA&5D44,X
    EOR &3A45,X:STA&5D45,X
    EOR &3A46,X:STA&5D46,X
    EOR &3A47,X:STA&5D47,X

    FOR A%,&5E80, &7A00, &140

        EOR A%-&2300,X:STAA%,X
        EOR A%-&22FF,X:STAA%+1,X
        EOR A%-&22FE,X:STAA%+2,X
        EOR A%-&22FD,X:STAA%+3,X
        EOR A%-&22FC,X:STAA%+4,X
        EOR A%-&22FB,X:STAA%+5,X
        EOR A%-&22FA,X:STAA%+6,X
        EOR A%-&22F9,X:STAA%+7,X

    NEXT 

    TXA:SBC#8:BCC filled:TAX:JMP loop6
    .filled 
    RTS
}

ENDIF

;----------------------------------------------------------------------------------------------------------
; setup quarter square multiplication tables
;----------------------------------------------------------------------------------------------------------
; f(x) = x^2 / 4. Then a*b = f(a+b) - f(a-b) 
;
; This implementation uses two tables of squares:
;  table1 = n*n/4, where n=0..510
;  table2 = (n-255)*(n-255)/4, where n=0..510
;  
; Unsigned multiplication of two 8-bit terms is computed as:
;  r = table1[a+b] - table2[(a EOR 255)+b]
; where r is a 16-bit unsigned result
;----------------------------------------------------------------------------------------------------------
; A clever innovation with this code is that it takes advantage of overlaps in the table
;  which means the tables fit into 1536 bytes instead of the usual 2048. 
;
; &0E00-&0FFF = table2 lsb
; &0F00-&10FF = table1 lsb
; &1100-&12FF = table2 msb
; &1200-&13FF = table1 msb 
;----------------------------------------------------------------------------------------------------------
.initialise_multiply
{

    ; set the msb of lmul0, lmul1, rmul0 and rmul1 just once
    ;  for the entire lifecycle of the application
    ;  - the lsb of these 16-bit addresses will be set as the multiplication terms
    LDA#HI(SQUARETABLE1_LSB):STA lmul0+1:STA rmul0+1
    LDA#HI(SQUARETABLE1_MSB):STA lmul1+1:STA rmul1+1

    ; compute table1
    
    ; x=y=lhs=0
    ; while y<256:
    ;     if y>0:
    ;         lhs += x
    ;         table1[offset+y] = lhs
    ;         x = x + 1
    ;
    ;     lhs += x
    ;     offset = y
    ;     table1[offset+y] = lhs    
    ;     y = y + 1

    ; effectively the same as:
    ; for n in range(0,511):  # 0-510
    ;     table1[n] = n*n/4

    ; initialise counters and indices    
    LDA#0:TAX:TAY
    STX lhs:STY lhs+1

    ; skip increment on first iteration
    CLC
    BCC go

    .loop2
    TXA:ADC lhs:STA lhs:STA(lmul0),Y
    LDA#0:ADC lhs+1:STA lhs+1:STA(lmul1),Y
    INX

    .go 
    STY lmul0:STY lmul1
    TXA:ADC lhs:STA lhs:STA(lmul0),Y
    LDA#0:ADC lhs+1:STA lhs+1:STA(lmul1),Y
    INY
    BNE loop2

    ; compute table2

    ; for x in range(0,256):
    ;     table2[x] = table1[255-x]
    ;     table2[x+256] = table1[x+1]    
    ;
    ; effectively the same as:
    ; for n in range(0,511):  # 0-510
    ;     table2[n] = (n-255)*(n-255)/4


    LDX#0:LDY#&FF
    .loop3
    LDA SQUARETABLE1_LSB+1,Y:STA SQUARETABLE2_LSB,X
    LDA SQUARETABLE1_MSB+1,Y:STA SQUARETABLE2_MSB,X
    DEY:INX:BNE loop3

    rts
}

;----------------------------------------------------------------------------------------------------------
; update model rotation angles
;----------------------------------------------------------------------------------------------------------
.rotate
{
    INC rx
    INC ry:INC ry
    INC rz:INC rz:INC rz
    RTS
}

;----------------------------------------------------------------------------------------------------------
; clear/reset the transformed vertex buffer array
;----------------------------------------------------------------------------------------------------------
; called once per frame
; ptsdone contains 0 if vertex has not yet been transformed
; or 255 if vertex has been transformed
;----------------------------------------------------------------------------------------------------------
.newpoints
{
    LDA#0:LDX npts
    .loop4
    STA ptsdone,X
    DEX:BPL loop4
    RTS
}

;----------------------------------------------------------------------------------------------------------
; table of addresses pointing to each address in the transform routine
;  that uses a coefficent of the 3x3 rotation matrix
;----------------------------------------------------------------------------------------------------------
; the 'matrix' routine uses this address table to load the computed
;  rotation matrix coefficients directly into the transform routine
;  for speed. (since transform is called multiple times per frame) 
;----------------------------------------------------------------------------------------------------------
.unitvectors
; offset 0 = lsb of each address
EQUB u00 AND &FF:EQUB u01 AND &FF:EQUB u02 AND &FF
EQUB u10 AND &FF:EQUB u11 AND &FF:EQUB u12 AND &FF
EQUB u20 AND &FF:EQUB u21 AND &FF:EQUB u22 AND &FF
; offset 9 = msb of each address
EQUB u00 DIV 256:EQUB u01 DIV 256:EQUB u02 DIV 256
EQUB u10 DIV 256:EQUB u11 DIV 256:EQUB u12 DIV 256
EQUB u20 DIV 256:EQUB u21 DIV 256:EQUB u22 DIV 256

;----------------------------------------------------------------------------------------------------------
; fetch a 2D screen space transformed vertex coordinate
;----------------------------------------------------------------------------------------------------------
; input - X=vertex id to fetch
; output - A is screen space X coord, Y is screen space Y coord
;  X is preserved
; (transformed vertices are cached)
;----------------------------------------------------------------------------------------------------------
.getcoordinates
{
    LDA ptsdone,X:BPL transform
    LDA sx,X
    LDY sy,X
    RTS
}

;----------------------------------------------------------------------------------------------------------
; Apply 3D -> 2D perspective projection transform to given vertex id
;----------------------------------------------------------------------------------------------------------
; inputs - 
;          X is vertex index N (0-npts)
;          .x, .y, .z addresses preloaded with vertex buffer address for current model
;          .u00...u22 addresses preloaded with rotation matrix coefficients by 'matrix' routine
; output - 
;           A, sx[N] = screen X coord
;           Y, sy[N] = screen Y coord
;           ptsdone[N] = 255
;----------------------------------------------------------------------------------------------------------
; uses table lookups for all multiplies for speed

; prior to calling this routine, the following address setup modifications have been completed:
;    1 - the X/Y/Z vertex buffer address for the currently selected model has been applied to .x, .y, .z
;    2 - the unit vectors for the current rotation matrix has been applied to u00 ... u22 by the matrix routine

; Matrix * Vector
; V' = M * V
;
;	x' = x*a + y*b + z*c
;	y' = x*d + y*e + z*f
;	z' = x*g + y*h + z*i
; 
; 9 multiplies, 6 adds

; where M = [ a b c ]
;           [ d e f ]
;           [ g h i ]
; and   V = [ x y z ]
;----------------------------------------------------------------------------------------------------------

.transform

    ; mark this vertex as transformed, so that it will be cached if re-used
    LDA#&FF:STA ptsdone,X

    ; fetch & transform vertex X coord
    ; (vertex buffer address set by load_next_model)
    .x LDY &8000,X

    ; x' = x*a
    SEC:.u00
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA xr
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA xr+1

    ; y' = x*b
    SEC:.u10
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA yr
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA yr+1

    ; z' = x*c
    SEC:.u20
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA zr
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA zr+1

    ; fetch & transform vertex Y coord
    ; (vertex buffer address set by load_next_model)    
    .y LDY &8000,X

    ; x' += y*d
    SEC:.u01
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA product
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA product+1
    LDA product:CLC:ADC xr:STA xr
    LDA product+1:ADC xr+1:STA xr+1

    ; y' += y*e
    SEC:.u11
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA product
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA product+1
    LDA product:CLC:ADC yr:STA yr
    LDA product+1:ADC yr+1:STA yr+1

    ; z' += y*f
    SEC:.u21
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA product
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA product+1
    LDA product:CLC:ADC zr:STA zr
    LDA product+1:ADC zr+1:STA zr+1

    ; fetch & transform vertex Z coord
    ; (vertex buffer address set by load_next_model)    
    .z LDY &8000,X

    ; x' += z*g
    SEC:.u02
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA product
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA product+1
    LDA product:CLC:ADC xr:STA xr
    LDA product+1:ADC xr+1:STA xr+1

    ; y' += z*h
    SEC:.u12
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA product
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA product+1
    LDA product:CLC:ADC yr:STA yr
    LDA product+1:ADC yr+1:STA yr+1

    ; z' += z*i
    SEC:.u22
    LDA SQUARETABLE2_LSB,Y:SBC SQUARETABLE2_LSB,Y:STA product
    LDA SQUARETABLE2_MSB,Y:SBC SQUARETABLE2_MSB,Y:STA product+1
    LDA product:CLC:ADC zr:STA zr
    LDA product+1:ADC zr+1

    ; xr, yr, zr now contain the rotated vertex coordinate
    ; A contains the msb of the z coordinate

    ; now calculate screen space coordinates using perspective projection
    

    ASL zr:ROL A:ASL zr
    ADC#&80:TAY
    
    CLC   
    LDA#&80:ADC perspective,Y:STA adr:STA adr+2

IF CONTIGUOUS_TABLES
    ; This routine assumes the square tables are contiguous in memory
    LDA#HI(SQUARETABLE2_LSB):ADC#0:STA adr+1
    ADC#3:STA adr+3 ; SQUARETABLE2_MSB
    CLC
    LDA adr:ADC#1:STA adr+4:STA adr+6
    CLC
    LDA adr+1:ADC#0:STA adr+5
    ADC#3:STA adr+7 ; SQUARETABLE2_MSB
ELSE
    LDA #0:ADC#0:STA adr+1:STA adr+3
    LDA adr:ADC#1:STA adr+4:STA adr+6
    LDA adr+1:ADC#0:STA adr+5:STA adr+7

    LDA#HI(SQUARETABLE2_LSB):CLC:ADC adr+1:STA adr+1
    LDA#HI(SQUARETABLE2_MSB):CLC:ADC adr+3:STA adr+3
    LDA#HI(SQUARETABLE2_LSB):CLC:ADC adr+5:STA adr+5
    LDA#HI(SQUARETABLE2_MSB):CLC:ADC adr+7:STA adr+7
ENDIF


    ; compute screen space Y coord
    LDA yr+1:ASL yr:ROL A:ASL yr
    ADC#&80:TAY:SEC:EOR #&FF:STY zr:STA zr+1
    LDA(adr),Y:LDY zr+1:SBC(adr+4),Y:STA yr
    LDY zr:LDA(adr+2),Y:LDY zr+1:SBC(adr+6),Y
    ASL yr:ADC#&80:STA sy,X

    ; compute screen space X coord
    LDA xr+1:ASL xr:ROL A:ASL xr
    ADC#&80:TAY:SEC:EOR #&FF:STY zr:STA zr+1
    LDA(adr),Y:LDY zr+1:SBC(adr+4),Y:STA xr
    LDY zr:LDA(adr+2),Y:LDY zr+1:SBC(adr+6),Y
    ASL xr:ADC#&80:STA sx,X

    LDY sy,X
    ; A contains screen space X coord
    ; Y contains screen space Y coord
    RTS

;----------------------------------------------------------------------------------------------------------
; construct a standard 3D XYZ rotation matrix 
;----------------------------------------------------------------------------------------------------------
; inputs - rx,ry,rz contain rotation angles (8 bit precision)
; outputs - m00 ... m22 contain the rotation matrix (16-bits precision)
;         - this routine also updates the transform routine directly with the
;             computed matrix coefficients, which is a useful optimization
;             since transform is called multiple times when transforming the model vertices
;----------------------------------------------------------------------------------------------------------

.matrix
{
    ; rx, ry, rz are the input X/Y/Z unsigned 8-bit rotation angles, 0-255

    ; m12 = -sin(rx)
    LDY rx
    SEC
    LDA#0:SBC slsb,Y:STA m12lsb
    LDA#0:SBC smsb,Y:ASL m12lsb
    ROL A:ASL m12lsb:ROL A:STA m12msb

    ; X = rx-ry
    ; adr[3] = rx-ry+rz
    TYA:SEC:SBC ry:TAX
    CLC:ADC rz:STA adr+3
    ; Y = rx+ry
    ; adr[2] = rx+ry+rz    
    TYA:CLC:ADC ry:TAY
    CLC:ADC rz:STA adr+2
    
    ; m02 = sin(rx-ry)-sin(rx+ry) 
    SEC
    LDA slsb,X:SBC slsb,Y:STA m02lsb
    LDA smsb,X:SBC smsb,Y:ASL m02lsb
    ROL A:STA m02msb
    
    ; m22 = cos(rx+ry)-cos(rx-ry)
    CLC
    LDA clsb,Y:ADC clsb,X:STA m22lsb
    LDA cmsb,Y:ADC cmsb,X:ASL m22lsb
    ROL A:STA m22msb

    ; m10
    ; adr[4] = rx+ry-rz
    ; adr[5] = rx-ry-rz
    TYA:SEC:SBC rz:STA adr+4
    TXA:SEC:SBC rz:STA adr+5
    LDA rx:CLC:ADC rz:TAY
    LDA rx:SEC:SBC rz:TAX
    
    SEC
    LDA slsb,Y:SBC slsb,X:STA m10lsb
    LDA smsb,Y:SBC smsb,X:ASL m10lsb
    ROL A:STA m10msb
    
    ; m11
    CLC
    LDA clsb,X:ADC clsb,Y:STA m11lsb
    LDA cmsb,Y:ADC cmsb,X:ASL m11lsb
    ROL A:STA m11msb

    ; m21
    LDA ry:SEC:SBC rz:TAY
    LDA rz:CLC:ADC ry:TAX:SEC
    LDA clsb,X:SBC clsb,Y:STA m21lsb
    LDA cmsb,X:SBC cmsb,Y:ASL m21lsb
    ROL A:STA m21msb
    
    ; m01
    SEC
    LDA slsb,Y:SBC slsb,X:STA m01lsb
    LDA smsb,Y:SBC smsb,X:ASL m01lsb
    ROL A:STA m01msb
    
    ; m00
    CLC
    LDA clsb,Y:ADC clsb,X:STA m00lsb
    LDA cmsb,X:ADC cmsb,Y:ASL m00lsb
    ROL A:STA m00msb
    
    ; m20
    CLC
    LDA slsb,X:ADC slsb,Y:STA m20lsb
    LDA smsb,Y:ADC smsb,X:ASL m20lsb
    ROL A:STA m20msb

    ; Y=
    LDY adr+4:LDX adr+3
    
    SEC
    LDA m00lsb:SBC slsb,X:STA m00lsb
    LDA m00msb:SBC smsb,X:STA m00msb
    
    CLC
    LDA slsb,Y:ADC m21lsb:STA m21lsb
    LDA m21msb:ADC smsb,Y:STA m21msb
    
    CLC
    LDA clsb,Y:ADC m20lsb:STA m20lsb
    LDA cmsb,Y:ADC m20msb:STA m20msb
    
    SEC
    LDA m01lsb:SBC clsb,X:STA m01lsb
    LDA m01msb:SBC cmsb,X:STA m01msb
    
    CLC
    LDA clsb,Y:ADC m01lsb:STA m01lsb
    LDA cmsb,Y:ADC m01msb:STA m01msb
    
    CLC
    LDA m21lsb:ADC slsb,X:STA m21lsb
    LDA m21msb:ADC smsb,X:STA m21msb
    
    SEC
    LDA m20lsb:SBC clsb,X:STA m20lsb
    LDA m20msb:SBC cmsb,X:STA m20msb
    
    SEC
    LDA m00lsb:SBC slsb,Y:STA m00lsb
    LDA m00msb:SBC smsb,Y:STA m00msb

    LDX adr+5:LDY adr+2
    
    SEC
    LDA m20lsb:SBC clsb,Y:STA m20lsb
    LDA m20msb:SBC cmsb,Y:STA m20msb
    
    CLC
    LDA m00lsb:ADC slsb,X:STA m00lsb
    LDA m00msb:ADC smsb,X:STA m00msb
    
    CLC
    LDA m21lsb:ADC slsb,X:STA m21lsb
    LDA m21msb:ADC smsb,X:STA m21msb
    
    CLC
    LDA clsb,Y:ADC m01lsb:STA m01lsb
    LDA cmsb,Y:ADC m01msb:STA m01msb
    
    CLC
    LDA slsb,Y:ADC m21lsb:STA m21lsb
    LDA smsb,Y:ADC m21msb:STA m21msb
    
    CLC
    LDA slsb,Y:ADC m00lsb:STA m00lsb
    LDA smsb,Y:ADC m00msb:STA m00msb
    
    CLC
    LDA clsb,X:ADC m20lsb:STA m20lsb
    LDA cmsb,X:ADC m20msb:STA m20msb
    
    SEC
    LDA m01lsb:SBC clsb,X:STA m01lsb
    LDA m01msb:SBC cmsb,X:STA m01msb

    ; m00 ... m22 lsb & msb
    ; now contain the 3x3 rotation matrix elements

    ; next, 
    ; for each element of the 3x3 matrix;
    ;   transfer the rotation matrix coefficients directly
    ;   into each related part of the (vector * matrix) vertex transform routine 

    LDX#8
    .loop7
    ; fetch the transform routine coefficient address for the current element of the matrix
    ; [ u00, u01, u02 ]
    ; [ u10, u11, u12 ]
    ; [ u20, u21, u22 ]

    LDA unitvectors,X:STA adr       ; lsb
    LDA unitvectors+9,X:STA adr+1   ; msb

    ; get the high byte of the coefficent
    ; complement with high bit of low byte and convert from signed to unsigned (for lookup table access)
    LDA m00msb,X:ASL m00lsb,X:ADC#&80
    
    ; store sin table offset lsb's for this coefficient directly into the transform routine

    ; (+1 is lsb after LDA instruction)
    ; (+9 is lsb after SBC instruction)
    LDY#1:STA(adr),Y:LDY#9:STA(adr),Y
    
    ; A = -A (two's complement negate)
    CLC:EOR #&FF:ADC#1    

    ; (+4 is lsb after second LDA instruction)
    ; (+12 is lsb after second SBC instruction)
    LDY#4:STA(adr),Y:LDY#&C:STA(adr),Y
    
    DEX:BPL loop7
    RTS
}

;----------------------------------------------------------------------------------------------------------
; perform hidden surface removal for the current model
;----------------------------------------------------------------------------------------------------------
; inputs -
;    prior to calling this routine, load_next_model has set the opposites lookups to point to the current model's
;     opposites array
;    oldsurfs, contains copy of previous visible surfs bit array (or &ffff on first render of this model)
;     
; outputs -
;    surfs contains 16 bit array of visible surfaces [where (bit == 1) ? hidden : visible] 
;    oldsurfs = surfs
;
;----------------------------------------------------------------------------------------------------------
; Back face culling can be CPU expensive, due to the need for cross product multiplications for visibility determination
; This routine uses four clever optimization techniques, that take advantage of the following observations:
;  1 - 3D models that are backface culled have a finite number of surfaces (maxvis) that can ever be visible at once
;  2 - visible surfaces tend to be consistently visible over multiple render frames
;  3 - 3D models tend to contain surfaces that are in direct opposition
;  4 - cross product implemented using a factored equation and table lookup based multiplication 
; 
; For 1, the routine has an early out once the maxvis surface visibility counter is reached, eliminating redundant visibility checks
; For 2, the routine first rechecks only surfaces that were previously visible on the last render frame
;         which increases probability of early-out with the minimum number (maxvis) of visibility calcs
; For 3, when calculating a surfaces visibility, the routine eliminates any opposite surfaces from 
;         further visibility calcs, which further reduces the number of overall visibility calculations that are required. 
;----------------------------------------------------------------------------------------------------------
.hiddensurfaceremoval

    ; set number of visible surfaces to zero
    LDA#0:STA visible

    ; oldsurfs = 16 bit array, where set bits mean surface is hidden, clear bits mean surface is visible
    ; surfs = 16 bit array, where set bits mean surface is hidden, clear bits mean surface is visible
    ; surfsdone = 16 bit array, where set bits mean surface has been processed this frame

    ; store inverted version of oldsurfs in surfsdone
    ; surfsdone = oldsurfs EOR &ffff 
    ; this effectively means any previously visible surfaces are pre-marked as done 
    ;  as far as the second phase of surface visibility testing goes.
    ; we can then deal with these specific cases in just the first phase.
    LDA oldsurfs:EOR #&FF:STA surfsdone
    LDA oldsurfs+1:EOR #&FF:STA surfsdone+1

    ; setup loop - bring the relevant bits of oldsurfs to the top of oldsurfs

    ; a=15-nsurfs
    ; if nsurfs == 15, skip the unused bits shift
    LDA#&F:SEC:SBC nsurfs:BEQ nloop

    ; discard unused bits in the oldsurfs bit array by shifting them out
    ; n = 15-nsurfs
    ; for y=n; y>0; y--)
    ;    oldsurfs <<= 1
    TAY
    LDA oldsurfs
    .loop8
    ASL A:ROL oldsurfs+1:DEY:BNE loop8
    STA oldsurfs
    ; now, the topmost bits in oldsurfs contains only the relevant bits we want to look at

    .nloop

    ; first phase - re-check visibility of previously visible surfaces
    ; 
    ; 1. loop through all surfaces, and check visibility only of those that were previously visible.
    ; 2. any that still remain visible have their opposite surface (if any) marked as hidden/done
    ; 3. maintain a counter of surfaces we know are visible in this render frame
    ;
    ; for Y=nsurfs, y>=0, --y
    LDY nsurfs

    .loop9
    ; grab the next visibility bit from the top of oldsurfs
    ; oldsurfs << 1, top bit into carry
    ASL oldsurfs:ROL oldsurfs+1

    ; check if this surface was previously hidden
    ; if C is set, this surface was previously hidden so,
    ;   skip first phase visibility test, as we're only interested in visible surfaces for this phase
    BCS hidesurface
    ; else
    ;  test visibility of surface Y, to see if it is still visible
    JSR clockwisetest:BCS hidesurface

    ; yep, surface Y is still visible, so mark it's opposite as 'done' (ie. deduced to be not visible)

    ; fetch the 16-bit opposite data array for this surface
    ;  and mark the opposite surface id as processed

    ; x=y*2 (word indexing for opposites array)
    TYA:ASL A:TAX
    ; surfsdone |= opposites[y]
    .opposite0 LDA &8000,X:INX
    ORA surfsdone:STA surfsdone
    .opposite1 LDA &8000,X
    ORA surfsdone+1:STA surfsdone+1
    
    ; ++visible
    INC visible

    ; carry bit is clear (visible surface)

    .hidesurface
    ; if we reached here because a previously visible surface is now hidden, the carry bit will be set

    ; update surfs bit array
    ; surfs = (surfs << 1) | (carryset ? 1 : 0)
    ROL surfs:ROL surfs+1
    DEY:BPL loop9

    ; now we have the bottom nsurf bits of surfs representing correct visibility status for any previously visible surfaces
    ; however any previously hidden surfaces may not have correct representation in the surfs bit array at this stage.
    ; if visible == maxvis however, that wont matter because we've already determined all possible visible surfaces
    ;  and we'll early out of the routine, however if visible != maxvis, we'll correct these bits in the next phase.

    ; SM: Possible bug here? - since we're not marking the phase 1 visible surfaces as done,
    ;        if visible != maxvis, wont we end up potentially re-calculating the same visible surfaces? 




    ; -------------------------------------------------
    ; second phase, loop again through each surface in the model
    ;  if not 'done', check visibility, update surfs bit array and mark it (and it's opposite) as done
    ;  importantly, we early exit once we've counted 'maxvis' visible surfaces
    ;   - thanks to the first phase loop, there's much higher probability of this early exit happening quickly

    ; for y=nsurfs; y>=0; --y
    LDY nsurfs
    ;   currentsurfacebit = bits[y]
    ; (oldsurfs gets reused as a temp var in this section)
    LDA bits,Y:STA oldsurfs
    LDA bits+16,Y:STA oldsurfs+1

    .loopA
    ; early out optimization
    ;   if (visible == maxvis) for this model
    ;       - early exit as there's no need to process any more surfaces since we've hit maxvis limit
    LDX visible:CPX maxvis:BEQ doneit

    ; check if current surface has already been 'done', and skip the clockwise test if so
    ;   if (surfsdone & currentsurfacebit) 
    ;       continue;
    LDA surfsdone:AND oldsurfs:BNE surfdone
    LDA surfsdone+1:AND oldsurfs+1:BNE surfdone

    ; otherwise check if surface is visible
    JSR clockwisetest:BCS surfdone

    ; surface is indeed visible,
    ;  so clear the bit for this surface in surfs bit array by masking surfs with inverse currentsurfacebit
    ;
    ; surfs = (currentsurfacebit EOR &FFFF) & surfs
    LDA oldsurfs:EOR #&FF:AND surfs:STA surfs
    LDA oldsurfs+1:EOR #&FF:AND surfs+1:STA surfs+1

    ; X=y*2 for word addressing
    ; mark the surface opposite this one as done
    ; surfsdone |= opposite[y]
    TYA:ASL A:TAX
    .opposite2 LDA &8000,X:INX
    ORA surfsdone:STA surfsdone
    .opposite3 LDA &8000,X
    ORA surfsdone+1:STA surfsdone+1

    ; visible++
    INC visible

    .surfdone

    ; currentsurfacebit >>= 1
    LSR oldsurfs+1:ROR oldsurfs

    ; continue
    DEY:BPL loopA

    ; 
    .doneit
    ; surfs now contains 16-bit array, where bit clear for visible surface, bit set for hidden surface
    ; oldsurfs = surfs
    ; return surfs[] - 16 bit array, where any bit set indicates a hidden surface
    LDA surfs:STA oldsurfs
    LDA surfs+1:STA oldsurfs+1
    RTS

;----------------------------------------------------------------------------------------------------------
; perform a clockwise orientation test for a given surface
;----------------------------------------------------------------------------------------------------------
;
;  on entry, Y is the index of the surface being tested
;
;  on exit, carry is set if surface is in anti-clockwise presentation (ie. hidden)
;           Y is preserved
;
; prior to calling this routine, load_next_model has stored the
; pointers to the surface data for the model into clock0/1/2
;
; surfaces are represented as 3 points representing a triangle, presented in CW winding order
; culling is determined by detecting CCW presentation of the surface vertices in screen space
; 

; cross product method:
; a = 1/2 * sum[0 to (n - 1)] of ( XiY((i+1) mod n) - X((i+1) mod n)Yi )

; a  = x0*y1 - x1*y0
; a += x1*y2 - x2*y1
; a += x2*y0 - x0*y2
; a *= 0.5
; if a > 0 then CCW
; if a < 0 the CW
; if a == 0 then back facing

; mac = sx0*sy1-sx1*sy0+sx1*sy2-sx2*sy1+sx2*sy0-sx0*sy2
;       SX0*SY1+SX1*SY2+SX2*SY0-SX0*SY2-SX1*SY0-SX2*SY1



; alternative:
; (y1 - y0) * (x2 - x1) - (x1 - x0) * (y2 - y1)

; (a+b) (a-b) = a x a + a x -b + b x a + b x -b  
; sy1*sx2-sx1*sy1-sy0*sx2+sx1*sy0-sx1*sy2-sy1*sx1-sx0*sy2+sy1*sx0
; if positive, surface is visible
;----------------------------------------------------------------------------------------------------------

.clockwisetest 

    ; store surface id being tested
    STY ls

    ; getcoordinates routine returns screen x in A and screen y in Y

    ; fetch screen coordinate for surface point 0
    .clock0 LDX &8000,Y
    JSR getcoordinates
    STA x0:STY y0
    ; fetch screen coordinate for surface point 1
    LDY ls
    .clock1 LDX &8000,Y
    JSR getcoordinates
    STA x1:STY y1
    ; fetch screen coordinate for surface point 2
    LDY ls
    .clock2 LDX &8000,Y
    JSR getcoordinates

    ; now:
    ; x0,y0 = point 0 on the surface tri
    ; y1,y1 = point 1 on the surface tri
    ; A,Y   = point 2 on the surface tri

    LDX#0

    ; abs(x2-x0)
    SEC
    SBC x0:BCS x20p 
    ; if x2 > x0, negate result, increment counter X
    SBC#0:EOR #&FF ; sets carry
    INX
    .x20p STA lmul0

    ; carry is set

    ; abs(y1-y0)
    LDA y1:SBC y0:BCS y10p
    ; if y1 > y0, negate result, decrement counter X
    SBC#0:EOR #&FF  ; sets carry
    DEX
    .y10p STA lmul1

;
    ; abs(y2-y0)
    TYA:LDY#0
    SBC y0:BCS y20p
    SBC#0:EOR #&FF:INY
    .y20p STA rmul0

    ; abs(x1-x0)
    LDA x1:SBC x0:BCS x10p
    SBC#0:EOR #&FF:DEY
    .x10p STA rmul1
    
    ; X contains 1,0, or 255 for the x2-x0 and y1-y0 test
    ; Y contains 1,0, or 255 for the y2-y0 and x1-x0 test
    ;  1 or 255 if just one test was less than the other
    ;  0 if both tests were less than the other
    ;  0 if both tests were greater than the other

    ; compare the results from both tests
    STX cnt
    CPX#1       ; set carry if X >= 1 (ie. not zero)
    TYA
    EOR cnt

    ; 1 eor 0 = 1, bit 0 set
    ; 0 eor 255 = 255, bit 0 set
    ; 1 eor 1 = 0, bit 0 clear
    ; 1 eor 255 = 254, bit 0 clear


    ; if bit 0 is clear, compute full outer product
    AND #1:BEQ compare
    ; otherwise:
    ; preserve Y and return 
    ; carry bit is set = surface is hidden/CCW
    LDY ls
    RTS
    
    ; if X = 0, carry is clear, both terms are positive
    ; 
    .compare BCC bothpos

    ; 
    JSR multiply
    LDY lhs:CPY rhs     ; carry set if rhs >= lhs
    LDA lhs+1:SBC rhs+1 
    ; preserve Y and return
    LDY ls
    RTS

    .bothpos SEC
    JSR multiply
    LDY rhs:CPY lhs
    LDA rhs+1:SBC lhs+1

    ; preserve Y and return
    LDY ls
    RTS

;----------------------------------------------------------------------------------------------------------
; outer/cross product multiplication
;----------------------------------------------------------------------------------------------------------
; inputs:
;  lmul0, lmul1 contain 1st two terms (8-bit values)
;  rmul0, rmul1 contain 2nd two terms (8-bit values)
; returns: 
;  lmul0 * lmul1 in lhs (16-bits)
;  rmul0 * rmul1 in rhs (16-bits)
;----------------------------------------------------------------------------------------------------------

.multiply
{
    LDY lmul1:TYA
    LDX lmul0:STX lmul1
    SBC lmul0:BCS mabsl
    SBC#0:EOR #&FF
    .mabsl TAX
    LDA(lmul0),Y:SBC SQUARETABLE1_LSB,X:STA lhs
    LDA(lmul1),Y:SBC SQUARETABLE1_MSB,X:STA lhs+1

    LDY rmul1:TYA
    LDX rmul0:STX rmul1
    SBC rmul0:BCS mabsr
    SBC#0:EOR #&FF
    .mabsr TAX
    LDA(rmul0),Y:SBC SQUARETABLE1_LSB,X:STA rhs
    LDA(rmul1),Y:SBC SQUARETABLE1_MSB,X:STA rhs+1
    RTS
}

;----------------------------------------------------------------------------------------------------------
; Initialise the line visibility array based on culling preference
;----------------------------------------------------------------------------------------------------------
.resetvisibility
{
   ; use culling value to reset the lines array
    ; if culling is on, they are reset to zero (and line visibility set to surface visibiity)
    ; if culling is off, they are all set to 255 (and line visibility is forced)
    LDA culling
    EOR #&FF
    TAY
    
    ; reset the 64-bit line visibility array 
    STY line
    STY line+1
    STY line+2
    STY line+3
    STY line+4
    STY line+5
    STY line+6
    STY line+7

    RTS
}

;----------------------------------------------------------------------------------------------------------
; Determine the minimum set of lines to be rendered for the current model based on currently visible surfaces
;----------------------------------------------------------------------------------------------------------
; inputs - 
;   (lines) points to the lines data block for this model
;   surfs contains 16-bit array of visible surfaces
; outputs -
;   line[] contains 64-bit array of visible lines to be rendered
;   surfs >>= nsurfs
;----------------------------------------------------------------------------------------------------------
; given the list of visible surfaces 
; set a bit in the 64-bit output array (ZP "line") array for each line
;  that is associated with the visible surface
; 8 bits x 8 bytes = 64 bits, or 64 lines per object 
; in this way, lines are only ever drawn once, even if shared across surfaces
;----------------------------------------------------------------------------------------------------------

.hiddenlineremoval
{
    LDY #0

    LDX nsurfs

    ; for y=0; y<nsurfs; ++y
    ;   if visible
    ;       line[] |= model.lines[surface]
    ;       (OR the 8 byte array of line bits set for this surface)
    .loopC

    ; surfs >>= 1 (low bit into carry)
    ;  if low bit was set, surface is not visible
    LSR surfs+1:ROR surfs:BCS nosurf


IF WIREFRAME
    LDA(lines),Y:ORA line:STA line:INY
    LDA(lines),Y:ORA line+1:STA line+1:INY
    LDA(lines),Y:ORA line+2:STA line+2:INY
    LDA(lines),Y:ORA line+3:STA line+3:INY
    LDA(lines),Y:ORA line+4:STA line+4:INY
    LDA(lines),Y:ORA line+5:STA line+5:INY
    LDA(lines),Y:ORA line+6:STA line+6:INY
    LDA(lines),Y:ORA line+7:STA line+7:INY
 ELSE
    LDA(lines),Y:EOR line:STA line:INY
    LDA(lines),Y:EOR line+1:STA line+1:INY
    LDA(lines),Y:EOR line+2:STA line+2:INY
    LDA(lines),Y:EOR line+3:STA line+3:INY
    LDA(lines),Y:EOR line+4:STA line+4:INY
    LDA(lines),Y:EOR line+5:STA line+5:INY
    LDA(lines),Y:EOR line+6:STA line+6:INY
    LDA(lines),Y:EOR line+7:STA line+7:INY
ENDIF
    DEX:BPL loopC
    RTS
    
    ; skip to lines entry for the next surface
    .nosurf TYA:ADC#7:TAY
    DEX:BPL loopC
    RTS
}


;----------------------------------------------------------------------------------------------------------
; present the list of visible lines for the current model to the line renderer
;----------------------------------------------------------------------------------------------------------
; inputs -
;   line[] contains 64-bit array of visible lines to be rendered
;   linestarts - points to the current models line indices array (p0)
;   lineends - points to the current models line indices array (p1)
;   (linestarts & lineends addresses are set by the 'load_next_model' routine)
;----------------------------------------------------------------------------------------------------------
; for each linestart and lineend vertex index (p0,p1) the routine fetches the transformed 2D screen coordinate
; and submits these coordinates to the line renderer.
; 2D screen coordinates are cached, so they are only ever transformed once, even if referenced by multiple lines
;----------------------------------------------------------------------------------------------------------


IF WIREFRAME
.drawlines

    ; index of current line in the line list for this model
    LDA#0:STA lhs+1
    ; number of lines in the line list for this model
    LDA nlines:STA lhs

    ; for each line in the linelist
    ;  fetch low bit of the 64 bit line[] array to determine
    ;   its visibility, draw only if marked as visible
    .loop
    LSR line+7
    ROR line+6
    ROR line+5
    ROR line+4
    ROR line+3
    ROR line+2
    ROR line+1
    ROR line
    BCC noline

    ; get the current line index
    LDY lhs+1

    ; linestarts and lineends are setup when model first loaded
    ; they point to the line list buffer
    .linestarts LDA &8000,Y:PHA
    .lineends LDA &8000,Y:TAX
    ; fetch screen coords for vertex linelist[1]
    JSR getcoordinates
    STA x0:STY y0
    PLA:TAX
    ; fetch screen coords for vertex linelist[0]
    JSR getcoordinates
    STA x1:STY y1

    ; render the line from x0,y0 to x1,y1
    JSR linedraw

    .noline 
    INC lhs+1   ; next line in list
    DEC lhs     ; for each line in list
    BPL loop
    RTS

ELSE


.drawlines

    LDA#0:STA lhs+1
    LDA nlines:STA lhs
    .loopD
    LDA line:AND #3:BEQ noline:PHA
    LDY lhs+1
    .linestarts LDA &8000,Y:PHA
    .lineends LDA &8000,Y:TAX
    JSR getcoordinates
    STA x0:STY y0
    PLA:TAX
    JSR getcoordinates
    STA x1:STY y1
    PLA:TAX:JSR linedraw
    .noline
    
    LSR line+7
    ROR line+6
    ROR line+5
    ROR line+4
    ROR line+3
    ROR line+2
    ROR line+1
    ROR line
    LSR line+7
    ROR line+6
    ROR line+5
    ROR line+4
    ROR line+3
    ROR line+2
    ROR line+1
    ROR line
    INC lhs+1:DEC lhs:BPL loopD
    RTS
ENDIF

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

INCLUDE "include/linedraw.asm"

INCLUDE "include/models.asm"





.end


PRINT "Coordinates data size is ", coordinates_end-coordinates_start, " bytes"
PRINT " Trig table data size is ", trigtable_end-trigtable_start, " bytes"
PRINT "Code from", ~start, "to", ~end, ", size is", (end-start), "bytes"

; Finish up with executable last on the disk
SAVE "Main", start, end, entry




