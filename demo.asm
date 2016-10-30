;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
;----------------------------------------------------------------------------------------------------------
;
; Original 1994 code written by Nick Jameson
; Ported to BeebAsm & Annotated - by https://github.com/simondotm
;----------------------------------------------------------------------------------------------------------

WIREFRAME = TRUE

; quarter square lookup tables
;  table1 = n*n/4, where n=0..510
;  table2 = (n-255)*(n-255)/4, where n=0..510
;
; table2 + table1 must be contiguous in memory and page aligned

SQUARETABLE2_LSB = &0E00
SQUARETABLE1_LSB = SQUARETABLE2_LSB+256
SQUARETABLE2_MSB = SQUARETABLE1_LSB+512
SQUARETABLE1_MSB = SQUARETABLE2_MSB+256


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



ORG &1400


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
IF WIREFRAME == FALSE    
    JSR rotate
ENDIF

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

;    LDA#&E:ADC#0:STA adr+1:ADC#3:STA adr+3
    LDA#HI(SQUARETABLE2_LSB):ADC#0:STA adr+1
    ADC#3:STA adr+3 ; SQUARETABLE2_MSB

    LDA adr:ADC#1:STA adr+4:STA adr+6
    LDA adr+1:ADC#0:STA adr+5
    ADC#3:STA adr+7 ; SQUARETABLE2_MSB

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
TEMP_ADDR = &7000
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
; so cannot be rendered using optimized surface removal
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


;----------------------------------------------------------------------------------------------------------
; Line rendering routines
; Not yet annotated
;----------------------------------------------------------------------------------------------------------


IF WIREFRAME

;x0=&70:y0=&71
;x1=&72:y1=&73
;scr=&74
;err=&76
;errs=&77
;cnt=&78
;ls=&79
;dx=&FF
;dy=&7A
;scrstrt=&7B

.strt0
EQUB a00 AND &FF:EQUB a10 AND &FF
EQUB a20 AND &FF:EQUB a30 AND &FF
EQUB a40 AND &FF:EQUB a50 AND &FF
EQUB a60 AND &FF:EQUB a70 AND &FF
EQUB a00 DIV 256:EQUB a10 DIV 256
EQUB a20 DIV 256:EQUB a30 DIV 256
EQUB a40 DIV 256:EQUB a50 DIV 256
EQUB a60 DIV 256:EQUB a70 DIV 256
.strt1
EQUB a71 AND &FF:EQUB a61 AND &FF
EQUB a51 AND &FF:EQUB a41 AND &FF
EQUB a31 AND &FF:EQUB a21 AND &FF
EQUB a11 AND &FF:EQUB a01 AND &FF
EQUB a71 DIV 256:EQUB a61 DIV 256
EQUB a51 DIV 256:EQUB a41 DIV 256
EQUB a31 DIV 256:EQUB a21 DIV 256
EQUB a11 DIV 256:EQUB a01 DIV 256
.strt2
EQUB p02 AND &FF:EQUB p12 AND &FF
EQUB p22 AND &FF:EQUB p32 AND &FF
EQUB p42 AND &FF:EQUB p52 AND &FF
EQUB p62 AND &FF:EQUB p72 AND &FF
EQUB p02 DIV 256:EQUB p12 DIV 256
EQUB p22 DIV 256:EQUB p32 DIV 256
EQUB p42 DIV 256:EQUB p52 DIV 256
EQUB p62 DIV 256:EQUB p72 DIV 256
.strt3
EQUB p73 AND &FF:EQUB p63 AND &FF
EQUB p53 AND &FF:EQUB p43 AND &FF
EQUB p33 AND &FF:EQUB p23 AND &FF
EQUB p13 AND &FF:EQUB p03 AND &FF
EQUB p73 DIV 256:EQUB p63 DIV 256
EQUB p53 DIV 256:EQUB p43 DIV 256
EQUB p33 DIV 256:EQUB p23 DIV 256
EQUB p13 DIV 256:EQUB p03 DIV 256


.linedraw

    LDA y0:SEC:SBC y1:BCS dyok
    SBC #0:EOR #&FF
    LDX x0:LDY x1:STY x0:STX x1
    LDY y1:STY y0
    .dyok STA dy
    LDA x0:SBC x1:PHP:BCS dxok
    SBC #0:EOR #&FF
    .dxok TAX
    LDA #&80:STA scr
    LDA y0:LSR A:LSR A:LSR A:STA scr+1
    LSR A:ROR scr:LSR A:ROR scr
    ADC scr+1:STA scr+1
    LDA x0:AND #&F8:ADC scr:STA scr
    LDA scrstrt:ADC scr+1:STA scr+1
    LDA x0:AND #7:TAY
    CPX dy:BCS notsteep:JMP steep

    .notsteep
    LDA dy:BEQ horizontal:STA cnt
    TXA:LSR A:STA err:STA errs
    LDA #2:STA ls
    .backh PLP:BCC right
    LDA strt1,Y:STA x1
    LDA strt1+8,Y:STA y1
    LDA y0:AND #7:TAY
    STX b01+5:STX b11+5
    STX b21+5:STX b31+5
    STX b41+5:STX b51+5
    STX b61+5:STX b71+5
    LDA dy
    STA b01+1:STA b11+1
    STA b21+1:STA b31+1
    STA b41+1:STA b51+1
    STA b61+1:STA b71+1
    SEC:JMP (x1)
    .horizontal STX err
    LDA #1:STA ls:STA cnt:STA dy
    BNE backh
    .right
    LDA strt0,Y:STA x1
    LDA strt0+8,Y:STA y1
    LDA y0:AND #7:TAY
    STX b00+5:STX b10+5
    STX b20+5:STX b30+5
    STX b40+5:STX b50+5
    STX b60+5:STX b70+5
    LDA dy
    STA b00+1:STA b10+1
    STA b20+1:STA b30+1
    STA b40+1:STA b50+1
    STA b60+1:STA b70+1
    SEC:JMP (x1)

    .steep
    TXA:BEQ vertical:STX cnt
    LDA dy:LSR A:STA errs
    LDA #2:STA ls
    .backv PLP:BCS left
    LDA strt2,Y:STA x1
    LDA strt2+8,Y:STA y1
    LDA y0:AND #7:TAY
    STX a02+1:STX a12+1
    STX a22+1:STX a32+1
    STX a42+1:STX a52+1
    STX a62+1:STX a72+1
    LDA dy
    STA b02+1:STA b12+1
    STA b22+1:STA b32+1
    STA b42+1:STA b52+1
    STA b62+1:STA b72+1
    LDX errs:SEC:JMP (x1)

    .vertical LDA dy:STA errs
    LDX #1:STX ls:STX cnt
    BNE backv
    .left
    LDA strt3,Y:STA x1
    LDA strt3+8,Y:STA y1
    LDA y0:AND #7:TAY
    STX a03+1:STX a13+1
    STX a23+1:STX a33+1
    STX a43+1:STX a53+1
    STX a63+1:STX a73+1
    LDA dy
    STA b03+1:STA b13+1
    STA b23+1:STA b33+1
    STA b43+1:STA b53+1
    STA b63+1:STA b73+1
    LDX errs:SEC:JMP (x1)

    .a00 LDA err:LDX #&FF
    .b00 SBC #dy:BCS b10:ADC #dx:STA err
    LDA #&80:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e00:.f00 DEY:BPL a10
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a10:DEC scr+1:SEC:BCS a10
    .e00 DEC ls:BEQ d00:LDA err:SBC errs
    STA err:INC cnt:BCS f00
    .d00 RTS

    .a10 LDA err:LDX #&7F
    .b10 SBC #dy:BCS b20:ADC #dx:STA err
    TXA:AND #&C0:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e10:.f10 DEY:BPL a20
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a20:DEC scr+1:SEC:BCS a20
    .e10 DEC ls:BEQ d10:LDA err:SBC errs
    STA err:INC cnt:BCS f10
    .d10 RTS

    .a20 LDA err:LDX #&3F
    .b20 SBC #dy:BCS b30:ADC #dx:STA err
    TXA:AND #&E0:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e20:.f20 DEY:BPL a30
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a30:DEC scr+1:SEC:BCS a30
    .e20 DEC ls:BEQ d20:LDA err:SBC errs
    STA err:INC cnt:BCS f20
    .d20 RTS

    .a30 LDA err:LDX #&1F
    .b30 SBC #dy:BCS b40:ADC #dx:STA err
    TXA:AND #&F0:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e30:.f30 DEY:BPL a40
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a40:DEC scr+1:SEC:BCS a40
    .e30 DEC ls:BEQ d30:LDA err:SBC errs
    STA err:INC cnt:BCS f30
    .d30 RTS

    .a40 LDA err:LDX #&F
    .b40 SBC #dy:BCS b50:ADC #dx:STA err
    TXA:AND #&F8:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e40:.f40 DEY:BPL a50
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a50:DEC scr+1:SEC:BCS a50
    .e40 DEC ls:BEQ d40:LDA err:SBC errs
    STA err:INC cnt:BCS f40
    .d40 RTS

    .a50 LDA err:LDX #7
    .b50 SBC #dy:BCS b60:ADC #dx:STA err
    TXA:AND #&FC:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e50:.f50 DEY:BPL a60
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a60:DEC scr+1:SEC:BCS a60
    .e50 DEC ls:BEQ d50:LDA err:SBC errs
    STA err:INC cnt:BCS f50
    .d50 RTS

    .a60 LDA err:LDX #3
    .b60 SBC #dy:BCS b70:ADC #dx:STA err
    TXA:AND #&FE:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e60:.f60 DEY:BPL a70
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a70:DEC scr+1:SEC:BCS a70
    .e60 DEC ls:BEQ d60:LDA err:SBC errs
    STA err:INC cnt:BCS f60
    .d60 RTS

    .a70 LDA err:LDX #1
    .b70 SBC #dy:BCS by0:ADC #dx:STA err
    TXA:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e70:.f70 DEY:BPL ad0
    LDA scr:SBC #&38:LDY #7:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP a00
    .e70 DEC ls:BEQ d70:LDA err:SBC errs
    STA err:INC cnt:BCS f70
    .d70 RTS:.by0 STA err
    TXA:ORA(scr),Y:STA (scr),Y
    .ad0 LDA scr:ADC #7:STA scr:BCS ac0
    SEC:JMP a00
    .ac0 INC scr+1:JMP a00
    .a01 LDA err:LDX #&FF
    .b01 SBC #dy:BCS b11:ADC #dx:STA err
    LDA #1:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e01:.f01 DEY:BPL a11
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a11:DEC scr+1:SEC:BCS a11
    .e01 DEC ls:BEQ d01:LDA err:SBC errs
    STA err:INC cnt:BCS f01
    .d01 RTS

    .a11 LDA err:LDX #&FE
    .b11 SBC #dy:BCS b21:ADC #dx:STA err
    TXA:AND #3:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e11:.f11 DEY:BPL a21
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a21:DEC scr+1:SEC:BCS a21
    .e11 DEC ls:BEQ d11:LDA err:SBC errs
    STA err:INC cnt:BCS f11
    .d11 RTS

    .a21 LDA err:LDX #&FC
    .b21 SBC #dy:BCS b31:ADC #dx:STA err
    TXA:AND #7:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e21:.f21 DEY:BPL a31
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a31:DEC scr+1:SEC:BCS a31
    .e21 DEC ls:BEQ d21:LDA err:SBC errs
    STA err:INC cnt:BCS f21
    .d21 RTS

    .a31 LDA err:LDX #&F8
    .b31 SBC #dy:BCS b41:ADC #dx:STA err
    TXA:AND #&F:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e31:.f31 DEY:BPL a41
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a41:DEC scr+1:SEC:BCS a41
    .e31 DEC ls:BEQ d31:LDA err:SBC errs
    STA err:INC cnt:BCS f31
    .d31 RTS

    .a41 LDA err:LDX #&F0
    .b41 SBC #dy:BCS b51:ADC #dx:STA err
    TXA:AND #&1F:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e41:.f41 DEY:BPL a51
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a51:DEC scr+1:SEC:BCS a51
    .e41 DEC ls:BEQ d41:LDA err:SBC errs
    STA err:INC cnt:BCS f41
    .d41 RTS

    .a51 LDA err:LDX #&E0
    .b51 SBC #dy:BCS b61:ADC #dx:STA err
    TXA:AND #&3F:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e51:.f51 DEY:BPL a61
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a61:DEC scr+1:SEC:BCS a61
    .e51 DEC ls:BEQ d51:LDA err:SBC errs
    STA err:INC cnt:BCS f51
    .d51 RTS

    .a61 LDA err:LDX #&C0
    .b61 SBC #dy:BCS b71:ADC #dx:STA err
    TXA:AND #&7F:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e61:.f61 DEY:BPL a71
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a71:DEC scr+1:SEC:BCS a71
    .e61 DEC ls:BEQ d61:LDA err:SBC errs
    STA err:INC cnt:BCS f61
    .d61 RTS

    .a71 LDA err:LDX #&80
    .b71 SBC #dy:BCS by1:ADC #dx:STA err
    TXA:ORA(scr),Y:STA (scr),Y
    DEC cnt:BEQ e71:.f71 DEY:BPL sb1
    LDA scr:SBC #&48:LDY #7:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP a01
    .e71 DEC ls:BEQ d71:LDA err:SBC errs
    STA err:INC cnt:BCS f71
    .d71 RTS:.by1 STA err
    TXA:ORA(scr),Y:STA (scr),Y
    .sb1 LDA scr:SBC #8:STA scr:BCC sc1
    JMP a01
    .sc1 DEC scr+1:SEC:JMP a01
    .p02 LDA #&80:ORA(scr),Y:STA (scr),Y
    TXA:.a02 SBC #dx:BCC b02:TAX
    DEY:BPL p02
    .s02 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p02:DEC scr+1:SEC:BCS p02
    .e02 DEC ls:BEQ d02:SBC errs
    INC cnt:BCS n02:.d02 RTS
    .b02 ADC #dy:DEC cnt:BEQ e02
    .n02 TAX:DEY:BMI s12
    .p12 LDA #&40:ORA(scr),Y:STA (scr),Y
    TXA:.a12 SBC #dx:BCC b12:TAX
    DEY:BPL p12
    .s12 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p12:DEC scr+1:SEC:BCS p12
    .e12 DEC ls:BEQ d12:SBC errs
    INC cnt:BCS n12:.d12 RTS
    .b12 ADC #dy:DEC cnt:BEQ e12
    .n12 TAX:DEY:BMI s22
    .p22 LDA #&20:ORA(scr),Y:STA (scr),Y
    TXA:.a22 SBC #dx:BCC b22:TAX
    DEY:BPL p22
    .s22 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p22:DEC scr+1:SEC:BCS p22
    .e22 DEC ls:BEQ d22:SBC errs
    INC cnt:BCS n22:.d22 RTS
    .b22 ADC #dy:DEC cnt:BEQ e22
    .n22 TAX:DEY:BMI s32
    .p32 LDA #&10:ORA(scr),Y:STA (scr),Y
    TXA:.a32 SBC #dx:BCC b32:TAX
    DEY:BPL p32
    .s32 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p32:DEC scr+1:SEC:BCS p32
    .e32 DEC ls:BEQ d32:SBC errs
    INC cnt:BCS n32:.d32 RTS
    .b32 ADC #dy:DEC cnt:BEQ e32
    .n32 TAX:DEY:BMI s42
    .p42 LDA #8:ORA(scr),Y:STA (scr),Y
    TXA:.a42 SBC #dx:BCC b42:TAX
    DEY:BPL p42
    .s42 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p42:DEC scr+1:SEC:BCS p42
    .e42 DEC ls:BEQ d42:SBC errs
    INC cnt:BCS n42:.d42 RTS
    .b42 ADC #dy:DEC cnt:BEQ e42
    .n42 TAX:DEY:BMI s52
    .p52 LDA #4:ORA(scr),Y:STA (scr),Y
    TXA:.a52 SBC #dx:BCC b52:TAX
    DEY:BPL p52
    .s52 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p52:DEC scr+1:SEC:BCS p52
    .e52 DEC ls:BEQ d52:SBC errs
    INC cnt:BCS n52:.d52 RTS
    .b52 ADC #dy:DEC cnt:BEQ e52
    .n52 TAX:DEY:BMI s62
    .p62 LDA #2:ORA(scr),Y:STA (scr),Y
    TXA:.a62 SBC #dx:BCC b62:TAX
    DEY:BPL p62
    .s62 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p62:DEC scr+1:SEC:BCS p62
    .e62 DEC ls:BEQ d62:SBC errs
    INC cnt:BCS n62:.d62 RTS
    .b62 ADC #dy:DEC cnt:BEQ e62
    .n62 TAX:DEY:BMI s72
    .p72 LDA #1:ORA(scr),Y:STA (scr),Y
    TXA:.a72 SBC #dx:BCC b72:TAX
    DEY:BPL p72
    .s72 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p72:DEC scr+1:SEC:BCS p72
    .e72 DEC ls:BEQ d72:SBC errs
    INC cnt:BCS n72:.d72 RTS
    .b72 ADC #dy:DEC cnt:BEQ e72
    .n72 TAX:DEY:BMI s82
    LDA scr:ADC #7:STA scr:BCS ac2
    SEC:JMP p02:.ac2 INC scr+1:JMP p02
    .s82 LDY #7:LDA scr:SBC #&38:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP p02
    .p03 LDA #1:ORA(scr),Y:STA (scr),Y
    TXA:.a03 SBC #dx:BCC b03:TAX
    DEY:BPL p03
    .s03 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p03:DEC scr+1:SEC:BCS p03
    .e03 DEC ls:BEQ d03:SBC errs
    INC cnt:BCS n03:.d03 RTS
    .b03 ADC #dy:DEC cnt:BEQ e03
    .n03 TAX:DEY:BMI s13
    .p13 LDA #2:ORA(scr),Y:STA (scr),Y
    TXA:.a13 SBC #dx:BCC b13:TAX
    DEY:BPL p13
    .s13 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p13:DEC scr+1:SEC:BCS p13
    .e13 DEC ls:BEQ d13:SBC errs
    INC cnt:BCS n13:.d13 RTS
    .b13 ADC #dy:DEC cnt:BEQ e13
    .n13 TAX:DEY:BMI s23
    .p23 LDA #4:ORA(scr),Y:STA (scr),Y
    TXA:.a23 SBC #dx:BCC b23:TAX
    DEY:BPL p23
    .s23 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p23:DEC scr+1:SEC:BCS p23
    .e23 DEC ls:BEQ d23:SBC errs
    INC cnt:BCS n23:.d23 RTS
    .b23 ADC #dy:DEC cnt:BEQ e23
    .n23 TAX:DEY:BMI s33
    .p33 LDA #8:ORA(scr),Y:STA (scr),Y
    TXA:.a33 SBC #dx:BCC b33:TAX
    DEY:BPL p33
    .s33 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p33:DEC scr+1:SEC:BCS p33
    .e33 DEC ls:BEQ d33:SBC errs
    INC cnt:BCS n33:.d33 RTS
    .b33 ADC #dy:DEC cnt:BEQ e33
    .n33 TAX:DEY:BMI s43
    .p43 LDA #&10:ORA(scr),Y:STA (scr),Y
    TXA:.a43 SBC #dx:BCC b43:TAX
    DEY:BPL p43
    .s43 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p43:DEC scr+1:SEC:BCS p43
    .e43 DEC ls:BEQ d43:SBC errs
    INC cnt:BCS n43:.d43 RTS
    .b43 ADC #dy:DEC cnt:BEQ e43
    .n43 TAX:DEY:BMI s53
    .p53 LDA #&20:ORA(scr),Y:STA (scr),Y
    TXA:.a53 SBC #dx:BCC b53:TAX
    DEY:BPL p53
    .s53 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p53:DEC scr+1:SEC:BCS p53
    .e53 DEC ls:BEQ d53:SBC errs
    INC cnt:BCS n53:.d53 RTS
    .b53 ADC #dy:DEC cnt:BEQ e53
    .n53 TAX:DEY:BMI s63
    .p63 LDA #&40:ORA(scr),Y:STA (scr),Y
    TXA:.a63 SBC #dx:BCC b63:TAX
    DEY:BPL p63
    .s63 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p63:DEC scr+1:SEC:BCS p63
    .e63 DEC ls:BEQ d63:SBC errs
    INC cnt:BCS n63:.d63 RTS
    .b63 ADC #dy:DEC cnt:BEQ e63
    .n63 TAX:DEY:BMI s73
    .p73 LDA #&80:ORA(scr),Y:STA (scr),Y
    TXA:.a73 SBC #dx:BCC b73:TAX
    DEY:BPL p73
    .s73 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:BCS p73:DEC scr+1:SEC:BCS p73
    .e73 DEC ls:BEQ d73:SBC errs
    INC cnt:BCS n73:.d73 RTS
    .b73 ADC #dy:DEC cnt:BEQ e73
    .n73 TAX:DEY:BMI s83
    LDA scr:SBC #8:STA scr:BCC ac3
    JMP p03:.ac3 DEC scr+1:SEC:JMP p03
    .s83 LDY #7:LDA scr:SBC #&48:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP p03


ELSE

;x0=&70:y0=&71
;x1=&72:y1=&73
;scr=&74
;err=&76
;errs=&77
;cnt=&78
;ls=&79
;dx=&FF
;dy=&7A
;scrstrt=&7B
;c=&7C

.colors EQUD&FFF00FED
.strt0
EQUB a00 AND &FF:EQUB a10 AND &FF
EQUB a60 AND &FF:EQUB a70 AND &FF
EQUB a00 DIV 256:EQUB a10 DIV 256
EQUB a60 DIV 256:EQUB a70 DIV 256
.strt1
EQUB a71 AND &FF:EQUB a61 AND &FF
EQUB a11 AND &FF:EQUB a01 AND &FF
EQUB a71 DIV 256:EQUB a61 DIV 256
EQUB a11 DIV 256:EQUB a01 DIV 256
.strt2
EQUB p02 AND &FF:EQUB p52 AND &FF
EQUB p62 AND &FF:EQUB p72 AND &FF
EQUB p02 DIV 256:EQUB p52 DIV 256
EQUB p62 DIV 256:EQUB p72 DIV 256
.strt3
EQUB p73 AND &FF:EQUB p63 AND &FF
EQUB p53 AND &FF:EQUB p03 AND &FF
EQUB p73 DIV 256:EQUB p63 DIV 256
EQUB p53 DIV 256:EQUB p03 DIV 256
.linedraw_noline RTS

.linedraw
    LDA colors,X:STA c
    LSR x0:LSR x1
    LDA x0:CMP x1:BCC itsoknow:BEQ linedraw_noline
    LDX x0:LDY x1:STY x0:STX x1
    LDX y0:LDY y1:STY y0:STX y1
    .itsoknow DEC x1
    LDA y0:SEC:SBC y1:BCS dyok
    SBC #0:EOR #&FF
    LDX x0:LDY x1:STY x0:STX x1
    LDY y1:STY y0
    .dyok STA dy
    LDA x0:SBC x1:PHP:BCS dxok
    SBC #0:EOR #&FF
    .dxok TAX
    LDA #&80:STA scr
    LDA y0:LSR A:LSR A:LSR A:STA scr+1
    LSR A:ROR scr:LSR A:ROR scr
    ADC scr+1:STA scr+1
    LDA x0:AND #&7C:ASL A:ADC scr:STA scr
    LDA scrstrt:ADC scr+1:STA scr+1
    LDA x0:AND #3:TAY
    CPX dy:BCS notsteep:JMP steep
    .notsteep
    LDA dy:BEQ horizontal:STA cnt
    TXA:LSR A:STA err:STA errs
    LDA #2:STA ls
    .backh PLP:BCC right
    LDA strt1,Y:STA x1
    LDA strt1+4,Y:STA y1
    LDA y0:AND #7:TAY
    STX b01+5:STX b51+5
    STX b61+5:STX b71+5
    LDA dy
    STA b01+1:STA b51+1
    STA b61+1:STA b71+1
    SEC:JMP (x1)
    .horizontal STX err
    LDA #1:STA ls:STA cnt:STA dy
    BNE backh
    .right
    LDA strt0,Y:STA x1
    LDA strt0+4,Y:STA y1
    LDA y0:AND #7:TAY
    STX b00+5:STX b50+5
    STX b60+5:STX b70+5
    LDA dy
    STA b00+1:STA b50+1
    STA b60+1:STA b70+1
    SEC:JMP (x1)
    .steep
    TXA:BEQ vertical:STX cnt
    LDA dy:LSR A:STA errs
    LDA #2:STA ls
    .backv PLP:BCS left
    LDA strt2,Y:STA x1
    LDA strt2+4,Y:STA y1
    LDA y0:AND #7:TAY
    STX a02+1:STX a52+1
    STX a62+1:STX a72+1
    LDA dy
    STA b02+1:STA b52+1
    STA b62+1:STA b72+1
    LDX errs:SEC:JMP (x1)
    .vertical LDA dy:STA errs
    LDX #1:STX ls:STX cnt
    BNE backv
    .left
    LDA strt3,Y:STA x1
    LDA strt3+4,Y:STA y1
    LDA y0:AND #7:TAY
    STX a03+1:STX a53+1
    STX a63+1:STX a73+1
    LDA dy
    STA b03+1:STA b53+1
    STA b63+1:STA b73+1
    LDX errs:SEC:JMP (x1)
    .a00 LDA err:LDX #&FF
    .b00 SBC #dy:BCS b50:ADC #dx:STA err
    LDA #&88:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e00:.f00 DEY:BPL a10
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a10:DEC scr+1:SEC:BCS a10
    .e00 DEC ls:BEQ d00:LDA err:SBC errs
    STA err:INC cnt:BCS f00
    .d00 RTS
    .a10 LDA err:LDX #&77
    .b50 SBC #dy:BCS b60:ADC #dx:STA err
    TXA:AND #&CC:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e50:.f50 DEY:BPL a60
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a60:DEC scr+1:SEC:BCS a60
    .e50 DEC ls:BEQ d50:LDA err:SBC errs
    STA err:INC cnt:BCS f50
    .d50 RTS
    .a60 LDA err:LDX #&33
    .b60 SBC #dy:BCS b70:ADC #dx:STA err
    TXA:AND #&EE:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e60:.f60 DEY:BPL a70
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a70:DEC scr+1:SEC:BCS a70
    .e60 DEC ls:BEQ d60:LDA err:SBC errs
    STA err:INC cnt:BCS f60
    .d60 RTS
    .a70 LDA err:LDX #&11
    .b70 SBC #dy:BCS by0:ADC #dx:STA err
    TXA:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e70:.f70 DEY:BPL ad0
    LDA scr:SBC #&38:LDY #7:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP a00
    .e70 DEC ls:BEQ d70:LDA err:SBC errs
    STA err:INC cnt:BCS f70
    .d70 RTS:.by0 STA err
    TXA:AND c:EOR (scr),Y:STA (scr),Y
    .ad0 LDA scr:ADC #7:STA scr:BCS ac0
    SEC:JMP a00
    .ac0 INC scr+1:JMP a00
    .a01 LDA err:LDX #&FF
    .b01 SBC #dy:BCS b51:ADC #dx:STA err
    LDA #&11:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e01:.f01 DEY:BPL a11
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a11:DEC scr+1:SEC:BCS a11
    .e01 DEC ls:BEQ d01:LDA err:SBC errs
    STA err:INC cnt:BCS f01
    .d01 RTS
    .a11 LDA err:LDX #&EE
    .b51 SBC #dy:BCS b61:ADC #dx:STA err
    TXA:AND #&33:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e51:.f51 DEY:BPL a61
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a61:DEC scr+1:SEC:BCS a61
    .e51 DEC ls:BEQ d51:LDA err:SBC errs
    STA err:INC cnt:BCS f51
    .d51 RTS
    .a61 LDA err:LDX #&CC
    .b61 SBC #dy:BCS b71:ADC #dx:STA err
    TXA:AND #&77:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e61:.f61 DEY:BPL a71
    LDA scr:SBC #&40:STA scr:DEC scr+1
    LDY #7:BCS a71:DEC scr+1:SEC:BCS a71
    .e61 DEC ls:BEQ d61:LDA err:SBC errs
    STA err:INC cnt:BCS f61
    .d61 RTS
    .a71 LDA err:LDX #&88
    .b71 SBC #dy:BCS by1:ADC #dx:STA err
    TXA:AND c:EOR (scr),Y:STA (scr),Y
    DEC cnt:BEQ e71:.f71 DEY:BPL sb1
    LDA scr:SBC #&48:LDY #7:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP a01
    .e71 DEC ls:BEQ d71:LDA err:SBC errs
    STA err:INC cnt:BCS f71
    .d71 RTS:.by1 STA err
    TXA:AND c:EOR (scr),Y:STA (scr),Y
    .sb1 LDA scr:SBC #8:STA scr:BCC sc1
    JMP a01
    .sc1 DEC scr+1:SEC:JMP a01
    .p02 LDA #&88:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a02 SBC #dx:BCC b02
    DEY:BPL a02:TAX
    .s02 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a02:DEC scr+1:SEC:BCS a02
    .e02 DEC ls:BEQ d02:SBC errs
    INC cnt:BCS n02:.d02 RTS
    .b02 ADC #dy:DEC cnt:BEQ e02
    .n02 TAX:DEY:BPL p52:LDY #7:DEC scr+1:LDA scr:SBC #&40:STA scr:BCS p52:DEC scr+1:SEC
    .p52 LDA #&44:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a52 SBC #dx:BCC b52
    DEY:BPL a52:TAX
    .s52 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a52:DEC scr+1:SEC:BCS a52
    .e52 DEC ls:BEQ d52:SBC errs
    INC cnt:BCS n52:.d52 RTS
    .b52 ADC #dy:DEC cnt:BEQ e52
    .n52 TAX:DEY:BPL p62:LDY #7:DEC scr+1:LDA scr:SBC #&40:STA scr:BCS p62:DEC scr+1:SEC
    .p62 LDA #&22:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a62 SBC #dx:BCC b62
    DEY:BPL a62:TAX
    .s62 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a62:DEC scr+1:SEC:BCS a62
    .e62 DEC ls:BEQ d62:SBC errs
    INC cnt:BCS n62:.d62 RTS
    .b62 ADC #dy:DEC cnt:BEQ e62
    .n62 TAX:DEY:BPL p72:LDY #7:DEC scr+1:LDA scr:SBC #&40:STA scr:BCS p72:DEC scr+1:SEC
    .p72 LDA #&11:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a72 SBC #dx:BCC b72
    DEY:BPL a72:TAX
    .s72 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a72:DEC scr+1:SEC:BCS a72
    .e72 DEC ls:BEQ d72:SBC errs
    INC cnt:BCS n72:.d72 RTS
    .b72 ADC #dy:DEC cnt:BEQ e72
    .n72 TAX:DEY:BMI s82
    LDA scr:ADC #7:STA scr:BCS ac2
    SEC:JMP p02:.ac2 INC scr+1:JMP p02
    .s82 LDY #7:LDA scr:SBC #&38:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP p02
    .p03 LDA #&11:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a03 SBC #dx:BCC b03
    DEY:BPL a03:TAX
    .s03 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a03:DEC scr+1:SEC:BCS a03
    .e03 DEC ls:BEQ d03:SBC errs
    INC cnt:BCS n03:.d03 RTS
    .b03 ADC #dy:DEC cnt:BEQ e03
    .n03 TAX:DEY:BPL p53:LDY #7:DEC scr+1:LDA scr:SBC #&40:STA scr:BCS p53:DEC scr+1:SEC
    .p53 LDA #&22:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a53 SBC #dx:BCC b53
    DEY:BPL a53:TAX
    .s53 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a53:DEC scr+1:SEC:BCS a53
    .e53 DEC ls:BEQ d53:SBC errs
    INC cnt:BCS n53:.d53 RTS
    .b53 ADC #dy:DEC cnt:BEQ e53
    .n53 TAX:DEY:BPL p63:LDY #7:DEC scr+1:LDA scr:SBC #&40:STA scr:BCS p63:DEC scr+1:SEC
    .p63 LDA #&44:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a63 SBC #dx:BCC b63
    DEY:BPL a63:TAX
    .s63 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a63:DEC scr+1:SEC:BCS a63
    .e63 DEC ls:BEQ d63:SBC errs
    INC cnt:BCS n63:.d63 RTS
    .b63 ADC #dy:DEC cnt:BEQ e63
    .n63 TAX:DEY:BPL p73:LDY #7:DEC scr+1:LDA scr:SBC #&40:STA scr:BCS p73:DEC scr+1:SEC
    .p73 LDA #&88:AND c:EOR (scr),Y:STA (scr),Y
    TXA:.a73 SBC #dx:BCC b73
    DEY:BPL a73:TAX
    .s73 LDY #7:DEC scr+1:LDA scr:SBC #&40
    STA scr:TXA:BCS a73:DEC scr+1:SEC:BCS a73
    .e73 DEC ls:BEQ d73:SBC errs
    INC cnt:BCS n73:.d73 RTS
    .b73 ADC #dy:DEC cnt:BEQ e73
    .n73 TAX:DEY:BMI s83
    LDA scr:SBC #8:STA scr:BCC ac3
    JMP p03:.ac3 DEC scr+1:SEC:JMP p03
    .s83 LDY #7:LDA scr:SBC #&48:STA scr
    LDA scr+1:SBC #1:STA scr+1:JMP p03


ENDIF




.end


PRINT "Coordinates data size is ", coordinates_end-coordinates_start, " bytes"
PRINT " Trig table data size is ", trigtable_end-trigtable_start, " bytes"

; Finish up with executable last on the disk
SAVE "Main", start, end, entry




