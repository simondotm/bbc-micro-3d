ALIGN 256


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