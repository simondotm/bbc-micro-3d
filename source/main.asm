;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------
; 
; Ported to BeebAsm & Annotated - by https://github.com/simondotm
;----------------------------------------------------------------------------------------------------------

; ENSURE WIREFRAME & 

; specify true to force multiplication tables to be contiguous in memory
; if false they are separated into different memory locations to free up 768 bytes
; which allows just enough more RAM to have more models in the wireframe demo 
CONTIGUOUS_TABLES = FALSE


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
opt_filled=&6A
opt_filled_db=&6B

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
; included first to ensure page alignment
INCLUDE "source/fastmultiply.asm"
INCLUDE "source/maths.asm"



;----------------------------------------------------------------------------------------------------------
; main loop
;----------------------------------------------------------------------------------------------------------

.entry
{
    ; initialise variables
    LDX#0
    STX adr
    STX space:STX p:STX f:STX flicker:STX cullingdb:STX opt_filled_db
    LDA#1:STA pause
    LDA#255:STA culling
    LDA#1:STA opt_filled

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

IF WIREFRAME==FALSE
    ; check for "L" pressed to toggle XOR poly filler
    LDA#&81:LDX#&A9:LDY#&FF:JSR &FFF4
    TYA:BEQ nol:LDA opt_filled_db:BNE nol
    LDA opt_filled:EOR #1:STA opt_filled:LDA#1
    .nol STA opt_filled_db
ENDIF


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
    LDA opt_filled
    BNE dofill
    JSR fill_copy   ; copy back buffer to front rather than xor fill from back to front
    JMP filldone
    
    .dofill
    JSR fill        ; xor fill back buffer to front buffer

    .filldone
ENDIF

    JMP frame

}

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
; Includes
;----------------------------------------------------------------------------------------------------------

; the following modules contain WIREFRAME conditional code, so cannot be earlier in the file without changing execution address
INCLUDE "source/culling.asm"


IF WIREFRAME
    INCLUDE "source/linedraw4.asm"
ELSE
    INCLUDE "source/linedraw5f.asm"
ENDIF

INCLUDE "source/renderer.asm"

INCLUDE "source/models.asm"


.end


PRINT "Coordinates data size is ", coordinates_end-coordinates_start, " bytes"
PRINT " Trig table data size is ", trigtable_end-trigtable_start, " bytes"
PRINT "Code from", ~start, "to", ~end, ", size is", (end-start), "bytes"




