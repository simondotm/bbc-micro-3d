
WIREFRAME = TRUE

IF WIREFRAME
    lineroutine=&26BA
    linedraw=&26FA
ELSE
    lineroutine=&2F03
    linedraw=&2F28
ENDIF


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

rx=&22
ry=&23
rz=&24

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
lines=&57
line=&59
odr=&61
space=&63
p=&64
f=&65
flicker=&66
pause=&67

x0=&70:y0=&71
x1=&72:y1=&73
scr=&74
err=&76
errs=&77
cnt=&78
ls=&79
dx=&FF
dy=&7A
scrstrt=&7B
c=&7C



ORG &1400
GUARD lineroutine

.start


MACRO FNperspective
d=&100
oz=&80
FOR Z%, -128, 127
EQUB &FF*d/(d+oz+Z%)+.5
NEXT 
ENDMACRO


MACRO FNsintab
FOR A%, 0, &13F
S% = &1FA0 * SIN( A%*2*PI /256 )+.5
EQUB LO(S%)
NEXT
FOR A%, 0, &13F
S% = &1FA0 * SIN( A%*2*PI /256 )+.5
EQUB HI(S%)
NEXT
ENDMACRO






.perspective FNperspective
.ptsdone SKIP &40 ;EQUS STRING$(&40," ")
.sx SKIP &40 ;EQUS STRING$(&40," ")
.sy SKIP &40 ;EQUS STRING$(&40," ")

.slsb FNsintab
; cos table offsets
clsb=slsb+&40
smsb=slsb+&140
cmsb=clsb+&140


.bits EQUD &08040201:EQUD &80402010
EQUD 0:EQUD 0
EQUD 0:EQUD 0
EQUD &08040201:EQUD &80402010

.vdus
IF WIREFRAME
    EQUB 22,4
ELSE
    EQUB 22,5
ENDIF

    EQUB 23,0,10,32,0,0,0,0,0,0
    EQUB 255

.entry
{

    LDX#0
    STX adr
    STX space:STX p:STX f:STX flicker
    LDA#1:STA pause
    .loop
    LDA vdus,X:BMI nomess:JSR &FFEE
    INX:BNE loop
    .nomess

    JSR load_models

    SEI:LDA#&40:STA &D00:LDX#&FF:TXS

    LDA#0:STA rx
    LDA#&7B:STA ry
    LDA#&C3:STA rz
    CLC
    LDA#&F:STA lmul0+1:STA rmul0+1
    LDA#&12:STA lmul1+1:STA rmul1+1
    LDA#0:TAX:TAY
    STX lhs:STY lhs+1:BCC go
    .loop2
    TXA:ADC lhs:STA lhs:STA(lmul0),Y
    LDA#0:ADC lhs+1:STA lhs+1:STA(lmul1),Y
    INX:.go STY lmul0:STY lmul1
    TXA:ADC lhs:STA lhs:STA(lmul0),Y
    LDA#0:ADC lhs+1:STA lhs+1:STA(lmul1),Y
    INY:BNE loop2
    LDX#0:LDY#&FF
    .loop3
    LDA &F01,Y:STA &E00,X
    LDA &1201,Y:STA &1100,X
    DEY:INX:BNE loop3
    JSR back

IF WIREFRAME
    LDA#&58:STA scrstrt
ELSE
    LDA#&35:STA scrstrt
ENDIF

    .frame
    LDA#&81:LDX#&9D:LDY#&FF:JSR &FFF4
    TYA:BEQ nopress:LDA space:BNE nopress
    JSR modify:LDA#1
    .nopress STA space

IF WIREFRAME
    LDA scrstrt:LSRA:LSRA:LSRA
    LDX#&C:STX &FE00:STA &FE01
    LDA#&81:LDX#&BC:LDY#&FF:JSR &FFF4
    TYA:BEQ nof:LDA f:BNE nof
    LDA flicker:EOR #1:STA flicker:LDA#1
    .nof STA f
    LDA flicker:AND pause:BNE fastandflicker
    CLI:LDA#19:JSR &FFF4:SEI
    .fastandflicker
    LDA scrstrt:EOR #&68:STA scrstrt
ENDIF

    JSR wipe
    LDA#&81:LDX#&C8:LDY#&FF:JSR &FFF4
    TYA:BEQ nop:LDA p:BNE nop
    LDA pause:EOR #1:STA pause:LDA#1
    .nop STA p
    LDA pause:BEQ nrot
    JSR rotate
IF WIREFRAME == FALSE    
    JSR rotate
ENDIF
    .nrot


    JSR matrix
    JSR newpoints
    JSR hiddensurfaceremoval
    JSR hiddenlineremoval
    JSR drawlines
IF WIREFRAME == FALSE
    JSR fill
ENDIF

    JMP frame

}


.rotate
{
    INC rx
    INC ry:INC ry
    INC rz:INC rz:INC rz
    RTS
}

.newpoints
{
    LDA#0:LDX npts
    .loop4
    STA ptsdone,X
    DEX:BPL loop4
    RTS
}

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

.unitvectors
EQUB u00 AND &FF:EQUB u01 AND &FF:EQUB u02 AND &FF
EQUB u10 AND &FF:EQUB u11 AND &FF:EQUB u12 AND &FF
EQUB u20 AND &FF:EQUB u21 AND &FF:EQUB u22 AND &FF
EQUB u00 DIV 256:EQUB u01 DIV 256:EQUB u02 DIV 256
EQUB u10 DIV 256:EQUB u11 DIV 256:EQUB u12 DIV 256
EQUB u20 DIV 256:EQUB u21 DIV 256:EQUB u22 DIV 256

.getcoordinates
{
    LDA ptsdone,X:BPL transform
    LDA sx,X
    LDY sy,X
    RTS
}

.transform
    LDA#&FF:STA ptsdone,X
    .x LDY &8000,X
    SEC:.u00
    LDA &E00,Y:SBC &E00,Y:STA xr
    LDA &1100,Y:SBC &1100,Y:STA xr+1
    SEC:.u10
    LDA &E00,Y:SBC &E00,Y:STA yr
    LDA &1100,Y:SBC &1100,Y:STA yr+1
    SEC:.u20
    LDA &E00,Y:SBC &E00,Y:STA zr
    LDA &1100,Y:SBC &1100,Y:STA zr+1
    .y LDY &8000,X
    SEC:.u01
    LDA &E00,Y:SBC &E00,Y:STA product
    LDA &1100,Y:SBC &1100,Y:STA product+1
    LDA product:CLC:ADC xr:STA xr
    LDA product+1:ADC xr+1:STA xr+1
    SEC:.u11
    LDA &E00,Y:SBC &E00,Y:STA product
    LDA &1100,Y:SBC &1100,Y:STA product+1
    LDA product:CLC:ADC yr:STA yr
    LDA product+1:ADC yr+1:STA yr+1
    SEC:.u21
    LDA &E00,Y:SBC &E00,Y:STA product
    LDA &1100,Y:SBC &1100,Y:STA product+1
    LDA product:CLC:ADC zr:STA zr
    LDA product+1:ADC zr+1:STA zr+1
    .z LDY &8000,X
    SEC:.u02
    LDA &E00,Y:SBC &E00,Y:STA product
    LDA &1100,Y:SBC &1100,Y:STA product+1
    LDA product:CLC:ADC xr:STA xr
    LDA product+1:ADC xr+1:STA xr+1
    SEC:.u12
    LDA &E00,Y:SBC &E00,Y:STA product
    LDA &1100,Y:SBC &1100,Y:STA product+1
    LDA product:CLC:ADC yr:STA yr
    LDA product+1:ADC yr+1:STA yr+1
    SEC:.u22
    LDA &E00,Y:SBC &E00,Y:STA product
    LDA &1100,Y:SBC &1100,Y:STA product+1
    LDA product:CLC:ADC zr:STA zr
    LDA product+1:ADC zr+1
    ASL zr:ROLA:ASL zr
    ADC#&80:TAY:CLC
    LDA#&80:ADC perspective,Y:STA adr:STA adr+2
    LDA#&E:ADC#0:STA adr+1:ADC#3:STA adr+3
    LDA adr:ADC#1:STA adr+4:STA adr+6
    LDA adr+1:ADC#0:STA adr+5:ADC#3:STA adr+7
    LDA yr+1:ASL yr:ROLA:ASL yr
    ADC#&80:TAY:SEC:EOR #&FF:STY zr:STA zr+1
    LDA(adr),Y:LDY zr+1:SBC(adr+4),Y:STA yr
    LDY zr:LDA(adr+2),Y:LDY zr+1:SBC(adr+6),Y
    ASL yr:ADC#&80:STA sy,X
    LDA xr+1:ASL xr:ROLA:ASL xr
    ADC#&80:TAY:SEC:EOR #&FF:STY zr:STA zr+1
    LDA(adr),Y:LDY zr+1:SBC(adr+4),Y:STA xr
    LDY zr:LDA(adr+2),Y:LDY zr+1:SBC(adr+6),Y
    ASL xr:ADC#&80:STA sx,X
    LDY sy,X
    RTS


.matrix
{
    LDY rx:SEC
    LDA#0:SBC slsb,Y:STA m12lsb
    LDA#0:SBC smsb,Y:ASL m12lsb
    ROLA:ASL m12lsb:ROLA:STA m12msb
    TYA:SEC:SBC ry
    TAX:CLC:ADC rz:STA adr+3
    TYA:CLC:ADC ry
    TAY:CLC:ADC rz:STA adr+2:SEC
    LDA slsb,X:SBC slsb,Y:STA m02lsb
    LDA smsb,X:SBC smsb,Y:ASL m02lsb
    ROLA:STA m02msb:CLC
    LDA clsb,Y:ADC clsb,X:STA m22lsb
    LDA cmsb,Y:ADC cmsb,X:ASL m22lsb
    ROLA:STA m22msb
    TYA:SEC:SBC rz:STA adr+4
    TXA:SEC:SBC rz:STA adr+5
    LDA rx:CLC:ADC rz:TAY
    LDA rx:SEC:SBC rz:TAX:SEC
    LDA slsb,Y:SBC slsb,X:STA m10lsb
    LDA smsb,Y:SBC smsb,X:ASL m10lsb
    ROLA:STA m10msb:CLC
    LDA clsb,X:ADC clsb,Y:STA m11lsb
    LDA cmsb,Y:ADC cmsb,X:ASL m11lsb
    ROLA:STA m11msb
    LDA ry:SEC:SBC rz:TAY
    LDA rz:CLC:ADC ry:TAX:SEC
    LDA clsb,X:SBC clsb,Y:STA m21lsb
    LDA cmsb,X:SBC cmsb,Y:ASL m21lsb
    ROLA:STA m21msb:SEC
    LDA slsb,Y:SBC slsb,X:STA m01lsb
    LDA smsb,Y:SBC smsb,X:ASL m01lsb
    ROLA:STA m01msb:CLC
    LDA clsb,Y:ADC clsb,X:STA m00lsb
    LDA cmsb,X:ADC cmsb,Y:ASL m00lsb
    ROLA:STA m00msb:CLC
    LDA slsb,X:ADC slsb,Y:STA m20lsb
    LDA smsb,Y:ADC smsb,X:ASL m20lsb
    ROLA:STA m20msb
    LDY adr+4:LDX adr+3:SEC
    LDA m00lsb:SBC slsb,X:STA m00lsb
    LDA m00msb:SBC smsb,X:STA m00msb:CLC
    LDA slsb,Y:ADC m21lsb:STA m21lsb
    LDA m21msb:ADC smsb,Y:STA m21msb:CLC
    LDA clsb,Y:ADC m20lsb:STA m20lsb
    LDA cmsb,Y:ADC m20msb:STA m20msb:SEC
    LDA m01lsb:SBC clsb,X:STA m01lsb
    LDA m01msb:SBC cmsb,X:STA m01msb:CLC
    LDA clsb,Y:ADC m01lsb:STA m01lsb
    LDA cmsb,Y:ADC m01msb:STA m01msb:CLC
    LDA m21lsb:ADC slsb,X:STA m21lsb
    LDA m21msb:ADC smsb,X:STA m21msb:SEC
    LDA m20lsb:SBC clsb,X:STA m20lsb
    LDA m20msb:SBC cmsb,X:STA m20msb:SEC
    LDA m00lsb:SBC slsb,Y:STA m00lsb
    LDA m00msb:SBC smsb,Y:STA m00msb
    LDX adr+5:LDY adr+2:SEC
    LDA m20lsb:SBC clsb,Y:STA m20lsb
    LDA m20msb:SBC cmsb,Y:STA m20msb:CLC
    LDA m00lsb:ADC slsb,X:STA m00lsb
    LDA m00msb:ADC smsb,X:STA m00msb:CLC
    LDA m21lsb:ADC slsb,X:STA m21lsb
    LDA m21msb:ADC smsb,X:STA m21msb:CLC
    LDA clsb,Y:ADC m01lsb:STA m01lsb
    LDA cmsb,Y:ADC m01msb:STA m01msb:CLC
    LDA slsb,Y:ADC m21lsb:STA m21lsb
    LDA smsb,Y:ADC m21msb:STA m21msb:CLC
    LDA slsb,Y:ADC m00lsb:STA m00lsb
    LDA smsb,Y:ADC m00msb:STA m00msb:CLC
    LDA clsb,X:ADC m20lsb:STA m20lsb
    LDA cmsb,X:ADC m20msb:STA m20msb:SEC
    LDA m01lsb:SBC clsb,X:STA m01lsb
    LDA m01msb:SBC cmsb,X:STA m01msb
    LDX#8
    .loop7
    LDA unitvectors,X:STA adr
    LDA unitvectors+9,X:STA adr+1
    LDA m00msb,X:ASL m00lsb,X:ADC#&80
    LDY#1:STA(adr),Y:LDY#9:STA(adr),Y
    CLC:EOR #&FF:ADC#1
    LDY#4:STA(adr),Y:LDY#&C:STA(adr),Y
    DEX:BPL loop7
    RTS
}

.hiddensurfaceremoval

    LDA#0:STA visible
    LDA oldsurfs:EOR #&FF:STA surfsdone
    LDA oldsurfs+1:EOR #&FF:STA surfsdone+1
    LDA#&F:SEC:SBC nsurfs:BEQ nloop
    TAY:LDA oldsurfs
    .loop8
    ASLA:ROL oldsurfs+1:DEY:BNE loop8
    STA oldsurfs:.nloop
    LDY nsurfs
    .loop9
    ASL oldsurfs:ROL oldsurfs+1
    BCS hidesurface
    JSR clockwisetest:BCS hidesurface
    TYA:ASLA:TAX
    .opposite0 LDA &8000,X:INX
    ORA surfsdone:STA surfsdone
    .opposite1 LDA &8000,X
    ORA surfsdone+1:STA surfsdone+1
    INC visible
    .hidesurface
    ROL surfs:ROL surfs+1
    DEY:BPL loop9
    LDY nsurfs
    LDA bits,Y:STA oldsurfs
    LDA bits+16,Y:STA oldsurfs+1
    .loopA
    LDX visible:CPX maxvis:BEQ doneit
    LDA surfsdone:AND oldsurfs:BNE surfdone
    LDA surfsdone+1:AND oldsurfs+1:BNE surfdone
    JSR clockwisetest:BCS surfdone
    LDA oldsurfs:EOR #&FF:AND surfs:STA surfs
    LDA oldsurfs+1:EOR #&FF:AND surfs+1:STA surfs+1
    TYA:ASLA:TAX
    .opposite2 LDA &8000,X:INX
    ORA surfsdone:STA surfsdone
    .opposite3 LDA &8000,X
    ORA surfsdone+1:STA surfsdone+1
    INC visible
    .surfdone
    LSR oldsurfs+1:ROR oldsurfs
    DEY:BPL loopA
    .doneit
    LDA surfs:STA oldsurfs
    LDA surfs+1:STA oldsurfs+1
    RTS


.clockwisetest 

    STY ls
    .clock0 LDX&8000,Y
    JSR getcoordinates
    STA x0:STY y0:LDY ls
    .clock1 LDX&8000,Y
    JSR getcoordinates
    STA x1:STY y1:LDY ls
    .clock2 LDX &8000,Y
    JSR getcoordinates
    SEC:LDX#0
    SBC x0:BCS x20p
    SBC#0:EOR #&FF:INX
    .x20p STA lmul0
    LDA y1:SBC y0:BCS y10p
    SBC#0:EOR #&FF:DEX
    .y10p STA lmul1
    TYA:LDY#0
    SBC y0:BCS y20p
    SBC#0:EOR #&FF:INY
    .y20p STA rmul0
    LDA x1:SBC x0:BCS x10p
    SBC#0:EOR #&FF:DEY
    .x10p STA rmul1:STX cnt
    CPX#1:TYA
    EOR cnt:AND #1:BEQ compare
    LDY ls:RTS
    .compare BCC bothpos
    JSR multiply
    LDY lhs:CPY rhs
    LDA lhs+1:SBC rhs+1
    LDY ls:RTS
    .bothpos SEC
    JSR multiply
    LDY rhs:CPY lhs
    LDA rhs+1:SBC lhs+1
    LDY ls
    RTS


.multiply
{
    LDY lmul1:TYA
    LDX lmul0:STX lmul1
    SBC lmul0:BCS mabsl
    SBC#0:EOR #&FF
    .mabsl TAX
    LDA(lmul0),Y:SBC &F00,X:STA lhs
    LDA(lmul1),Y:SBC &1200,X:STA lhs+1
    LDY rmul1:TYA
    LDX rmul0:STX rmul1
    SBC rmul0:BCS mabsr
    SBC#0:EOR #&FF
    .mabsr TAX
    LDA(rmul0),Y:SBC &F00,X:STA rhs
    LDA(rmul1),Y:SBC &1200,X:STA rhs+1
    RTS
}

.hiddenlineremoval
{
    LDX nsurfs:LDY#0
    STY line
    STY line+1
    STY line+2
    STY line+3
    STY line+4
    STY line+5
    STY line+6
    STY line+7
    .loopC
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
    DEX:BPL loopC:RTS
    .nosurf TYA:ADC#7:TAY
    DEX:BPL loopC:RTS
}

IF WIREFRAME
.drawlines
    LDA#0:STA lhs+1
    LDA nlines:STA lhs
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
    LDY lhs+1
    .linestarts LDA &8000,Y:PHA
    .lineends LDA &8000,Y:TAX
    JSR getcoordinates
    STA x0:STY y0
    PLA:TAX
    JSR getcoordinates
    STA x1:STY y1
    JSR linedraw
    .noline INC lhs+1:DEC lhs:BPL loop
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

.back 
{
    LDA#coordinates AND &FF:STA odr
    LDA#coordinates DIV 256:STA odr+1
}
.modify
{
    LDA#&FF:STA oldsurfs:STA oldsurfs+1
    LDY#0
    LDA(odr),Y:BMI back:STA npts:INY
    LDA(odr),Y:STA nlines:INY
    LDA(odr),Y:STA nsurfs:INY
    LDA(odr),Y:STA maxvis
    LDA odr:SEC:ADC#3:STA odr:STA x+1
    LDA odr+1:ADC#0:STA odr+1:STA x+2
    LDA odr:SEC:ADC npts:STA odr:STA y+1
    LDA odr+1:ADC#0:STA odr+1:STA y+2
    LDA odr:SEC:ADC npts:STA odr:STA z+1
    LDA odr+1:ADC#0:STA odr+1:STA z+2
    LDA odr:SEC:ADC npts:STA odr:STA clock0+1
    LDA odr+1:ADC#0:STA odr+1:STA clock0+2
    LDA odr:SEC:ADC nsurfs:STA odr:STA clock1+1
    LDA odr+1:ADC#0:STA odr+1:STA clock1+2
    LDA odr:SEC:ADC nsurfs:STA odr:STA clock2+1
    LDA odr+1:ADC#0:STA odr+1:STA clock2+2
    LDA odr:SEC:ADC nsurfs:STA odr:STA opposite0+1:STA opposite1+1:STA opposite2+1:STA opposite3+1
    LDA odr+1:ADC#0:STA odr+1:STA opposite0+2:STA opposite1+2:STA opposite2+2:STA opposite3+2
    LDA odr:SEC:ADC nsurfs:STA odr
    LDA odr+1:ADC#0:STA odr+1
    LDA odr:SEC:ADC nsurfs:STA odr:STA lines
    LDA odr+1:ADC#0:STA odr+1:STA lines+1
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

; De-serialise vertex data (vertices & surfaces)
; Pass A=npts, X/Y=points array
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

    ; copy back interleaved
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

.load_models
{
    FIX_MODEL   model_data_cube, verts_data_cube, surfs_data_cube
    FIX_MODEL   model_data_viper, verts_data_viper, surfs_data_viper
    FIX_MODEL   model_data_cobra, verts_data_cobra, surfs_data_cobra
    rts
}

.coordinates 

MACRO MD_HEADER npts, nlines, nsurfs, maxvis
    EQUB npts-1
    EQUB nlines-1
    EQUB nsurfs-1
    EQUB maxvis
ENDMACRO    

MACRO MD_POINT x, y, z, scale
    EQUB INT(x * scale + 128)
    EQUB INT(y * scale + 128)
    EQUB INT(z * scale + 128)
ENDMACRO




MACRO MD_SURF  p0, p1, p2
    EQUB p0
    EQUB p1
    EQUB p2
ENDMACRO



MACRO MD_OPP    opps
    IF opps > &7f
        EQUW 0
    ELSE
        EQUW 2 ^ opps
    ENDIF
ENDMACRO

MACRO MD_LINE   p0, p1
    EQUD    p1
    EQUD    p0
ENDMACRO

MACRO MD_INDEX  p0, p1
    EQUB p0
    EQUB p1
ENDMACRO





; Cube data
.model_data_cube
CUBE_NPTS = 8
CUBE_NLINES = 12
CUBE_NSURFS = 6
CUBE_MAXVIS = 3
CUBE_SCALE = 1

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



CLEAR lineroutine,lineroutine+1
ORG lineroutine

IF WIREFRAME
    INCBIN "src\Line.o"
ELSE
    INCBIN "src\Line5.o"
ENDIF


.end




; Finish up with executable last on the disk
SAVE "Main", start, end, entry




