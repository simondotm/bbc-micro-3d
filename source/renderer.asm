
IF WIREFRAME
    linedraw = linedraw4
ELSE
    linedraw = linedraw5f
ENDIF

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


; xor fill the back buffer to the front buffer
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

; copy the back buffer to the front buffer
.fill_copy
{
    LDA#0:LDX#&2F
    .loop5
    FOR Y%, &3A40, &5700, &140
        FOR X%, Y%, Y%+144, 48
           LDA X%,X:STA X%+(&5D40-&3A40),X
        NEXT
    NEXT
    DEX:BMI wiped:JMP loop5
    .wiped 
    RTS
}

ENDIF


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