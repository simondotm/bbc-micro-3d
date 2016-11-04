
;----------------------------------------------------------------------------------------------------------
; Line rendering routine for 2-bits per pixel mode 5, filled/XOR mode
; Not yet annotated
;----------------------------------------------------------------------------------------------------------



.linedraw5f_noline RTS

.linedraw5f
{
    LDA colors,X:STA c
    LSR x0:LSR x1
    LDA x0:CMP x1:BCC itsoknow:BEQ linedraw5f_noline
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
    LDA scr+1:SBC #1:STA scr+1
    JMP p03


    ; 2bpp mode colour lookup table
    ; 0 = WWYR - not used, as no lines are rendered in black
    ; 1 = RRRR
    ; 2 = YYYY
    ; 3 = WWWW
    .colors EQUD&FFF00F00;ED   


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
}
