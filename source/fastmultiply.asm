
; quarter square lookup tables
;  table1 = n*n/4, where n=0..510
;  table2 = (n-255)*(n-255)/4, where n=0..510
;
; table2 + table1 origins must be page aligned



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
; &0000-&01FF = table2 lsb
; &0100-&02FF = table1 lsb
; &0300-&04FF = table2 msb
; &0400-&05FF = table1 msb 
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


