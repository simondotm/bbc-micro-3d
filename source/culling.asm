

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
; surface index to 16-bit bitmask
; lookup table
;----------------------------------------------------------------------------------------------------------

; used by hiddensurfaceremoval routine to convert a surface id to a bitfield for use with surfs array
; supports maximum 16 surfaces

; SM: could ALIGN 32 this table to ensure no page boundary cycle hits, but might waste a few bytes and not sure its worth the memory/speed tradeoff.
.bits   
        ; lsb table for surface ID[0-7]
        EQUB 1,2,4,8,16,32,64,128
        EQUD 0:EQUD 0
        ; msb table for surface ID[8-15]
        EQUD 0:EQUD 0
        EQUB 1,2,4,8,16,32,64,128

    