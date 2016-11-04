;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------
; Ported to BeebAsm & Annotated - by https://github.com/simondotm
;----------------------------------------------------------------------------------------------------------

; This project builds the "beeb3d.ssd" demo disk, containing both the original BASIC code and the BeebAsm ports along with a Menu
; BeebAsm doesn't have a way to reset variables in one compilation run so the build is a bit long winded.
; A pre-requisite for assembling this file is to assemble beeb3d_solid.asm first, to output a beeb3d_solid.bin file
; - this is the executable for the solid demo
; (if using BeebVSC, the tasks.json file includes this pre-requisite)
;
; we then include main.asm and assemble the wireframe demo here, then write the solid executable to the same disk 
; (both demos have the same load & execution address fortunately)


;----------------------------------------------------------------------------------------------------------
; Assemble the wireframe version of the code
;----------------------------------------------------------------------------------------------------------
; Build options
WIREFRAME = TRUE
INCLUDE "source/main.asm"
SAVE "A.Beeb3D", start, end, entry

;----------------------------------------------------------------------------------------------------------
; write the assembled wireframe code to disk too, same code entry point
;----------------------------------------------------------------------------------------------------------
PUTFILE "beeb3d_solid.bin", "A.Beeb3Ds", start, entry

;----------------------------------------------------------------------------------------------------------
; Now create a disk of various BASIC source file demonstrations
;----------------------------------------------------------------------------------------------------------
PUTBASIC "basic\3D.txt", "3D"
PUTBASIC "basic\3DSolid.txt", "3DSolid"
PUTBASIC "basic\Polyhed.txt", "Polyhed"
PUTBASIC "basic\Menu.txt", "Menu"
PUTBASIC "basic\Filler.txt", "Filler"
PUTBASIC "basic\Line5.txt", "S.Line5"   ; for reference
PUTBASIC "basic\Line3D.txt", "S.Line3D"  ; for reference

; Compiled object files for line drawing (loaded by the BASIC programs)
PUTFILE "basic\Line5.o", "Line5", &2F03
PUTFILE "basic\Line3D.o", "Line3D", &26BA

; Add the project readme
PUTFILE "README.md", "README", 0

; Add the boot file
PUTFILE "basic\!Boot.txt", "!Boot", 0

; done



