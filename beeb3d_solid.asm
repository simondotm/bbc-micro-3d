;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------
; 
; Ported to BeebAsm & Annotated - by https://github.com/simondotm
;----------------------------------------------------------------------------------------------------------

; Build options
WIREFRAME = FALSE

INCLUDE "source/main.asm"

; output a binary file of the compiled code (for inclusion in source.asm to build the demo disk)
; compile with "BeebAsm.exe -i beeb3d_solid.asm"
SAVE "beeb3d_solid.bin", start, end, entry
