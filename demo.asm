;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------
; Ported to BeebAsm & Annotated - by https://github.com/simondotm
;----------------------------------------------------------------------------------------------------------

; Build options
WIREFRAME = FALSE

INCLUDE "source/main.asm"

SAVE "Main", start, end, entry
