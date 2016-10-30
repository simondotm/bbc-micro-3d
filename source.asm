;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 source code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------

; Create a disk of various BASIC source file demonstrations
PUTBASIC "basic\3D.txt", "3D"
PUTBASIC "basic\3DSolid.txt", "3DSolid"
PUTBASIC "basic\Polyhed.txt", "Polyhed"
PUTBASIC "basic\Menu.txt", "Menu"
PUTBASIC "basic\Filler.txt", "Filler"
PUTBASIC "basic\Line5.txt", "S.Line5"
PUTBASIC "basic\Line3D.txt", "S.Line3D"

; Compiled object files for line drawing
PUTFILE "basic\Line5.o", "Line5", &2F03
PUTFILE "basic\Line3D.o", "Line3D", &26BA

PUTFILE "README.md", "README", 0

; Add the boot file
PUTFILE "basic\!Boot.txt", "!Boot", 0

;SAVE "!Boot", start, end, entry




