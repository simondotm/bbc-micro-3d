;----------------------------------------------------------------------------------------------------------
; Fast 3D Rendering Demo
; 6502/BBC Micro
; Original 1994 source code written by Nick Jameson
;----------------------------------------------------------------------------------------------------------

; Create a disk of various BASIC source file demonstrations
PUTBASIC "source\3D.txt", "3D"
PUTBASIC "source\3DSolid.txt", "3DSolid"
PUTBASIC "source\Polyhed.txt", "Polyhed"
PUTBASIC "source\Menu.txt", "Menu"
PUTBASIC "source\Filler.txt", "Filler"
PUTBASIC "source\Line5.txt", "S.Line5"
PUTBASIC "source\Line3D.txt", "S.Line3D"

; Compiled object files for line drawing
PUTFILE "source\Line5.o", "Line5", &2F03
PUTFILE "source\Line3D.o", "Line3D", &26BA

PUTFILE "README.md", "README", 0

; Add the boot file
PUTFILE "source\!Boot.txt", "!Boot", 0

;SAVE "!Boot", start, end, entry




