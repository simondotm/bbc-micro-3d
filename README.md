# 3D Rendering Demo for BBC Micro

Written in 6502/BBC BASIC/BeebAsm

Original source written in 1994 by [Nick Jameson](http://nojameson.net/)

BeebAsm port by [Simon M](https://github.com/simondotm)


# Introduction

Nick recently shared one of his archived BBC Micro disks that contained some interesting routines he'd written back in the 90's. A closer look at this revealed there were a few demos on these disks that were presenting some amazing high performance 3D rendered graphics, that were clearly pushing the limits of the BBC Micro's abilities.

To achieve this high performance, Nick had incorporated a number of very innovative coding techniques so we wanted to figure out how that worked!

This led to an annotated version of the code being ported to BeebAsm for the benefit of the 6502/BBC Micro development community, and so here it is.

# Contents



# Techniques

Some of the technical innovations that give the demo it's speed are as follows:
- Fast 3D rotation matrix building using compound angle formulae rather than multiplications
- Fast 3D vertex transform routine, using pre-loaded table look-ups rather than multiplications
- Table based sin/cos tables, with 16-bit fixed point precision
- Fast multiplication based on quarter square tables, but overlapped to used 1536 bytes of RAM instead of the usual 2048 bytes
- Screen space hidden surface removal
- Caching of transformed screen space vertices
- Temporal caching of visible surfaces to minimize number of hidden surface tests
- Optimized unrolled Bresenham line drawing for 1-bit-per-pixel mode 4
- Optimized unrolled Bresenham line drawing for 2-bit-per-pixel mode 5, with XOR filling
- Optimized polygon filling based on XOR scan line buffers
- Optimal model data format enables only the minimum number of lines to be rendered given a list of visible surfaces
- Rendering window, to enable double buffering and/or rapid buffer updates

These routines are particularly well optimized for the 8-bit 6502 CPU.

# Building the project

Use [BeebAsm](https://github.com/tom-seddon/beebasm) to assemble the code and compile the demo disk SSD file.

There is also a build configuration for Visual Studio Code using the [Beeb VSC](https://marketplace.visualstudio.com/items?itemName=simondotm.beeb-vsc) extension. Hit `CTRL+SHIFT+B` to build, and `CTRL+SHIFT+T` to run in BeebEm.



