# Some notes on these executables

These executables all read an input file version number as the fourth
number in form 1B.  Versions between 4.1 and a number read from the
variable 'SESVER' are allowed (currently 4.3).  If the input file version
number is not present it will use 4.2.

* The file "OpenSES-macOS" was compiled for macOS under Apple Silicon with
the "-static-libgfortran" compiler switch.  It won't work on macs that
use Intel chips.

* The file "OpenSES-linux" was compiled for Linux Mint 18.1 Cinnamon 64-bit
with the "-static" compiler switch.

* The file "OpenSES.exe" was compiled on Windows 10 with the "-static"
compiler switch.
