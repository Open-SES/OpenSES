# Changes made to OpenSES source code in this branch

The files in this source code directory are based on the OpenSES
"Develop" branch, cloned on 23 January 2023 using:
```
  git clone -b Develop https://github.com/Open-SES/OpenSES
```

The following changes were made to the source code files.

DSHARE
------
Declared all the variables in the COMMON block, so that new routines can
use 'implicit none' and still import it.

Changed all comment characters from "C" to "!" so that the file can be
included in Fortran 90 and later files.

Changed the character declarations to modern-style Fortran that gfortran's
-pedantic switch doesn't complain about old-style declarations.

Modified the common block to put the widest variable (XV) at the start
and the narrowest (TNUMV) and the character variables at the end.  This
cures gfortran of wanting byte padding in the common block.

Added explicit declaration of integer INPVER in DSHARE and added it to
the common block.  See Record.txt for a description.

Wrote a list of changes to DSHARE since the v4.1 version of it.


DSHRHS
------
Declared all the variables in the COMMON block, so that new routines can
use 'implicit none' and still import it.

Changed all comment characters from "C" to "!" so that the file can be
included in Fortran 90 and later files.


RECORD.TXT
----------
Added entries describing INPVER and SCREEN.

Changed the version at the top to version 4.3 modification level alpha.

Modified the entry for SESVER string to state that it is now 8 characters
long instead of 5.


FILESUBS.FOR
------------
Made two changes in GETARGU, as follows:

Removed the declaration of IDOT as an integer.  IDOT is now explicitly
declared in DSHARE, so the declaration in GETARGU became a re-declaration
and raised a compile error.

Added processing of lower-case input extensions ".ses" and ".inp",
so that users of linux aren't forced to change to upper-case filename
extensions in their file names (this isn't an issue on Windows or
macOS but is useful on linux if, like me, you tend to use ".ses" as
the input file extension).


INPUT.FOR
---------
Increased the size of the array 'TITLINES' to store the line read for
form 1B.

Added a call to subroutine GetVersion() before reading form 1C.

Changed format field 1520 so that the text it prints does not differ
depending on which compiler you use.  The old version of format field
1520 printed "DEG F" to the output file when the executable was compiled
by the Microsoft compiler and printed "DEG  F" when compiled by the
gfortran compiler (the Fortran specification has a lot of ambiguities
and different compilers use different interpretations).


makefile
--------
Modified the echoed strings to work on Gnu make 3.81 on macOS.

Added compilation of .f95 files with the -pedantic switch and without
the -std=legacy switch.

Added code to force recompilation of selected files if the common block
files they rely on are changed.

Added comments storing the "-static-libgfortran" compile switch, which
may be needed when compiling with gfortran on macOS.  Linux and Windows
seem to prefer "-static".


getver.f95
----------
Contains a new subroutine GetVersion.

This takes the line holding form 1B as input and looks for a number in
the string slice 31 to 40.

If it doesn't find a number, it defaults to running as OpenSES v4.2 and
asks the user to add an input file version number to form 1B.

If it finds the number it checks its value (no lower than 4.1, no higher
than the number in the program version (in string variable SESVER).
It multiplies it by 1000 and stores it in DSHARE integer variable 'inpver'.

One complication is that string variable SESVER may contains numbers and
text (at the time of writing SESVER in the Development branch is set
to '4.3ALPHA').
So when reading a number from SESVER, GetVersion makes a copy of the
string, changes all except numbers and decimal points into spaces (for
example '4.3ALPHA' would become '4.3     ' in the copy), then tries to
read the version number from that.
If a programmer were to set the contents of SESVER to '4.3.2   ' then
the reading of a number fails and triggers error 1001.  There is no
input file that can trigger error 1001; you can only trigger it if you
are a programmer who changed the text assigned to SESVER in DSES.FOR.


Errors 1001 to 1004 may be raised.  These are printed to the .OUT file
and to the screen before the program exits with their number as the
exitcode.
The test files in the 'verification' folder raise errors 1002 to 1004.
```
*ERROR* TYPE 1001                    ******************************
Ugh.  The SES program version string SESVER that you set in
DSES.FOR is one that a number cannot be read from in the READ
statement in SUBROUTINE GetVersion.  Please change your version
string "4.52.3XX" to something that doesn't trigger this error.
For what it's worth, GetVersion changed the version string to
"4.52.3  " before trying and failing to read a number from it.

*ERROR* TYPE 1002                    ******************************
Failed to find an input file version number in the file.  Are
you sure this is an input file for OpenSES?  Such files ought to
have a version number (4.1 to 4.3) after the year in form 1B.
There was something in that slot, but it was not a valid number.
Faulty line of input is:
> 17.       7         2022         5.inf
Faulty file is "fault-1002-GetVersion.INP".

*ERROR* TYPE 1003                    ******************************
Found an input file version number that was too low (4.05),
the lowest valid version is 4.1.  Please edit the file to
correct this.
Faulty line of input is:
> 17.       7         2022            4.05
Faulty file is "fault-1003-GetVersion.ses".

**ERROR* TYPE 1004                    ******************************
Found an input file version number that was higher than this
version of OpenSES can handle (this is OpenSES v4.3ALPHA, the file
can only be run in v99.9 or higher).  Please either use a newer
version of OpenSES or edit the file to reduce the input file
version to between 4.2 and 4.3.
Faulty line of input is:
> 17.       7         2022      99.9
<<<<<<< HEAD
Faulty file is "fault-1004-GetVersion.INP".
```

ancill.f95
----------

A routine to hold ancillary routines that convert to lower case, reliably
pick a file name out of a string that may hold a directory path + file name
or just a file name, and write a faulty line of input and file name as
part of an error message.

