# OpenSES
Development of an open-source fork of version 4.1 of the Subway Environment Simulation (SES) one-dimensional tunnel model.


# Changes in branch 'inp_version'

## Added an input file version number to input files

Added code to read and check a fourth entry in form 1B.  In SES v4.1
form 1B is as follows:
```
17.       7         2022
hour    month       year
```

In this branch, it is
```
17.       7         2022      4.2
hour    month       year    input file
                             version
```
The new number will only be picked up if it is between the 31st and 40th
characters inclusive (same as all the other numbers).  The new files can
still be run in SES v4.1.  The input file version is multiplied by 1000
and stored as an integer in a variable named 'inpver' in the 'DSHARE'
COMMON block.


Added test files for the input file version, see the 'verification' and
'samples' folders.  Ran them all on macOS, linux and Windows and checked
the differences - nothing significant.  Diff file transcripts are included.


## Added support for the 'implicit none' pragma in Fortran 90 and above.

Modified 'DSHARE' and 'DSHRHS' to declare all variables and change the
comment character to work with new versions of Fortran.  Evidence that
it is correct is held in the 'verification/implicit-none' folder.

## Minor changes

1. Modified subroutine GETARGU to handle lower-case input filename extensions on Linux.
2. Modified format statement 1520 in INPUT.FOR to prevent gfortran generating code that prints "DEG  F" instead of "DEG F".
3. Modified 'makefile' to compile .f95 files and check the COMMON files.

