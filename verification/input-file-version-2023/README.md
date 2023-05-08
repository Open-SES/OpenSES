# Input files for fault testing

The three input files whose names start with "fault" in this directory trigger
three error messages in subroutine Getversion.  All are related to input file
version numbers.

The files have upper-case and lower-case filename extensions to check
code added to Subroutine GETARGU to process input files where the extension
is not provided in the run command and SES has to figure out which extension
to use.

```
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


*ERROR* TYPE 1004                    ******************************
Found an input file version number that was higher than this
version of OpenSES can handle (this is OpenSES v4.3ALPHA, the file
can only be run in v99.9 or higher).  Please either use a newer
version of OpenSES or edit the file to reduce the input file
version to between 4.2 and 4.3.
Faulty line of input is:
> 17.       7         2022      99.9
Faulty file is "fault-1004-GetVersion.inp".
```

# Input files for version testing
The six files whose names begin with "version-test" use all the branches
in getver.f95 that can be reached if none of the three errors above are
triggered.   These branches write output to the screen and to the .OUT
files.  The transcript written to the screen is below.

```
% ../../source/release/OpenSES version-test-01-version-absent
OpenSES Version 4.3ALPHA
Executing...
Input Verification Started...
Couldn't find an input file version in the 31st to 40th
characters of form 1B.
17.       7         2022
______________________________^^^^^^^^^^
Defaulting to OpenSES v4.2.  Please add an OpenSES version
number (4.2 to 4.3) to the input file.
% ../../source/release/OpenSES version-test-02-out-of-slice
OpenSES Version 4.3ALPHA
Executing...
Input Verification Started...
Couldn't find an input file version in the 31st to 40th
characters of form 1B.
17.       7         2022                          4.3
______________________________^^^^^^^^^^
Defaulting to OpenSES v4.2.  Please add an OpenSES version
number (4.2 to 4.3) to the input file.
% ../../source/release/OpenSES version-test-03-4100
OpenSES Version 4.3ALPHA
Executing...
Input Verification Started...
Input file is v4.1...
% ../../source/release/OpenSES version-test-04-4200
OpenSES Version 4.3ALPHA
Executing...
Input Verification Started...
Input file is v4.2...
% ../../source/release/OpenSES version-test-05-4300
OpenSES Version 4.3ALPHA
Executing...
Input Verification Started...
Input file is v4.3...
% ../../source/release/OpenSES version-test-06-4250
OpenSES Version 4.3ALPHA
Executing...
Input Verification Started...
Input file is v4.25...
%
```

