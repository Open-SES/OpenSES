The four files in this directory trigger four error messages in subroutine
Getversion related to an input file version number.

The upper-case and lower-case filename extensions are present to check
code added to Subroutine GETARGU to process input files where the extension
is not provided in the run command and SES has to figure out which extension
to use.

Each of the files has been run on macOS, linux and Windows.  The relevant
.OUT files are stored in the subfolder 'differences', along with a transcript
comparing their contents.  The only differences are the times the files
were run and the lower case autocompleted extensions (.ses, .inp) on linux.
