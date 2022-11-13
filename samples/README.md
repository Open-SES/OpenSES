Two input files in this folder (inferno.inp and normal.inp) have been
modified to run with a version number in form 1B.  They work in both
SES v4.1 and OpenSES v4.2.

The third input file is a test file from offline-SES that is one of many
used to figure out the variation of pressure loss factor with tee junction
flow ratio.

Each of the files has been run on macOS, linux and Windows.  The relevant
.OUT files are stored in the subfolder 'differences', along with a transcript
comparing their contents.  There are slight differences in the volume
flows and heat gains, due to how the different operating systems handle
floating-point calculations.
