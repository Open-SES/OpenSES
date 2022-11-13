This is a readme for a set of files that demonstrate that the two common
block files "DSHARE-explicit" and "DSHRHS-explicit" create the same set
of variables as the two files "DSHARE-implicit" and "DSHRHS-implicit"
(same variable type, same array dimensions).

SES v4.1 has two files that define common blocks: DSHARE and DSHRHS.  These
are imported into the main Fortran routines.  In DSHARE and DSHRHS some of
the variables are not explicitly declared.  Instead they are declared in
the COMMON statements using Fortran's default rule "variables starting with
I to N are integers, variables starting with A to H & O to Z are real
numbers".

Implicitly defining variables allows dangerous errors to occur, so modern
Fortran (Fortran 77 and later) has a pragma statement "implicit none" that
forces programmers to declare all variables before using them.  The
advantage of using "implicit none" in new Fortran files is that it
eliminates an entire class of errors that can occur when you mistype
something.

The most famous example is the typo "DO 10 I = 1.10" from early in the
US space program.  The programmer typed a decimal point instead of a
comma - the code ought to have read
```
"DO 10 I = 1,10".
------------^
```
If implicit declarations are allowed, "DO 10 I = 1.10" creates a new
variable named "DO10I" and assigns it the value 1.10.

If implicit declarations are not allowed, the compiler reads the line
"DO 10 I = 1.10" and raises an error asking the programmer to declare
the variable DO10I.  The programmer will eventually spot the mistake
and change it so that the code runs a DO loop with 10 iterations.
Source:    https://arstechnica.com/civis/viewtopic.php?t=862715


The two short Fortran files "Dsh-explicit.for" and "Dsh-implicit.for"
import the relevant common blocks.  "Dsh-expl.for" starts with the
statement "implicit none", forbidding implicit declarations.

Both files were compiled with gfortran's "-fdump-parse-tree" option, which
tells gfortran to print the type of every variable, whether it is implicitly
or explicitly declared, and the array dimensions.  These transcripts were
saved to the files "transcript-implicit.txt" and "transcript-explicit.txt".
Command-line instructions were:
```
% gfortran -fno-align-commons -fdump-parse-tree Dsh-implicit.for > transcript-implicit.txt
% gfortran -fdump-parse-tree Dsh-explicit.for > transcript-explicit.txt
```
"transcript-diff.txt" is the output of a diff on the command line:
```
% diff -b transcript-implicit.txt transcript-explicit.txt > transcript-diff.txt
```
This lists all the differences, ignoring changes in whitespace.

The only differences are:
 * the namespace rules (at the top of the transcript)
 * the subroutine names
 * the absence of implicit tags in "transcript-explicit.txt".
The type and array sizes are all the same - there are no differences in
Fortran type spec.

If you compile the implicit routine without the "-fno-align-commons" flag
the compiler warns about byte padding.  The order of entries in the common
block has been rearranged in "DSHARE-explicit" so that the flag is not
needed.
