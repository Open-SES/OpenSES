      subroutine test_explicit()
C     An empty routine for testing changes to DSHARE and DSHRHS.  All it does is
C     import modified versions of 'DSHARE' and 'DSHRHS'.  In these modified
C     files all the variables are explicitly declared and the common block has
C     been rearranged so that gfortran doesn't need the -fno-align-commons
C     switch.  The inclusion of 'implicit none' checks that no variables
C     have been missed.
C
C     Compile with "gfortran -fdump-parse-tree Dshare-expl.for"
C     to get a transcript of the type and size of each variable.  Diff that
C     against the transcript from compiling Dsh-implicit.for.
C
        implicit none
        include  'DSHARE-explicit'
        include  'DSHRHS-explicit'
      return
      end subroutine
