      subroutine test_implicit()
C     An empty routine for testing changes to DSHARE and DSHRHS.  All it does is
C     import the first versions of 'DSHARE' and 'DSHRHS' that were loaded into
C     the OpenSES github repository (12 April 2021).  These two files contain
C     implicit declarations in common blocks, so for the purpose of this test
C     have been renamed 'DSHARE-implicit' and 'DSHRHS-implicit'.
C
C
C     Compile with "gfortran -fno-align-commons -fdump-parse-tree Dsh-impl.for"
C     to get a transcript of the type and size of each variable.  Diff that
C     against the transcript from compiling Dsh-explicit.for.
C
C
        include  'DSHARE-implicit'
        include  'DSHRHS-implicit'
      return
      end subroutine
