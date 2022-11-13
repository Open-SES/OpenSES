subroutine GetVersion(str_1B)
! This routine takes an 80-character string (the line that holds form 1B).
!
! It checks if the slice (31:40) of the string has a floating point number
! in it (integers are acceptable too).  We limit the read to that slice in
! case a future version of OpenSES wants to put more stuff on the line,
! such as a number to switch between US customary units and SI units for
! the input file.
!
! If it finds a number, it checks the number's value.  If the value is
! under 4.1 it faults.  If the value is over the number stored in the
! string SESVER (set in 'DSES.FOR') it also faults, because the input
! file is a later version than this version of OpenSES can process.
!
! If the number passes those tests it multiplies the number by 1000, turns
! it into an integer and stores the result in the variable "inpver".
! "Inpver" is defined in the COMMON block in 'DSHARE', so that it can
! be used in other routines to determine what stuff to read and what
! restrictions to apply (such as bugfixes or derating of jet fans due to
! local air temperature).
!
! Restrictions:
!
!  * Major version numbers are expected to be of the format field F3.1,
!    such as 4.2.  But when reading the number this routine reads it as
!    F10.4 because we may have minor versions of input files during
!    development.

      implicit none
      include 'DSHARE'

      ! The highest allowable input file version (this version of OpenSES
      ! can handle versions from 4.1 up to the number in 'max_version').
      real :: max_version, inp_version

      character(len = 80), intent(in) :: str_1B
      integer :: int_maxver


      ! Turn SESVER into a floating-point number then into an integer
      ! one thousand times higher.  Doing this might seem a bit weird
      ! but it means we can handle major versions (4.1, 4.2, 4.3 etc.)
      ! and minor versions (such as 4.301) without having to worry
      ! about floating point mismatches when checking numbers.
      read(SESVER, '(f8.4)') max_version
      int_maxver = int(1000 * max_version)


      if (trim(str_1B(31:40)) .eq. '') then
1001    format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
          'Failed to find an input file version number in the file.  Are',/,&
          'you sure this is an input file for OpenSES?  Such files ought',/, &
          'to have a number between 4.1 and ',A,' as a fourth entry in',/, &
          'form 1B (after the year).')
        write(screen, 1001) 1001, trim(sesver)
        write( out,   1001) 1001, trim(sesver)
        call LineComplaint(str_1B, .True.)
        call exit (1001)
      end if

      ! The rest of the line is not blank.  Try to read a floating-point
      ! number from the 31st to 40th characters.
      read(str_1B, fmt='(30X, f10.3)', err=101) inp_version


      ! Turn the input file version into the integer 'inpver' which is
      ! stored in the COMMON block so it can be used in other modules.
      inpver = int(1000 * inp_version)

      ! Skip over the next few lines of code, which raise an error if we
      ! can't read a number in the read statement above.
      goto 102

1002  format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
        'Failed to find an input file version number in the file.  Are',/,&
        'you sure this is an input file for OpenSES?  Such files ought to',/,&
        'have a version number (4.1 to ',A,') after the year in form 1B.',/,&
        'There was something in that slot, but it was not a valid number.')
101   write(screen, 1002) 1002, trim(sesver)
      write( out,   1002) 1002, trim(sesver)
      call LineComplaint(str_1B, .True.)
      call exit (1002)
102   continue

      ! Check the number.  The lowest version of input file with a version
      ! string in it is 4.1.
      if ( inpver .lt. 4100 ) then
1003   format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
        'Found an input file version number that was too low (',A,'),',/,&
        'the lowest valid version is 4.1.  Please edit the file to',/,&
        'correct this.')
        write(screen, 1003) 1003, trim(adjustl(str_1B(31:40)))
        write( out,   1003) 1003, trim(adjustl(str_1B(31:40)))
        call LineComplaint(str_1B, .True.)
        call exit (1003)
      else if ( inpver .gt. int_maxver ) then
1004   format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
        'Found an input file version number that was higher than this',/,&
        'version of OpenSES can handle (this is OpenSES v',A,', the file',/,&
        'can only be run in v',A,' or higher).  Please either use a newer',/,&
        'version of OpenSES or edit the file to reduce the input file',/,&
        'version to between 4.1 and ',A,'.')
        write(screen, 1004) 1004, trim(sesver), trim(adjustl(str_1B(31:40))), &
                            trim(sesver)
        write( out,   1004) 1004, trim(sesver), trim(adjustl(str_1B(31:40))), &
                            trim(sesver)
        call LineComplaint(str_1B, .True.)
        call exit (1004)
      end if

      ! If we get to here we have a valid version of the input file and
      ! have stored it as an integer.

!**** Write out the input file version and a description of what it does
!**** to the screen and to the .OUT file.
110   format("Input file is v",A,"...")
      write (screen, 110) trim(adjustl(str_1B(31:40)))

111   format("This OpenSES input file is a version ", A, " input file.")
      write (out, 111) trim(adjustl(str_1B(31:40)))

      if (inpver .le. 4200) then
142     format("This file will run as SES v4.1 but give OpenSES output.")
        write (out, 142)
      else if (inpver .eq. 4300) then
143     format("This file will be processed as an OpenSES 4.3 file,",/,      &
               "which has the following additional capabilities:",/,         &
!***            " * it derates jetfans in fire segments by local density",/,  &
               " * <give detail here>")
        write (out, 143)
      else
199     format("This version number has not been assigned a description", &
                   /, "in GetVersion, so it must be a development run.")
        write (out, 199)
      end if
      return
end subroutine GetVersion
