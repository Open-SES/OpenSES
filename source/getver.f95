subroutine GetVersion(str_1B)
! This routine takes an 80-character string (the line that holds form 1B)
! as an argument.
!
! If it finds nothing but whitespace in 31st to 40th characters on the
! line, it issues a warning about the input file not having a version
! number (on the screen and in the output file), then sets the input file
! version variable "inpver" to 4200.
! "Inpver" is defined in the COMMON block in 'DSHARE', so that it can be
! used in other routines to determine what stuff to read and what
! restrictions to apply (such as bugfixes or derating of jet fans due to
! local air temperature).
!
! If there is something in the slice (31:40), we check if it is a number.
! We limit the read to the slice (31:40) in case a future version of
! OpenSES wants to put more stuff on the line, such as a number to
! switch between US customary units and SI units for the input file.
!
! If it finds a number in the slice, it checks the number's value.  If
! the value is under 4.1 it faults.  If the value is over the number in
! in the string SESVER (set in 'DSES.FOR') it also faults, because the
! input file is a later version than this version of OpenSES can process.
!
! If the number passes those tests it multiplies the number by 1000, turns
! it into an integer and stores the result in "inpver".
!
! Restrictions:
!
!  * Major version numbers are expected to be of the format field F3.1,
!    such as 4.2.  But when reading the number this routine reads it as
!    F10.4 because we may have minor versions of input files during
!    development.
!
      implicit none
      include 'DSHARE'

      ! Variables holding the highest allowable input file version this
      ! version of OpenSES can handle, and the actual input file version.
      real :: max_version, inp_version

      character(len = 80), intent(in) :: str_1B

      ! Make a character variable to hold the SES version with spaces in
      ! place of everything except the numbers and decimal points.
      character(len = len(sesver)) :: version

      integer :: int_maxver   ! 1000 * max_version as an integer
      ! Turn SESVER into a floating-point number then into an integer
      ! one thousand times higher.  Doing this might seem a bit weird
      ! but it means we can handle major versions (4.1, 4.2, 4.3 etc.)
      ! and minor versions (such as 4.301) without having to worry
      ! about floating point mismatches when checking numbers.
      ! Interim versions of OpenSES may have letters in the string
      ! (at the time of writing SESVER was "4.3ALPHA").  In order to
      ! prevent this causing problems, we call a routine to remove all
      ! except the numbers and decimal points before we try to read a
      ! number.
      ! This is fragile: if programmers start getting creative with the
      ! version string, then the read statement below will raises error
      ! 1001.  For example, setting SESVER to "4.3.2   " in DSES.FOR
      ! would raise it.
      call ToSpaces(sesver, version)
      read(version, '(f8.3)', err=101) max_version
      int_maxver = int(1000 * max_version)

      ! Skip over the next few lines of code, which are an error raised
      ! by the read statement above.
      goto 102

1001  format(/, '*ERROR* TYPE ', I0, 20X, 30('*'),/, &
       'Ugh.  The SES program version string SESVER that you set in',/,&
       "DSES.FOR is one that a number cannot be read from in the READ",/,&
       'statement in SUBROUTINE GetVersion.  Please change your version',/,&
       'string "',A,'"'," to something that doesn't trigger this error.",/,&
       "For what it's worth, GetVersion changed the version string to",/,&
       '"',A,'" before trying and failing to read a number from it.')
101   write(screen, 1001) 1001, SESVER, version
      write( out,   1001) 1001, SESVER, version
      call exit (1001)

102   continue

      ! Uncomment the line below to test the writing of error no. 1001.
      ! write(screen, 1001) 1001, trim(SESVER), version

      if (trim(str_1B(31:40)) .eq. '') then
        ! There was no input file version number in form 1B.  Either the
        ! user didn't set an input file version in form 1B or they put
        ! the number too far to the right on the line.  Set inpver to
        ! 4200, then complain that they haven't included an input file
        ! version number and point to the relevant part of the line.
111     format("Couldn't find an input file version in the 31st to 40th ",/, &
               "characters of form 1B.",/,A,/,30("_"),10("^"),/, &
               "Defaulting to OpenSES v4.2.  Please add an OpenSES version",/, &
               "number (4.2 to ", A, ") to the input file.")
        write ( out  , 111) trim(str_1B), trim(version)
        write (screen, 111) trim(str_1B), trim(version)

112     format("This file will run as SES v4.1 and give OpenSES output.")
        write (out, 112)
        inpver = 4200
      else
        ! Try to read a floating-point number from the 31st to 40th
        ! characters.
        read(str_1B, fmt='(30X, f10.3)', err=121) inp_version

        ! Turn the input file version into the integer 'inpver' which is
        ! stored in the COMMON block so it can be used in other modules.
        inpver = int(1000 * inp_version)

        ! Skip over the next few lines of code, which are an error raised
        ! by the read statement above.
        goto 122

1002    format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
          'Failed to find an input file version number in the file.  Are',/,&
          'you sure this is an input file for OpenSES?  Such files ought to',/,&
          'have a version number (4.1 to ',A,') after the year in form 1B.',/,&
          'There was something in that slot, but it was not a valid number.')
121     write(screen, 1002) 1002, trim(version)
        write( out,   1002) 1002, trim(version)
        call LineComplaint(str_1B, .True.)
        call exit (1002)

122     continue
        ! Check the number.  The lowest allowable is 4.1.  There is no
        ! difference between the behaviour of version 4.1 and version 4.2
        ! input files, accepting 4.1 is for backwards compatibility.
        if ( inpver .lt. 4100 ) then
1003      format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
          'Found an input file version number that was too low (',A,'),',/,&
          'the lowest valid version is 4.1.  Please edit the file to',/,&
          'correct this.')
          write(screen, 1003) 1003, trim(adjustl(str_1B(31:40)))
          write( out,   1003) 1003, trim(adjustl(str_1B(31:40)))
          call LineComplaint(str_1B, .True.)
          call exit (1002)
        else if ( inpver .gt. int_maxver ) then
1004      format(/, '*ERROR* TYPE ', I0, 20X, 30('*'), /, &
          'Found an input file version number that was higher than this',/,&
          'version of OpenSES can handle (this is OpenSES v',A,', the file',/,&
          'can only be run in v',A,' or higher).  Please either use a newer',/,&
          'version of OpenSES or edit the file to reduce the input file',/,&
          'version to between 4.2 and ',A,'.')
          write(screen, 1004) 1004, trim(sesver), trim(adjustl(str_1B(31:40))),&
                              trim(version)
          write( out,   1004) 1004, trim(sesver), trim(adjustl(str_1B(31:40))), &
                              trim(version)
          call LineComplaint(str_1B, .True.)
          call exit (1003)
        end if

        ! If we get to here we have a valid version of the input file and
        ! have stored it as an integer.  All is well.

!****   Write out the input file version and a description of what it does
!****   to the screen and to the .OUT file.
131     format("Input file is v",A,"...")
        write (screen, 131) trim(adjustl(str_1B(31:40)))

132     format("This OpenSES input file is a version ", A, " input file.")
        write (out, 132) trim(adjustl(str_1B(31:40)))

        if (inpver .le. 4200) then
142       format("This file will run as SES v4.1 but give OpenSES output.")
          write (out, 142)
        else if (inpver .eq. 4300) then
143       format("This file will be processed as an OpenSES 4.3 file,",/,    &
                 "which has the following additional capabilities:",/,       &
                 " * it writes output in JSON format as well as text")
          write (out, 143)
        else
199       format("This version number has not been assigned a description", &
                     /, "in GetVersion, so it must be a development run.")
          write (out, 199)
        end if
      end if
      return

end subroutine GetVersion
