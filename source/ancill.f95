! This file contains routines that do boring stuff like string slicing
! and case conversion.


subroutine ToLower(old_string, lc_string)
! This subroutine takes a UTF-8 string, copies it into a new string then
! converts all the letters to lower case.  Non-alphabetic characters
! are left unchanged.
!
      implicit none

      character(len = *),               intent(in) :: old_string
      character(len = len(old_string)), intent(out) :: lc_string

      integer ::  nn, code

      lc_string = old_string
      do nn = 1, len(old_string)
        code = ichar(old_string(nn:nn))
        if ((code .ge. 65) .and. (code .le. 90)) then
          ! This character is an upper case letter, change it to lower case.
          lc_string(nn:nn) = achar(code + 32)
        end if
      end do
end subroutine ToLower


subroutine ToSpaces(old_string, sp_string)
! This subroutine takes a UTF-8 string, copies it into a new string then
! converts everything except numbers and decimal points in it to spaces.
! It is used to to remove letters from the string SESVER so that a number
! can be read from the string, because development versions of OpenSES
! set SESVER to strings like "4.3ALPHA".
!
      implicit none

      character(len = *),               intent(in) :: old_string
      character(len = len(old_string)), intent(out) :: sp_string

      integer ::  nn, code

      sp_string = old_string
      do nn = 1, len(old_string)
        code = ichar(old_string(nn:nn))
        if ((code .lt. 48 .or. code .gt. 57) .and. code .ne. 46) then
          ! This character is not a digit or ".", change it to a space.
          sp_string(nn:nn) = ' '
        end if
      end do
end subroutine ToSpaces


subroutine GetFileStem(filestring, start, end)
! This routine takes a string that represents a filename or path+filename
! and returns the extents of the filestem of the filename.  It works
! on macOS and Windows.
!
! Sample input and output:
!   1                      25       34
!   |                       |       |
!   /Users/tester/Documents/test_file.ses    >>>  start = 25, end = 34
!   C:\Users\tester\Documents\test_file.ses  >>>  start = 27, end = 36
!   test_file.ses                            >>>  start = 1, end = 9
!   test_file                                >>>  start = 1, end = 9
!
! It takes the letters between the last "." and the last "/" or "\",
! whichever is closest to the end of the string.  It limits the search
! for the last "." to the last five characters at the end of the string.
!
!
      implicit none
      character(len = *), intent(in) :: filestring
      integer, intent(out) :: start, end

      integer ::  start1, start2

      ! Get the locations of the last instances of '/' and '\'.
      start1 = index(filestring, '/', .TRUE.)
      start2 = index(filestring, '\', .TRUE.)
      if (start1 .gt. start2) then
        ! We have a forward slash (macOS and linux).  Use it as the boundary.
        start = start1 + 1
      else if (start2 .ne. 0) then
        ! There was a '\' in the string, we're probably on Windows with
        ! a full path name.
        start = start2 + 1
      else
        ! Both start1 and start2 were zero, so neither were in the
        ! string.  Use 1, the first character.
        start = 1
      end if

      ! Check the last part of the filestring for full stops.  We don't
      ! check the whole thing in case there are full stops earlier in
      ! the file name.
      end = index(filestring(len(filestring)-5:), '.', .TRUE.)
      if (end .eq. 0) then
        ! There was no '.' near the end of the string.
        end = len_trim(filestring)
      else
        ! There was a '.' in the string.  Point to the letter before it.
        end = end - 1
      end if

      ! Now that we have set start and end, the calling routine can
      ! pick them up, use a suitable slice of the filestring and add
      ! the correct extension.
      return
end subroutine GetFileStem


subroutine LineComplaint(line, pre_quote)
! Write a standard message complaining about a line of input (quoting the
! faulty line) and giving the name of the file it is in.  If the Boolean
! pre_quote is .True. we include a line with "Faulty line of input is:" at
! the start.  If it is .False. we don't (the routine that called it will
! already have written slightly different text that is more appropriate).
      implicit none
      include 'DSHARE'
      character(len = *), intent(in) :: line
      logical, intent(in) :: pre_quote

      integer :: start, stop

      ! Get the name of the input file out of INPFILE, excluding the
      ! path.  We don't use FILENAME because it might not have the
      ! extension (.inp or .ses) in it.
      call GetFileStem(inpfile, start, stop)

      if (pre_quote) then
10      format('Faulty line of input is:')
        write(screen, 10)
        write(out, 10)
      end if

20    format('> ', A,/, &
             'Faulty file is "',A,'".')
      write(screen, 20) trim(line), trim(inpfile(start:))
      write(out, 20) trim(line), trim(inpfile(start:))
      return
end subroutine LineComplaint

