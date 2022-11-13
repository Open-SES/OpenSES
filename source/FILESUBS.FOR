C**********************************************************************
C
C****     OPENRRST        - OPENS THE READ RESTART FILE
C
C**********************************************************************
C**********************************************************************
      SUBROUTINE OPENRRST
C**** THIS SUBROUTINE OPENS THE READ RESTART FILE
C****
      INCLUDE 'DSHARE'
      INTEGER I1, I2
      I1=INDEX(FILENAME,'/',.TRUE.)
      OPEN(UNIT=7,FILE=RESTART,STATUS='OLD', ERR=10103, 
     1     ACTION='READ')
	GOTO 100
C**** ERROR OPENING SPECIFIED READ RESTART FILE 
10103 EXITCODE=103
      CALL EXIT (EXITCODE)
  100 RETURN
      END
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
C****     OPENWRST        - OPENS WRITE RESTART FILE
C
C**********************************************************************
C**********************************************************************
      SUBROUTINE OPENWRST
C**** THIS SUBROUTINE OPENS THE WRITE RESTART FILE
C****
      INCLUDE 'DSHARE'
      OPEN(UNIT=7,FILE=FILENAME(1:IDOT)//'.RST',ERR=10104, 
     1     STATUS='UNKNOWN', ACTION='WRITE')
	GOTO 100
C**** ERROR OPENING SPECIFIED WRITE RESTART FILE 
10104 EXITCODE=104
      CALL EXIT (EXITCODE)
  100 RETURN
      END
C      
C      
C**********************************************************************
C**********************************************************************
C**********************************************************************
      SUBROUTINE GETARGU
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**** THIS SUBROUTINE OBTAINS THE NAME OF THE SPECIFIED INPUT FILE FROM
C**** THE COMMAND LINE, AND OPENS THE INPUT AND OUTPUT FILES.
C**** IT ALSO CHECKS FOR ANY COMMAND LINE SWITCHES.
C**** CURRENT SWITCHES ARE:
C****     /IVONLY     - HALT AFTER INPUT VERIFICATION
C****
C****
C****
      INCLUDE 'DSHARE'

	INTEGER*2 ARGC, FLAG1, II
      INTEGER*2 I1,I2
      CHARACTER*256 ARGV

C     ARGC IS THE NUMBER OF COMMAND LINE ARGUMENTS INCLUDING THE COMMAND
C     ARGV IS THE TEXT OF A PARTICULAR COMMAND LINE ARGUMENT

      FLAG1=0
	IVONLY=0
C**** GET NAME OF INPUT FILE FROM COMMAND LINE
      ARGC=IARGC()
      IF (ARGC .LE. 0) THEN
          !THE COMMAND WAS RUN WITHOUT ANY ARGUMENTS 
          !OTHER THAN THE COMMAND ITSELF EXIT GRACEFULLY
          EXITCODE=51
          CALL EXIT(EXITCODE)
      ENDIF

C     LOOP OVER THE COMMAND LINE ARGUMENTS            
      DO II= 1, ARGC
          CALL GETARG(II,ARGV)          
C         IF (ARGV(1:1) .EQ. '/') THEN !A SWITCH WAS FOUND
C             IF ((ARGV .EQ. '/IVONLY') .OR.
C    1            (ARGV .EQ. '/ivonly')) THEN
C                 IVONLY=1
C             ELSE
C                 !IGNORE THE SWITCH 
C             END IF
          IF (FLAG1 .EQ. 0) THEN !INPUT FILE NOT SELECTED YET
              INPFILE=ADJUSTL(ARGV)
              I1=LEN_TRIM(INPFILE)
              I2=INDEX(INPFILE,'/',.TRUE.)
              IDOT=INDEX(INPFILE(I2+1:I1), '.', .TRUE.)
              IF (IDOT.NE.0) THEN
                  IDOT=I2+(IDOT-1)
              ELSE
                  IDOT=I1
              END IF
	        FILENAME=INPFILE(1:IDOT)
              FLAG1=1
          ELSE
              !IGNORE THE ARGUMENT
          END IF              
      END DO
      IF (FLAG1 .EQ. 0) THEN ! THE INPUT FILE WAS NOT SPECIFIED
	    EXITCODE=52
	    CALL EXIT(EXITCODE)
	END IF

C**** OPEN INPUT AND TEMPORARY OUTPUT FILES
      OPEN(UNIT=5,FILE=INPFILE,ERR=20,STATUS='OLD',
     1     ACTION='READ')
      GOTO 40
C**** INPUT FILE NOT FOUND.  CHECK IF A FILE EXTENSION WAS SPECIFIED
20    IF (IDOT .EQ. I1)THEN
C**** NO EXTENSION WAS SPECIFIED
C**** SEARCH FOR FILE 'FILENAME.INP'
        INPFILE=FILENAME(1:IDOT)//'.INP'
        OPEN(UNIT=5,FILE=INPFILE,ERR=30,STATUS='OLD',
     1       ACTION='READ')
        GOTO 40
C**** FILE "FILENAME.INP" NOT FOUND SEARCH FOR FILE "FILENAME.SES"
30      INPFILE=FILENAME(1:IDOT)//'.SES'
        OPEN(UNIT=5,FILE=INPFILE,ERR=31,STATUS='OLD',
     1       ACTION='READ')
        GOTO 40
C**** FILE "FILENAME.SES" NOT FOUND SEARCH FOR FILE 'FILENAME.inp'
C**** The following snippet of code is useful for people using linux and
C**** who have lots of SES files that end in '.inp' or '.ses'.
31      INPFILE=FILENAME(1:IDOT)//'.inp'
        OPEN(UNIT=5,FILE=INPFILE,ERR=32,STATUS='OLD',
     1       ACTION='READ')
        GOTO 40
C**** FILE "FILENAME.inp" NOT FOUND SEARCH FOR FILE "FILENAME.ses"
32      INPFILE=FILENAME(1:IDOT)//'.ses'
        OPEN(UNIT=5,FILE=INPFILE,ERR=10101,STATUS='OLD',
     1       ACTION='READ')
      ELSE
C**** AN EXTENSION WAS SPECIFIED
C**** ERROR OPENING INPUT FILE
        GOTO 10101
      END IF
40    OPEN(UNIT=6,FILE=FILENAME(1:IDOT)//'.OUT', ERR=10102,
     1     STATUS='UNKNOWN', ACTION='WRITE')
	GOTO 100
C**** ERROR OPENING INPUT FILE 
10101 EXITCODE=101
      CALL EXIT (EXITCODE)
C**** ERROR OPENING TEMPORARY OUTPUT FILE "FILENAME.OUT" 
10102 EXITCODE=102
      CALL EXIT (EXITCODE)
  100 RETURN
      END
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
