C     PROGRAM DSES                                                      
C              SUBWAY ENVIRONMENT SIMULATION (SES)                      
C                                                                       
C                   MAIN PROGRAM --  DSES                               
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
      DIMENSION IDNT(8)
      CHARACTER DATEIS*8, TIMEIS*10
C
    1 FORMAT(A1)
    5 FORMAT(/// T16,'END OF SES INPUT VERIFICATION.',I4,                  
     1               ' ERRORS WERE FOUND.' )                                      
    6 FORMAT(/ T16,'EXECUTION OF THIS SUBWAY ENVIRONMENT SIMULATION',  
     1              ' HAS BEEN SUPRESSED AT THE USER''S OPTION.' )                        
    7 FORMAT(/ T16,'EXECUTION OF THIS SUBWAY ENVIRONMENT SIMULATION',
     1             'HAS BEEN SUPRESSED BY INPUT ERRORS.' )                              
    8 FORMAT(/ T16,'EXECUTION OF THIS SUBWAY ENVIRONMENT SIMULATION',  
     1             ' IS TO PROCEED.')                                                    
    9 FORMAT(/ T45,'END OF SIMULATION')                           
   10 FORMAT(/ T16,'***** NOTE- OPTION',I2,' INITIALIZATION FILE ',
     1             'DATA HAS BEEN WRITTEN *****')                                         
   11 FORMAT('OpenSES Version ', A8, / 'Executing...')
   12 FORMAT('Input Verification Started...')
   13 FORMAT('Input Verification Completed, Simulation Commencing...')
   14 FORMAT('Writing Initialization File...')
   15 FORMAT('Performing Environmental Control Estimate...')
C
C
      MONTHS( 1) = 'JANUARY  '
      MONTHS( 2) = 'FEBRUARY '
      MONTHS( 3) = 'MARCH    '
      MONTHS( 4) = 'APRIL    '
      MONTHS( 5) = 'MAY      '
      MONTHS( 6) = 'JUNE     '
      MONTHS( 7) = 'JULY     '
      MONTHS( 8) = 'AUGUST   '
      MONTHS( 9) = 'SEPTEMBER'
      MONTHS(10) = 'OCTOBER  '
      MONTHS(11) = 'NOVEMBER '
      MONTHS(12) = 'DECEMBER '
C
C**** SET SES VERSION
      SESVER='4.3ALPHA'
C**** PROGRAM EQUIVALENT OF INFINITY - REAL VARIABLES
      ZAP=1.0E20
C**** PROGRAM EQUIVALENT OF INFINITY - INTEGER VARIABLES
C**** IZAP = (2**31)-1
      IZAP=2147483647
C**** SET NEW
      NEW=0
C     
C*****FORTRAN LOGICAL UNIT NUMBER OF INPUT FILE                      
      IN  = 5                                                            
C*****FORTRAN LOGICAL UNIT NUMBER OF OUTPUT FILE              
      OUT = 6                                                           
C*****FORTRAN LOGICAL UNIT NUMBER OF RESTART FILE              
      RST = 7                     
C*****FORTRAN LOGICAL UNIT NUMBER FOR STDERR (SCREEN/CONSOLE)   
      SCREEN = 0
      WRITE (SCREEN, 11) SESVER
C     FORTRAN LOGICAL UNIT NUMBER OF JSON FILE
      JSON = 8
C**** OBTAIN THE NAME OF THE INPUT FILE FROM COMMAND LINE
      CALL GETARGU
C**** GET THE DATE AND TIME OF THE SIMULATION FROM SYSTEM CLOCK
      CALL DATE_AND_TIME (DATE=DATEIS,TIME=TIMEIS,VALUES=IDNT)
      TITLE=MONTHS(IDNT(2))
      DATETIME(1:11)=DATEIS(7:8)//' '//TITLE(1:3)//' '//DATEIS(1:4)
      DATETIME(12:20)=' '//TIMEIS(1:2)//':'//TIMEIS(3:4)//':'
     1                  //TIMEIS(5:6)
C

C
C*****CERTAIN PHYSICAL CONSTANTS - DEFINED                              
C                                                                       
C*****ABSOLUTE TEMPERATURE CONVERSION CONSTANT  DEG F TO DEG R          
      ABTEMP=459.67                                                     
C*****CONVERSION FROM BTU TO FT-LBS - (FT-LB)/BTU                       
      BTUFLB=778.0                                                      
C*****SPECIFIC HEAT OF AIR  BTU/(SLUG-DEG F)                            
      CP=7.728                                                          
C*****CONVERSION FROM FT-LBS TO HP - HP/(FT-LB)                         
      FLBHP=1.0/550.0                                                   
C*****CONVERSION FROM FEET TO INCHES - INCHES/FT                        
      FTIN=12.0                                                         
C*****AMCA VALUE OF ACCELERATION CAUSED BY GRAVITY - FT/(SEC**2)        
      GRACC=32.1740                                                     
C*****HEAT OF VAPORIZATION OF WATER - BTU/(LB OF H2O VAPORIZED)         
      HVH2O=1050.0                                                      
C*****CONVERSION OF MILES/HOUR TO FEET/SECOND - ((FT-HR)/(MILES-SEC))   
      MPHFPS=88.0/60.0                                                  
C*****RATIO OF CIRCUMFERENCE OF A CIRCLE TO ITS DIAMETER                
      PI = 3.14159265                                                   
C*****GAS CONSTANT FOR DRY AIR - ((FT/SEC)**2)/DEG R                    
      RDRYAR=53.34                                                      
C*****CONVERSION OF STANDARD ATMOSPHERE TO IN. HG. - (IN. HG.)/(S.A.)   
      SAINHG=29.921                                                     
C*****CONVERSION OF STANDARD ATMOSPHERE TO LBS/SQ IN - LBS/(SQ IN-S.A.) 
      SAPSI=14.696                                                      
C*****STEFAN-BOLTZMAN CONSTANT - BTU/(HR*FT*FT*(DEG R**4))              
      STEFAN=1.714E-09                                                  
C*****THERMAL CONDUCTIVITY OF AIR  BTU/(FT-SEC-DEG F)                   
      THCON=4.27777778E-06                                              
C*****CONVERSION FROM TONS WEIGHT TO LBS - LBS/TON                      
      TONLB=2000.0                                                      
C**** ABSOLUTE VISCOSITY OF AIR  SLUG/(FT-SEC)     (AT 100 DEG F)       
      VISAIR=0.399E-06                                                  
C*****AMCA WATER GUAGE PRESSURE CONVERSION CONS. - (LBS H2O)/(FT*FT*IN) 
      WGCONS=(62.3205-0.075)/12.0                                       
C*****CONVERSION FROM WATTS TO BTU/SEC - BTU/(WATT-SEC)                 
      WTBTUS=9.4866E-04                                                 
C                                                                       
C*****CERTAIN PHYSICAL CONTANTS - COMPUTED                              
C                                                                       
C*****CONVERSION FROM FT-LBS TO BTU - BTU/(FT-LB)                       
      FLBBTU=1.0/BTUFLB                                                 
C*****CONVERSION OF FEET/SECOND TO MILES/HOUR - ((MILES-SEC)/(FT-HR))   
      FPSMPH=1.0/MPHFPS                                                 
C*****PRANDTL NUMBER FOR AIR - PR EQUALS ABOUT 0.72081                  
C*****                         PR**0.6 EQUALS ABOUT 0.82166             
      PR=CP*VISAIR/THCON                                                
C                                                                       
C*****SES INPUT VERIFICATION                                            
C                                                                       
      WRITE (SCREEN, 12)
      CALL INPUT                                                     
C**** END OF INPUT VERIFICATION - IF FATAL ERROR HAS OCCURRED (NEW = 1)     
C**** TERMINATE SIMULATION                                              
      IF (NEW .EQ. 1)GOTO 517      
      NUMY1 = IABS( LIFE )                                              
      WRITE(OUT,5) NUMY1                                                
      IF( LIFE + DEATH ) 302,350,350                                    
  302 IF( DEATH ) 303,304,304                                           
  303 WRITE(OUT,6)                                                      
      GO TO 517                                                         
  304 WRITE(OUT,7)                                                      
      GO TO 517                                                         
C----                                                                   
  350 WRITE(OUT,8)                                                      
      LIFE = NSIMER                                                     
      ISIMOP = 1                                                        
C****
C---- SES SIMULATION                                                    
C                                                                       
      WRITE (SCREEN,13)
  375 CALL SIMLAT( ISIMOP )                                          
      ISIMOP = 2                                                        
C                                                                       
C---- IF 'NEW=1', THE SIMULATION HAS BEEN COMPLETED THROUGH AN ERROR    
C                 CONDITION FROM MEMBER 'SIMLAT'.  SET EXITCODE=16.
C     IF 'NEW=2', THE SIMULATION HAS BEEN COMPLETED THROUGH A NORMAL    
C                 EXIT FROM MEMBER 'SIMLAT'.                           
C     IN ANY CASE, THE SIMULATION WILL TERMINATE.
C                                                                       
      SELECT CASE (NEW)
	  CASE (1)
	    EXITCODE=16
	    GOTO 517                               
	  CASE (2)
	    EXITCODE=100
          GOTO 515
	END SELECT
C                                                                       
C---- ENVIRONMENTAL CONTROL LOAD ESTIMATE
      IF (HCOPT*THOPT*JRATTH) 460,460,410                               
C                                                                       
C---- ENVIRONMENTAL LOAD EVALUATION                                     
C                                                                       
  410 WRITE (SCREEN, 15)
      CALL DTHTS2                                                    
C**** THE SIMULATION HAS BEEN COMPLETED THROUGH AN ERROR CONDITION FROM 
C**** MEMBER 'DTHTS2'.  SET EXITCODE=17.
  460 IF (NEW .EQ. 1)THEN
        EXITCODE=17
        GOTO 517                                            
	END IF
C     IF LAST TIME OR LAST PRINT GROUP, AND JUST RETURNING FROM 
C     ECZ ESTIMATE, PRINT OUT A LAST JSONPIT.
      IF ((NTIME - MAXTIM .GE. 0) .OR. (IPRGRP - NPGRP .GE. 0)) THEN
        NEW = 2
        CALL JSONPIT
      ENDIF

C                                                                       
C---- CHECK IF MAXIMUM TIME OR PRINT GROUPS HAVE BEEN EXHAUSTED         
      IF (NTIME - MAXTIM) 510,512,512                                
  510 IF (IPRGRP - NPGRP) 375,375,512                                
  512 WRITE (OUT,9)                                                
C                                                                       
C---- WRITE INITIALIZATION DATA IN RESTART FILE
C                                                                       
  515 IF (RSTWOPT .EQ. 0)  GO TO 517
      EXITCODE=100                                     
      WRITE (SCREEN,14)
      WRITE (OUT,1) CHAR(12)
      CALL RSTWRITE                                                    
      WRITE (OUT,10)  RSTWOPT
      CLOSE (RST)                              
  517 CONTINUE
      CLOSE (IN)
	CLOSE (OUT)
      CLOSE (JSON)
      CALL EXIT (EXITCODE)                                                              
      END                                                               