      SUBROUTINE  SIMERR( ERTYP )                                       
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INTEGER  ERTYP                                                    
C                                                                       
  501 FORMAT(10X,'ATTEMPT TO EXCEED THE MAXIMUM NUMBER OF SIMULTANEOUS O
     1PERATING TRAINS',/,10X,'THE CURRENT TRAIN WILL NOT BE PLACED INTO 
     2OPERATION.' )                                                     
  502 FORMAT(10X,'DIVISION BY ZERO.',5X,'TIME =',F9.2 )                 
  503 FORMAT(10X,'EXPONENT OVERFLOW.',5X,'TIME =',F9.2 )                
  504 FORMAT(10X,'MORE THAN',I2,' TRAINS ARE IN THE ABOVE SEGMENT.'/    
     1 10X,'THE ABOVE TRAIN WILL BE TEMPORARILY CONSIDERED TO BE OUTSIDE
     2 THE TUNNEL SYSTEM.')                                             
  505 FORMAT(10X,'THERMODYNAMIC VELOCITY-TIME STABILITY CRITERIA HAS BEE
     1N EXCEEDED.')                                                     
  506 FORMAT(10X,'THE FAN IN THE ABOVE VENTILATION SHAFT HAS EXCEEDED EI
     1THER ITS UPPER OR LOWER FLOW LIMIT.' )                            
  507 FORMAT( / ,15X,'THE COEFFICIENT MATRIX FOR THE SET OF AERODYNAMIC 
     1EQUATIONS IS NOT POSITIVE DEFINITE.',/,15X,'THIS IS CAUSED BY:'/, 
     217X,'1. THE TOTAL TRAIN CROSS SECTION AREA OVERLAPPING THE TUNNEL 
     3CROSS SECTION AREA(BLOCKAGE',/,20X,'RATIO GREATER THAN OR EQUAL TO
     4 ONE) OR',/,17X,'2. THE TIME INTERVAL USED FOR THE INTEGRATION OF 
     5THE AERODYNAMIC EQUATIONS IS TOO LARGE.')                         
  508 FORMAT(10X,'THE NUMBER OF ITERATIONS REQUIRED FOR THE HEAT SINK AN
     1ALYSIS TO CONVERGE FOR THE ZONE BELOW HAS EXCEEDED 50.'/10X,'THE H
     2EAT SINK ANALYSIS FOR THE ZONE BELOW PROVIDES THE RESULTS OF THE 5
     31ST ITERATION.'/10X,'TO ENABLE THE HEAT SINK ANALYSIS TO PROPERLY 
     4CONVERGE, DO THE FOLLOWING:'/                                     
     5 15X,'RE-RUN THE PROGRAM AND RE-SET THE INITIAL WALL TEMPERATURES 
     6IN THE ZONE BELOW TO THE WALL'/15X,'TEMPERATURE VALUES PRINTED IN 
     7THIS 51ST ITERATION OF THE HEAT SINK ANALYSIS.')                  
  509 FORMAT(' ')                                                      
  510 FORMAT(' ')                                                       
  511 FORMAT(10X,'ILL-CONDITIONED OR SINGULAR DYNAMIC HUMIDITY RESPONSE 
     1MATRIX')                                                          
  512 FORMAT(10X,'ILL-CONDITIONED OR SINGULAR DYNAMIC THERMAL RESPONSE M
     1ATRIX')                                                           
C                                                                       
C             
      WRITE(OUT,1000)CHAR(12),ERTYP                                             
 1000 FORMAT( A1,/,'SIMULATION *ERROR* TYPE',I4 ,10X,80('*') )           
C                                                                       
      GO TO ( 1,2,3,4,5,6,7,8,9,10,11,12 ), ERTYP                       
C                                                                       
    1 WRITE(OUT,501)                                                    
      GO TO 500                                                         
    2 TIME = NTIME / 100.0                                              
      WRITE(OUT,502)  TIME                                              
      GO TO 500                                                         
    3 TIME = NTIME / 100.0                                              
      WRITE(OUT,503)  TIME                                              
      GO TO 500                                                         
    4 WRITE(OUT,504) LMTRSG                                             
      GO TO 500                                                         
    5 WRITE(OUT,505)                                                    
      GO TO 500                                                         
    6 WRITE(OUT,506)                                                    
      GO TO 500                                                         
    7 WRITE(OUT,507)                                                    
      NEW=1                                                             
      GO TO 500                                                         
    8 WRITE(OUT,508)                                                    
      GO TO 513                                                         
    9 WRITE(OUT,509)                                                    
      GO TO 500                                                         
   10 WRITE(OUT,510)                                                    
      GO TO 500                                                         
   11 WRITE(OUT,511)                                                    
      GO TO 500                                                         
   12 WRITE(OUT,512)                                                    
      GO TO 500                                                         
C                                                                       
C----- NON FATAL ERROR                                                  
C                                                                       
C---- PRINT SYSTEM CONDITIONS AT TIME OF ERROR                          
  500 IKPRNT = KPRNTH                                                   
      KPRNTH = 1000000                                                  
      CALL  PRINT
      CALL  JSONPIT                                                       
      KPRNTH = IKPRNT                                                   
  513 IF (NEW .EQ. 1) GOTO 620                                           
C                                                                       
  606 LIFE = LIFE - 1                                                   
      IF( LIFE ) 620,620,660                                            
  660 RETURN                                                            
C                                                                       
C*****FATAL ERROR EXIT - TERMINATE PROGRAM EXECUTION                    
C                                                                       
  620 WRITE(OUT,1010)                                                   
 1010 FORMAT( / ,9X,'THIS SIMULATION IS TERMINATED BECAUSE THE NUMBER OF
     1 ALLOWABLE SIMULATION ERRORS',/,9X,                              
     2 'OR AN IRRECOVERABLE ERROR HAS BEEN ENCOUNTERED' //)             
      EXITCODE=16
	CALL EXIT (EXITCODE)                                                              
      END                                                               
