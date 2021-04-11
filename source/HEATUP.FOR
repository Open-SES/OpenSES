      SUBROUTINE HEATUP( TICTOC,ISTAGE )                                
C                                                                       
C**** THIS SUBROUTINE COMPUTES THE RESPONSE OF THE WALL SURFACE         
C**** TEMPERATURE TO AN ARBITRARY HEAT FLUX RATE AT WALL SURFACE.  THIS 
C**** COMPUTATION IS PERFORMED FOR FIRE SEGMENTS ONLY.                  
C                                                                       
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
      INCLUDE  'HEATUC'                                                 
C                                                                       
      IF( ISTAGE .GT. 1 ) GO TO 200                                     
C                                                                       
C**********************  INITIALIZATION - STAGE 1  *********************
C                                                                       
C**** THIS LEVEL OF INITIALIZATION IS PERFORMED ONLY ONCE -- DURING     
C**** THE FIRST CALL TO THIS SUBROUTINE                                 
C                                                                       
C**** NOTE -                                                            
C**** THE WALL MATERIAL IS ASSUMED TO BE CONCRETE WITH THE FOLLOWING    
C**** PROPERTIES -                                                      
C**** THERMAL CONDUCTIVITY = 0.7 BTU/(HR-FT-DEG F)                      
C**** THERMAL DIFFUSIVITY  = 0.025 FT**2/HR                             
C****                                                                   
C**** THIS RESULTS IN THE FOLLOWING MULTIPLICATIVE CONSTANT -           
C**** PROPTY = SQRT(1.5 * 0.025/3600.0)/(0.7/3600.0)                    
C****                                                                   
      PROPTY = 16.59850006                                              
      DO 10 ISS = 1, TNLSS                                              
      ICRITL(ISS) = 0                                                   
      S(ISS) = 1.0                                                      
      SIGNS(ISS) = 1.0
	DO 12 ICRPT=1,LMCRPT                                                   
        QMAX(ICRPT,ISS) = 0.0
 12   CONTINUE
      QOLD(1,ISS)=QWALSS(ISS)
	DO 14 ICRPT=2,LMCRPT+1
        QOLD(ICRPT,ISS) = 0.0
 14   CONTINUE
 	DO 16 ICRPT=1,LMCRPT+1                                                   
        THERM(ICRPT,ISS) = 0.0
 16   CONTINUE
      QRTOLD(ISS) = QWALSS(ISS)                                         
      TZERO(ISS) = TSFSS(ISS)                                           
 10   CONTINUE

C                                                                       
C**** TEMPORARILY PRINTOUT MISCELLANEOUS VARIABLES                      
C     WRITE(OUT,20)                                                     
C 20  FORMAT(T5,'NTIME',T15,'SUBSEG.',T27,'TSFSS(ISS)',T43,'S',T47, 
C    1'SIGN',T55,'ICRITL',T65,'THERM1',T76,'THERM2',T87,'THERM3',T98,  
C    2'THERM4',T109,'THERM5',T120,'QRTOLD',/)                           
C                                                                       
C**************************  END OF INITIALIZATION - LEVEL 1  **********
C                                                                       
C                                                                       
C***********************   INITIALIZATION - LEVEL 2  *******************
C                                                                       
C**** THESE COMPUTATIONS ARE PERFORMED DURING THE FIRST TIME INCREMENT  
C**** ONLY                                                              
C                                                                       
C**** LOOP OVER ALL LINE SECTIONS IN THE SYSTEM                         
      DO 130 ISCT = 1,NSEC                                              
C**** CHECK IF THE SECTION IS A VENTILATION SHAFT                       
      IF( TABL9(ISCT) .LT. 0 ) GO TO 130                                
C                                                                       
C**** LOOP OVER EACH LINE SEGMENT IN THE LINE SECTION                   
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      DO 120 ILS = ISEGL,ISEGH                                          
C**** CHECK IF FIRE SEGMENT                                             
      IF( TYPLS(ILS) .GT. 0 ) GO TO 120                                 
C                                                                       
C**** THIS IS A FIRE SEGMENT -- LOOP OVER EACH SUBSEGMENT               
C                                                                       
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      DO 110 ISS = ISSL,ISSH                                            
C**** COMPUTE THE INITIAL CHANGE IN HEAT FLUX RATE                      
      QSLOPE = QWALSS(ISS) - QRTOLD(ISS)                                
C**** INITIALIZE INDICATOR OF SLOPE FOR HEAT FLUX RATE FUNCTION         
      IF( QSLOPE .GE. 0.0 ) GO TO 50                                    
      SIGNS(ISS) = -1.0                                                  
C**** INITIALIZE THE DIRECTION OF HEAT FLOW --                          
C**** (+) = INTO WALL   AND  (-) = OUT OF WALL                          
50    IF( QWALSS(ISS) .GE. 0.0 ) GO TO 60                               
      S(ISS) = -1.0                                                     
60    CONTINUE                                                          
C**** CHECK IF INITIAL VALUE OF HEAT FLUX RATE IS ALSO A MAXIMUM OR     
C**** MINIMUM POINT ( CRITICAL POINT )                                  
      IF( SIGNS(ISS) * S(ISS) .GE. 0.0 ) GO TO 70                        
C                                                                       
C**** THE INITIAL VALUE OF HEAT FLUX RATE IS A CRITICAL VALUE           
C                                                                       
      ICRITL(ISS) = 1                                                   
      QMAX(1,ISS) = QRTOLD(ISS)                                          
70    CONTINUE                                                          
C                                                                       
C**** COMPUTE THE FIRST AND SECOND (IF NECESSARY) THERMAL COMPONENTS    
C**** OF THE WALL SURFACE TEMPERATURE                                   
C                                                                       
      PLUS = 0.0                                                        
      IF( ICRITL(ISS) .GT. 0 ) THEN
	  QNEW=QMAX(1,ISS)
	ELSE
	  QNEW = QWALSS(ISS)                                               
	END IF
C**** COMPUTE AND STORE THE HEAT FLUX (PER UNIT AREA) AT WALL SURFACE   
      THERM(1,ISS) = THERM(1,ISS) + 0.5*( QNEW + QOLD(1,ISS) )* TICTOC    
      THRMQ = ABS( QNEW * THERM(1,ISS) )                               
      THRMQ = SQRT( THRMQ )                                           
      PLUS = PLUS + THRMQ * S(ISS)                                     
      QOLD(1,ISS) = QNEW                                               
C                                                                       
C**** SECOND COMPONENT                                                  
      IF( ICRITL(ISS) .LT. 1 ) GO TO 100                                
      QNEW = QWALSS(ISS) - QMAX(1,ISS)                                  
      THERM(2,ISS) = THERM(2,ISS) + 0.5*( QNEW + QOLD(2,ISS) )* TICTOC    
      THRMQ = ABS( QNEW * THERM(2,ISS) )                               
      THRMQ = SQRT( THRMQ )                                           
      PLUS = PLUS + THRMQ * S(ISS) * (-1.0)                            
      QOLD(2,ISS) = QNEW                                                
C**** COMPUTE WALL TEMPERATURE                                          
100   TSFSS(ISS) = TZERO(ISS) + PLUS * PROPTY                           
      QRTOLD(ISS)= QWALSS(ISS)                                          
C**** END OF SUBSEGMENT DO LOOP                                         
110   CONTINUE                                                          
C**** END OF SEGMENT DO LOOP                                            
120   CONTINUE                                                          
C**** END OF SECTION DO LOOP                                            
130   CONTINUE                                                          
      GO TO 440                                                         
C**************************  END OF INITIALIZATION - STAGE 1  **********
C                                                                       
C**** THE INITILIZATION PHASE HAS BEEN COMPLETED - PROCEED WITH         
C**** COMPUTATIONS                                                      
C                                                                       
C***************************  STAGE 2  *********************************
C                                                                       
C**** LOOP OVER ALL LINE SECTIONS AND OVER ALL THE LINE SEGMENTS WITHIN 
C**** EACH LINE SECTION SEARCHING FOR FIRE LINE SEGMENTS. WHEN A FIRE   
C**** SEGMENT IS FOUND, LOOP OVER EACH SUBSEGMENT AND COMPUTE THE NEW   
C**** WALL SURFACE TEMPERATURE.                                         
C                                                                       
200   CONTINUE                                                          
C                                                                       
C**** TEMPORARILY PRINTOUT MISCELLANEOUS VARIABLES                      
C     WRITE(OUT,20)                                                     
C                                                                       
      DO 430 ISCT = 1,NSEC                                              
C**** CHECK IF SECTION IS A VENTILATION SHAFT                           
      IF( TABL9(ISCT) .LT. 0 ) GO TO 430                                
C                                                                       
C**** LOOP OVER EACH LINE SEGMENT IN THE LINE SECTION                   
C                                                                       
      ISEGL = TABL9(ISCT)                                               
      ISEGH = TABL10(ISCT)                                              
      DO 420 ILS = ISEGL,ISEGH                                          
C**** CHECK IF FIRE SEGMENT                                             
      IF( TYPLS(ILS) .GT. 0 ) GO TO 420                                 
C                                                                       
C**** THIS IS A FIRE SEGMENT -- LOOP OVER EACH SUBSEGMENT IN SEGMENT    
C                                                                       
      ISSL = TABL3(ILS)                                                 
      ISSH = TABL4(ILS)                                                 
      DO 410 ISS = ISSL,ISSH                                            
C**** CHECK IF LIMIT OF CRITICAL POINTS HAS BEEN EXCEEDED               
      IF ( ICRITL(ISS) .GT. LMCRPT ) GO TO 410                               
C**** COMPUTE CHANGE IN HEAT FLUX RATE                                  
      QSLOPE = QWALSS(ISS) - QRTOLD(ISS)                                
C**** CHECK FOR CHANGE IN SLOPE IN HEAT FLUX RATE CURVE                 
      IF( QSLOPE * SIGNS(ISS) .GE. 0.0 ) GO TO 250                       
C                                                                       
      IF( ABS(QSLOPE) .GT. 0.005 ) GO TO 201                            
      QWALSS(ISS) = QRTOLD(ISS)                                         
      GO TO 250                                                         
C                                                                       
C**** THERE HAS BEEN A SIGN CHANGE                                      
201   ICRITL(ISS) = ICRITL(ISS) + 1                                     
      SIGNS(ISS) = -1.0 * SIGNS(ISS)                                      
C                                                                       
C**** CHECK IF THE MAXIMUM ALLOWABLE NUMBER OF CRITICAL POINTS HAS BEEN 
C**** EXCEEDED -- IF SO, DO NOT PROCESS THIS SUBSEGMENT , THEREFORE THE 
C**** WALL TEMPERATURE WILL REMAIN CONSTANT AT ITS LAST COMPUTED VALUE  
C                                                                       
      IF( ICRITL(ISS) - LMCRPT ) 206,206,202                                 
202   CALL INTEXT(ISS,ISCTX,ILSX,ISSX)                                  
      WRITE(OUT,203)CHAR(12)                                            
  203 FORMAT(A1,130('*') )                                             
      DUMY1 = FLOAT(NTIME) / 100.0 + 0.001                              
      WRITE(OUT,204) LMCRPT,DUMY1,ISCTX,ILSX,ISSX,ICRITL(ISS),TSFSS(ISS)       
204   FORMAT(T4,'THE NUMBER OF CRITICAL POINTS IN SUBROUTINE HEATUP HAS 
     1EXCEEDED',I3,' WITHIN THIS SUBSEGMENT.  THE WALL SURFACE',/,T4,   
     2'TEMPERATURE WILL REMAIN CONSTANT FOR THE REMAINDER OF THE SIMULAT
     3ION.',/,T4,'TIME =',F9.2,T20,'SECONDS',T35,I3,' -',I3,' -',I3,    
     4T54,'NO. OF CRITICAL PTS = ',I3,T85,'WALL TEMP. =',F6.1 // )      
      GO TO 410                                                         
206   QMAX(ICRITL(ISS),ISS) = QOLD(ICRITL(ISS),ISS)                                           
C                                                                       
C**** COMPUTE THE ACTIVE THERMAL COMPONENTS OF THE WALL SURFACE         
C**** TEMPERATURE                                                       
C                                                                       
250   NUMY1=ICRITL(ISS)
      PLUS = 0.0
	SSIGN=1.0
	DO 220 I=0, NUMY1
	  IF (NUMY1 .EQ. I) THEN
	    QREF=0.0
		DO 210 J=1, I
		  QREF=QREF+QMAX(J,ISS)
210       CONTINUE
          QNEW=QWALSS(ISS)-QREF
	  ELSE
	    QNEW=QMAX(I+1,ISS)
	  END IF
        THERM(I+1,ISS)=THERM(I+1,ISS)+0.5*(QNEW+QOLD(I+1,ISS))*TICTOC   
        THRMQ=ABS(QNEW*THERM(I+1,ISS))                              
        THRMQ=SQRT(THRMQ)                                             
        PLUS=PLUS+THRMQ*S(ISS)*SSIGN
        QOLD(I+1,ISS)=QNEW
	  SSIGN=-SSIGN
220   CONTINUE
C                                                                       
C**** COMPUTE WALL SURFACE TEMPERATURE                                  
400   TSFSS(ISS) = TZERO(ISS) + PLUS * PROPTY                           
C                                                                       
      QRTOLD(ISS) = QWALSS(ISS)                                         
C                                                                       
C**** TEMPORARILY PRINTOUT MISCELLANEOUS VARIABLES                      
c      WRITE(OUT,30) NTIME,ISS,TSFSS(ISS),S(ISS),SIGNS(ISS),ICRITL(ISS),
c     1              (THERM(I,ISS),I=1,LMCRPT), QRTOLD(ISS)                                                       
c  30  FORMAT(I9,3X,I3,F8.2,F6.1,F6.1,3X,I3,3X,12(F8.3),F8.3)                                                      
C                                                                       
C                                                                       
C**** END OF SUBSEGMENT DO LOOP                                         
410   CONTINUE                                                          
C                                                                       
C**** END OF LINE SEGMENT DO LOOP                                       
420   CONTINUE                                                          
C                                                                       
C**** END OF LINE SECTION DO LOOP                                       
430   CONTINUE                                                          
C                                                                       
C                                                                       
440   CONTINUE                                                          

      RETURN                                                            
      END                                                               
