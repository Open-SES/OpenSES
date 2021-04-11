      SUBROUTINE  VSINS( IVS )                                          
C                                                                       
C      VENTILATION SHAFT INPUT SUBROUTINE                               
C                                                                       
C     INCLUDE                                  ( DATE: 15 AUGUST 1997 ) 
C     
      INCLUDE  'DSHARE'                                                 
C                                                                       
C                                                                       
C                                                                       
   20 FORMAT( 8F10.0 )                                                  
   21 FORMAT( 2F10.0, A36 )                                             
   50 FORMAT('INPUT VERIFICATION FOR VENTILATION SHAFT',           
     1  I5,' -',I3,T56,A36,T112,'FORM 5A' )                             
   52 FORMAT(/,'SECTION TYPE',T79,I14,T96,'(VENTILATION SHAFT)' )    
   53 FORMAT(/,'SECTION TYPE',T79,I14,T96,'(STAIRWAY)' )             
   54 FORMAT(/,'SECTION TYPE',T79,I14,T96,'(SPECIAL VENT SHAFT)')    
   56 FORMAT(/,'NUMBER OF SEGMENTS IN THIS VENT SHAFT', T79,I14,T112,
     1'FORM 5B')                                                        
   57 FORMAT(/,'NUMBER OF SUBSEGMENTS IN RESULTING EQUIVALENT VENT SHAFT
     1', T79,I14 )                                                  
   58 FORMAT(/,'GRATE FREE AREA',T79,F14.1,T96,'SQ FT')              
   59 FORMAT(/,'DESIGN MAXIMUM OUTFLOW AIR VELOCITY AT GRATE',       
     1  T79,F14.0,T96,'FPM'   )                                         
   61 FORMAT(/,'WALL SURFACE TEMPERATURE', T79,F14.1,T96,'DEG F' )   
   62 FORMAT(/,'INITIAL AIR DRY-BULB TEMPERATURE',T79,F14.1,         
     1  T96,'DEG F' )                                                   
   63 FORMAT(/,'INITIAL AIR WET-BULB TEMPERATURE',T79,F14.1,         
     2  T96,'DEG F' )                                                   
   64 FORMAT(/,'STACK HEIGHT',T79,F14.1,T96,'FT' )                   
   65 FORMAT(/,'FAN TYPE',T79,I14,T112,'FORM 5C')                    
   66 FORMAT(/,'FAN TYPE',T79,I14,T96,'(NO FAN)',T112,'FORM 5C')     
   67 FORMAT(/'A VALUE OF ZERO HAS BEEN ENTERED FOR THE DIRECTION ',
     1        'OF FAN OPERATION.  THIS FAN WILL BE TURNED OFF FOR ',
     2        'THE DURATION OF THE SIMULATION')
   68 FORMAT(/,'SIMULATION TIME AFTER WHICH FAN SWITCHES ON',        
     1  T79,I14,T96,'SECONDS' )                                         
   69 FORMAT(/,'SIMULATION TIME AFTER WHICH FAN SWITCHES OFF',       
     1  T79,I14,T96,'SECONDS' )                                         
   70 FORMAT(/,'DIRECTION OF FAN OPERATION',T79,I14,                 
     1  T96,'(OUTFLOW OR EXHAUST)' )                                    
   71 FORMAT(/,'DIRECTION OF FAN OPERATION',T79,I14,                 
     1  T96,'(INFLOW OR SUPPLY)' )
   73 FORMAT(/,'DIRECTION OF FAN OPERATION',T79,I14,                 
     1  T96,'(OFF)' )
   75 FORMAT(/,'SEGMENT CHARACTERISTICS',T112,'FORM 5D'/             
     1  T69,'SEGMENT TOTAL HEAD LOSS COEFFICIENTS' //                   
     2  T17,'LENGTH',T33,'AREA',T46,'PERIMETER',T65,'FORWARD',          
     3  T76,'FORWARD',T90,'BACKWARD',T101,'BACKWARD' /                  
     4  T18,'(FT)',T32,'(SQ FT)',T49,'(FT)',T65,'POSITIVE',             
     5  T76,'NEGATIVE',T90,'POSITIVE',T101,'NEGATIVE' /)                
   77 FORMAT( T6,I4,F12.1,F16.1,F15.1,F18.2,F11.2,F14.2,F11.2 )         
   80 FORMAT(/,'EQUIVALENT VENTILATION SHAFT PROPERTIES' /           
     1  /,T6,'LENGTH',T24,F11.1,3X,'FT',                              
     2  T66,'TOTAL HEAD CHANGE COEFFICIENTS' /                          
     3  /,T6,'AREA',T24,F11.1,3X,'SQ FT',T59,'POSITIVE FLOW',F15.2/   
     4  /,T6,'PERIMETER',T24,F11.1,3X,'FT',T59,'NEGATIVE FLOW',F15.2) 
C                                                                       
C     INPUT FORM 5A                                                     
C                                                                       
      READ(IN,21) DUMY1, DUMY2, NAMVS(IVS)                    
      NUMY1=DUMY1                                                       
      NUMVS(IVS)=NUMY1                                                  
      TYPVS(IVS) = DUMY2                                                
      ISCT = NLSEC + IVS                                                
      ISCTX = TABL11( ISCT )                                            
      WRITE(OUT,50) ISCTX, NUMY1, NAMVS(IVS)                  
      CALL CHECKI( NUMY1, 1, 999, 149 )                                 
C---- CHECK FOR DUPLICATE I.D. NUMBERS                                  
      NUMY1 = IVS - 1                                                   
      IF( NUMY1 ) 104,104,101                                           
  101 DO  103  J = 1, NUMY1                                             
      IF( NUMVS(J) - NUMVS(IVS) ) 103,102,103                           
  102 CALL EERROR( 162 )                                                 
  103 CONTINUE                                                          
  104 DO  1042  J=1,NLS                                                 
      IF( NUMLS(J) - NUMVS(IVS) ) 1042,1041,1042                        
 1041 CALL EERROR( 162 )                                                 
 1042 CONTINUE                                                          
      ITYP=TYPVS(IVS)                                                   
      IF (ITYP .LT.0 .OR. ITYP .GT. 10)  GO TO 109                      
      GO TO (106,107,108,108,108,108,108,108,108,108), ITYP             
  106 WRITE(OUT,52) TYPVS(IVS)                                          
      GO TO 110                                                         
  107 WRITE(OUT,53) TYPVS(IVS)                                          
      GO TO 110                                                         
  108 WRITE(OUT,54)  TYPVS(IVS)                                         
      GO TO 110                                                         
  109 WRITE(OUT,52)  TYPVS(IVS)                                         
      CALL EERROR( 167 )                                                 
      TYPVS(IVS) = 1                                                    
C                                                                       
C     INPUT FORM 5B                                                     
C                                                                       
  110 READ(IN,20) DUMY1,DUMY2,AGVS(IVS),VOMXVS(IVS),DUMY3,DUMY4,        
     1  DUMY5, STAKVS(IVS)                                              
      NVSEG = DUMY1                                                     
      WRITE(OUT,56) NVSEG                                               
      IF( NVSEG ) 111,111,112                                           
  111 CALL EERROR( 172 )                                                 
      GO TO 990                                                         
  112 NSS = DUMY2                                                       
      WRITE(OUT,57) NSS                                                 
      CALL CHECKI( NSS, 1, LMSS, 42 )                                   
C---- ASSIGN SUBSEGMENT LOCATIONS                                       
      ISSL = TNSS + 1                                                   
      TABL5(IVS) = ISSL                                                 
      TNSS = TNSS + NSS                                                 
      TABL6(IVS) = TNSS                                                 
      DO 114 ISS = ISSL,TNSS                                            
  114 TABL7(ISS) = -IVS                                                 
      CALL CHECKI( TNSS, 1, LMSS, 43 )                                  
      IF (NEW .EQ. 1)GOTO 990                                             
  120 WRITE(OUT,58) AGVS(IVS)                                           
      CALL CHECKR( AGVS(IVS), 3.0, 3000.0, 38 )                         
      WRITE(OUT,59) VOMXVS(IVS)                                         
      CALL CHECKR( VOMXVS(IVS), 0.0, 6000.0, 169 )                      
      VOMXVS(IVS) = VOMXVS(IVS) / 60.0                                  
      WRITE(OUT,61) DUMY3                                               
      CALL CHECKR(DUMY3,0.0,130.0,23)                                   
      WRITE(OUT,62) DUMY4                                               
      CALL CHECKR( DUMY4, 0.0, 130.0, 24 )                              
      WRITE(OUT,63) DUMY5                                               
      CALL CHECKR( DUMY5, 0.0, DUMY4, 25 )                              
      WRITE(OUT,64) STAKVS(IVS)                                         
      CALL CHECKR( STAKVS(IVS),-1000.0, 1000.0, 37 )                    
      CALL LATENT( DUMY4, DUMY5, DUMY6 )                                
C---- LOOP OVER SUBSEGMENTS IN VENT SHAFT                               
      DO  130  ISS = ISSL, TNSS                                         
      QERTSS(ISS) = 0.0                                                 
      QERHSS(ISS) = 0.0                                                 
      QRADSS(ISS) = 0.0                                                 
      LHLTSS(ISS) = 0.0                                                 
      SHLTSS(ISS) = 0.0                                                 
      SHUXSS(ISS) = 0.0                                                 
      FFSS(ISS) = 0.0                                                   
      FBSS(ISS) = 0.0                                                   
      HTRNSS(ISS) = 0.0                                                 
      TSFSS(ISS) = DUMY3                                                
      TDBSS(ISS) = DUMY4                                                
      TTMPSS(ISS) = DUMY4                                               
      TWBSS(ISS) = DUMY5                                                
      HUMSS(ISS) = DUMY6                                                
  130 HTMPSS(ISS) = DUMY6                                               
C                                                                       
C---- FAN INFORMATION            FORM 5C                                
      IF( NFNTYP ) 151,151,132                                          
  132 READ(IN,20) DUMY1,DUMY2,DUMY3,DUMY4                               
      FTYPVS(IVS) = DUMY1                                               
      IF( FTYPVS(IVS) ) 155,150,155                                     
  150 WRITE(OUT,66) FTYPVS(IVS)                                         
  151 FTYPVS(IVS) = 0                                                   
      FDIRVS(IVS) = 0.0                                                 
      FTONVS(IVS) = 0                                                   
      FTOFVS(IVS) = 0                                                   
      GO TO 170                                                         
  155 WRITE(OUT,65) FTYPVS(IVS)                                         
      IF( FTYPVS(IVS) - NFNTYP ) 157,157,156                            
  156 CALL EERROR( 170 )                                                 
  157 IF( TYPVS(IVS) - 2 ) 160,158,160                                  
  158 CALL EERROR( 171 )                                                 
  160 NUMY1=DUMY4
C**** IF A ZERO HAS BEEN ENTERED FOR THE DIRECTION OF FAN OPERATION      
C**** (I.E. FAN IS OFF) SET THE FAN TYPE TO ZERO.
      IF (NUMY1 .EQ. 0)THEN
        WRITE(OUT,73) NUMY1                                               
        WRITE(OUT,67)
        FTYPVS(IVS) = 0                                                   
        FDIRVS(IVS) = 0.0                                                 
        FTONVS(IVS) = 0                                                   
        FTOFVS(IVS) = 0                                                   
        GO TO 170                                                         
      END IF
      FTONVS(IVS) = DUMY2                                               
      FTOFVS(IVS) = DUMY3                                               
      WRITE(OUT,68) FTONVS(IVS)                                         
      WRITE(OUT,69) FTOFVS(IVS)                                         
      IF( FTONVS(IVS) - FTOFVS(IVS) )162,162,161                        
  161 CALL EERROR( 76 )                                                  
  162 FTONVS(IVS) = FTONVS(IVS) * 100                                   
      IF( FTONVS(IVS) ) 164,164,163                                     
  163 FTONVS(IVS) = FTONVS(IVS) + 1                                     
  164 FTOFVS(IVS) = FTOFVS(IVS) * 100                                   
      IF( NUMY1 ) 166,165,165                                           
  165 WRITE(OUT,70) NUMY1                                               
      FDIRVS(IVS) = 1.0                                                 
      GO TO 167                                                         
  166 WRITE(OUT,71) NUMY1                                               
      FDIRVS(IVS) = -1.0                                                
C---- DETERMINE SIGN OF OUTFLOW OR EXHAUST                              
  167 NODE = TABL12( ISCT )                                             
      IF( TABL16( NODE ) - 1 ) 170,168,170                              
C --  BACKWARD END IS CONNECTED TO ATMOSPHERE - REVERSE DIRECTION SIGN  
  168 FDIRVS(IVS) = - FDIRVS(IVS)                                       
C                                                                       
C---- SEGMENT CHARACTERISITCS - COMPUTE EQUIVALENT VENT SHAFT   FORM 5D 
  170 WRITE(OUT,75)                                                     
      CEPS(ISCT) = 0.0                                                  
      CENS(ISCT) = 0.0                                                  
      X3 = 0.0                                                          
      X4 = 0.0                                                          
      X5 = 0.0                                                          
      X6 = 0.0                                                          
C---- READ SEGMENT PROPERTIES AND COMPUTE SUMMATIONS                    
      DO  240  ISEG = 1, NVSEG                                          
      READ(IN,20) DUMY1,DUMY2, DUMY3, DUMY4, DUMY5, DUMY6, DUMY7        
      WRITE(OUT,77) ISEG,DUMY1,DUMY2,DUMY3,DUMY4,DUMY5,DUMY6,DUMY7      
C --  LENGTH                                                            
      CALL CHECKR( DUMY1, 0.0, 2000.0, 40 )                             
C --  AREA                                                              
      CALL CHECKR( DUMY2, 3.0, 3000.0, 38 )                             
C --  PERIMETER                                                         
      CALL CHECKR( DUMY3, 5.0, 500.0, 39 )                              
C --  FORWARD POSITIVE                                                  
      CALL CHECKR( DUMY4, 0.0, 1000.0, 28 )                              
C --  FORWARD NEGATIVE                                                  
      CALL CHECKR( DUMY5, 0.0, 1000.0, 28 )                              
C --  BACKWARD POSITIVE                                                 
      CALL CHECKR( DUMY6, 0.0, 1000.0, 28 )                              
C --  BACKWARD NEGATIVE                                                 
      CALL CHECKR( DUMY7, 0.0, 1000.0, 28 )                              
      DUMY8 = (4.0*PI*DUMY2)**0.5                                       
      IF( DUMY3 - DUMY8 ) 153,180,180                                   
  153 CALL EERROR( 70 )                                                  
C                                                                       
  180 IF(ISEG-1) 185,185,190                                            
  185 ALOWS(ISCT) = DUMY2                                               
  190 IF(NVSEG-ISEG) 195,195,200                                        
  195 AHIGHS(ISCT) = DUMY2                                              
  200 CEPS(ISCT) = CEPS(ISCT) + 0.25 * (DUMY4 + DUMY6) / (DUMY2 * DUMY2)
      CENS(ISCT) = CENS(ISCT) + 0.25 * (DUMY5 + DUMY7) / (DUMY2 * DUMY2)
C***** SUM OF L OVER A                                                  
      X3 = X3 + DUMY1/DUMY2                                             
C --  SUM OF SURFACE AREA                                               
      X4 = X4 + DUMY1 * DUMY3                                           
C --  SUM OF VOLUME                                                     
      X5 = X5 + DUMY1 * DUMY2                                           
C*****SUM OF LENGTHS                                                    
  240 X6 = X6 + DUMY1                                                   
      AVS(IVS) = SQRT(X5/X3)                                            
      LVS(IVS) = SQRT(X5*X3)                                            
      BS(ISCT) = X3                                                     
      BCS(ISCT)=BS(ISCT)                                                
      PERIM = X4 / LVS(IVS)                                             
      RNFVS(IVS)=4.0*RHOMAS/(PERIM*VISAIR)                              
      DUMY1=4.0*AVS(IVS)*AVS(IVS)*CEPS(ISCT)                            
      DUMY2=4.0*AVS(IVS)*AVS(IVS)*CENS(ISCT)                            
      WRITE(OUT,80) LVS(IVS),AVS(IVS),DUMY1,PERIM,DUMY2                 
      VOLVSS(IVS) = X5 / FLOAT( NSS )                                   
      SURVSS(IVS) = X4 / FLOAT( NSS )                                   
C---- CHECK IF STACK HEIGHT EXCEEDS SUM OF INDIVIDUAL LENGTHS           
      IF( ABS(STAKVS(IVS)) - X6 ) 255,255,250                           
  250 CALL EERROR( 173 )                                                 
  255 STAKVS(IVS)=STAKVS(IVS)*GRACC                                     
      LVSS(IVS)=LVS(IVS)/NSS                                            
      CALL CHECKR(X6,10.0,2000.0,190)                                   
  990 RETURN                                                            
      END                                                               
